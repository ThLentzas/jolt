use crate::parsing::error::{StringError, StringErrorKind};
use crate::parsing::escapes::{self, EscapeError, EscapeErrorKind};
use crate::parsing::utf8;
use crate::parsing::value::error::{PointerError, PointerErrorKind};

// when we split the input pointer path to create tokens, apart from the value we also need to know
// where the token started based on the underline buffer for better error messaging
//
// we are building the token value based on the pointer path, but we will need to do replacements
// (replacing ~0 and ~1), RefTokens must own their data rather than referencing slices of the
// original pointer path.
pub(super) struct RefToken {
    pub(super) val: String,
    pub(super) pos: usize,
}

pub(super) struct Pointer<'a> {
    buffer: &'a [u8],
    pos: usize,
}

impl<'a> Pointer<'a> {
    pub(super) fn new(buffer: &'a [u8]) -> Self {
        Self { buffer, pos: 0 }
    }

    //  when we encounter '/' we treat it as a delimiter, and we create a token(apart from the 1st one)
    //
    //  if we have '/' as part of a key name we need to escape it, ~1 -> /
    //  /foo/bar evaluates to: token_1: foo, token_2: bar but /foo~1bar evaluates to token: foo/bar
    //  the order matters; if we map first and then try to split we will end up with the wrong tokens
    //  /foo~1bar -> /foo/bar -> token_1: foo, token_2: bar which is incorrect
    //  The same logic applies if we want to escape `~` we write it as `~0`
    //
    //  To handle this correctly, we must process the input character-by-character in a single pass:
    //
    //  For input "/foo~1bar", we:
    //  1. Skip the initial '/' delimiter
    //  2. Build the current token character by character
    //  3. When we hit '~1', we immediately convert it to '/' and add to token: "foo/"
    //  4. Continue building the same token: "foo/b", "foo/ba", "foo/bar"
    //  5. When we hit the end of input (or another unescaped '/'), we finalize the token
    //
    //  This way the '/' from ~1 becomes part of the token's content rather than being mistaken for a
    //  delimiter. We never rescan or reprocess characters, each character in the input is examined
    //  exactly once. We handle cases like "~01" where "~0" becomes '~' and then "~1" becomes "/", "~01"
    //  is just "~1"
    pub(super) fn gen_ref_token(&mut self) -> Result<Option<RefToken>, StringError> {
        let len = self.buffer.len();
        if self.pos >= len {
            return Ok(None);
        }

        let mut token = String::new();
        // similar validation to Json String, the only difference is that because pointer is &str,
        // it is guaranteed that any utf8 byte sequence we encounter is always valid so we can skip
        // that step https://doc.rust-lang.org/std/primitive.str.html
        let mut current = self.buffer[self.pos];
        let mut start = self.pos;
        while self.pos < len && current != b'/' {
            match current {
                c if c.is_ascii_control() => {
                    return Err(StringError {
                        kind: StringErrorKind::InvalidControlCharacter { byte: current },
                        pos: self.pos,
                    });
                }
                b'\\' => {
                    escapes::check_escape_character(self.buffer, self.pos)?;
                    let ch = escapes::map_escape_character(self.buffer, self.pos);
                    self.pos += escapes::len(self.buffer, self.pos) - 1;
                    if ch == '~' {
                        // check the next character in the buffer after the Unicode sequence mapped to `~`
                        token.push(self.check_pointer_escape()?);
                    }
                }
                b'~' => token.push(self.check_pointer_escape()?),
                c if c.is_ascii() => token.push(c as char),
                _ => {
                    token.push(utf8::read_utf8_char(self.buffer, self.pos));
                    // without - 1 we would move i to the next character after the sequence then i += 1;
                    // gets executed, and we skip a character
                    self.pos += utf8::utf8_char_width(current) - 1;
                }
            }
            self.pos += 1;
            if self.pos < len {
                current = self.buffer[self.pos];
            }
        }

        // at this point we either traversed the entire buffer or we encounter '/'; if we encountered '/'
        // we need to consider the edge case below and move pos to the 1st character past '/' for subsequent
        // calls to start from the correct position
        // empty strings are allowed as keys in objects("//foo/bar"); for an
        // empty string there is no starting index so we set pos to usize:MAX
        // if start and pos are the at the same index and the current char is '/' it means we
        // encountered an empty string as key
        if current == b'/' {
            start = if start == self.pos { usize::MAX } else { start };
            self.pos += 1;
        }

        // We never check if token exceeds the StringValueLengthLimit because even if it does, no key
        // will ever map to that and we will return None
        Ok(Some(RefToken {
            val: token,
            pos: start,
        }))
    }

    pub(super) fn check_start(&mut self) -> Result<(), PointerError> {
        // neither '/', nor a Unicode sequence that could map to '/'
        // a pointer path always starts with '/', unless it is empty which we already checked by the time
        // this method gets called
        if self.buffer[0] != b'/' && self.buffer[0] != b'\\' {
            return Err(PointerError {
                kind: PointerErrorKind::InvalidPointerSyntax,
                pos: 0,
            });
        }
        self.pos += 1; // move past '/' or '\'

        // was an escape sequence but didn't map to '/'
        if self.buffer[0] == b'\\' {
            if let Err(e) = escapes::check_escape_character(self.buffer, 0) {
                return Err(PointerError::from(StringError::from(e)));
            }
            if escapes::map_escape_character(self.buffer, 0) != '/' {
                return Err(PointerError {
                    kind: PointerErrorKind::InvalidPointerSyntax,
                    pos: 0,
                });
            }
            self.pos += 5; // move past the sequence that mapped to '\'
        }

        Ok(())
    }

    // When this method gets called buffer[pos] is at '~', we only need to check the next character
    // This method is only used by our pointer, and we can mutate the pos index. In escapes and utf8,
    // we don't mutate the index we just advance after the method returns accordingly
    fn check_pointer_escape(&mut self) -> Result<char, EscapeError> {
        let next = self.buffer.get(self.pos + 1);

        // advance the pointer of the buffer to the next character
        match next {
            Some(b'0') => {
                self.pos += 1;
                Ok('~')
            }
            Some(b'1') => {
                self.pos += 1;
                Ok('/')
            }
            Some(b'\\') => {
                // an escape sequence that could map to '0' or '1'
                escapes::check_escape_character(self.buffer, self.pos)?;
                let ch = escapes::map_escape_character(self.buffer, self.pos + 1);
                match ch {
                    '0' => {
                        self.pos += 6;
                        Ok('~')
                    }
                    '1' => {
                        self.pos += 6;
                        Ok('/')
                    }
                    _ => Err(EscapeError {
                        kind: EscapeErrorKind::UnknownEscapedCharacter { byte: b'\\' },
                        pos: self.pos + 1,
                    }),
                }
            }
            // ~8
            Some(b) => Err(EscapeError {
                kind: EscapeErrorKind::UnknownEscapedCharacter { byte: *b },
                pos: self.pos + 1,
            }),
            // ~ unpaired
            None => Err(EscapeError {
                kind: EscapeErrorKind::UnexpectedEof,
                pos: self.pos,
            }),
        }
    }
}

// This method does not depend on the state of the pointer
pub(super) fn check_array_index(token: &RefToken) -> Result<Option<usize>, PointerError> {
    let first = token.val.chars().nth(0);
    let second = token.val.chars().nth(1);

    match (first, second) {
        (Some('+') | Some('-'), Some('0'..='9')) => {
            return Err(PointerError {
                kind: PointerErrorKind::InvalidIndex {
                    message: "index can not be prefixed with a sign",
                },
                pos: token.pos,
            });
        }
        (Some('0'), Some('0'..='9')) => {
            return Err(PointerError {
                kind: PointerErrorKind::InvalidIndex {
                    message: "leading zeros are not allowed",
                },
                pos: token.pos,
            });
        }
        (Some('-'), None) => return Ok(None),
        // if the token val does not start with a digit, it is invalid
        (Some(d), _) if !d.is_ascii_digit() => {
            return Err(PointerError {
                kind: PointerErrorKind::InvalidIndex {
                    message: "invalid array index",
                },
                pos: token.pos,
            });
        }
        _ => (),
    }

    // token had a digit as the first character, but it was not parsable to usize, we treat that
    // as not found case, not an error
    match token.val.parse::<usize>() {
        Ok(index) => Ok(Some(index)),
        Err(_) => Ok(None),
    }
}
