use crate::parsing::error::{StringError, StringErrorKind};
use crate::parsing::escapes::{self, EscapeError, EscapeErrorKind};
use crate::parsing::utf8;
use crate::parsing::value::error::{PointerError, PointerErrorKind};
use std::iter::Peekable;
use std::str::Chars;

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
                        token.push(self.map_pointer_escape()?);
                    }
                }
                b'~' => token.push(self.map_pointer_escape()?),
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
    fn map_pointer_escape(&mut self) -> Result<char, EscapeError> {
        // advance the pointer of the buffer to the next character
        match self.buffer.get(self.pos + 1) {
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

// returns a pointer path if npath is valid, None otherwise
//
// npath: $['store']['book'][0]['title']
// pointer path: /store/book/0/title
pub fn to_ptr_path(npath: &str) -> Option<String> {
    if !npath.starts_with('$') {
        return None;
    }

    let mut path = String::new();
    // npath: "$" -> ptr_path: ""
    if npath.len() == 1 {
        return Some(path);
    }
    path.push('/');
    let mut chars = npath[1..].chars().peekable();

    // parse '[' -> parse key/index -> parse ']'
    while chars.peek().is_some() {
        if chars.next().unwrap() != '[' {
            return None;
        }
        match chars.peek() {
            Some(c) if c.is_ascii_digit() => parse_index(&mut chars, &mut path),
            // keys in a npath always start with single quote
            Some('\'') => {
                if let Some(key) = parse_key(&mut chars) {
                    path.push_str(&key);
                } else {
                    return None;
                }
            }
            Some(_) | None => return None,
        }
        path.push('/');
        // after parsing a key/index we expect ']'
        match chars.peek() {
            Some(c) if *c == ']' => {
                chars.next();
            }
            Some(_) | None => return None,
        }
    }
    // remove the last '/',
    // ['store']['book'][0]['title'] -> /store/book/0/title and not /store/book/0/title/
    path.pop();
    Some(path)
}

fn parse_key(chars: &mut Peekable<Chars>) -> Option<String> {
    chars.next(); // consume opening single quote '
    let mut key = String::new();

    while let Some(c) = chars.peek() {
        match *c {
            '\\' => {
                chars.next();
                match chars.peek() {
                    // 'tit\'le' escaped single quote in npath is mapped to tit'le
                    Some('\'') => {
                        key.push('\'');
                        chars.next();
                    }
                    Some('\\') => {
                        key.push('\\');
                        chars.next();
                    }
                    // same logic applies as npath '\' and 'n' stay as is and represent the '\n'
                    Some(c) if matches!(*c, 'b' | 'f' | 'n' | 'r' | 't') => {
                        key.push('\\');
                        key.push(*c);
                        chars.next();
                    }
                    Some('u') => {
                        chars.next();
                        let mut hex = String::with_capacity(4);
                        for _ in 0..4 {
                            if let Some(c) = chars.next() {
                                // normal-HEXDIG = DIGIT / %x61-66    ; "0"-"9", "a"-"f"
                                // in a valid npath, a Unicode sequence can only appear with lowercase
                                // letters
                                if !c.is_ascii_digit() && !c.is_lowercase() {
                                    return None;
                                }
                                hex.push(c);
                            } else {
                                // Not enough characters to form a Unicode sequence
                                return None;
                            }
                        }
                        let cp = u16::from_str_radix(&hex, 16).ok()?;
                        // the only Unicode sequences that can appear as \u{....} in an npath are in
                        // the 00..1f range, apart from b, f, n, r, t
                        if matches!(cp, 0x08 | 0x09 | 0x0A | 0x0C | 0x0D) || cp >= 0x20 {
                            return None;
                        }
                        key.push_str("\\u");
                        // push_str clones and appends the bytes of key; this is why it works with &key
                        // despite key being a local variable
                        key.push_str(&hex);
                    }
                    Some(_) | None => {
                        return None;
                    }
                }
            }
            // forward slash as a literal in a pointer path is represented as ~1
            '/' => {
                key.push('~');
                key.push('1');
                chars.next();
            }
            // tilde as a literal in a pointer path is represented as ~0
            '~' => {
                key.push('~');
                key.push('0');
                chars.next();
            }
            // closing single quote
            '\'' => {
                chars.next();
                break;
            }
            c if c.is_ascii_control() => return None,
            c => {
                key.push(c);
                chars.next();
            }
        }
    }

    // we never encountered a closing single quote '
    if chars.peek().is_none() {
        return None;
    }
    Some(key)
}

// parse_index() reads as long as it encounters a digit; any syntax error is handled by to_ptr_path()
// this is why we can modify path directly; we don't have to return an Option<String> we could also
// return a String either would work
fn parse_index(chars: &mut Peekable<Chars>, path: &mut String) {
    while let Some(c) = chars.peek() {
        if c.is_ascii_digit() {
            path.push(*c);
            chars.next();
        } else {
            break;
        }
    }
}

#[cfg(test)]
mod test {
    use crate::macros::json;
    use crate::parsing::error::{StringError, StringErrorKind};
    use crate::parsing::value::error::{PointerError, PointerErrorKind};
    use crate::parsing::value::IndexMap;

    fn invalid_paths() -> Vec<(&'static str, PointerError)> {
        vec![
            // does not start with '/'
            (
                "foo/bar",
                PointerError {
                    kind: PointerErrorKind::InvalidPointerSyntax,
                    pos: 0,
                },
            ),
            // does not start with the Unicode sequence of  '/'
            (
                "\\u005E",
                PointerError {
                    kind: PointerErrorKind::InvalidPointerSyntax,
                    pos: 0,
                },
            ),
            // unpaired pointer escape
            (
                "/foo/bar~",
                PointerError::from(StringError {
                    kind: StringErrorKind::UnexpectedEndOf,
                    pos: 8,
                }),
            ),
            // unknown pointer escape
            (
                "/foo/bar~3",
                PointerError::from(StringError {
                    kind: StringErrorKind::UnknownEscapedCharacter { byte: b'3' },
                    pos: 9,
                }),
            ),
            // ~e
            (
                "/foo/bar~\\u0065",
                PointerError::from(StringError {
                    kind: StringErrorKind::UnknownEscapedCharacter { byte: b'\\' },
                    pos: 9,
                }),
            ),
            // passing it as "/\u{007e}" is wrong because this is not how we use Unicode sequence in json strings
            // the parser has to map the sequence to the character
            // unpaired pointer escape where '~' is represented as Unicode sequence
            (
                "/\\u007e",
                PointerError::from(StringError {
                    kind: StringErrorKind::UnexpectedEndOf,
                    pos: 6,
                }),
            ),
            // unknown pointer escape where '~' is represented as Unicode
            (
                "/\\u007e4",
                PointerError::from(StringError {
                    kind: StringErrorKind::UnknownEscapedCharacter { byte: b'4' },
                    pos: 7,
                }),
            ),
            // // unknown pointer escape where '~' and the next character are represented as Unicode sequences
            (
                "/\\u007e\\u0065",
                PointerError::from(StringError {
                    kind: StringErrorKind::UnknownEscapedCharacter { byte: b'\\' },
                    pos: 7,
                }),
            ),
            (
                "/foo/+1",
                PointerError {
                    kind: PointerErrorKind::InvalidIndex {
                        message: "index can not be prefixed with a sign",
                    },
                    pos: 5,
                },
            ),
            (
                "/foo/01",
                PointerError {
                    kind: PointerErrorKind::InvalidIndex {
                        message: "leading zeros are not allowed",
                    },
                    pos: 5,
                },
            ),
            (
                "/foo/+",
                PointerError {
                    kind: PointerErrorKind::InvalidIndex {
                        message: "invalid array index",
                    },
                    pos: 5,
                },
            ),
        ]
    }

    #[test]
    fn test_to_ptr_path() {
        let paths = vec![
            ("$", Some(String::new())),
            (
                "$['store']['book'][0]['title']",
                Some(String::from("/store/book/0/title")),
            ),
            ("$['it\\'s']", Some(String::from("/it's"))),
            ("$['a~b']", Some(String::from("/a~0b"))),
            ("$['a/b']", Some(String::from("/a~1b"))),
            ("$['a~/b']", Some(String::from("/a~0~1b"))),
            ("$['\\b']", Some(String::from("/\\b"))),
            ("$['']", Some(String::from("/"))),
            ("$['ab'", None),
            ("$[ab]", None),
            ("$['ab]", None),
            ("$[12a", None),
            ("$['\\']", None),
            ("$['\\a']", None),
            // incomplete
            ("$['\\u000']", None),
            // upper
            ("$['\\u000F']", None),
            // unknown
            ("$['\\u000g']", None),
            // should be "\\b"
            ("$['\\u0008']", None),
            // out of range 00 - 1f range
            ("$['\\u0021']", None),
        ];

        for (npath, ptr_path) in paths {
            assert_eq!(ptr_path, super::to_ptr_path(npath));
        }
    }

    #[test]
    fn test_invalid_paths() {
        let val = json!({ "foo": [1] });

        for (path, err) in invalid_paths() {
            // problem: if the call to pointer() returned Ok() the test passes but that is not what we want
            // if let Err(e) = val.pointer(path) {
            //     assert_eq!(e, err);
            // }
            let result = val.pointer(path);
            assert_eq!(result, Err(err), "invalid path: {path}");
        }
    }
}
