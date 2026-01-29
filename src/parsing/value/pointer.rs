use crate::parsing;
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
#[derive(Debug, PartialEq, Eq)]
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
    //  /foo/bar evaluates to: token_1: foo, token_2: bar but /foo~1bar evaluates to 1 token: foo/bar
    //  the order matters; if we map first and then try to split we will end up with the wrong tokens
    //  /foo~1bar -> /foo/bar -> token_1: foo, token_2: bar which is incorrect
    //  The same logic applies if we want to escape `~` we write it as `~0`
    //
    //  To handle this correctly, we must process the input character-by-character in a single pass:
    //
    //  input "/foo~1bar":
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
    //
    // the logic is the same as in Parser::parse_string(). Don't try to push a character at a time
    // Scan ahead to find '/' or the end of string. Based on the index returned by find() we create
    // the slice, and we work with the slice, not the whole buffer. We don't use self.buffer and
    // self.pos; we have 'i', an index that helps us traverse the slice.
    pub(super) fn next(&mut self) -> Result<Option<RefToken>, StringError> {
        let len = self.buffer.len();
        if self.pos >= len {
            return Ok(None);
        }

        // find next occurrence of '/' or the end of the input
        let end = parsing::find(&self.buffer[self.pos..], b'/', |b| escapes::is_escape(b))
            .map(|i| self.pos + i)
            .unwrap_or(len);

        let slice = &self.buffer[self.pos..end];
        let mut val = String::with_capacity(slice.len());

        // Case: /foo//
        // When we have an empty token, that is considered valid as key in a map, there is no index
        // in between '/' and '/'. We use as start the index of the 2nd '\'. We put the index that
        // we would typically expect a value.
        if slice.is_empty() {
            // move past the 2nd '/'
            self.pos = end + 1;
            return Ok(Some(RefToken { val, pos: end }));
        }

        let start = self.pos;
        let mut i = 0;

        while i < slice.len() {
            let j = i;
            while i < slice.len() && slice[i] != b'\\' && slice[i] != b'~' {
                if slice[i].is_ascii_control() {
                    return Err(StringError {
                        kind: StringErrorKind::InvalidControlCharacter { byte: slice[i] },
                        // for the error we use the absolute position
                        pos: self.pos + i,
                    });
                }
                i += utf8::char_width(slice[i]);
            }

            val.push_str(unsafe { str::from_utf8_unchecked(&slice[j..i]) });
            if i >= slice.len() {
                break;
            }

            match slice[i] {
                b'\\' => {
                    escapes::check_escape_char(slice, i)?;
                    let ch = escapes::map_escape_char(slice, i);
                    i += escapes::len(slice, i) - 1;
                    if ch == '~' {
                        // check the next character in the buffer after the Unicode sequence mapped to `~`
                        val.push(self.map_ptr_escape(slice, &mut i)?);
                    }
                }
                // can only be '~'
                _ => val.push(self.map_ptr_escape(slice, &mut i)?),
            }
            i += 1;
        }
        if val.len() > parsing::STRING_LENGTH_LIMIT {
            return Err(StringError {
                kind: StringErrorKind::LengthLimitExceeded {
                    len: parsing::STRING_LENGTH_LIMIT,
                },
                pos: start,
            });
        }
        // move past '/'
        self.pos = end + 1;
        Ok(Some(RefToken { val, pos: start }))
    }

    pub(super) fn check_start(&mut self) -> Result<(), PointerError> {
        // pointer path is a json string and it can also start with the Unicode sequence that maps
        // to '/'
        // neither '/', nor a Unicode sequence that could map to '/'
        // a pointer path always starts with '/', unless it is empty which we already checked by the time
        // this method gets called
        if self.buffer[0] != b'/' && self.buffer[0] != b'\\' {
            return Err(PointerError {
                kind: PointerErrorKind::InvalidPointerSyntax,
                pos: 0,
            });
        }

        if self.buffer[0] == b'/' {
            self.pos += 1; // move past '/'
            return Ok(());
        }

        // was '\'
        if let Err(e) = escapes::check_escape_char(self.buffer, 0) {
            return Err(PointerError::from(StringError::from(e)));
        }
        if escapes::map_escape_char(self.buffer, self.pos) != '/' {
            return Err(PointerError {
                kind: PointerErrorKind::InvalidPointerSyntax,
                pos: 0,
            });
        }
        self.pos += 6; // move past the sequence that mapped to '\'

        Ok(())
    }
    // When this method gets called slice[i] is at '~', we only need to check the next character
    fn map_ptr_escape(&self, slice: &[u8], i: &mut usize) -> Result<char, EscapeError> {
        // advance the pointer of the buffer to the next character
        match slice.get(*i + 1) {
            Some(b'0') => {
                *i += 1;
                Ok('~')
            }
            Some(b'1') => {
                *i += 1;
                Ok('/')
            }
            Some(b'\\') => {
                // an escape sequence that could map to '0' or '1'
                escapes::check_escape_char(slice, *i + 1)?;
                let ch = escapes::map_escape_char(slice, *i + 1);
                match ch {
                    '0' => {
                        *i += 6;
                        Ok('~')
                    }
                    '1' => {
                        *i += 6;
                        Ok('/')
                    }
                    _ => Err(EscapeError {
                        kind: EscapeErrorKind::UnknownEscapedCharacter { byte: b'\\' },
                        pos: self.pos + *i + 1,
                    }),
                }
            }
            // ~8
            Some(b) => Err(EscapeError {
                kind: EscapeErrorKind::UnknownEscapedCharacter { byte: *b },
                pos: self.pos + *i + 1,
            }),
            // ~ unpaired
            None => Err(EscapeError {
                kind: EscapeErrorKind::UnexpectedEof,
                pos: self.pos + *i,
            }),
        }
    }
}

pub(super) fn check_array_index(token: &RefToken) -> Result<Option<usize>, PointerError> {
    // empty token called on array is treated as None
    if token.val.is_empty() {
        return Ok(None);
    }

    let buffer = token.val.as_bytes();
    match buffer {
        [b'+' | b'-', b'0'..=b'9', ..] => {
            return Err(PointerError {
                kind: PointerErrorKind::InvalidIndex {
                    message: "index can not be prefixed with a sign",
                },
                pos: token.pos,
            });
        }
        [b'0', b'0'..=b'9', ..] => {
            return Err(PointerError {
                kind: PointerErrorKind::InvalidIndex {
                    message: "leading zeros are not allowed",
                },
                pos: token.pos,
            });
        }
        [b'-'] => return Ok(None),
        [first, ..] if !first.is_ascii_digit() => {
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

// This method performs the same validation as check_array_index() but for invalid cases returns an
// error instead of None. It is used by descend to make sure indices are valid when applied in an
// array. According to the spec, the target location MUST always exist, which means any invalid index
// case must return an error and not None as we did for pointer.
pub(super) fn check_array_index_strict(token: &RefToken) -> Result<usize, PointerError> {
    // path: /foo//
    if token.val.is_empty() {
        return Err(PointerError {
            kind: PointerErrorKind::InvalidIndex {
                message: "empty value as index",
            },
            pos: token.pos,
        });
    }

    let buffer = token.val.as_bytes();

    match buffer {
        [b'+' | b'-', b'0'..=b'9', ..] => {
            return Err(PointerError {
                kind: PointerErrorKind::InvalidIndex {
                    message: "index can not be prefixed with a sign",
                },
                pos: token.pos,
            });
        }
        [b'0', b'0'..=b'9', ..] => {
            return Err(PointerError {
                kind: PointerErrorKind::InvalidIndex {
                    message: "leading zeros are not allowed",
                },
                pos: token.pos,
            });
        }
        [b'-'] => {
            return Err(PointerError {
                kind: PointerErrorKind::InvalidIndex {
                    message: "invalid array index",
                },
                pos: token.pos,
            });
        }
        [first, ..] if !first.is_ascii_digit() => {
            return Err(PointerError {
                kind: PointerErrorKind::InvalidIndex {
                    message: "invalid array index",
                },
                pos: token.pos,
            });
        }
        _ => (),
    }

    match token.val.parse::<usize>() {
        Ok(index) => Ok(index),
        Err(_) => Err(PointerError {
            kind: PointerErrorKind::InvalidIndex {
                message: "invalid array index",
            },
            pos: token.pos,
        }),
    }
}

// returns a pointer path if npath is valid, None otherwise
//
// npath: $['store']['book'][0]['title']
// pointer path: /store/book/0/title
/// Converts a normalized path to a pointer path.
///
/// A normalized path uses bracket notation (`$['store']['book'][0]['title']`), while a pointer
/// path uses slash notation (`/store/book/0/title`).

/// Returns `None` if the input is an invalid npath.
/// # Example
///
/// ```
/// # use jolt::pointer;
/// #
/// let npath = "$['store']['book'][0]['title']";
/// let ptr_path = pointer::to_ptr_path(npath).unwrap();
///
/// assert_eq!(ptr_path, "/store/book/0/title");
/// ```
pub fn to_ptr_path(npath: &str) -> Option<String> {
    let buffer = npath.as_bytes();
    if buffer.is_empty() || buffer[0] != b'$' {
        return None;
    }

    let mut path = String::new();
    // npath: "$" -> ptr_path: ""
    if buffer.len() == 1 {
        return Some(path);
    }
    path.push('/');
    let mut pos = 1;

    // parse '[' -> parse key/index -> parse ']'
    while buffer.get(pos).is_some() {
        if buffer[pos] != b'[' {
            return None;
        }
        pos += 1;
        match buffer.get(pos) {
            Some(b) if b.is_ascii_digit() => parse_index(&buffer, &mut pos, &mut path),
            // keys in a npath always start with single quote
            Some(b'\'') => {
                if let Some(key) = parse_key(buffer, &mut pos) {
                    path.push_str(&key);
                } else {
                    return None;
                }
            }
            Some(_) | None => return None,
        }
        path.push('/');
        // after parsing a key/index we expect ']'
        match buffer.get(pos) {
            Some(b']') => pos += 1,
            Some(_) | None => return None,
        }
    }
    // remove the last '/',
    // ['store']['book'][0]['title'] -> /store/book/0/title and not /store/book/0/title/
    path.pop();
    Some(path)
}

fn parse_key(buffer: &[u8], pos: &mut usize) -> Option<String> {
    *pos += 1; // consume opening single quote '
    let mut key = String::new();

    // standard escapes  + '\'' since it can appear escaped in single quoted strings
    let end = parsing::find(&buffer[*pos..], b'\'', |b| escapes::is_escape(b) || b == b'\'')
        .map(|i| *pos + i)?;

    let slice = &buffer[*pos..end];
    let mut i = 0;

    while let Some(b) = slice.get(i) {
        let j = i;
        // as long as we don't encounter any char that needs special handling keep going
        while i < slice.len() && slice[i] != b'\\' && slice[i] != b'~' && slice[i] != b'/' {
            if slice[i].is_ascii_control() {
                return None;
            }
            i += utf8::char_width(*b);
        }

        key.push_str(unsafe { str::from_utf8_unchecked(&slice[j..i]) });
        if i >= slice.len() {
            break;
        }

        match slice[i] {
            b'\\' => {
                match slice.get(i + 1) {
                    Some(b) if matches!(*b, b'b' | b'f' | b'n' | b'r' | b't' | b'\\') => {
                        key.push('\\');
                        key.push(*b as char);
                        i += 2;
                    }
                    Some(b'u') => {
                        i += 2;
                        let mut hex = String::with_capacity(4);
                        for _ in 0..4 {
                            if let Some(b) = slice.get(i) {
                                // normal-HEXDIG = DIGIT / %x61-66    ; "0"-"9", "a"-"f"
                                // in a valid npath, a Unicode sequence can only appear with lowercase
                                // letters
                                if !b.is_ascii_digit() && !(*b >= b'a' && *b <= b'f')  {
                                    return None;
                                }
                                hex.push(*b as char);
                                i += 1;
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
                    Some(b'\'') => {
                        key.push('\'');
                        i += 2;
                    }
                    Some(_) | None => return None,
                }
            }
            // forward slash as a literal in a pointer path is represented as ~1
            b'/' => {
                key.push('~');
                key.push('1');
                i += 1;
            }
            // can only be '~', '~' as a literal in a pointer path is represented as ~0
            _ => {
                key.push('~');
                key.push('0');
                i += 1;
            }
        }
    }
    *pos = end + 1;

    Some(key)
}

// this method is used only when converting npath to ptr path
// parse_index() reads as long as it encounters a digit; any syntax error is handled by to_ptr_path()
// this is why we can modify path directly; we don't have to return an Option<String> we could also
// return a String, either would work
fn parse_index(buffer: &[u8], pos: &mut usize, path: &mut String) {
    while let Some(c) = buffer.get(*pos) {
        if c.is_ascii_digit() {
            path.push(*c as char);
            *pos += 1;
        } else {
            break;
        }
    }
}

#[cfg(test)]
mod test {
    use crate::json;
    use crate::parsing::error::{StringError, StringErrorKind};
    use crate::parsing::value::error::{PointerError, PointerErrorKind};

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
                    kind: StringErrorKind::UnexpectedEof,
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
                    kind: StringErrorKind::UnexpectedEof,
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
