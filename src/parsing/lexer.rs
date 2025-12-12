use crate::parsing::error::{ParserErrorKind, ParserError, StringErrorKind, StringError};
use crate::parsing::{self, escapes, number, utf8};

#[derive(Debug, PartialEq, Clone, Copy)]
pub(super) enum LexerTokenType {
    LCurlyBracket,
    RCurlyBracket,
    LSquareBracket,
    RSquareBracket,
    Colon,
    Comma,
    Number,
    String,
    Boolean,
    Null,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub(super) struct LexerToken {
    start_index: usize,
    offset: u32,
    token_type: LexerTokenType, // type is reserved
}

impl LexerToken {
    // toDo: this should be an immutable borrow? copying here is fine?
    pub(super) fn start_index(&self) -> usize {
        self.start_index
    }

    pub(super) fn offset(&self) -> u32 {
        self.offset
    }

    pub(super) fn token_type(&self) -> &LexerTokenType {
        &self.token_type
    }
}

pub(super) struct Lexer<'a> {
    buffer: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub(super) fn new(buffer: &'a [u8]) -> Self {
        Self { buffer, pos: 0 }
    }

    pub(super) fn consume(&mut self, n: usize) {
        self.pos += n;
    }

    // toDo: explain why we moved away from storing the tokens in a vector?
    pub(super) fn next(&mut self) -> Result<Option<LexerToken>, ParserError> {
        parsing::skip_whitespaces(self.buffer, &mut self.pos);
        if self.pos >= self.buffer.len() {
            return Ok(None);
        }

        let current = self.buffer[self.pos];
        match current {
            b'{' => Ok(Some(LexerToken {
                start_index: self.pos,
                offset: 1,
                token_type: LexerTokenType::LCurlyBracket
            })),
            b'}' => Ok(Some(LexerToken {
                start_index: self.pos,
                offset: 1,
                token_type: LexerTokenType::RCurlyBracket
            })),
            b'[' => Ok(Some(LexerToken {
                start_index: self.pos,
                offset: 1,
                token_type: LexerTokenType::LSquareBracket
            })),
            b']' => Ok(Some(LexerToken {
                start_index: self.pos,
                offset: 1,
                token_type: LexerTokenType::RSquareBracket
            })),
            b':' => Ok(Some(LexerToken {
                start_index: self.pos,
                offset: 1,
                token_type: LexerTokenType::Colon
            })),
            b',' => Ok(Some(LexerToken {
                start_index: self.pos,
                offset: 1,
                token_type: LexerTokenType::Comma
            })),
            b'-' | b'+' | b'0'..=b'9' => {
                let start = self.pos;
                self.read_number()?;
                Ok(Some(LexerToken {
                    start_index: start,
                    offset: (self.pos - start + 1) as u32,
                    token_type: LexerTokenType::Number
                }))
            }
            b'"' => {
                let start = self.pos;
                self.read_string()?;
                Ok(Some(LexerToken {
                    start_index: start,
                    offset: (self.pos - start + 1) as u32,
                    token_type: LexerTokenType::String
                }))
            }
            b't' | b'f' => {
                let start = self.pos;
                self.read_boolean()?;
                Ok(Some(LexerToken {
                    start_index: start,
                    offset: (self.pos - start + 1) as u32,
                    token_type: LexerTokenType::Boolean
                }))
            }
            b'n' => {
                let start = self.pos;
                self.read_null()?;
                Ok(Some(LexerToken {
                    start_index: start,
                    offset: (self.pos - start + 1) as u32,
                    token_type: LexerTokenType::Null
                }))
            }
            _ => Err(ParserError {
                kind: ParserErrorKind::UnexpectedCharacter { byte: current },
                pos: Some(self.pos)
            })
        }
    }

    // before we return we call self.pos -= 1; self.pos is at the 1st character after a valid number
    // and the control returns back to lex(), back to peek() and eventually advance() is called
    // which moves self.pos, and we skip that character if we don't move pos
    fn read_number(&mut self) -> Result<(), ParserError> {
        let current = self.buffer[self.pos];
        let next = self.buffer.get(self.pos + 1);
        let start = self.pos;

        match(current, next) {
            // sign into eof
            (b'-', None) => return Err(ParserError {
                kind: ParserErrorKind::UnexpectedEof,
                pos: Some(self.pos)
            }),
            // +9
            (b'+', Some(n)) if !n.is_ascii_digit() => return Err(ParserError {
                kind: ParserErrorKind::UnexpectedCharacter { byte: current },
                pos: Some(self.pos)
            }),
            (b'+', None) => return Err(ParserError {
                kind: ParserErrorKind::UnexpectedCharacter { byte: current },
                pos: Some(self.pos)
            }),
            _ => number::read(self.buffer, &mut self.pos)
                // why not from? This the only case where we have to convert from a NumericError to
                // a ParserError, so I thought to do it via map_err(). The body of the closure would
                // be the body of the from() impl.
                .map_err(|err| {
                    let pos = err.pos;
                    ParserError { kind: ParserErrorKind::InvalidNumber(err), pos: Some(pos) }
                })?,
        }

        // 1 edge case to consider, single digit numbers, read the comment in number::read()
        if self.pos != start {
            self.pos -= 1;
        }

        Ok(())
    }

    fn read_string(&mut self) -> Result<(), StringError> {
        let len = self.buffer.len();
        self.pos += 1; // skip opening "

        while self.pos < len && self.buffer[self.pos] != b'"' {
            let current = self.buffer[self.pos];
            match current {
                // raw control characters are not allowed
                // [34, 10, 34] is invalid - the control character is passed as raw byte, and it is unescaped
                // but [34, 92, 110, 34] should be considered valid as a new line character
                c if c.is_ascii_control() => {
                    return Err(StringError {
                        kind: StringErrorKind::InvalidControlCharacter { byte: current },
                        pos: self.pos
                    });
                }
                c if !c.is_ascii() => {
                    utf8::check_utf8_sequence(&self.buffer, self.pos)?;
                    self.pos += utf8::utf8_char_width(current) - 1;
                }
                b'\\' => {
                    escapes::check_escape_character(&self.buffer, self.pos)?;
                    self.pos += escapes::len(&self.buffer, self.pos) - 1;
                }
                _ => {}
            }
            self.pos += 1;
        }
        if self.pos == len {
            return Err(StringError { kind: StringErrorKind::UnexpectedEndOf, pos: self.pos - 1 })
        }
        Ok(())
    }

    // an approach with starts_with() could work but in the case of mismatch we won't know the index
    //
    // before we return we call self.pos -= 1; self.pos is at the 1st character after a valid literal
    // and the control returns back to lex(), back to peek() and eventually advance() is called
    // which moves self.pos, and we skip that character
    fn read_boolean(&mut self) -> Result<(), ParserError> {
        let target = if self.buffer[self.pos] == b't' {
            "true".as_bytes()
        } else {
            "false".as_bytes()
        };
        parsing::read_keyword(self.buffer, &mut self.pos, target)?;
        self.pos -= 1;

        Ok(())
    }

    fn read_null(&mut self) -> Result<(), ParserError> {
        parsing::read_keyword(self.buffer, &mut self.pos, "null".as_bytes())?;
        self.pos -= 1;

        Ok(())
    }
}

// We declare Lexer as let mut lexer because lex takes a mutable reference to self
#[cfg(test)]
mod tests {
    use super::*;

    fn valid_numbers() -> Vec<(&'static [u8], LexerToken)> {
        vec![
            (b"0", LexerToken { start_index: 0, offset: 1, token_type: LexerTokenType::Number }),
            (b"-0", LexerToken { start_index: 0, offset: 2, token_type: LexerTokenType::Number }),
            (b"123", LexerToken { start_index: 0, offset: 3, token_type: LexerTokenType::Number }),
            (b"-45", LexerToken { start_index: 0, offset: 3, token_type: LexerTokenType::Number }),
            (b"123.45", LexerToken { start_index: 0, offset: 6, token_type: LexerTokenType::Number }),
            (b"1e10", LexerToken { start_index: 0, offset: 4, token_type: LexerTokenType::Number }),
            (b"-0.1e-2", LexerToken { start_index: 0, offset: 7, token_type: LexerTokenType::Number }),
        ]
    }

    // rest of the cases are tested in number::read()
    fn invalid_signs() -> Vec<(&'static [u8], ParserError)> {
        vec![
            (b"-", ParserError {
                kind: ParserErrorKind::UnexpectedEof,
                pos: Some(0)
            }),
            (b"+", ParserError {
                kind: ParserErrorKind::UnexpectedCharacter { byte: b'+' },
                pos: Some(0)
            }),
            (b"+a", ParserError {
                kind: ParserErrorKind::UnexpectedCharacter { byte: b'+' },
                pos: Some(0)
            }),
        ]
    }

    fn valid_strings() -> Vec<(&'static [u8], LexerToken)> {
        vec![
            (b"\"abc\"", LexerToken {
                start_index: 0,
                offset: 5,
                token_type: LexerTokenType::String
            }),
            (b"\"A\\uD83D\\uDE00B\"", LexerToken {
                start_index: 0,
                offset: 16,
                token_type: LexerTokenType::String
            }),
            (b"\"b\\n\"", LexerToken {
                start_index: 0,
                offset: 5,
                token_type: LexerTokenType::String
            }),
            (b"\"\\u00E9\"", LexerToken {
                start_index: 0,
                offset: 8,
                token_type: LexerTokenType::String
            }),
            // 2-byte sequence same as (b"\"\xC3\xA9\"", 0, 4)
            ("\"é\"".as_bytes(), LexerToken {
                start_index: 0,
                offset: 4,
                token_type: LexerTokenType::String
            }),
        ]
    }

    fn invalid_strings() -> Vec<(&'static [u8], ParserError)> {
        vec![
            // raw byte control character
            (b"\"\x00", ParserError::from(StringError {
                kind: StringErrorKind::InvalidControlCharacter { byte: 0 },
                pos: 1
            })),
            // unpaired escaped
            (b"\"\\", ParserError::from(StringError {
                kind: StringErrorKind::UnexpectedEndOf,
                pos: 1
            })),
            // unknown escaped
            (b"\"\\g\"", ParserError::from(StringError {
                kind: StringErrorKind::UnknownEscapedCharacter { byte: b'g' },
                pos: 2
            })),
            // incomplete unicode
            (b"\"\\u", ParserError::from(StringError {
                kind: StringErrorKind::UnexpectedEndOf,
                pos: 2
            })),
            // // not enough bytes to form a low surrogate
            (b"\"\\uD83D\"", ParserError::from(StringError {
                kind: StringErrorKind::UnexpectedEndOf,
                pos: 7
            })),
            // // high not followed by low
            (b"\"\\uD83Dabcdef\"", ParserError::from(StringError {
                kind: StringErrorKind::InvalidSurrogate,
                pos: 1
            })),
            // // high followed by high
            (b"\"\\uD83D\\uD83D\"", ParserError::from(StringError {
                kind: StringErrorKind::InvalidSurrogate,
                pos: 1
            })),
            // // low surrogate
            (b"\"\\uDC00\"", ParserError::from(StringError {
                kind: StringErrorKind::InvalidSurrogate,
                pos: 1
            })),
            (b"\"abc", ParserError::from(StringError {
                kind: StringErrorKind::UnexpectedEndOf,
                pos: 3
            })),
        ]
    }

    fn valid_literals() -> Vec<(&'static [u8], LexerToken)> {
        vec![
            (b"false", LexerToken { start_index: 0, offset: 5, token_type: LexerTokenType::Boolean }),
            (b"true", LexerToken { start_index: 0, offset: 4, token_type: LexerTokenType::Boolean }),
            (b"null", LexerToken { start_index: 0, offset: 4, token_type: LexerTokenType::Null }),
        ]
    }

    // 1 mismatch and 1 not enough characters is enough
    fn invalid_literals() -> Vec<(&'static [u8], ParserError)> {
        vec![
            (b"falte", ParserError {
                kind: ParserErrorKind::UnexpectedCharacter { byte: b't' },
                pos: Some(3)
            }),
            (b"tru", ParserError { kind: ParserErrorKind::UnexpectedEof, pos: Some(2)
            }),
        ]
    }

    #[test]
    fn test_valid_numbers() {
        for (buffer, token) in valid_numbers() {
            let mut lexer = Lexer::new(buffer);
            let t = lexer.next().unwrap().unwrap();

            assert_eq!(t, token);
        }
    }

    #[test]
    fn test_invalid_numbers() {
        for (buffer, error) in invalid_signs() {
            let mut lexer = Lexer::new(buffer);
            let result = lexer.next();

            assert_eq!(result, Err(error), "failed to tokenize: {buffer:?}");
        }
    }

    #[test]
    fn test_valid_strings() {
        for (buffer, token) in valid_strings() {
            let mut lexer = Lexer::new(buffer);
            let t = lexer.next().unwrap().unwrap();

            assert_eq!(t, token);
        }
    }

    #[test]
    fn test_invalid_strings() {
        for (buffer, error) in invalid_strings() {
            let mut lexer = Lexer::new(buffer);
            let result = lexer.next();

            assert_eq!(result, Err(error), "failed to tokenize: {buffer:?}");
        }
    }

    #[test]
    fn test_valid_literals() {
        for (buffer, token) in valid_literals() {
            let mut lexer = Lexer::new(buffer);
            let t = lexer.next().unwrap().unwrap();

            assert_eq!(t, token);
        }
    }

    #[test]
    fn test_invalid_literals() {
        for (buffer, error) in invalid_literals() {
            let mut lexer = Lexer::new(buffer);
            let result = lexer.next();

            assert_eq!(result, Err(error), "failed to tokenize: {buffer:?}");
        }
    }

    #[test]
    fn test_unexpected_character() {
        // @
        let mut lexer = Lexer::new(&[64]);
        let result = lexer.next();
        let error = ParserError {
            kind: ParserErrorKind::UnexpectedCharacter { byte: b'@' },
            pos: Some(0)
        };

        assert_eq!(result, Err(error));
    }
}