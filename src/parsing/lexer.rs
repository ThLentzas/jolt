use crate::parsing::error::{LexError, LexErrorKind, StringError, StringErrorKind};
use crate::parsing::{escapes, number, utf8};

#[derive(Debug, PartialEq, Clone, Copy)]
pub(super) enum TokenKind {
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
pub(super) struct Token {
    pub(super) start_index: usize,
    pub(super) offset: usize,
    pub(super) kind: TokenKind,
}

pub(super) struct Lexer<'a> {
    pub(super) buffer: &'a [u8],
    pub(super) pos: usize,
}

impl<'a> Lexer<'a> {
    pub(super) fn new(buffer: &'a [u8]) -> Self {
        Self { buffer, pos: 0 }
    }

    pub(super) fn lex(&mut self) -> Result<Option<Token>, LexError> {
        super::skip_whitespaces(self.buffer, &mut self.pos);
        if self.pos >= self.buffer.len() {
            return Ok(None);
        }

        let current = self.buffer[self.pos];
        match current {
            b'{' => {
                let token = Token {
                    start_index: self.pos,
                    offset: 1,
                    kind: TokenKind::LCurlyBracket,
                };
                self.advance_by(1);
                Ok(Some(token))
            },
            b'}' => {
                let token = Token {
                    start_index: self.pos,
                    offset: 1,
                    kind: TokenKind::RCurlyBracket,
                };
                self.advance_by(1);
                Ok(Some(token))
            },
            b'[' => {
                let token = Token {
                    start_index: self.pos,
                    offset: 1,
                    kind: TokenKind::LSquareBracket,
                };
                self.advance_by(1);
                Ok(Some(token))
            },
            b']' => {
                let token = Token {
                    start_index: self.pos,
                    offset: 1,
                    kind: TokenKind::RSquareBracket,
                };
                self.advance_by(1);
                Ok(Some(token))
            },
            b':' => {
                let token = Token {
                    start_index: self.pos,
                    offset: 1,
                    kind: TokenKind::Colon,
                };
                self.advance_by(1);
                Ok(Some(token))
            },
            b',' => {
                let token = Token {
                    start_index: self.pos,
                    offset: 1,
                    kind: TokenKind::Comma,
                };
                self.advance_by(1);
                Ok(Some(token))
            },
            b'-' | b'+' | b'0'..=b'9' => {
                let start = self.pos;
                self.read_number()?;
                Ok(Some(Token {
                    start_index: start,
                    offset: self.pos - start,
                    kind: TokenKind::Number,
                }))
            }
            b'"' => {
                let start = self.pos;
                self.read_string()?;
                Ok(Some(Token {
                    start_index: start,
                    offset: self.pos - start,
                    kind: TokenKind::String,
                }))
            }
            b't' | b'f' => {
                let start = self.pos;
                self.read_boolean()?;
                Ok(Some(Token {
                    start_index: start,
                    offset: self.pos - start,
                    kind: TokenKind::Boolean,
                }))
            }
            b'n' => {
                let start = self.pos;
                self.read_null()?;
                Ok(Some(Token {
                    start_index: start,
                    offset: self.pos - start,
                    kind: TokenKind::Null,
                }))
            }
            _ => Err(LexError {
                kind: LexErrorKind::UnexpectedCharacter { byte: current },
                pos: self.pos,
            }),
        }
    }

    pub(super) fn advance_by(&mut self, n: usize) {
        self.pos += n;
    }

    fn read_number(&mut self) -> Result<(), LexError> {
        let current = self.buffer[self.pos];
        let next = self.buffer.get(self.pos + 1);

        match (current, next) {
            // '-' into eof
            (b'-', None) => {
                return Err(LexError {
                    kind: LexErrorKind::UnexpectedEof,
                    pos: self.pos,
                });
            }
            // +a
            (b'+', Some(n)) if !n.is_ascii_digit() => {
                return Err(LexError {
                    kind: LexErrorKind::UnexpectedCharacter { byte: current },
                    pos: self.pos,
                });
            }
            (b'+', None) => {
                return Err(LexError {
                    kind: LexErrorKind::UnexpectedCharacter { byte: current },
                    pos: self.pos,
                });
            }
            _ => number::read(self.buffer, &mut self.pos)?,
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
                // [34, 10, 34] is invalid, the control character is passed as raw byte, and it is unescaped
                // but [34, 92, 110, 34] is valid as a new line character
                c if c.is_ascii_control() => {
                    return Err(StringError {
                        kind: StringErrorKind::InvalidControlCharacter { byte: current },
                        pos: self.pos,
                    });
                }
                c if !c.is_ascii() => {
                    utf8::check_utf8_sequence(&self.buffer, self.pos)?;
                    self.pos += utf8::utf8_char_width(current);
                }
                b'\\' => {
                    escapes::check_escape_character(&self.buffer, self.pos)?;
                    self.pos += escapes::len(&self.buffer, self.pos);
                }
                // ascii 
                _ => self.pos += 1
            }
        }
        if self.pos == len {
            return Err(StringError {
                kind: StringErrorKind::UnexpectedEndOf,
                pos: self.pos - 1,
            });
        }
        // consume closing "
        self.advance_by(1);
        Ok(())
    }

    // an approach with starts_with() could work but in the mismatch case we won't know the index
    fn read_boolean(&mut self) -> Result<(), LexError> {
        let keyword = if self.buffer[self.pos] == b't' {
            "true".as_bytes()
        } else {
            "false".as_bytes()
        };
        super::read_keyword(self.buffer, &mut self.pos, keyword)?;
        Ok(())
    }

    fn read_null(&mut self) -> Result<(), LexError> {
        super::read_keyword(self.buffer, &mut self.pos, "null".as_bytes())?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn valid_numbers() -> Vec<(&'static [u8], Token)> {
        vec![
            (
                b"0",
                Token {
                    start_index: 0,
                    offset: 1,
                    kind: TokenKind::Number,
                },
            ),
            (
                b"-0",
                Token {
                    start_index: 0,
                    offset: 2,
                    kind: TokenKind::Number,
                },
            ),
            (
                b"123",
                Token {
                    start_index: 0,
                    offset: 3,
                    kind: TokenKind::Number,
                },
            ),
            (
                b"-45",
                Token {
                    start_index: 0,
                    offset: 3,
                    kind: TokenKind::Number,
                },
            ),
            (
                b"123.45",
                Token {
                    start_index: 0,
                    offset: 6,
                    kind: TokenKind::Number,
                },
            ),
            (
                b"1e10",
                Token {
                    start_index: 0,
                    offset: 4,
                    kind: TokenKind::Number,
                },
            ),
            (
                b"-0.1e-2",
                Token {
                    start_index: 0,
                    offset: 7,
                    kind: TokenKind::Number,
                },
            ),
        ]
    }

    // rest of the cases are tested in number::read()
    fn invalid_signs() -> Vec<(&'static [u8], LexError)> {
        vec![
            (
                b"-",
                LexError {
                    kind: LexErrorKind::UnexpectedEof,
                    pos: 0,
                },
            ),
            (
                b"+",
                LexError {
                    kind: LexErrorKind::UnexpectedCharacter { byte: b'+' },
                    pos: 0,
                },
            ),
            (
                b"+a",
                LexError {
                    kind: LexErrorKind::UnexpectedCharacter { byte: b'+' },
                    pos: 0,
                },
            ),
        ]
    }

    fn valid_strings() -> Vec<(&'static [u8], Token)> {
        vec![
            (
                b"\"abc\"",
                Token {
                    start_index: 0,
                    offset: 5,
                    kind: TokenKind::String,
                },
            ),
            (
                b"\"A\\uD83D\\uDE00B\"",
                Token {
                    start_index: 0,
                    offset: 16,
                    kind: TokenKind::String,
                },
            ),
            (
                b"\"b\\n\"",
                Token {
                    start_index: 0,
                    offset: 5,
                    kind: TokenKind::String,
                },
            ),
            (
                b"\"\\u00E9\"",
                Token {
                    start_index: 0,
                    offset: 8,
                    kind: TokenKind::String,
                },
            ),
            // 2-byte sequence same as (b"\"\xC3\xA9\"", 0, 4)
            (
                "\"é\"".as_bytes(),
                Token {
                    start_index: 0,
                    offset: 4,
                    kind: TokenKind::String,
                },
            ),
        ]
    }

    fn invalid_strings() -> Vec<(&'static [u8], LexError)> {
        vec![
            // raw byte control character
            (
                b"\"\x00",
                LexError::from(StringError {
                    kind: StringErrorKind::InvalidControlCharacter { byte: 0 },
                    pos: 1,
                }),
            ),
            // unpaired escaped
            (
                b"\"\\",
                LexError::from(StringError {
                    kind: StringErrorKind::UnexpectedEndOf,
                    pos: 1,
                }),
            ),
            // unknown escaped
            (
                b"\"\\g\"",
                LexError::from(StringError {
                    kind: StringErrorKind::UnknownEscapedCharacter { byte: b'g' },
                    pos: 2,
                }),
            ),
            // incomplete unicode
            (
                b"\"\\u",
                LexError::from(StringError {
                    kind: StringErrorKind::UnexpectedEndOf,
                    pos: 2,
                }),
            ),
            // // not enough bytes to form a low surrogate
            (
                b"\"\\uD83D\"",
                LexError::from(StringError {
                    kind: StringErrorKind::UnexpectedEndOf,
                    pos: 7,
                }),
            ),
            // // high not followed by low
            (
                b"\"\\uD83Dabcdef\"",
                LexError::from(StringError {
                    kind: StringErrorKind::InvalidSurrogate,
                    pos: 1,
                }),
            ),
            // // high followed by high
            (
                b"\"\\uD83D\\uD83D\"",
                LexError::from(StringError {
                    kind: StringErrorKind::InvalidSurrogate,
                    pos: 1,
                }),
            ),
            // // low surrogate
            (
                b"\"\\uDC00\"",
                LexError::from(StringError {
                    kind: StringErrorKind::InvalidSurrogate,
                    pos: 1,
                }),
            ),
            (
                b"\"abc",
                LexError::from(StringError {
                    kind: StringErrorKind::UnexpectedEndOf,
                    pos: 3,
                }),
            ),
        ]
    }

    fn valid_literals() -> Vec<(&'static [u8], Token)> {
        vec![
            (
                b"false",
                Token {
                    start_index: 0,
                    offset: 5,
                    kind: TokenKind::Boolean,
                },
            ),
            (
                b"true",
                Token {
                    start_index: 0,
                    offset: 4,
                    kind: TokenKind::Boolean,
                },
            ),
            (
                b"null",
                Token {
                    start_index: 0,
                    offset: 4,
                    kind: TokenKind::Null,
                },
            ),
        ]
    }

    // 1 mismatch, 1 eof
    fn invalid_literals() -> Vec<(&'static [u8], LexError)> {
        vec![
            (
                b"falte",
                LexError {
                    kind: LexErrorKind::UnexpectedCharacter { byte: b't' },
                    pos: 3,
                },
            ),
            (
                b"tru",
                LexError {
                    kind: LexErrorKind::UnexpectedEof,
                    pos: 2,
                },
            ),
        ]
    }

    #[test]
    fn test_valid_numbers() {
        for (buffer, token) in valid_numbers() {
            let mut lexer = Lexer::new(buffer);
            let t = lexer.lex().unwrap().unwrap();

            assert_eq!(t, token);
        }
    }

    #[test]
    fn test_invalid_numbers() {
        for (buffer, error) in invalid_signs() {
            let mut lexer = Lexer::new(buffer);
            let result = lexer.lex();

            assert_eq!(result, Err(error), "failed to tokenize: {buffer:?}");
        }
    }

    #[test]
    fn test_valid_strings() {
        for (buffer, token) in valid_strings() {
            let mut lexer = Lexer::new(buffer);
            let t = lexer.lex().unwrap().unwrap();

            assert_eq!(t, token);
        }
    }

    #[test]
    fn test_invalid_strings() {
        for (buffer, error) in invalid_strings() {
            let mut lexer = Lexer::new(buffer);
            let result = lexer.lex();

            assert_eq!(result, Err(error), "failed to tokenize: {buffer:?}");
        }
    }

    #[test]
    fn test_valid_literals() {
        for (buffer, token) in valid_literals() {
            let mut lexer = Lexer::new(buffer);
            let t = lexer.lex().unwrap().unwrap();

            assert_eq!(t, token);
        }
    }

    #[test]
    fn test_invalid_literals() {
        for (buffer, error) in invalid_literals() {
            let mut lexer = Lexer::new(buffer);
            let result = lexer.lex();

            assert_eq!(result, Err(error), "failed to tokenize: {buffer:?}");
        }
    }

    #[test]
    fn test_unexpected_character() {
        // @
        let mut lexer = Lexer::new(&[64]);
        let result = lexer.lex();
        let error = LexError {
            kind: LexErrorKind::UnexpectedCharacter { byte: b'@' },
            pos: 0,
        };

        assert_eq!(result, Err(error));
    }
}
