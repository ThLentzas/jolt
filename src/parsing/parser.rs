use super::error::{ParseError, ParseErrorKind};
use super::lexer::{Lexer, Token, TokenKind};
use super::value::Value;
use super::{escapes, number, utf8};
use crate::parsing::number::Number;
use indexmap::IndexMap;
use std::string::String;

pub(super) struct Parser<'a> {
    buffer: &'a [u8],
    lexer: Lexer<'a>,
    depth: u16,
    peeked: Option<Token>,
}

impl<'a> Parser<'a> {
    pub(super) fn new(buffer: &'a [u8]) -> Self {
        Self {
            buffer,
            lexer: Lexer::new(buffer),
            depth: 0,
            peeked: None,
        }
    }

    pub(super) fn parse(&mut self) -> Result<Value, ParseError> {
        // [](empty input buffer) is invalid, JsonText = ws value ws, a value is mandatory
        if self.buffer.is_empty() {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedEof,
                pos: self.buffer.len(),
            });
        }
        if self.buffer.len() > super::INPUT_BUFFER_LIMIT {
            return Err(ParseError {
                kind: ParseErrorKind::InputBufferLimitExceeded {
                    len: super::INPUT_BUFFER_LIMIT,
                },
                pos: 0,
            });
        }
        // https://www.rfc-editor.org/rfc/rfc8259#section-8.1
        if utf8::is_bom_present(self.buffer) {
            self.lexer.advance_by(3);
        }

        let val = self.parse_value()?;
        if self.peek()?.is_some() {
            // after successfully parsing a value we can't have leftover tokens
            // false5, "abc"123, {}  null,
            // note that this could return an error, {}001, -> leading zeros are not allowed
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken { expected: None },
                pos: self.peeked.unwrap().start_index,
            });
        }
        Ok(val)
    }

    fn parse_value(&mut self) -> Result<Value, ParseError> {
        let Some(token) = self.next()? else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedEof,
                pos: self.buffer.len() - 1,
            });
        };

        let val = match token.kind {
            TokenKind::LCurlyBracket => Value::Object(self.parse_object()?),
            TokenKind::LSquareBracket => Value::Array(self.parse_array()?),
            TokenKind::Number => {
                Value::Number(self.parse_number(token.start_index, token.offset))
            }
            TokenKind::String => {
                Value::String(self.parse_string(token.start_index, token.offset)?)
            }
            TokenKind::Boolean => Value::Boolean(self.parse_bool(token.start_index)),
            TokenKind::Null => Value::Null,
            _ => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken {
                        expected: Some("json value"),
                    },
                    pos: token.start_index,
                });
            }
        };
        Ok(val)
    }

    // We peek and advance 4 times, after moving past the opening '{'
    //
    // 1. peek and expect either '}' for an empty object or a key string
    // 2. peek and expect ':' after key
    // 3. after colon we call parse_value()
    // 4. peek, after value we expect either ',' to separate from the next key-value pair or '}' 
    //
    // When encountering '}' in steps 1 or 4, we break WITHOUT calling advance(). The closing '}'
    // will be consumed by the caller (parse_value()) after this function returns.
    fn parse_object(&mut self) -> Result<IndexMap<String, Value>, ParseError> {
        if self.depth + 1 > super::NESTING_DEPTH_LIMIT {
            return Err(ParseError {
                kind: ParseErrorKind::NestingDepthLimitExceeded {
                    depth: super::NESTING_DEPTH_LIMIT,
                },
                // the position of the buffer
                pos: self.lexer.pos,
            });
        }

        self.depth += 1;
        let len = self.buffer.len();
        let mut map = IndexMap::new();

        // We don't know how many tokens are part of the current object, the moment we encounter '}'
        // we break
        loop {
            // Initially had the following:
            //     loop {
            //         match self.peek()? {
            //             .....
            //             Some(token) if expect(token.kind(), TokenKind::String) => {
            //                 let key = self.parse_string(token.start_index(), token.offset())?;
            //
            // The compiler will complain that we have 2 mutable borrows for self. We do, self.peek()
            // is still active and then we call self.parse_string() which also takes &mut self.
            //
            // With this approach we extract the information that we need, then the 1st borrow ends
            // after we are done with token, and we can call parse_string()
            let Some(token) = self.peek()? else {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedEof,
                    pos: len - 1,
                });
            };

            let kind = token.kind;
            let start = token.start_index;
            let offset = token.offset;
            match kind {
                // {"foo": "bar",}
                TokenKind::RCurlyBracket => {
                    self.next()?;
                    self.depth -= 1;
                    break;
                }
                TokenKind::String => {
                    // We don't allow duplicate names at the same depth level
                    // {"key": false, "key": true} not allowed, duplicate name at the same level
                    // {"key": {"key": {}}} allowed, they are on a different depth level
                    let name = self.parse_string(start, offset)?;
                    if map.contains_key(&name) {
                        return Err(ParseError {
                            kind: ParseErrorKind::DuplicateName {
                                // SAFETY: lexer already validated the sequence
                                name,
                            },
                            pos: start,
                        });
                    }
                    self.next()?;
                    match self.peek()? {
                        Some(t) if expect(t.kind, TokenKind::Colon) => {
                            self.next()?;
                        }
                        // mismatch on colon
                        Some(t) => {
                            return Err(ParseError {
                                kind: ParseErrorKind::UnexpectedToken {
                                    expected: Some("colon ':'"),
                                },
                                pos: t.start_index,
                            });
                        }
                        None => {
                            return Err(ParseError {
                                kind: ParseErrorKind::UnexpectedEof,
                                pos: self.buffer.len() - 1,
                            });
                        }
                    }
                    map.insert(name, self.parse_value()?);
                }
                // mismatch on key
                _ => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken {
                            expected: Some("object name"),
                        },
                        pos: start,
                    });
                }
            }

            match self.peek()? {
                Some(token) if expect(token.kind, TokenKind::Comma) => {
                    self.next()?;
                    if let Some(next) = self.peek()? {
                        if expect(next.kind, TokenKind::RCurlyBracket) {
                            return Err(ParseError {
                                kind: ParseErrorKind::UnexpectedToken {
                                    expected: Some("string key"),
                                },
                                pos: next.start_index,
                            });
                        }
                    }
                }
                Some(token) if expect(token.kind, TokenKind::RCurlyBracket) => {
                    self.next()?;
                    self.depth -= 1;
                    break;
                }
                // mismatch
                Some(token) => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken {
                            expected: Some("'}' or ','"),
                        },
                        pos: token.start_index,
                    });
                }
                None => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedEof,
                        pos: len - 1,
                    });
                }
            }
        }
        Ok(map)
    }

    // We peek and advance 2 times, after moving past the opening '['
    //
    // 1. peek and expect either ']' for an empty array or a json value(parse_value() will advance)
    // 2. peek and expect ',' to separate values -> advance, or ']' -> break;
    //
    // When encountering ']', we break WITHOUT calling advance(). The closing ']' will be consumed
    // by the caller (parse_value()) after this function returns.
    fn parse_array(&mut self) -> Result<Vec<Value>, ParseError> {
        if self.depth + 1 > super::NESTING_DEPTH_LIMIT {
            return Err(ParseError {
                kind: ParseErrorKind::NestingDepthLimitExceeded {
                    depth: super::NESTING_DEPTH_LIMIT,
                },
                // the position of the buffer
                pos: self.lexer.pos,
            });
        }

        self.depth += 1;
        let mut arr = Vec::new();
        // when we encounter ']' we break
        loop {
            match self.peek()? {
                // [1,2,]
                Some(next) if expect(next.kind, TokenKind::RSquareBracket) => {
                    // []
                    self.next()?;
                    self.depth -= 1;
                    break;
                }
                // Some(next) or None parse.value() will handle both cases
                _ => arr.push(self.parse_value()?),
            }
            match self.peek()? {
                Some(next) if expect(next.kind, TokenKind::Comma) => {
                    self.next()?;
                    // trailing comma case: [1,2,]
                    if let Some(next) = self.peek()? {
                        if expect(next.kind, TokenKind::RSquareBracket) {
                            return Err(ParseError {
                                kind: ParseErrorKind::UnexpectedToken {
                                    expected: Some("json value"),
                                },
                                pos: next.start_index,
                            });
                        }
                    }
                }
                Some(next) if expect(next.kind, TokenKind::RSquareBracket) => {
                    self.next()?;
                    self.depth -= 1;
                    break;
                }
                Some(next) => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken {
                            expected: Some("',' or ']'"),
                        },
                        pos: next.start_index,
                    });
                }
                None => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedEof,
                        pos: self.buffer.len() - 1,
                    });
                }
            }
        }
        Ok(arr)
    }

    //         let token = &self.tokens[self.pos];
    //         let mut val = String::with_capacity(token.offset - 2); // drop quotes
    //         let mut i = token.start_index + 1; // skip opening quote
    //
    //         while i < token.start_index + token.offset - 1 {
    //             // skip closing quote
    //             let current = self.buffer[i];
    //             match current {
    //                 b'\\' => {
    //                     val.push(escapes::map_escape_character(self.buffer, i));
    //                     i += escapes::len(self.buffer, i);
    //                 }
    //                 b if !b.is_ascii() => {
    //                     val.push(utf8::read_utf8_char(self.buffer, i));
    //                     i += utf8::utf8_char_width(current);
    //                 }
    //                 _ => {
    //                     val.push(current as char);
    //                     i += 1;
    //                 }
    //             }
    //             if val.len() > super::STRING_LENGTH_LIMIT {
    //                 return Err(ParseError {
    //                     kind: ParseErrorKind::StringLengthLimitExceeded {
    //                         len: super::STRING_LENGTH_LIMIT,
    //                     },
    //                     pos: token.start_index,
    //                 });
    //             }
    //         }
    //         Ok(val)
    //
    // had the above code which runs in O(n) time/space where n is the number of characters in the
    // string. The profiler though showed that it is slow. The reason is that it is much more efficient
    // to write 10 characters in memory at once rather than 1 character 10 times. Everytime we write
    // certain things need to be checked, when we can call push() 10 times we do that every time
    // which impacts the performance, but when we push chunks we only get to do it once. This is
    // based on how the hardware works.
    // In the new approach we keep scanning until we encounter a non ascii byte or an escape sequence
    // Those 2 need special handling. In any other case, as long as we keep encountering ASCII characters
    // we can keep track of that slice and call push_str() instead of push(). push() needs to increase
    // len, do bounds checks etc each time is called.
    // This approach improves performance by 1.5ms, from 7.4 to 5.9
    // 
    // to extract the value from slice all we need is the starting index and the offset; we pass only
    // what we need, not the whole token
    fn parse_string(&mut self, start: usize, offset: usize) -> Result<String, ParseError> {
        let slice = &self.buffer[start + 1..start + offset - 1];

        // calls iter.any() internally
        if !slice.contains(&b'\\') {
            // SAFETY: lexer already verified the sequence
            let val = unsafe { String::from_utf8_unchecked(Vec::from(slice)) };
            if val.len() > super::STRING_LENGTH_LIMIT {
                return Err(ParseError {
                    kind: ParseErrorKind::StringLengthLimitExceeded {
                        len: super::STRING_LENGTH_LIMIT,
                    },
                    pos: start,
                });
            }
            return Ok(val);
        }

        let mut val = String::with_capacity(slice.len());
        let mut i = 0;

        while i < slice.len() {
            let j = i;

            while i < slice.len() && slice[i] != b'\\' && slice[i].is_ascii() {
                i += 1;
            }
            if i > j {
                // SAFETY: lexer already verified the sequence
                let chunk = unsafe { str::from_utf8_unchecked(&slice[j..i]) };
                if chunk.len() > super::STRING_LENGTH_LIMIT {
                    return Err(ParseError {
                        kind: ParseErrorKind::StringLengthLimitExceeded {
                            len: super::STRING_LENGTH_LIMIT,
                        },
                        pos: start,
                    });
                }
                val.push_str(chunk);
            }
            if i >= slice.len() {
                break;
            }
            match slice[i] {
                b'\\' => {
                    val.push(escapes::map_escape_character(slice, i));
                    i += escapes::len(slice, i);
                }
                // can only be utf8
                b => {
                    val.push(utf8::read_utf8_char(slice, i));
                    i += utf8::utf8_char_width(b);
                }
            }
        }
        Ok(val)
    }

    fn parse_number(&mut self, start: usize, offset: usize) -> Number {
        let slice = &self.buffer[start..start + offset];
        number::parse(slice)
    }

    fn parse_bool(&mut self, start: usize) -> bool {
        self.buffer[start] == b't'
    }

    fn peek(&mut self) -> Result<Option<&Token>, ParseError> {
        if self.peeked.is_none() {
            self.peeked = self.lexer.lex()?;
        }
        Ok(self.peeked.as_ref())
    }

    fn next(&mut self) -> Result<Option<Token>, ParseError> {
        match self.peeked.take() {
            Some(token) => Ok(Some(token)),
            None => Ok(self.lexer.lex()?),
        }
    }
}

fn expect(left: TokenKind, right: TokenKind) -> bool {
    left == right
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsing;
    use crate::parsing::number::Number;
    #[cfg(feature = "arbitrary_precision")]
    use bigdecimal::BigDecimal;
    #[cfg(feature = "arbitrary_precision")]
    use bigdecimal::num_bigint::BigInt;
    #[cfg(feature = "arbitrary_precision")]
    use std::str::FromStr;

    fn invalid_objects() -> Vec<(&'static [u8], ParseError)> {
        vec![
            (
                b"{",
                ParseError {
                    kind: ParseErrorKind::UnexpectedEof,
                    pos: 0,
                },
            ),
            (
                b"{ null : 1 }",
                ParseError {
                    kind: ParseErrorKind::UnexpectedToken {
                        expected: Some("object name"),
                    },
                    pos: 2,
                },
            ),
            (
                b"{ \"foo\": 5,",
                ParseError {
                    kind: ParseErrorKind::UnexpectedEof,
                    pos: 10,
                },
            ),
            (
                b"{ \"foo\": 5",
                ParseError {
                    kind: ParseErrorKind::UnexpectedEof,
                    pos: 9,
                },
            ),
            (
                b"{ \"foo\"",
                ParseError {
                    kind: ParseErrorKind::UnexpectedEof,
                    pos: 6,
                },
            ),
            (
                b"{ \"foo\" 3",
                ParseError {
                    kind: ParseErrorKind::UnexpectedToken {
                        expected: Some("colon ':'"),
                    },
                    pos: 8,
                },
            ),
            (
                b"{ \"foo\": \"value\" } 123",
                ParseError {
                    kind: ParseErrorKind::UnexpectedToken { expected: None },
                    pos: 19,
                },
            ),
            (
                b"{ \"foo\": \"bar\", \"foo\": \"baz\"}",
                ParseError {
                    kind: ParseErrorKind::DuplicateName {
                        name: String::from("foo"),
                    },
                    pos: 16,
                },
            ),
        ]
    }

    fn invalid_arrays() -> Vec<(&'static [u8], ParseError)> {
        vec![
            (
                b"[",
                ParseError {
                    kind: ParseErrorKind::UnexpectedEof,
                    pos: 0,
                },
            ),
            (
                b"[116, 943",
                ParseError {
                    kind: ParseErrorKind::UnexpectedEof,
                    pos: 8,
                },
            ),
            (
                b"[116, 943,",
                ParseError {
                    kind: ParseErrorKind::UnexpectedEof,
                    pos: 9,
                },
            ),
            (
                b"[116 true]",
                ParseError {
                    kind: ParseErrorKind::UnexpectedToken {
                        expected: Some("',' or ']'"),
                    },
                    pos: 5,
                },
            ),
            (
                b"[:]",
                ParseError {
                    kind: ParseErrorKind::UnexpectedToken {
                        expected: Some("json value"),
                    },
                    pos: 1,
                },
            ),
        ]
    }

    #[test]
    fn test_invalid_objects() {
        for (buffer, error) in invalid_objects() {
            let mut parser = Parser::new(buffer);
            let result = parser.parse();

            assert_eq!(result, Err(error), "failed to parse: {buffer:?}");
        }
    }

    #[test]
    fn test_invalid_arrays() {
        for (buffer, error) in invalid_arrays() {
            let mut parser = Parser::new(buffer);
            let result = parser.parse();

            assert_eq!(result, Err(error), "failed to tokenize: {buffer:?}");
        }
    }

    #[test]
    #[cfg(feature = "arbitrary_precision")]
    fn valid_array() {
        let buffer = "[116, -943, 9007199254740991, -3.14159265358979e+100, 0.1, 340282366920938463463374607431768211456]".as_bytes();

        let mut numbers = Vec::new();
        numbers.push(Value::Number(Number::from(116i64)));
        numbers.push(Value::Number(Number::from(-943i64)));
        numbers.push(Value::Number(Number::from(9007199254740991)));
        numbers.push(Value::Number(Number::from(
            BigDecimal::from_str("-3.14159265358979e+100").unwrap(),
        )));
        numbers.push(Value::Number(Number::from(
            BigDecimal::from_str("0.1").unwrap(),
        )));
        numbers.push(Value::Number(Number::from(
            BigInt::from_str("340282366920938463463374607431768211456").unwrap(),
        )));

        let mut parser = Parser::new(buffer);
        let result = parser.parse().unwrap();

        assert_eq!(Value::Array(numbers), result);
    }

    #[test]
    #[cfg(not(feature = "arbitrary_precision"))]
    fn valid_object() {
        // can't use br## because 🙂 is a Non ASCII character, empty strings as keys are allowed
        let buffer = r#"{
            "4_byte_sequence": "🙂",
            "surrogate_pair": "\uD83D\uDE00",
            "escape_characters": "\\\"\/\b\f\n\r\t",
            "boolean" : false,
            "numbers": [116, -943, 9007199254740991, -3.14159265358979e+100, 6.02214076e+23, 2.718281828e-50],
            "": true,
            "null": null
        }"#
        .as_bytes();

        // couldn't set up with json!() had problems with large numbers
        let mut numbers = Vec::new();
        numbers.push(Value::Number(Number::from(116i64)));
        numbers.push(Value::Number(Number::from(-943i64)));
        numbers.push(Value::Number(Number::from(9007199254740991)));
        numbers.push(Value::Number(Number::from(-3.14159265358979e+100)));
        numbers.push(Value::Number(Number::from(6.02214076e+23)));
        numbers.push(Value::Number(Number::from(2.718281828e-50)));

        let mut map = IndexMap::new();
        map.insert(
            "4_byte_sequence".to_string(),
            Value::String(String::from("🙂")),
        );
        map.insert(
            "surrogate_pair".to_string(),
            Value::String(String::from("😀")),
        );
        map.insert(
            "escape_characters".to_string(),
            Value::String(String::from("\\\"/\x08\x0C\n\r\t")),
        );
        map.insert("boolean".to_string(), Value::Boolean(false));
        map.insert("numbers".to_string(), Value::Array(numbers));
        map.insert("".to_string(), Value::Boolean(true));
        map.insert("null".to_string(), Value::Null);

        let mut parser = Parser::new(buffer);
        let result = parser.parse().unwrap();

        assert_eq!(Value::Object(map), result);
    }

    // same logic applies for [\n, \t, '\r', ' ']
    #[test]
    fn empty_input() {
        let buffer = [];
        let mut parser = Parser::new(&buffer);
        let error = ParseError {
            kind: ParseErrorKind::UnexpectedEof,
            pos: 0,
        };
        let result = parser.parse();

        assert_eq!(result, Err(error));
    }

    #[test]
    fn skip_whitespaces() {
        // \t, \n, \r, ' '
        let buffer: [u8; 4] = [9, 10, 13, 32];
        let mut parser = Parser::new(&buffer);
        let result = parser.parse();
        let error = ParseError {
            kind: ParseErrorKind::UnexpectedEof,
            pos: 3,
        };

        assert_eq!(result, Err(error));
    }

    #[test]
    fn empty_object() {
        let buffer = b"{}";
        let mut parser = Parser::new(buffer);
        let result = parser.parse().unwrap();

        assert_eq!(Value::Object(IndexMap::new()), result);
    }

    #[test]
    fn empty_array() {
        let buffer = b"[]";
        let mut parser = Parser::new(buffer);
        let result = parser.parse().unwrap();

        assert_eq!(Value::Array(Vec::new()), result);
    }

    //#[test]
    //  fn input_buffer_exceeds_size_limit() {
    //      let buffer: [u8; INPUT_BUFFER_LIMIT + 1] = [0; INPUT_BUFFER_LIMIT + 1];
    //      let mut parser = Parser::new(&buffer);
    //      let result = parser.parse();
    //
    //      assert_eq!(result, Err(JsonError::new(JsonErrorKind::InputBufferLimitExceeded { len: INPUT_BUFFER_LIMIT }, None)));
    //  }
    //
    //  The above test won't work, we get a Stack Overflow error because the allocation happens in the stack
    //  and might not have 4MBs of memory
    #[test]
    fn input_buffer_exceeds_size_limit() {
        let buffer = vec![b'"'; parsing::INPUT_BUFFER_LIMIT + 1];
        let mut parser = Parser::new(&buffer);
        let result = parser.parse();

        assert_eq!(
            result,
            Err(ParseError {
                kind: ParseErrorKind::InputBufferLimitExceeded {
                    len: parsing::INPUT_BUFFER_LIMIT
                },
                pos: 0
            })
        );
    }

    #[test]
    fn string_value_exceeds_length_limit() {
        let mut buffer = Vec::with_capacity(parsing::STRING_LENGTH_LIMIT + 3);

        buffer.push(b'"');
        buffer.extend(vec![b'a'; parsing::STRING_LENGTH_LIMIT + 1]);
        buffer.push(b'"');

        let mut parser = Parser::new(&buffer);
        let result = parser.parse();

        assert_eq!(
            result,
            Err(ParseError {
                kind: ParseErrorKind::StringLengthLimitExceeded {
                    len: parsing::STRING_LENGTH_LIMIT
                },
                pos: 0
            })
        );
    }

    // We can't call: let buffer = format!("{}{}", "[".repeat(257), "]".repeat(257)).as_bytes();
    //                              ^-- Creates String                                ^-- Borrows &[u8] from String
    //                              String is dropped here!
    // buffer now points to freed memory
    //
    // format!() creates a String, then .as_bytes() borrows from it, but the String gets dropped immediately after that line
    #[test]
    fn array_exceeds_nesting_depth() {
        let text = format!("{}{}", "[".repeat(257), "]".repeat(257));
        let buffer = text.as_bytes();
        let mut parser = Parser::new(buffer);
        let result = parser.parse();

        assert_eq!(
            result,
            Err(ParseError {
                kind: ParseErrorKind::NestingDepthLimitExceeded {
                    depth: parsing::NESTING_DEPTH_LIMIT
                },
                pos: parser.lexer.pos
            })
        );
    }

    #[test]
    fn object_exceeds_nesting_depth() {
        let mut text = "{}".to_string();
        for _ in 0..256 {
            // {}
            // {"key": {}}
            // {"key": {"key": {}}}
            text = format!(r#"{{"key": {}}}"#, text);
        }

        let buffer = text.as_bytes();
        let mut parser = Parser::new(buffer);
        let result = parser.parse();

        assert_eq!(
            result,
            Err(ParseError {
                kind: ParseErrorKind::NestingDepthLimitExceeded {
                    depth: parsing::NESTING_DEPTH_LIMIT
                },
                pos: parser.lexer.pos
            })
        );
    }
}
