use super::error::{ParseError, ParseErrorKind};
use super::lex::{Lexer, Token, TokenKind};
use super::value::Value;
use super::{escapes, number, utf8};
use crate::json::number::Number;
use indexmap::IndexMap;
use std::string::String;

enum Action {
    Continue,
    Break,
}

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
                pos: self.peeked.unwrap().start(),
            });
        }
        Ok(val)
    }

    // this method peeks to determine which method to call to parse the corresponding value, then
    // when we are ready to process the token each parse method calls next() to consume the token
    // and return the value
    //
    // The method that parses a value is responsible for consuming the tokens that make up that value.
    // The first call of every parse method is next() to consume the token that triggerred the call.
    //
    // parse_object() consumes '{'
    // parse_array consumes '['
    // parse_number,string,bool all need 1 token; they use the starting index and offset to parse
    // the value from the buffer.
    //
    // After parsing a value, peek() references the next token after it
    fn parse_value(&mut self) -> Result<Value, ParseError> {
        let Some(token) = self.peek()? else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedEof,
                pos: self.buffer.len() - 1,
            });
        };

        let val = match token.kind {
            TokenKind::LCurlyBracket => Value::Object(self.parse_object()?),
            TokenKind::LSquareBracket => Value::Array(self.parse_array()?),
            TokenKind::Number => Value::Number(self.parse_number()),
            TokenKind::String => Value::String(self.parse_string()?),
            TokenKind::Boolean => Value::Bool(self.parse_bool()),
            TokenKind::Null => {
                self.next()?;
                Value::Null
            }
            _ => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken {
                        expected: Some("json value"),
                    },
                    pos: token.start(),
                });
            }
        };
        Ok(val)
    }

    // read parse_value()
    fn parse_object(&mut self) -> Result<IndexMap<String, Value>, ParseError> {
        self.next()?; // Consume '{'

        self.check_depth()?;
        self.depth += 1;
        let len = self.buffer.len();
        let mut map = IndexMap::new();

        // Case: empty object {}
        match self.peek()? {
            Some(token) if token.kind == TokenKind::RCurlyBracket => {
                self.next()?;
                self.depth -= 1;
                return Ok(map);
            }
            Some(_) => (),
            None => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedEof,
                    pos: len - 1,
                });
            }
        }

        // We don't know how many tokens are part of the current object, the moment we encounter '}'
        // we break
        loop {
            // Initially had the following:
            //     loop {
            //         match self.peek()? {
            //             .....
            //             Some(token) if expect(token.kind, TokenKind::String) => {
            //                 let key = self.parse_string(token.start_index(), token.offset())?;
            //
            // The compiler will complain that we have 2 mutable borrows for self. We do, self.peek()
            // and then we call self.parse_string() which also takes &mut self, the borrow from peek
            // is still active part of the match block
            //
            // With this approach we extract the information we need, then the 1st borrow ends
            // after we are done with token, and we can call parse_string()
            let Some(token) = self.peek()? else {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedEof,
                    pos: len - 1,
                });
            };
            let kind = token.kind;
            let start = token.start();

            match kind {
                // expect key
                TokenKind::String => {
                    // We don't allow duplicate names at the same depth level
                    // {"key": false, "key": true} not allowed, duplicate name at the same level
                    // {"key": {"key": {}}} allowed, they are on a different depth level
                    let name = self.parse_string()?;
                    if map.contains_key(&name) {
                        return Err(ParseError {
                            kind: ParseErrorKind::DuplicateName { name },
                            pos: start,
                        });
                    }
                    self.expect_colon()?;
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
            // expect Comma to separate key-value pairs or '}' to end current object
            match self.expect_comma_or_end("string key", TokenKind::RCurlyBracket)? {
                Action::Continue => (),
                Action::Break => break,
            }
        }
        Ok(map)
    }

    // read parse_value()
    fn parse_array(&mut self) -> Result<Vec<Value>, ParseError> {
        self.next()?; // Consume '['

        self.check_depth()?;
        self.depth += 1;
        let mut arr = Vec::new();

        // Case: empty array []
        match self.peek()? {
            Some(token) if token.kind == TokenKind::RSquareBracket => {
                self.next()?;
                self.depth -= 1;
                return Ok(arr);
            }
            Some(_) => (),
            None => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedEof,
                    pos: self.buffer.len() - 1,
                });
            }
        }

        // when we encounter ']' we break
        loop {
            arr.push(self.parse_value()?);
            match self.expect_comma_or_end("json value", TokenKind::RSquareBracket)? {
                Action::Continue => (),
                Action::Break => break,
            }
        }
        Ok(arr)
    }

    // read parse_value()
    // to extract the value from slice all we need is the starting index and the offset
    // the only type of error that can occur at this point is LenLimitExceeded.
    fn parse_string(&mut self) -> Result<String, ParseError> {
        let token = self.next()?.unwrap();
        let slice = &self.buffer[token.start() + 1..token.end() - 1];
        let mut val = String::with_capacity(slice.len());
        let mut i = 0;

        while i < slice.len() {
            let j = i;
            // find the chunk that does not contain any escape that needs special handling
            while i < slice.len() && slice[i] != b'\\' {
                i += utf8::char_width(slice[i]);
            }
            // if it contains at least 1 character parse it
            if i > j {
                // SAFETY: lexer already verified the sequence
                val.push_str(unsafe { str::from_utf8_unchecked(&slice[j..i]) });
            }
            if i >= slice.len() {
                break;
            }
            // can be only be an escape
            val.push(escapes::map_escape_char(slice, i));
            i += escapes::len(slice, i);
        }

        if val.len() > super::STRING_LENGTH_LIMIT {
            return Err(ParseError {
                kind: ParseErrorKind::StringLengthLimitExceeded {
                    len: super::STRING_LENGTH_LIMIT,
                },
                pos: token.start(),
            });
        }

        Ok(val)
    }

    fn parse_number(&mut self) -> Number {
        // always safe to call unwrap twice; next() is called after parse_value() called peek()
        // so it can't be an Error and it can't be None
        let token = self.next().unwrap().unwrap();
        let slice = &self.buffer[token.start()..token.end()];
        number::parse(slice)
    }

    fn parse_bool(&mut self) -> bool {
        // always safe to call unwrap twice; next() is called after parse_value() called peek()
        // so it can't be an Error and it can't be None
        let token = self.next().unwrap().unwrap();
        self.buffer[token.start()] == b't'
    }

    fn expect_colon(&mut self) -> Result<(), ParseError> {
        match self.peek()? {
            Some(token) if token.kind == TokenKind::Colon => {
                self.next()?;
                Ok(())
            }
            // mismatch
            Some(token) => Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken {
                    expected: Some("colon ':'"),
                },
                pos: token.start(),
            }),
            None => Err(ParseError {
                kind: ParseErrorKind::UnexpectedEof,
                pos: self.buffer.len() - 1,
            }),
        }
    }

    fn check_depth(&mut self) -> Result<(), ParseError> {
        if self.depth + 1 > super::NESTING_DEPTH_LIMIT {
            return Err(ParseError {
                kind: ParseErrorKind::NestingDepthLimitExceeded {
                    depth: super::NESTING_DEPTH_LIMIT,
                },
                // the position of the buffer
                pos: self.lexer.pos,
            });
        }
        Ok(())
    }

    fn expect_comma_or_end(
        &mut self,
        expected: &'static str,
        bracket: TokenKind,
    ) -> Result<Action, ParseError> {
        let Some(token) = self.peek()? else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedEof,
                pos: self.buffer.len() - 1,
            });
        };

        match token.kind {
            TokenKind::Comma => {
                self.next()?;
                if let Some(next) = self.peek()? {
                    // Case: {"foo": "bar",}
                    if next.kind == bracket {
                        return Err(ParseError {
                            kind: ParseErrorKind::UnexpectedToken {
                                expected: Some(expected),
                            },
                            pos: next.start(),
                        });
                    }
                }
                // if we don't encounter a closing bracket or peek() returned None, continue and let
                // parse_value() handle it
                Ok(Action::Continue)
            }
            t if t == bracket => {
                self.next()?; // Consume '}'
                self.depth -= 1;
                Ok(Action::Break)
            }
            // mismatch
            _ => Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken {
                    expected: Some("',' or closing bracket"),
                },
                pos: token.start(),
            }),
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::json;
    use crate::json::number::Number;
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
                b"{ \"foo\": 5,}",
                ParseError {
                    kind: ParseErrorKind::UnexpectedToken {
                        expected: Some("string key"),
                    },
                    pos: 11,
                },
            ),
            (
                b"{ \"foo\": 5 null",
                ParseError {
                    kind: ParseErrorKind::UnexpectedToken {
                        expected: Some("',' or closing bracket"),
                    },
                    pos: 11,
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
                        expected: Some("',' or closing bracket"),
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
        numbers.push(Value::Number(Number::from(9007199254740991i64)));
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
        }"#.as_bytes();

        // couldn't set up with json!() had problems with large numbers
        let mut numbers = Vec::new();
        numbers.push(Value::Number(Number::from(116i64)));
        numbers.push(Value::Number(Number::from(-943i64)));
        numbers.push(Value::Number(Number::from(9007199254740991i64)));
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
            // the value is actually the "Grinning Face" emoji, codepoint U+1F600 but for some reason
            // it is not getting displayed
            Value::String(String::from("😀")),
        );
        map.insert(
            "escape_characters".to_string(),
            Value::String(String::from("\\\"/\x08\x0C\n\r\t")),
        );
        map.insert("boolean".to_string(), Value::Bool(false));
        map.insert("numbers".to_string(), Value::Array(numbers));
        map.insert("".to_string(), Value::Bool(true));
        map.insert("null".to_string(), Value::Null);

        let mut parser = Parser::new(buffer);
        let result = parser.parse().unwrap();

        assert_eq!(Value::Object(map), result);
    }

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
    fn only_whitespaces() {
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
        let buffer = vec![b'"'; json::INPUT_BUFFER_LIMIT + 1];
        let mut parser = Parser::new(&buffer);
        let result = parser.parse();

        assert_eq!(
            result,
            Err(ParseError {
                kind: ParseErrorKind::InputBufferLimitExceeded {
                    len: json::INPUT_BUFFER_LIMIT
                },
                pos: 0
            })
        );
    }

    #[test]
    fn string_len_exceeds_limit() {
        let mut buffer = Vec::with_capacity(json::STRING_LENGTH_LIMIT + 3);

        buffer.push(b'"');
        buffer.extend(vec![b'a'; json::STRING_LENGTH_LIMIT + 1]);
        buffer.push(b'"');

        let mut parser = Parser::new(&buffer);
        let result = parser.parse();

        assert_eq!(
            result,
            Err(ParseError {
                kind: ParseErrorKind::StringLengthLimitExceeded {
                    len: json::STRING_LENGTH_LIMIT
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
    fn exceeds_depth_limit() {
        let text = format!("{}{}", "[".repeat(257), "]".repeat(257));
        let buffer = text.as_bytes();
        let mut parser = Parser::new(buffer);
        let result = parser.parse();

        assert_eq!(
            result,
            Err(ParseError {
                kind: ParseErrorKind::NestingDepthLimitExceeded {
                    depth: json::NESTING_DEPTH_LIMIT
                },
                pos: parser.lexer.pos
            })
        );
    }
}
