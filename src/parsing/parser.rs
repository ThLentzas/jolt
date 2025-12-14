use std::string::String;
use std::cmp::PartialEq;
use std::collections::HashSet;
use indexmap::IndexMap;
use super::lexer::{Lexer, LexerToken, LexerTokenType};
use super::value::Value;
use super::error::{ParserError, ParserErrorKind, StringError, StringErrorKind};
use super::{
    escapes,
    number,
    utf8,
    INPUT_BUFFER_LIMIT,
    NESTING_DEPTH_LIMIT,
    STRING_VALUE_LENGTH_LIMIT
};
use super::number::Number;

#[derive(Debug, PartialEq)]
enum ParserTokenType {
    ObjectStart,
    ObjectEnd,
    NameSeparator,
    ValueSeparator,
    ArrayStart,
    ArrayEnd,
    Number,
    String,
    Boolean,
    Null,
}

struct ParserToken {
    start_index: usize,
    offset: u32,
    token_type: ParserTokenType,
}

pub(super) struct Parser<'a> {
    buffer: &'a [u8],
    lexer: Lexer<'a>,
    tokens: Vec<ParserToken>,
    pos: usize,
    depth: u16
}

impl<'a> Parser<'a> {
    pub(super) fn new(buffer: &'a [u8]) -> Self {
        Self {
            buffer,
            lexer: Lexer::new(buffer),
            tokens: Vec::new(),
            pos: 0,
            depth: 0,
        }
    }

    // toDo: advantages of peek/advance and recursive descendant parser, peek is just look ahead without consuming
    pub(super) fn parse(&mut self) -> Result<Value, ParserError> {
        if self.buffer.len() > INPUT_BUFFER_LIMIT {
            return Err(ParserError {
                kind: ParserErrorKind::InputBufferLimitExceeded { len: INPUT_BUFFER_LIMIT },
                pos: None
            });
        }
        // https://www.rfc-editor.org/rfc/rfc8259#section-8.1
        if utf8::is_bom_present(self.buffer) {
            self.lexer.consume(3);
        }

        let token = self.peek()?;
        // &token would create &Option<LexerToken> not Option<&LexerToken>
        self.parse_value(token.as_ref())?;
        match self.peek()? {
            // after successfully parsing a value we can't have leftover tokens
            // false5, "abc"123, {}  null, note that this could return an error, {}001, -> leading zeros are not allowed
            Some(token) => Err(ParserError {
                kind: ParserErrorKind::UnexpectedToken { expected: None },
                pos: Some(token.start_index())
            }),
            // whoever calls parse() will be the owner of the return value
            None => Ok(self.value()?)
        }
    }

    // None of the parse_* calls moves past the related tokens, we advance after
    // For cases like object and array we perform a dfs because if an object has a value that is
    // object or array we need to full instantiate that first and then return to continue with the
    // rest of values. (similar to recursive descendant in path)
     fn parse_value(&mut self, token: Option<&LexerToken>) -> Result<(), ParserError> {
        match token {
            Some(t) => {
                match t.token_type() {
                    LexerTokenType::LCurlyBracket => self.parse_object(t)?,
                    LexerTokenType::LSquareBracket => self.parse_array(t)?,
                    LexerTokenType::Number => self.parse_number(t),
                    LexerTokenType::String => self.parse_string(t),
                    LexerTokenType::Boolean => self.parse_boolean(t),
                    LexerTokenType::Null => self.parse_null(t),
                    _ => return Err(ParserError {
                        kind: ParserErrorKind::UnexpectedToken { expected: Some("json value") },
                        pos: Some(t.start_index())
                    })
                }
            }
            None => {
                let pos = if self.buffer.is_empty() {
                    self.buffer.len()
                } else {
                    self.buffer.len() - 1
                };
                return Err(ParserError { kind: ParserErrorKind::UnexpectedEof, pos: Some(pos) });
            }
        }
        self.consume(1);
         Ok(())
     }

    // We peek and advance 4 times, after moving past the opening '{'
    //
    // 1. peek and expect either '}' for an empty object or a key string -> advance
    // 2. peek and expect ':' after key -> advance
    // 3. after colon we call parse_value(), it advances internally the pointer to the next token
    // 4. peek, after value we expect either ',' to separate from the next key-value pair -> advance
    // or '}' -> break
    //
    // When encountering '}' in steps 1 or 4, we break WITHOUT calling advance(). The closing '}'
    // will be consumed by the caller (parse_value()) after this function returns.
    fn parse_object(&mut self, token: &LexerToken) -> Result<(), ParserError> {
        if self.depth + 1 > NESTING_DEPTH_LIMIT {
            return Err(ParserError {
                kind: ParserErrorKind::NestingDepthLimitExceeded { depth: NESTING_DEPTH_LIMIT },
                pos: None
            });
        }
        self.tokens.push(ParserToken {
            start_index: token.start_index(),
            offset: token.offset(),
            token_type: ParserTokenType::ObjectStart
        });
        // '{'
        self.consume(1);
        self.depth += 1;
        let buffer_len = self.buffer.len();
        let mut names = HashSet::new();

        // We don't know how many tokens are part of the current object, the moment we encounter '}' we break
        loop {
            match self.peek()? {
                // {"foo": "bar",}
                Some(next) if expect(next.token_type(), LexerTokenType::RCurlyBracket) => {
                    if self.tokens.last().unwrap().token_type == ParserTokenType::ValueSeparator {
                        return Err(ParserError {
                            kind: ParserErrorKind::UnexpectedToken { expected: Some("object name") },
                            pos: Some(next.start_index())
                        });
                    }
                    // {}
                    self.tokens.push(ParserToken {
                        start_index: next.start_index(),
                        offset: next.offset(),
                        token_type: ParserTokenType::ObjectEnd
                    });
                    self.depth -= 1;
                    break;
                }
                Some(next) if expect(next.token_type(), LexerTokenType::String) => {
                    // Drop opening/closing quotes
                    let name= &self.buffer[next.start_index() + 1..next.start_index() + (next.offset() - 1) as usize];
                    // We don't allow duplicate names at the same depth level
                    // {"key": false, "key": true} not allowed, duplicate name at the same level
                    // {"key": {"key": {}}} allowed, they are on a different depth level
                    if names.contains(name) {
                        return Err(ParserError {
                            kind: ParserErrorKind::DuplicateName {
                                name: String::from_utf8(Vec::from(name)).unwrap()
                        },
                            pos:Some(next.start_index())
                        });
                    }
                    names.insert(name);
                    self.parse_string(&next);
                    self.consume(1);
                }
                // mismatch
                Some(next) => {
                    return Err(ParserError {
                        kind: ParserErrorKind::UnexpectedToken { expected: Some("object name") },
                        pos: Some(next.start_index())
                    });
                }
                None => return Err(ParserError {
                    kind: ParserErrorKind::UnexpectedEof,
                    pos: Some(buffer_len - 1)
                })
            }

            match self.peek()? {
                Some(next) if expect(next.token_type(), LexerTokenType::Colon) => {
                    self.tokens.push(ParserToken {
                        start_index: next.start_index(),
                        offset: next.offset(),
                        token_type: ParserTokenType::NameSeparator
                    });
                    self.consume(1);
                }
                // mismatch
                Some(next) => {
                    return Err(ParserError {
                        kind: ParserErrorKind::UnexpectedToken { expected: Some("colon ':'") },
                        pos: Some(next.start_index())
                    });
                }
                None => return Err(ParserError {
                    kind: ParserErrorKind::UnexpectedEof,
                    pos: Some(buffer_len - 1)
                })
            }
            let next = self.peek()?;
            self.parse_value(next.as_ref())?;

            match self.peek()? {
                Some(next) if expect(next.token_type(), LexerTokenType::Comma) => {
                    self.tokens.push(ParserToken {
                        start_index: next.start_index(),
                        offset: next.offset(),
                        token_type: ParserTokenType::ValueSeparator
                    });
                    self.consume(1);
                }
                Some(next) if expect(next.token_type(), LexerTokenType::RCurlyBracket) => {
                    self.tokens.push(ParserToken {
                        start_index: next.start_index(),
                        offset: next.offset(),
                        token_type: ParserTokenType::ObjectEnd
                    });
                    self.depth -= 1;
                    break;
                }
                // mismatch
                Some(next) => return Err(ParserError {
                    kind: ParserErrorKind::UnexpectedToken { expected: Some("'}' or ','") },
                    pos: Some(next.start_index())
                }),
                None => return Err(ParserError {
                    kind: ParserErrorKind::UnexpectedEof,
                    pos: Some(buffer_len - 1)
                }),
            }
        }
        Ok(())
    }

    // We peek and advance 2 times, after moving past the opening '['
    //
    // 1. peek and expect either ']' for an empty array or a json value(parse_value() will advance)
    // 2. peek and expect ',' to separate values -> advance, or ']' -> break;
    //
    // When encountering ']', we break WITHOUT calling advance(). The closing ']' will be consumed
    // by the caller (parse_value()) after this function returns.
    fn parse_array(&mut self, token: &LexerToken) -> Result<(), ParserError> {
        if self.depth + 1 > NESTING_DEPTH_LIMIT {
            return Err(ParserError {
                kind: ParserErrorKind::NestingDepthLimitExceeded { depth: NESTING_DEPTH_LIMIT },
                pos: None
            });
        }

        self.tokens.push(ParserToken {
            start_index: token.start_index(),
            offset: token.offset(),
            token_type: ParserTokenType::ArrayStart
        });
        self.consume(1);
        self.depth += 1;
        let buffer_len = self.buffer.len();

        loop {
            match self.peek()? {
                // [1,2,]
                Some(next) if expect(next.token_type(), LexerTokenType::RSquareBracket) => {
                    if self.tokens.last().unwrap().token_type == ParserTokenType::ValueSeparator {
                        return Err(ParserError {
                            kind: ParserErrorKind::UnexpectedToken { expected: Some("json value") },
                            pos: Some(next.start_index())
                        });
                    }
                    // []
                    self.tokens.push(ParserToken {
                        start_index: next.start_index(),
                        offset: next.offset(),
                        token_type: ParserTokenType::ArrayEnd
                    });
                    self.depth -= 1;
                    break;
                }
                // Some(next) or None parse.value() will handle both cases
                // Some(next) will be parsed if possible
                // None is UnexpectedEof
                next => self.parse_value(next.as_ref())?,
            }
            match self.peek()? {
                Some(next) if expect(next.token_type(), LexerTokenType::Comma) => {
                    self.tokens.push(ParserToken {
                        start_index: next.start_index(),
                        offset: next.offset(),
                        token_type: ParserTokenType::ValueSeparator
                    });
                    self.consume(1);
                }
                Some(next) if expect(next.token_type(), LexerTokenType::RSquareBracket) => {
                    self.tokens.push(ParserToken {
                        start_index: next.start_index(),
                        offset: next.offset(),
                        token_type: ParserTokenType::ArrayEnd
                    });
                    self.depth -= 1;
                    break;
                }
                Some(next) => return Err(ParserError {
                    kind: ParserErrorKind::UnexpectedToken { expected: Some("',' or ']'") },
                    pos: Some(next.start_index())
                }),
                None => return Err(ParserError {
                    kind: ParserErrorKind::UnexpectedEof,
                    pos: Some(buffer_len - 1)
                }),
            }
        }
        Ok(())
    }

    fn parse_number(&mut self, token: &LexerToken) {
        self.tokens.push(ParserToken {
            start_index: token.start_index(),
            offset: token.offset(),
            token_type: ParserTokenType::Number
        });
    }

    fn parse_string(&mut self, token: &LexerToken) {
        self.tokens.push(ParserToken {
            start_index: token.start_index(),
            offset: token.offset(),
            token_type: ParserTokenType::String
        });
    }

    fn parse_boolean(&mut self, token: &LexerToken) {
        self.tokens.push(ParserToken {
            start_index: token.start_index(),
            offset: token.offset(),
            token_type: ParserTokenType::Boolean
        });
    }

    fn parse_null(&mut self, token: &LexerToken) {
        self.tokens.push(ParserToken {
            start_index: token.start_index(),
            offset: token.offset(),
            token_type: ParserTokenType::Null
        });
    }

    fn peek(&mut self) -> Result<Option<LexerToken>, ParserError> {
       Ok(self.lexer.next()?)
    }

    fn consume(&mut self, n: usize) {
        self.lexer.consume(n)
    }

    // peek and consume are used during the parsing process, next is used when we build the value
    // to access the tokens produced by the parser
    fn next(&self) -> Option<&ParserToken>{
        self.tokens.get(self.pos)
    }

    fn value(&mut self) -> Result<Value, ParserError> {
        let token = &self.tokens[self.pos];
        let val = match token.token_type {
            ParserTokenType::ObjectStart => Value::Object(self.object_value()?),
            ParserTokenType::ArrayStart => Value::Array(self.array_value()?),
            ParserTokenType::Number => Value::Number(self.number_value()),
            ParserTokenType::String => Value::String(self.string_value()?),
            ParserTokenType::Boolean => Value::Boolean(self.boolean_value()),
            ParserTokenType::Null => Value::Null,
            // only the above cases will start a json value
            _ => unreachable!("Unexpected token type for the start of json value at index {}", self.pos),
        };
        self.pos += 1;

        Ok(val)
    }

    fn object_value(&mut self) -> Result<IndexMap<String, Value>, ParserError> {
        let mut map = IndexMap::new();
        self.pos += 1; // move past '{'

        // empty object
        if self.tokens[self.pos].token_type == ParserTokenType::ObjectEnd {
            return Ok(map);
        }

        while let Some(token) = self.next() {
            // Drop opening/closing quotes
            let name: &[u8] = &self.buffer[token.start_index + 1 ..token.start_index + (token.offset - 1)  as usize];
            let key = String::from_utf8(Vec::from(name)).unwrap(); // toDo: can this be &str?
            self.pos += 2; // skip key,colon
            let value = self.value()?;
            map.insert(key, value);
            // we expect either ',' or '}'. The structure of the object is always valid at this point
            if let Some(token) = self.next() {
                if token.token_type == ParserTokenType::ObjectEnd {
                    break;
                }
            }
            self.pos += 1; // skip comma
        }
        Ok(map)
    }

    fn array_value(&mut self) -> Result<Vec<Value>, ParserError> {
        let mut values = Vec::new();
        self.pos += 1; // move past '['

        // empty array
        if self.tokens[self.pos].token_type == ParserTokenType::ArrayEnd {
            return Ok(values);
        }

        while let Some(_) = self.next() {
            values.push(self.value()?);
            if let Some(token) = self.next() {
                if token.token_type == ParserTokenType::ArrayEnd {
                    break;
                }
            }
            self.pos += 1; // skip comma
        }
        Ok(values)
    }

    fn number_value(&mut self) -> Number {
        let token = &self.tokens[self.pos];
        let slice = &self.buffer[token.start_index..token.start_index + token.offset as usize];
        number::parse(slice)
    }

    fn string_value(&self) -> Result<String, ParserError> {
        let token = &self.tokens[self.pos];
        let mut val = String::new();
        let mut i = token.start_index + 1; // Skip opening quotation

        while i < token.start_index + (token.offset - 1) as usize { // Skip closing quotation
            let current = self.buffer[i];
            match current {
                b'\\' => {
                    val.push(escapes::map_escape_character(self.buffer, i));
                    i += escapes::len(self.buffer, i) - 1;
                }
                b if !b.is_ascii() => {
                    val.push(utf8::read_utf8_char(self.buffer, i));
                    i += utf8::utf8_char_width(current) - 1;
                }
                _ => val.push(current as char)
            }
            i += 1;
        }

        if val.len() > STRING_VALUE_LENGTH_LIMIT {
            return Err(ParserError::from(StringError {
                kind: StringErrorKind::StringValueLengthLimitExceed {
                    len: STRING_VALUE_LENGTH_LIMIT },
                pos: token.start_index
            }));
        }
        Ok(val)
    }

    fn boolean_value(&mut self) -> bool {
        let token = &self.tokens[self.pos];
        self.buffer[token.start_index] == b't'
    }
}

fn expect(left: &LexerTokenType, right: LexerTokenType) -> bool {
    *left == right
}

#[cfg(test)]
mod tests {
    #[cfg(feature = "big_decimal")]
    use std::str::FromStr;
    #[cfg(feature = "big_decimal")]
    use bigdecimal::BigDecimal;
    #[cfg(feature = "big_decimal")]
    use bigdecimal::num_bigint::BigInt;
    use super::*;

    fn invalid_objects() -> Vec<(&'static [u8], ParserError)> {
        vec![
            (b"{", ParserError { kind: ParserErrorKind::UnexpectedEof, pos: Some(0) }),
            (b"{ null : 1 }", ParserError {
                kind: ParserErrorKind::UnexpectedToken { expected: Some("object name") },
                pos: Some(2)
            }),
            (b"{ \"foo\": 5,", ParserError { kind: ParserErrorKind::UnexpectedEof, pos: Some(10) }),
            (b"{ \"foo\": 5", ParserError { kind: ParserErrorKind::UnexpectedEof, pos: Some(9) }),
            (b"{ \"foo\"", ParserError { kind: ParserErrorKind::UnexpectedEof, pos: Some(6) }),
            (b"{ \"foo\" 3", ParserError {
                kind: ParserErrorKind::UnexpectedToken { expected: Some("colon ':'") },
                pos: Some(8)
            }),
            (b"{ \"foo\": \"value\" } 123", ParserError {
                kind: ParserErrorKind::UnexpectedToken { expected: None },
                pos: Some(19)
            }),
            (b"{ \"foo\": \"bar\", \"foo\": \"baz\"}", ParserError {
                kind: ParserErrorKind::DuplicateName { name: String::from("foo") },
                pos: Some(16)
            })
        ]
    }

    fn invalid_arrays() -> Vec<(&'static [u8], ParserError)> {
        vec![
            (b"[", ParserError { kind: ParserErrorKind::UnexpectedEof, pos: Some(0) }),
            (b"[116, 943", ParserError { kind: ParserErrorKind::UnexpectedEof, pos: Some(8) }),
            (b"[116, 943,", ParserError { kind: ParserErrorKind::UnexpectedEof, pos: Some(9) }),
            (b"[116 true]", ParserError {
                kind: ParserErrorKind::UnexpectedToken { expected: Some("',' or ']'") },
                pos: Some(5)
            }),
            (b"[:]", ParserError {
                kind: ParserErrorKind::UnexpectedToken { expected: Some("json value") },
                pos: Some(1)
            }),
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
    #[cfg(feature = "big_decimal")]
    fn valid_array() {
        let buffer = "[116, -943, 9007199254740991, -3.14159265358979e+100, 0.1, 340282366920938463463374607431768211456]".as_bytes();

        let mut numbers = Vec::new();
        numbers.push(Value::Number(Number::from(116i64)));
        numbers.push(Value::Number(Number::from(-943i64)));
        numbers.push(Value::Number(Number::from(9007199254740991)));
        numbers.push(Value::Number(Number::from(BigDecimal::from_str("-3.14159265358979e+100").unwrap())));
        numbers.push(Value::Number(Number::from(BigDecimal::from_str("0.1").unwrap())));
        numbers.push(Value::Number(Number::from(BigInt::from_str("340282366920938463463374607431768211456").unwrap())));

        let mut parser = Parser::new(buffer);
        let result = parser.parse().unwrap();

        assert_eq!(Value::Array(numbers), result);
    }

    #[test]
    #[cfg(not(feature = "big_decimal"))]
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
        numbers.push(Value::Number(Number::from(9007199254740991)));
        numbers.push(Value::Number(Number::from(-3.14159265358979e+100)));
        numbers.push(Value::Number(Number::from(6.02214076e+23)));
        numbers.push(Value::Number(Number::from(2.718281828e-50)));

        let mut map = IndexMap::new();
        map.insert("4_byte_sequence".to_string(), Value::String(String::from("🙂")));
        map.insert("surrogate_pair".to_string(), Value::String(String::from("😀")));
        map.insert("escape_characters".to_string(), Value::String(String::from("\\\"/\x08\x0C\n\r\t")));
        map.insert("boolean".to_string(), Value::Boolean(false));
        map.insert("numbers".to_string(), Value::Array(numbers));
        map.insert("".to_string(), Value::Boolean(true));
        map.insert("null".to_string(), Value::Null);

        let mut parser = Parser::new(buffer);
        let result = parser.parse().unwrap();

        assert_eq!(Value::Object(map), result);
    }

    // [](empty input buffer) is invalid, JsonText = ws value ws, a value is mandatory
    // same logic applies for [\n, \t, '\r', ' ']
    #[test]
    fn empty_input() {
        let buffer = [];
        let mut parser = Parser::new(&buffer);
        let error = ParserError { kind: ParserErrorKind::UnexpectedEof, pos: Some(0) };
        let result = parser.parse();

        assert_eq!(result, Err(error));
    }

    #[test]
    fn skip_whitespaces() {
        // \t, \n, \r, ' '
        let buffer: [u8; 4] = [9, 10, 13, 32];
        let mut parser = Parser::new(&buffer);
        let result = parser.parse();
        let error = ParserError { kind: ParserErrorKind::UnexpectedEof, pos: Some(3) };

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
    //  toDo: consider this when we implement Reader
    //
    //  The above test won't work, we get a Stack Overflow error because the allocation happens in the stack
    //  and might not have 4MBs of memory
    //
    //  We use heap allocation because Vec always allocates its data on the heap and even if buffer
    //  is Vec<u8> a reference to buffer is &[u8]
    #[test]
    fn input_buffer_exceeds_size_limit() {
        let buffer = vec![b'"'; INPUT_BUFFER_LIMIT + 1];
        let mut parser = Parser::new(&buffer);
        let result = parser.parse();

        assert_eq!(result, Err(ParserError {
            kind: ParserErrorKind::InputBufferLimitExceeded { len: INPUT_BUFFER_LIMIT },
            pos: None
        }));
    }

    #[test]
    fn string_value_exceeds_length_limit() {
        let mut buffer = Vec::with_capacity(STRING_VALUE_LENGTH_LIMIT + 3);

        buffer.push(b'"');
        buffer.extend(vec![b'a'; STRING_VALUE_LENGTH_LIMIT + 1]);
        buffer.push(b'"');

        let mut parser = Parser::new(&buffer);
        let result = parser.parse();

        assert_eq!(result, Err(ParserError::from(StringError { kind: StringErrorKind::StringValueLengthLimitExceed { len: STRING_VALUE_LENGTH_LIMIT }, pos: 0 })));
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

        assert_eq!(result, Err(ParserError {
            kind: ParserErrorKind::NestingDepthLimitExceeded { depth: NESTING_DEPTH_LIMIT },
            pos: None
        }));
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

        assert_eq!(result, Err(ParserError {
            kind: ParserErrorKind::NestingDepthLimitExceeded { depth: NESTING_DEPTH_LIMIT },
            pos: None
        }));
    }
}