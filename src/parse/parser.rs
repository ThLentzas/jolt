use std::string::String;
use std::cmp::PartialEq;
use std::collections::HashSet;
use linked_hash_map::LinkedHashMap;
use crate::error::ParserError;
use crate::parse::tokenizer::{TokenizerToken, TokenizerTokenType};
use crate::utils::{numeric_utils, utf8_utils};
use crate::utils::numeric_utils::Number;

const MAX_DEPTH: u16 = 256;

#[derive(Debug, PartialEq)]
pub enum ParserTokenType {
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

pub struct ParserToken {
    start_index: usize,
    offset: u32,
    token_type: ParserTokenType,
}

#[derive(Debug, PartialEq)]
pub enum JsonValue {
    Null,
    Boolean(bool),
    Number(Number),
    String(String),
    Array(Vec<JsonValue>),
    Object(LinkedHashMap<String, JsonValue>),
}

#[derive(Debug, PartialEq)]
pub struct JsonNode {
    // [](empty input buffer) is not invalid, the value of the root of AST is None
    // same logic applies for [\n, \t, '\r', ' ']
    val: Option<JsonValue>
}

impl ParserToken {
    fn new(start_index: usize, offset: u32, token_type: ParserTokenType) -> Self {
        Self {
            start_index,
            offset,
            token_type,
        }
    }
}

pub struct Parser<'a> {
    buffer: &'a [u8],
    tokens: &'a Vec<TokenizerToken>,
    _tokens: Vec<ParserToken>,
    pos: usize,
    token_idx: usize,
    depth: u16
}


impl<'a> Parser<'a> {
    pub fn new(buffer: &'a [u8], tokens: &'a Vec<TokenizerToken>) -> Self {
        Self {
            buffer,
            tokens,
            _tokens: Vec::new(),
            pos: 0,
            depth: 0,
            token_idx: 0
        }
    }

    pub fn parse(&mut self) -> Result<JsonNode, ParserError> {
        if self.tokens.is_empty() {
            return Ok(JsonNode { val: None });
        }

        self.parse_value()?;
        // toDo: consider resetting indices in case someone calls parser.parse() multiple times
        // false5, "abc"123, {}null
        if self.pos < self.tokens.len() {
            return Err(ParserError::UnexpectedToken { pos: self.tokens[self.pos].start_index(), token: None }); // change that to the actual value since we know
            // that the token is valid from tokenizer, we can create a slice from the original buffer that does have a text representation
        }

        self.token_idx = 0;
        self.pos = 0;
        Ok(JsonNode { val: Some(self.json_value()) })
    }

     fn parse_value(&mut self) -> Result<(), ParserError> {
         match self.tokens.get(self.pos) {
             Some(token) => match *token.token_type() {
                 TokenizerTokenType::LeftCurlyBracket => self.parse_object()?,
                 TokenizerTokenType::LeftSquareBracket => self.parse_array()?,
                 TokenizerTokenType::Number => self.parse_number(),
                 TokenizerTokenType::String => self.parse_string(),
                 TokenizerTokenType::Boolean => self.parse_boolean(),
                 TokenizerTokenType::Null => self.parse_null(),
                 _ => return Err(ParserError::UnexpectedToken { token: Some("json value"), pos: token.start_index() })
             }
             None => return Err(ParserError::UnexpectedEndOfInput { pos: self.buffer.len() - 1})
         }
         Ok(())
     }
    
    fn parse_object(&mut self) -> Result<(), ParserError> {
        if self.depth + 1 > MAX_DEPTH {
            return Err(ParserError::DepthLimitExceeded { depth: MAX_DEPTH });
        }

        let current = &self.tokens[self.pos];
        let buffer_len = self.buffer.len();
        self._tokens.push(ParserToken::new(current.start_index(), current.offset(), ParserTokenType::ObjectStart));
        self.depth += 1;
        self.advance();

        let mut names = HashSet::new();
        // We don't know how many tokens are part of the current object, the moment we encounter '}' we break
        loop {
            // {"foo": "bar",}
            match self.peek() {
                Some(next) if expect(next.token_type(), TokenizerTokenType::RightCurlyBracket) => {
                    if self._tokens.last().unwrap().token_type == ParserTokenType::ValueSeparator {
                        return Err(ParserError::UnexpectedToken { pos: next.start_index(), token: Some("object name") });
                    }
                }
                // {}
                Some(next) if expect(next.token_type(), TokenizerTokenType::RightCurlyBracket) => {
                    self._tokens.push(ParserToken::new(next.start_index(), next.offset(), ParserTokenType::ObjectEnd));
                    break;
                }
                Some(next) if expect(next.token_type(), TokenizerTokenType::String) => {
                    // Drop opening/closing quotes
                    let name: &[u8] = &self.buffer[next.start_index() + 1..next.start_index() + (next.offset() - 1) as usize];
                    // We don't allow duplicate names at the same depth level
                    // {"key": false, "key": true} not allowed, duplicate name at the same level
                    // {"key": {"key": {}}} allowed, they are on a different depth level
                    if names.contains(name) {
                        return Err(ParserError::DuplicateName { name: String::from_utf8(Vec::from(name)).unwrap(), pos: next.start_index() });
                    }
                    names.insert(name);
                    self.parse_string();
                    self.advance();
                }
                // mismatch
                Some(next) => {
                    return Err(ParserError::UnexpectedToken { pos: next.start_index(), token: Some("object name") });
                }
                None => return Err(ParserError::UnexpectedEndOfInput { pos: buffer_len - 1 }),
            }

            match self.peek() {
                Some(next) if expect(next.token_type(), TokenizerTokenType::Colon) => {
                    self._tokens.push(ParserToken::new(next.start_index(), next.offset(), ParserTokenType::NameSeparator));
                    self.advance();
                }
                // mismatch
                Some(next) => {
                    return Err(ParserError::UnexpectedToken { token: Some("colon ':'"), pos: next.start_index() });
                }
                None => return Err(ParserError::UnexpectedEndOfInput { pos: buffer_len - 1 }),
            }

            self.parse_value()?;
            self.advance();

            match self.peek() {
                Some(next) if expect(next.token_type(), TokenizerTokenType::Comma) => {
                    self._tokens.push(ParserToken::new(next.start_index(), next.offset(), ParserTokenType::ValueSeparator));
                    self.advance();
                }
                Some(next) if expect(next.token_type(), TokenizerTokenType::RightCurlyBracket) => {
                    self._tokens.push(ParserToken::new(next.start_index(), next.offset(), ParserTokenType::ObjectEnd));
                    self.depth -= 1;
                    break;
                }
                // mismatch
                Some(next) => {
                    return Err(ParserError::UnexpectedToken { token: Some("'}' or ','"), pos: next.start_index() });
                }
                None => return Err(ParserError::UnexpectedEndOfInput { pos: buffer_len - 1 }),
            }
        }
        self.advance();

        Ok(())
    }

    fn parse_array(&mut self) -> Result<(), ParserError> {
        if self.depth + 1 > MAX_DEPTH {
            return Err(ParserError::DepthLimitExceeded { depth: MAX_DEPTH });
        }

        let current = &self.tokens[self.pos];
        let buffer_len = self.buffer.len();
        self._tokens.push(ParserToken::new(current.start_index(), current.offset(), ParserTokenType::ArrayStart));
        self.depth += 1;
        self.advance();

        loop {
            match self.peek() {
                // [1,2,]
                Some(next) if expect(next.token_type(), TokenizerTokenType::RightSquareBracket) => {
                    if self._tokens.last().unwrap().token_type == ParserTokenType::ValueSeparator {
                        return Err(ParserError::UnexpectedToken { pos: next.start_index(), token: Some("json value") });
                    }
                }
                // []
                Some(next) if expect(next.token_type(), TokenizerTokenType::RightSquareBracket) => {
                    self._tokens.push(ParserToken::new(next.start_index(), next.offset(), ParserTokenType::ArrayEnd));
                    break;
                }
                Some(_) => {
                    self.parse_value()?;
                    self.advance();
                },
                None => return Err(ParserError::UnexpectedEndOfInput { pos: buffer_len - 1 }),
            }

            match self.peek() {
                Some(next) if expect(next.token_type(), TokenizerTokenType::Comma) => {
                    self._tokens.push(ParserToken::new(next.start_index(), next.offset(), ParserTokenType::ValueSeparator));
                    self.advance();
                }
                Some(next) if expect(next.token_type(), TokenizerTokenType::RightSquareBracket) => {
                    self._tokens.push(ParserToken::new(next.start_index(), next.offset(), ParserTokenType::ArrayEnd));
                    self.depth -= 1;
                    break;
                }
                Some(next) => {
                    return Err(ParserError::UnexpectedToken { token: Some("',' or ']'"), pos: next.start_index() });
                }
                None => return Err(ParserError::UnexpectedEndOfInput { pos: buffer_len - 1 }),
            }
        }
        self.advance();
        Ok(())
    }

    fn parse_number(&mut self) {
        let token = &self.tokens[self.pos];
        self._tokens.push(ParserToken::new(token.start_index(), token.offset(), ParserTokenType::Number))
    }

    fn parse_string(&mut self) {
        let token = &self.tokens[self.pos];
        self._tokens.push(ParserToken::new(token.start_index(), token.offset(), ParserTokenType::String))
    }

    fn parse_boolean(&mut self) {
        let token = &self.tokens[self.pos];
        self._tokens.push(ParserToken::new(token.start_index(), token.offset(), ParserTokenType::Boolean))
    }

    fn parse_null(&mut self) {
        let token = &self.tokens[self.pos];
        self._tokens.push(ParserToken::new(token.start_index(), token.offset(), ParserTokenType::Null))
    }

    fn peek(& self) -> Option<&TokenizerToken> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn next(& self) -> Option<&ParserToken>{
        self._tokens.get(self.token_idx)
    }

    fn json_value(&mut self) -> JsonValue {
        let token = &self._tokens[self.token_idx];
        match token.token_type {
            ParserTokenType::ObjectStart => { JsonValue::Object(self.object_value()) },
            ParserTokenType::ArrayStart => { JsonValue::Array(self.array_value()) },
            ParserTokenType::Number => { JsonValue::Number(self.number_value()) }
            ParserTokenType::String => { JsonValue::String(self.string_value()) },
            ParserTokenType::Boolean => { JsonValue::Boolean(self.boolean_value()) }
            ParserTokenType::Null => { JsonValue::Null }
            _ => unreachable!("Unexpected token type for the start of json value"),
        }
    }

    fn object_value(&mut self) -> LinkedHashMap<String, JsonValue> {
        let mut map = LinkedHashMap::new();
        self.token_idx += 1; // move past '{'

        // empty object
        if self._tokens[self.token_idx].token_type == ParserTokenType::ObjectEnd {
            return map;
        }

        while let Some(token) = self.next() {
            // Drop opening/closing quotes
            let name: &[u8] = &self.buffer[token.start_index + 1 ..token.start_index + (token.offset - 1)  as usize];
            let key = String::from_utf8(Vec::from(name)).unwrap(); // toDo: can this be &str?
            // skip colon
            self.token_idx += 1;
            let value = self.json_value();
            map.insert(key, value);
            // move past the value, we expect either ',' or '}'. The structure of the object is always
            // valid at this point
            self.token_idx += 1;
            if let Some(token) = self.next() {
                if token.token_type == ParserTokenType::ObjectEnd {
                    break;
                }
            }
            // at this point the current token is ',' we move to the next key
            self.token_idx += 1;
        }
        map
    }

    fn array_value(&mut self) -> Vec<JsonValue> {
        let mut values = Vec::new();
        self.token_idx += 1; // move past '['

        // empty array
        if self._tokens[self.token_idx].token_type == ParserTokenType::ArrayEnd {
            return values;
        }

        while let Some(_) = self.next() {
            values.push(self.json_value());
            self.token_idx += 1;

            if let Some(token) = self.next() {
                if token.token_type == ParserTokenType::ArrayEnd {
                    break;
                }
            }
            self.token_idx += 1;
        }
        values
    }

    fn boolean_value(&self) -> bool {
        let token = &self._tokens[self.token_idx]; // toDo: check if we need to increment the index

        self.buffer[token.start_index] == b't'
    }

    fn string_value(&self) -> String {
        let token = &self._tokens[self.token_idx];
        let mut val = String::new();
        let mut index = token.start_index;

        while index < token.start_index + token.offset as usize {
            let byte = self.buffer[index];
            match byte {
                b'\\' => {
                    index += 1;
                    match self.buffer[index] {
                        b'\\' => val.push('\\'), // toDo: what is the return type?
                        b'"' => val.push('"'),
                        b'/' => val.push('/'),
                        b'b' => val.push('\x08'),
                        b'f' => val.push('\x0C'),
                        b'n' => val.push('\n'),
                        b'r' => val.push('\r'),
                        b't' => val.push('\t'),
                        b'u' => {
                            index += 1;
                            let code_unit = numeric_utils::hex_to_u16(&self.buffer[index..index + 4]).unwrap();
                            if utf8_utils::is_surrogate(code_unit) {
                                let high = code_unit;
                                index += 6; // 4 hex digits + \u
                                let low = numeric_utils::hex_to_u16(&self.buffer[index..index + 4]).unwrap();
                                let code_point= utf8_utils::decode_surrogate_pair(high, low);
                                val.push(char::from_u32(code_point).unwrap());
                                index += 3; // Move to the last hex digit
                            } else {
                                val.push(char::from_u32(code_unit as u32).unwrap());
                                index += 3; // Move to the last hex digit
                            }
                        }
                        _ => unreachable!("backslash not followed by an escape character"),
                    }
                    // move past the escaped character or the last hex digit
                    index += 1;
                }
                b if b.is_ascii() => {
                    val.push(b as char);
                    index += 1;
                }
                _ => {
                    let width = utf8_utils::utf8_char_width(byte);
                    val.push_str(str::from_utf8(&self.buffer[index..index + width]).unwrap());
                    index += width;
                }
            }
        }
        // for (i, &byte) in self.buffer[token.start_index..token.start_index + token.offset as usize].iter().enumerate() {}
        // toDo: learn about the &byte pattern and why we don't need to dereference. How references work in literals like &5 passed in a function like the contains()
        val
    }

    fn number_value(&mut self) -> Number {
        let token = &self._tokens[self.token_idx];
        let slice = &self.buffer[token.start_index..token.start_index + token.offset as usize];
        let float = slice.iter().any(|&b| matches!(b, b'.' | b'e' | b'E'));
        let s = std::str::from_utf8(slice).unwrap();

        if float {
            return Number::from_f64(s.parse::<f64>().unwrap());
        }

        if s.starts_with('-') {
            Number::from_i64(s.parse::<i64>().unwrap())
        } else {
            // Positive that might fit in i64
            let val = s.parse::<u64>().unwrap();
            if val <= i64::MAX as u64 {
                Number::from_i64(val as i64)
            } else {
                Number::from_u64(val)
            }
        }
    }
}

fn expect(left: &TokenizerTokenType, right: TokenizerTokenType) -> bool {
    *left == right
}

#[cfg(test)]
mod tests {
    use crate::parse::tokenizer::Tokenizer;
    use super::*;

    fn invalid_objects() -> Vec<(&'static [u8], ParserError)> {
        vec![
            (b"{", ParserError::UnexpectedEndOfInput { pos : 0 }),
            (b"{ null : 1 }", ParserError::UnexpectedToken { token: Some("object name"), pos: 2 }),
            (b"{ \"foo\": 5,", ParserError::UnexpectedEndOfInput { pos : 10 }),
            (b"{ \"foo\": 5", ParserError::UnexpectedEndOfInput { pos: 9 }),
            (b"{ \"foo\"",ParserError::UnexpectedEndOfInput { pos: 6 }),
            (b"{ \"foo\" 3", ParserError::UnexpectedToken { token: Some("colon ':'"),  pos: 8 }),
            (b"{ \"foo\": \"value\" } 123", ParserError::UnexpectedToken { token: None, pos: 19 }),
            (b"{ \"foo\": \"bar\", \"foo\": \"baz\"}", ParserError::DuplicateName { name: String::from("\"foo\""), pos: 16 })
        ]
    }

    fn invalid_arrays() -> Vec<(&'static [u8], ParserError)> {
        vec![
            (b"[", ParserError::UnexpectedEndOfInput { pos : 0 }),
            (b"[116, 943", ParserError::UnexpectedEndOfInput { pos : 8 }),
            (b"[116, 943,", ParserError::UnexpectedEndOfInput { pos : 9 }),
            (b"[116 943]", ParserError::UnexpectedToken { token: Some("',' or ']'"), pos: 5 }),
            (b"[:]",ParserError::UnexpectedToken { token: Some("json value"), pos: 1 }),
        ]
    }

    #[test]
    fn test_invalid_objects() {
        for (buffer, error) in invalid_objects() {
            let mut tokenizer = Tokenizer::new(buffer);
            let tokens = tokenizer.tokenize().unwrap();
            let mut parser = Parser::new(buffer, &tokens);
            let result = parser.parse();

            if let Err(e) = result {
                assert_eq!(e, error);
            }
        }
    }

    #[test]
    fn test_invalid_arrays() {
        for (buffer, error) in invalid_arrays() {
            let mut tokenizer = Tokenizer::new(buffer);
            let tokens = tokenizer.tokenize().unwrap();
            let mut parser = Parser::new(buffer, &tokens);
            let result = parser.parse();

            if let Err(e) = result {
                assert_eq!(e, error);
            }
        }
    }

    // We can't call: let buffer = format!("{}{}", "[".repeat(257), "]".repeat(257)).as_bytes();
    //                              ^-- Creates String                                ^-- Borrows &[u8] from String
    //                              String is dropped here!
    // buffer now points to freed memory
    //
    // format!() creates a String, then .as_bytes() borrows from it, but the String gets dropped immediately after that line
    #[test]
    fn array_exceeds_max_depth() {
        let text = format!("{}{}", "[".repeat(257), "]".repeat(257));
        let buffer = text.as_bytes();
        let mut tokenizer = Tokenizer::new(buffer);
        let tokens = tokenizer.tokenize().unwrap();
        let mut parser = Parser::new(buffer, &tokens);
        let result = parser.parse();

        if let Err(e) = result {
            assert_eq!(e, ParserError::DepthLimitExceeded { depth: 256 });
        }
    }

    #[test]
    fn object_exceeds_max_depth() {
        let mut json = "{}".to_string();
        for _ in 0..256 {
            // {}
            // {"key": {}}
            // {"key": {"key": {}}}
            json = format!(r#"{{"key": {}}}"#, json);
        }

        let buffer = json.as_bytes();
        let mut tokenizer = Tokenizer::new(buffer);
        let tokens = tokenizer.tokenize().unwrap();
        let mut parser = Parser::new(buffer, &tokens);
        let result = parser.parse();

        if let Err(e) = result {
            assert_eq!(e, ParserError::DepthLimitExceeded { depth: 256 });
        }
    }
}