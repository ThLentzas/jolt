use std::string::String;
use std::cmp::PartialEq;
use std::collections::HashSet;
use linked_hash_map::LinkedHashMap;
use super::tokenizer::{TokenizerToken, TokenizerTokenType};
use super::value::Value;
use crate::parsing::error::ParserError;
use crate::parsing::{escapes, utf8};
use crate::parsing::escapes::len;
use crate::parsing::number::Number;

const MAX_DEPTH: u16 = 256;

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

impl ParserToken {
    fn new(start_index: usize, offset: u32, token_type: ParserTokenType) -> Self {
        Self {
            start_index,
            offset,
            token_type,
        }
    }
}

pub(super) struct Parser<'a> {
    buffer: &'a [u8],
    tokens: &'a Vec<TokenizerToken>,
    _tokens: Vec<ParserToken>, // toDo: rename
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

    // toDo: advantages of peek/advance and recursive decent parser
    pub(super) fn parse(&mut self) -> Result<Value, ParserError> {
        // [](empty input buffer) is not invalid, the value of the root of AST is None
        // same logic applies for [\n, \t, '\r', ' ']

        self.parse_value()?;
        // false5, "abc"123, {}null
        if self.pos < self.tokens.len() {
            return Err(ParserError::UnexpectedToken { pos: self.tokens[self.pos].start_index(), token: None });
        }
        // someone could call parsing() multiples, for the same input we return the same node by resetting the indices
        self.token_idx = 0;
        self.pos = 0;

        Ok(self.compute_value()) // toDo: Does this move or it is a copy?
    }

    // None of the parse_* calls moves past the related tokens, we advance after
     fn parse_value(&mut self) -> Result<(), ParserError> {
         match self.peek() {
             Some(token) => match token.token_type() {
                 TokenizerTokenType::LeftCurlyBracket => self.parse_object()?,
                 TokenizerTokenType::LeftSquareBracket => self.parse_array()?,
                 TokenizerTokenType::Number => self.parse_number(),
                 TokenizerTokenType::String => self.parse_string(),
                 TokenizerTokenType::Boolean => self.parse_boolean(),
                 TokenizerTokenType::Null => self.parse_null(),
                 _ => return Err(ParserError::UnexpectedToken { token: Some("json value"), pos: token.start_index() })
             }
             None => return Err(ParserError::UnexpectedEndOfInput { pos: self.buffer.len() - 1 })
         }
         self.advance();
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
            match self.peek() {
                // {"foo": "bar",}
                Some(next) if expect(next.token_type(), TokenizerTokenType::RightCurlyBracket) => {
                    if self._tokens.last().unwrap().token_type == ParserTokenType::ValueSeparator {
                        return Err(ParserError::UnexpectedToken { pos: next.start_index(), token: Some("object name") });
                    }
                    // {}
                    self._tokens.push(ParserToken::new(next.start_index(), next.offset(), ParserTokenType::ObjectEnd));
                    self.depth -= 1;
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
                Some(next) => return Err(ParserError::UnexpectedToken { token: Some("'}' or ','"), pos: next.start_index() }),
                None => return Err(ParserError::UnexpectedEndOfInput { pos: buffer_len - 1 }),
            }
        }
        Ok(())
    }

    // We peek and advance 3 times, after moving past the opening '['
    //
    // 1. peek and expect either ']' for an empty array or a json value(parse_value() will peek and advance)
    // 2. peek and expect ',' to separate values -> advance, or ']' -> break;
    //
    // When encountering ']', we break WITHOUT calling advance(). The closing ']' will be consumed
    // by the caller (parse_value()) after this function returns.
    fn parse_array(&mut self) -> Result<(), ParserError> {
        if self.depth + 1 > MAX_DEPTH {
            return Err(ParserError::DepthLimitExceeded { depth: MAX_DEPTH });
        }

        let current = &self.tokens[self.pos];
        let len = self.buffer.len();
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
                    // []
                    self._tokens.push(ParserToken::new(next.start_index(), next.offset(), ParserTokenType::ArrayEnd));
                    self.depth -= 1;
                    break;
                }
                Some(_) => self.parse_value()?,
                None => return Err(ParserError::UnexpectedEndOfInput { pos: len - 1 }),
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
                Some(next) => return Err(ParserError::UnexpectedToken { token: Some("',' or ']'"), pos: next.start_index() }),
                None => return Err(ParserError::UnexpectedEndOfInput { pos: len - 1 }),
            }
        }
        Ok(())
    }

    fn parse_number(&mut self) {
        let token = &self.tokens[self.pos];
        self._tokens.push(ParserToken::new(token.start_index(), token.offset(), ParserTokenType::Number));
    }

    fn parse_string(&mut self) {
        let token = &self.tokens[self.pos];
        self._tokens.push(ParserToken::new(token.start_index(), token.offset(), ParserTokenType::String));
    }

    fn parse_boolean(&mut self) {
        let token = &self.tokens[self.pos];
        self._tokens.push(ParserToken::new(token.start_index(), token.offset(), ParserTokenType::Boolean));
    }

    fn parse_null(&mut self) {
        let token = &self.tokens[self.pos];
        self._tokens.push(ParserToken::new(token.start_index(), token.offset(), ParserTokenType::Null));
    }

    // peek and advance are used during the parsing process, next is used when we compute the value
    // to access the tokens produced by the parser
    fn peek(&self) -> Option<&TokenizerToken> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn next(&self) -> Option<&ParserToken>{
        self._tokens.get(self.token_idx)
    }

    fn compute_value(&mut self) -> Value {
        let token = &self._tokens[self.token_idx];
        let val = match token.token_type {
            ParserTokenType::ObjectStart => Value::Object(self.object_value()),
            ParserTokenType::ArrayStart => Value::Array(self.array_value()),
            ParserTokenType::Number => Value::Number(self.number_value()),
            ParserTokenType::String => Value::String(self.string_value()),
            ParserTokenType::Boolean => Value::Boolean(self.boolean_value()),
            ParserTokenType::Null => Value::Null,
            // only the above cases will start a json value
            _ => unreachable!("Unexpected token type for the start of json value at index {}", self.token_idx),
        };
        self.token_idx += 1;

        val
    }

    fn object_value(&mut self) -> LinkedHashMap<String, Value> {
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
            // skip key,colon
            self.token_idx += 2;
            let value = self.compute_value();
            map.insert(key, value);
            // we expect either ',' or '}'. The structure of the object is always valid at this point
            if let Some(token) = self.next() {
                if token.token_type == ParserTokenType::ObjectEnd {
                    break;
                }
            }
            // skip comma
            self.token_idx += 1;
        }
        map
    }

    fn array_value(&mut self) -> Vec<Value> {
        let mut values = Vec::new();
        self.token_idx += 1; // move past '['

        // empty array
        if self._tokens[self.token_idx].token_type == ParserTokenType::ArrayEnd {
            return values;
        }

        while let Some(_) = self.next() {
            values.push(self.compute_value());
            if let Some(token) = self.next() {
                if token.token_type == ParserTokenType::ArrayEnd {
                    break;
                }
            }
            // skip comma
            self.token_idx += 1;
        }
        values
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

    fn string_value(&self) -> String {
        let token = &self._tokens[self.token_idx];
        let mut val = String::new();
        let mut index = token.start_index + 1; // Skip opening quotation mark

        while index < token.start_index + (token.offset - 1) as usize { // Skip closing quotation mark
            let byte = self.buffer[index];
            match byte {
                b'\\' => {
                    val.push(escapes::map_escape_character(self.buffer, index));
                    index += len(self.buffer, index);
                }
                b if b.is_ascii() => {
                    val.push(b as char);
                    index += 1;
                }
                _ => {
                    let width = utf8::utf8_char_width(byte);
                    val.push_str(str::from_utf8(&self.buffer[index..index + width]).unwrap());
                    index += width;
                }
            }
        }
        // for (i, &byte) in self.buffer[token.start_index..token.start_index + token.offset as usize].iter().enumerate() {}
        // toDo: learn about the &byte pattern and why we don't need to dereference. How references work in literals like &5 passed in a function like the contains()
        val
    }

    fn boolean_value(&mut self) -> bool {
        let token = &self._tokens[self.token_idx];

        self.buffer[token.start_index] == b't'
    }
}

fn expect(left: &TokenizerTokenType, right: TokenizerTokenType) -> bool {
    *left == right
}

#[cfg(test)]
mod tests {
    use crate::parsing::tokenizer::Tokenizer;
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
            (b"{ \"foo\": \"bar\", \"foo\": \"baz\"}", ParserError::DuplicateName { name: String::from("foo"), pos: 16 })
        ]
    }

    fn invalid_arrays() -> Vec<(&'static [u8], ParserError)> {
        vec![
            (b"[", ParserError::UnexpectedEndOfInput { pos : 0 }),
            (b"[116, 943", ParserError::UnexpectedEndOfInput { pos : 8 }),
            (b"[116, 943,", ParserError::UnexpectedEndOfInput { pos : 9 }),
            (b"[116 true]", ParserError::UnexpectedToken { token: Some("',' or ']'"), pos: 5 }),
            (b"[:]", ParserError::UnexpectedToken { token: Some("json value"), pos: 1 }),
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

    #[test]
    fn valid_object() {
        // can't use br## because 💖 is a Non ASCII character, empty strings as keys are allowed
        let buffer = r#"{
            "4_byte_sequence": "💖",
            "surrogate_pair": "\uD83D\uDE00",
            "escape_characters": "\\\"\/\b\f\n\r\t",
            "boolean" : false,
            "numbers": [116, -943, 9223372036854775808, -3.14159265358979e+100, 6.02214076e+23, 2.718281828e-50, -456.78],
            "": true,
            "null": null
        }"#.as_bytes();

        let mut numbers = Vec::new();
        numbers.push(Value::Number(Number::from_i64(116)));
        numbers.push(Value::Number(Number::from_i64(-943)));
        numbers.push(Value::Number(Number::from_u64(9223372036854775808)));
        numbers.push(Value::Number(Number::from_f64(-3.14159265358979e+100)));
        numbers.push(Value::Number(Number::from_f64(6.02214076e+23)));
        numbers.push(Value::Number(Number::from_f64(2.718281828e-50)));
        numbers.push(Value::Number(Number::from_f64(-456.78)));

        let mut map = LinkedHashMap::new();
        map.insert("4_byte_sequence".to_string(), Value::String(String::from("💖")));
        map.insert("surrogate_pair".to_string(), Value::String(String::from("😀")));
        map.insert("escape_characters".to_string(), Value::String(String::from("\\\"/\x08\x0C\n\r\t")));
        map.insert("boolean".to_string(), Value::Boolean(false));
        map.insert("numbers".to_string(), Value::Array(numbers));
        map.insert("".to_string(), Value::Boolean(true));
        map.insert("null".to_string(), Value::Null);

        let mut tokenizer = Tokenizer::new(buffer);
        let tokens = tokenizer.tokenize().unwrap();
        let mut parser = Parser::new(buffer, &tokens);
        let result = parser.parse().unwrap();

        assert_eq!(Value::Object(map), result);
    }

    #[test]
    fn empty_object() {
        let buffer = b"{}";
        let mut tokenizer = Tokenizer::new(buffer);
        let tokens = tokenizer.tokenize().unwrap();
        let mut parser = Parser::new(buffer, &tokens);
        let result = parser.parse().unwrap();

        assert_eq!(Value::Object(LinkedHashMap::new()), result);
    }

    #[test]
    fn empty_array() {
        let buffer = b"[]";
        let mut tokenizer = Tokenizer::new(buffer);
        let tokens = tokenizer.tokenize().unwrap();
        let mut parser = Parser::new(buffer, &tokens);
        let result = parser.parse().unwrap();

        assert_eq!(Value::Array(Vec::new()), result);
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