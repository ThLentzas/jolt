use std::string::String;
use std::cmp::PartialEq;
use std::collections::HashSet;
#[cfg(feature = "big_decimal")]
use bigdecimal::BigDecimal;
#[cfg(feature = "big_decimal")]
use bigdecimal::num_bigint::BigInt;
use indexmap::IndexMap;
use super::lexer::{Lexer, LexerToken, LexerTokenType};
use super::value::Value;
use super::error::{JsonErrorKind, JsonError};
use super::{escapes, number, utf8, INPUT_BUFFER_LIMIT, NESTING_DEPTH_LIMIT, STRING_VALUE_LENGTH_LIMIT};
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

    // toDo: advantages of peek/advance and recursive decent parser
    pub(super) fn parse(&mut self) -> Result<Value, JsonError> {
        if self.buffer.len() > INPUT_BUFFER_LIMIT {
            return Err(JsonError::new(JsonErrorKind::InputBufferLimitExceeded { len: INPUT_BUFFER_LIMIT }, None));
        }
        // https://www.rfc-editor.org/rfc/rfc8259#section-8.1
        if utf8::is_bom_present(self.buffer) {
            self.lexer.advance(3);
        }

        let token = self.peek()?;
        // &token would create &Option<LexerToken> not Option<&LexerToken>
        self.parse_value(token.as_ref())?;
        match self.peek()? {
            // after successfully parsing a value we can't have leftover tokens
            // false5, "abc"123, {}null, note that this could return an error, {}001, -> leading zeros are not allowed
            Some(token) => Err(JsonError::new(JsonErrorKind::UnexpectedToken { expected: None }, Some(token.start_index()))),
            // whoever calls parse() will be the owner of the return value
            None => Ok(self.compute_value()?)
        }
    }

    // None of the parse_* calls moves past the related tokens, we advance after
     fn parse_value(&mut self, token: Option<&LexerToken>) -> Result<(), JsonError> {
        match token {
            Some(t) => {
                match t.token_type() {
                    // an alternative to passing the token, would be to have a method that returns
                    // the tokens produced by the lexer and retrieve the last; we could
                    // call this method from each parse function
                    LexerTokenType::LCurlyBracket => self.parse_object(t)?,
                    LexerTokenType::LSquareBracket => self.parse_array(t)?,
                    LexerTokenType::Number => self.parse_number(t),
                    LexerTokenType::String => self.parse_string(t),
                    LexerTokenType::Boolean => self.parse_boolean(t),
                    LexerTokenType::Null => self.parse_null(t),
                    _ => return Err(JsonError::new(JsonErrorKind::UnexpectedToken { expected: Some("json value") }, Some(t.start_index())))
                }
            }
            None => {
                let pos = if self.buffer.is_empty() { self.buffer.len() } else { self.buffer.len() - 1 };
                return Err(JsonError::new(JsonErrorKind::UnexpectedEof, Some(pos)));
            }
        }
        self.advance(1);
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
    fn parse_object(&mut self, token: &LexerToken) -> Result<(), JsonError> {
        if self.depth + 1 > NESTING_DEPTH_LIMIT {
            return Err(JsonError::new(JsonErrorKind::NestingDepthLimitExceeded { depth: NESTING_DEPTH_LIMIT }, None));
        }
        // '{'
        self.tokens.push(ParserToken::new(token.start_index(), token.offset(), ParserTokenType::ObjectStart));
        self.advance(1);
        self.depth += 1;
        let buffer_len = self.buffer.len();
        let mut names = HashSet::new();

        // We don't know how many tokens are part of the current object, the moment we encounter '}' we break
        loop {
            match self.peek()? {
                // {"foo": "bar",}
                Some(next) if expect(next.token_type(), LexerTokenType::RCurlyBracket) => {
                    if self.tokens.last().unwrap().token_type == ParserTokenType::ValueSeparator {
                        return Err(JsonError::new(JsonErrorKind::UnexpectedToken { expected: Some("object name") }, Some(next.start_index())));
                    }
                    // {}
                    self.tokens.push(ParserToken::new(next.start_index(), next.offset(), ParserTokenType::ObjectEnd));
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
                        return Err(JsonError::new(JsonErrorKind::DuplicateName { name: String::from_utf8(Vec::from(name)).unwrap() }, Some(next.start_index())));
                    }
                    names.insert(name);
                    self.parse_string(&next);
                    self.advance(1);
                }
                // mismatch
                Some(next) => {
                    return Err(JsonError::new(JsonErrorKind::UnexpectedToken { expected: Some("object name") }, Some(next.start_index())));
                }
                None => return Err(JsonError::new(JsonErrorKind::UnexpectedEof, Some(buffer_len - 1)))
            }

            match self.peek()? {
                Some(next) if expect(next.token_type(), LexerTokenType::Colon) => {
                    self.tokens.push(ParserToken::new(next.start_index(), next.offset(), ParserTokenType::NameSeparator));
                    self.advance(1);
                }
                // mismatch
                Some(next) => {
                    return Err(JsonError::new(JsonErrorKind::UnexpectedToken { expected: Some("colon ':'") }, Some(next.start_index())));
                }
                None => return Err(JsonError::new(JsonErrorKind::UnexpectedEof, Some(buffer_len - 1)))
            }
            let next = self.peek()?;
            self.parse_value(next.as_ref())?;

            match self.peek()? {
                Some(next) if expect(next.token_type(), LexerTokenType::Comma) => {
                    self.tokens.push(ParserToken::new(next.start_index(), next.offset(), ParserTokenType::ValueSeparator));
                    self.advance(1);
                }
                Some(next) if expect(next.token_type(), LexerTokenType::RCurlyBracket) => {
                    self.tokens.push(ParserToken::new(next.start_index(), next.offset(), ParserTokenType::ObjectEnd));
                    self.depth -= 1;
                    break;
                }
                // mismatch
                Some(next) => return Err(JsonError::new(JsonErrorKind::UnexpectedToken { expected: Some("'}' or ','") }, Some(next.start_index()))),
                None => return Err(JsonError::new(JsonErrorKind::UnexpectedEof, Some(buffer_len - 1))),
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
    fn parse_array(&mut self, token: &LexerToken) -> Result<(), JsonError> {
        if self.depth + 1 > NESTING_DEPTH_LIMIT {
            return Err(JsonError::new(JsonErrorKind::NestingDepthLimitExceeded { depth: NESTING_DEPTH_LIMIT }, None));
        }

        self.tokens.push(ParserToken::new(token.start_index(), token.offset(), ParserTokenType::ArrayStart));
        self.advance(1);
        self.depth += 1;
        let buffer_len = self.buffer.len();

        loop {
            match self.peek()? {
                // [1,2,]
                Some(next) if expect(next.token_type(), LexerTokenType::RSquareBracket) => {
                    if self.tokens.last().unwrap().token_type == ParserTokenType::ValueSeparator {
                        return Err(JsonError::new(JsonErrorKind::UnexpectedToken { expected: Some("json value") }, Some(next.start_index())));
                    }
                    // []
                    self.tokens.push(ParserToken::new(next.start_index(), next.offset(), ParserTokenType::ArrayEnd));
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
                    self.tokens.push(ParserToken::new(next.start_index(), next.offset(), ParserTokenType::ValueSeparator));
                    self.advance(1);
                }
                Some(next) if expect(next.token_type(), LexerTokenType::RSquareBracket) => {
                    self.tokens.push(ParserToken::new(next.start_index(), next.offset(), ParserTokenType::ArrayEnd));
                    self.depth -= 1;
                    break;
                }
                Some(next) => return Err(JsonError::new(JsonErrorKind::UnexpectedToken { expected: Some("',' or ']'") }, Some(next.start_index()))),
                None => return Err(JsonError::new(JsonErrorKind::UnexpectedEof, Some(buffer_len - 1))),
            }
        }
        Ok(())
    }

    fn parse_number(&mut self, token: &LexerToken) {
        self.tokens.push(ParserToken::new(token.start_index(), token.offset(), ParserTokenType::Number));
    }

    fn parse_string(&mut self, token: &LexerToken) {
        self.tokens.push(ParserToken::new(token.start_index(), token.offset(), ParserTokenType::String));
    }

    fn parse_boolean(&mut self, token: &LexerToken) {
        self.tokens.push(ParserToken::new(token.start_index(), token.offset(), ParserTokenType::Boolean));
    }

    fn parse_null(&mut self, token: &LexerToken) {
        self.tokens.push(ParserToken::new(token.start_index(), token.offset(), ParserTokenType::Null));
    }

    // Why this returns a copy?
    //
    // Initially the return value was Result<Option<&LexerToken>, JsonError> but that wouldn't work
    // In parse object:

    //      match self.peek()? {
    //          Some(next) if expect(next.token_type(), LexerTokenType::RightCurlyBracket) => {
    //              if self.tokens.last().unwrap().token_type == ParserTokenType::ValueSeparator
    //
    // We get an error: cannot borrow `self.tokens` as immutable because it is also borrowed as mutable
    //
    // self.peek()? takes a mutable borrow of self, and that borrow extends through the entire match expression
    // Then inside the match arm, we are trying to access self.tokens.last() which requires an
    // immutable borrow, but self is already mutably borrowed
    //
    // In Rust, when we have a method that takes &mut self and returns a reference to something inside
    // self, that reference keeps the mutable borrow alive for as long as the reference exists
    //
    // 1. self.peek()? mutably borrows self and returns Option<&LexerToken>
    // 2. This reference is used in the match expression
    // 3. The mutable borrow must stay active for the entire match
    // 4. Inside the match, you try to access self.tokens, but self is already mutably borrowed
    //
    // How we solve this with a copy?
    //  Whoever calls peek() is the owner of the copy
    //  In this case, match will keep it alive, the value is not dropped immediately, it will be dropped
    //  when match exits which allows us to pass a reference to parse_value() in the case of parse_array()
    //             match self.peek()? {
    //                 ...
    //                 next => self.parse_value(next.as_ref())?,
    //             }
    //  next: is whatever self.peek() returned, can be passed a reference to parse_value() without
    //  creating a dangling pointer because the value can still be referenced, it has not been dropped yet
    //
    //  toDo: is it a cheap copy? can we solve this with unsafe?
    fn peek(&mut self) -> Result<Option<LexerToken>, JsonError> {
       Ok(self.lexer.peek()?)
    }

    fn advance(&mut self, offset: usize) {
        self.lexer.advance(offset)
    }

    // peek and advance are used during the parsing process, next is used when we compute the value
    // to access the tokens produced by the parser
    fn next(&self) -> Option<&ParserToken>{
        self.tokens.get(self.pos)
    }

    fn compute_value(&mut self) -> Result<Value, JsonError> {
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

    fn object_value(&mut self) -> Result<IndexMap<String, Value>, JsonError> {
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
            let value = self.compute_value()?;
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

    fn array_value(&mut self) -> Result<Vec<Value>, JsonError> {
        let mut values = Vec::new();
        self.pos += 1; // move past '['

        // empty array
        if self.tokens[self.pos].token_type == ParserTokenType::ArrayEnd {
            return Ok(values);
        }

        while let Some(_) = self.next() {
            values.push(self.compute_value()?);
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
        let float = slice.iter().any(|&b| matches!(b, b'.' | b'e' | b'E'));
        let s = std::str::from_utf8(slice).unwrap();

        #[cfg(feature = "big_decimal")]
        type N = BigDecimal;
        #[cfg(not(feature = "big_decimal"))]
        type N = f64;
        if float {
            return Number::from(s.parse::<N>().unwrap());
        }

        // Try to optimize even if big_decimal is true; for any integer we can still store it as i64 or
        // u64 as long as it is not out of range. In read_number() we don't check if the number is
        // out of range when big_decimal is enabled
        match s.starts_with('-') {
            true => {
                #[cfg(feature = "big_decimal")]
                if number::is_out_of_range_i64(slice) {
                    return Number::from(s.parse::<BigInt>().unwrap());
                }
                return Number::from(s.parse::<i64>().unwrap());
            }
            _ => ()
        }

        #[cfg(feature = "big_decimal")]
        if number::is_out_of_range_u64(slice) {
            return Number::from(s.parse::<BigInt>().unwrap());
        }

        let num = s.parse::<u64>().unwrap();
        if num <= i64::MAX as u64 {
            Number::from(num as i64)
        } else {
            Number::from(num)
        }
    }

    fn string_value(&self) -> Result<String, JsonError> {
        let token = &self.tokens[self.pos];
        let mut val = String::new();
        let mut i = token.start_index + 1; // Skip opening quotation

        while i < token.start_index + (token.offset - 1) as usize { // Skip closing quotation
            let byte = self.buffer[i];
            match byte {
                b'\\' => {
                    val.push(escapes::map_escape_character(self.buffer, i));
                    i += escapes::len(self.buffer, i);
                }
                b if b.is_ascii() => {
                    val.push(b as char);
                    i += 1;
                }
                _ => {
                    let width = utf8::utf8_char_width(byte);
                    val.push_str(str::from_utf8(&self.buffer[i..i + width]).unwrap());
                    i += width;
                }
            }
        }

        if val.len() > STRING_VALUE_LENGTH_LIMIT {
            return Err(JsonError::new(JsonErrorKind::StringValueLengthLimitExceed { len: STRING_VALUE_LENGTH_LIMIT }, Some(token.start_index)));
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
    use super::*;

    fn invalid_objects() -> Vec<(&'static [u8], JsonError)> {
        vec![
            (b"{", JsonError::new(JsonErrorKind::UnexpectedEof, Some(0))),
            (b"{ null : 1 }", JsonError::new(JsonErrorKind::UnexpectedToken { expected: Some("object name") }, Some(2))),
            (b"{ \"foo\": 5,", JsonError::new(JsonErrorKind::UnexpectedEof, Some(10))),
            (b"{ \"foo\": 5", JsonError::new(JsonErrorKind::UnexpectedEof, Some(9))),
            (b"{ \"foo\"", JsonError::new(JsonErrorKind::UnexpectedEof, Some(6))),
            (b"{ \"foo\" 3", JsonError::new(JsonErrorKind::UnexpectedToken { expected: Some("colon ':'") }, Some(8))),
            (b"{ \"foo\": \"value\" } 123", JsonError::new(JsonErrorKind::UnexpectedToken { expected: None }, Some(19))),
            (b"{ \"foo\": \"bar\", \"foo\": \"baz\"}", JsonError::new(JsonErrorKind::DuplicateName { name: String::from("foo") }, Some(16)))
        ]
    }

    fn invalid_arrays() -> Vec<(&'static [u8], JsonError)> {
        vec![
            (b"[", JsonError::new(JsonErrorKind::UnexpectedEof, Some(0))),
            (b"[116, 943", JsonError::new(JsonErrorKind::UnexpectedEof, Some(8))),
            (b"[116, 943,", JsonError::new(JsonErrorKind::UnexpectedEof, Some(9))),
            (b"[116 true]", JsonError::new(JsonErrorKind::UnexpectedToken { expected: Some("',' or ']'") }, Some(5))),
            (b"[:]", JsonError::new(JsonErrorKind::UnexpectedToken { expected: Some("json value") }, Some(1))),
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
        let buffer = "[116, -943, 9223372036854775808, -3.14159265358979e+100, 0.1, 340282366920938463463374607431768211456]".as_bytes();

        let mut numbers = Vec::new();
        numbers.push(Value::Number(Number::from(116i64)));
        numbers.push(Value::Number(Number::from(-943i64)));
        numbers.push(Value::Number(Number::from(9223372036854775808u64)));
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
        // can't use br## because 💖 is a Non ASCII character, empty strings as keys are allowed
        let buffer = r#"{
            "4_byte_sequence": "💖",
            "surrogate_pair": "\uD83D\uDE00",
            "escape_characters": "\\\"\/\b\f\n\r\t",
            "boolean" : false,
            "numbers": [116, -943, 9223372036854775808, -3.14159265358979e+100, 6.02214076e+23, 2.718281828e-50],
            "": true,
            "null": null
        }"#.as_bytes();

        // couldn't set up with json!() had problems with large numbers
        let mut numbers = Vec::new();
        numbers.push(Value::Number(Number::from(116i64)));
        numbers.push(Value::Number(Number::from(-943i64)));
        numbers.push(Value::Number(Number::from(9223372036854775808u64)));
        numbers.push(Value::Number(Number::from(-3.14159265358979e+100)));
        numbers.push(Value::Number(Number::from(6.02214076e+23)));
        numbers.push(Value::Number(Number::from(2.718281828e-50)));

        let mut map = IndexMap::new();
        map.insert("4_byte_sequence".to_string(), Value::String(String::from("💖")));
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
        let error = JsonError::new(JsonErrorKind::UnexpectedEof, Some(0));
        let result = parser.parse();

        assert_eq!(result, Err(error));
    }

    #[test]
    fn skip_whitespaces() {
        // \t, \n, \r, ' '
        let buffer: [u8; 4] = [9, 10, 13, 32];
        let mut parser = Parser::new(&buffer);
        let result = parser.parse();
        let error = JsonError::new(JsonErrorKind::UnexpectedEof, Some(3));

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

        assert_eq!(result, Err(JsonError::new(JsonErrorKind::InputBufferLimitExceeded { len: INPUT_BUFFER_LIMIT }, None)));
    }

    #[test]
    fn string_value_exceeds_length_limit() {
        let mut buffer = Vec::with_capacity(STRING_VALUE_LENGTH_LIMIT + 3);

        buffer.push(b'"');
        buffer.extend(vec![b'a'; STRING_VALUE_LENGTH_LIMIT + 1]);
        buffer.push(b'"');

        let mut parser = Parser::new(&buffer);
        let result = parser.parse();

        assert_eq!(result, Err(JsonError::new(JsonErrorKind::StringValueLengthLimitExceed { len: STRING_VALUE_LENGTH_LIMIT }, Some(0))));
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

        assert_eq!(result, Err(JsonError::new(JsonErrorKind::NestingDepthLimitExceeded { depth: NESTING_DEPTH_LIMIT }, None)));
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

        assert_eq!(result, Err(JsonError::new(JsonErrorKind::NestingDepthLimitExceeded { depth: NESTING_DEPTH_LIMIT }, None)));
    }
}