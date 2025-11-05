use crate::parsing::error::{JsonErrorKind, JsonError, StringErrorKind, StringError};
use crate::parsing::{escapes, number, utf8};

// much better approach than passing boolean flags around, foo(true, true, false) is hard to understand
struct NumberState {
    decimal_point: bool,
    scientific_notation: bool,
    negative: bool
}

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
    fn new(start_index: usize, offset: u32, token_type: LexerTokenType) -> Self {
        Self { start_index, offset, token_type }
    }

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
    tokens: Vec<LexerToken>
}

impl<'a> Lexer<'a> {
    pub(super) fn new(buffer: &'a [u8]) -> Self {
        Self {
            buffer,
            tokens: Vec::new(),
            pos: 0
        }
    }

    pub(super) fn peek(&mut self) -> Result<Option<LexerToken>, JsonError> {
        if self.pos >= self.buffer.len() {
            return Ok(None);
        }

        let len = self.tokens.len();
        self.lex()?;
        if len == self.tokens.len() {
            return Ok(None);
        }

        Ok(Some(self.tokens.last().unwrap().clone()))
    }

    pub(super) fn advance(&mut self, n: usize) {
        self.pos += n;
    }

    fn lex(&mut self) -> Result<(), JsonError> {
        // defined as private in the parent mod, and it is visible to the child super::parsing::skip_whitespaces()
        // would work but the method is also needed in path.rs
        crate::parsing::skip_whitespaces(self.buffer, &mut self.pos);
        if self.pos >= self.buffer.len() {
            return Ok(());
        }

        let current = self.buffer[self.pos];
        if current.is_ascii() {
            match current {
                b'{' => self.tokens.push(LexerToken::new(self.pos, 1, LexerTokenType::LCurlyBracket)),
                b'}' => self.tokens.push(LexerToken::new(self.pos, 1, LexerTokenType::RCurlyBracket)),
                b']' => self.tokens.push(LexerToken::new(self.pos, 1, LexerTokenType::RSquareBracket)),
                b'[' => self.tokens.push(LexerToken::new(self.pos, 1, LexerTokenType::LSquareBracket)),
                b':' => self.tokens.push(LexerToken::new(self.pos, 1, LexerTokenType::Colon)),
                b',' => self.tokens.push(LexerToken::new(self.pos, 1, LexerTokenType::Comma)),
                b'-' | b'+' | b'0'..=b'9' => {
                    let start = self.pos;
                    self.read_number()?;
                    self.tokens.push(LexerToken::new(start, (self.pos - start + 1) as u32, LexerTokenType::Number));
                }
                b'"' => {
                    let start = self.pos;
                    self.read_string()?;
                    self.tokens.push(LexerToken::new(start, (self.pos - start + 1) as u32, LexerTokenType::String));
                }
                b't' | b'f' => {
                    let start = self.pos;
                    self.read_boolean()?;
                    self.tokens.push(LexerToken::new(start, (self.pos - start + 1) as u32, LexerTokenType::Boolean));
                }
                b'n' => {
                    let start = self.pos;
                    self.read_null()?;
                    self.tokens.push(LexerToken::new(start, (self.pos - start + 1) as u32, LexerTokenType::Null));
                }
                _ => return Err(JsonError::new(JsonErrorKind::UnexpectedCharacter { byte: current }, Some(self.pos)))
            }
        } else {
            return Err(JsonError::new(JsonErrorKind::UnexpectedCharacter { byte: current }, Some(self.pos)));
        }
        Ok(())
    }

    // before we return we call self.pos -= 1; self.pos is at the 1st character after a valid number
    // and the control returns back to tokenize(), back to peek() and eventually advance() is called
    // which moves self.pos, and we skip that character
    fn read_number(&mut self) -> Result<(), JsonError> {
        let len = self.buffer.len();
        let start = self.pos;
        let mut current = self.buffer[self.pos];
        let next = self.buffer.get(self.pos + 1);

        match(current, next) {
            // +9
            (b'+', Some(n)) if n.is_ascii_digit() => return Err(JsonError::new(JsonErrorKind::InvalidNumber {
                message: "json specification prohibits numbers from being prefixed with a plus sign" }, Some(self.pos))),
            // +a or +
            (b'+', _) => return Err(JsonError::new(JsonErrorKind::UnexpectedCharacter { byte: current }, Some(self.pos))),
            (b'-', None) => return Err(JsonError::new(JsonErrorKind::UnexpectedEof, Some(self.pos))),
            // -0 is valid
            (b'-', Some(n)) if !n.is_ascii_digit() => return Err(JsonError::new(JsonErrorKind::InvalidNumber {
                message: "a valid numeric value requires a digit (0-9) after the minus sign" }, Some(self.pos))),
            // 05 not allowed
            (b'0', Some(n)) if n.is_ascii_digit() => return Err(JsonError::new(JsonErrorKind::InvalidNumber {
                message: "leading zeros are not allowed" }, Some(self.pos))),
            _ => (),
        }

        // edge case for the logic mention in the comment above; this is the case for single digit
        // numbers '3' where next is none, self.pos is at the digit itself, not in the 1st character
        // after reading the number, we don't need to reset the position, later advance() is called
        // self.pos moves correctly to the next character without skipping one
        if next.is_none() {
            return Ok(());
        }

        self.pos += 1;
        current = *next.unwrap();
        let mut state = NumberState { decimal_point: false, scientific_notation: false, negative: self.buffer[start] == b'-' };
        while self.pos < len {
            match current {
                b'0'..=b'9' => (),
                b'.' => self.check_decimal_point(&mut state)?,
                b'e' | b'E' | b'-' | b'+'=> self.check_scientific_notation(&mut state)?,
                _ => break
            }
            self.pos += 1;
            // update current only if we did not reach the end of the buffer
            if self.pos < len {
                current = self.buffer[self.pos];
            }
        }
        
        if !cfg!(feature = "big_decimal") && self.is_out_of_range(start, state) {
            return Err(JsonError::new(JsonErrorKind::InvalidNumber { message: "number out of range" }, Some(start)));
        }

        self.pos -= 1;
        Ok(())
    }

    fn check_decimal_point(&self, state: &mut NumberState) -> Result<(), JsonError> {
        // 1.2.3
        if state.decimal_point {
            return Err(JsonError::new(JsonErrorKind::InvalidNumber { message: "double decimal point found" }, Some(self.pos)));
        }
        // 1. 2.g
        if self.pos + 1 >= self.buffer.len() || !self.buffer[self.pos + 1].is_ascii_digit() {
            return Err(JsonError::new(JsonErrorKind::InvalidNumber { message: "decimal point must be followed by a digit" }, Some(self.pos)));
        }
        // 1e4.5
        if state.scientific_notation {
            return Err(JsonError::new(JsonErrorKind::InvalidNumber { message: "decimal point is not allowed after exponential notation" }, Some(self.pos)));
        }
        state.decimal_point = true;

        Ok(())
    }

    fn check_scientific_notation(&self, state: &mut NumberState) -> Result<(), JsonError> {
        let current = self.buffer[self.pos];

        match current {
            b'e' | b'E' => {
                // 1e2E3
                if state.scientific_notation {
                    return Err(JsonError::new(JsonErrorKind::InvalidNumber { message: "double exponential notation('e' or 'E') found" }, Some(self.pos)));
                }
                // 1e 1eg
                if self.pos + 1 >= self.buffer.len() || !matches!(self.buffer[self.pos + 1], b'-' | b'+' | b'0'..b'9') {
                    return Err(JsonError::new(JsonErrorKind::InvalidNumber { message: "exponential notation must be followed by a digit or a sign" }, Some(self.pos)));
                }
                // Leading zeros are allowed on the exponent 1e005 evaluates to 100000
                state.scientific_notation = true;
            }
            b'+' | b'-' => {
                // 1+2
                if !matches!(self.buffer[self.pos - 1], b'e' | b'E') {
                    return Err(JsonError::new(JsonErrorKind::InvalidNumber { message: "sign ('+' or '-') is only allowed as part of exponential notation" }, Some(self.pos)));
                }
                // 1E+g
                if self.pos + 1 >= self.buffer.len()  || !self.buffer[self.pos + 1].is_ascii_digit() {
                    return Err(JsonError::new(JsonErrorKind::InvalidNumber { message: "exponential notation must be followed by a digit" }, Some(self.pos)));
                }
            }
            _ => unreachable!("Called with {} instead of 'e', 'E', '+', or '-'", current)
        }
        Ok(())
    }

    // If we wanted to be more strict about integer ranges we could set the allowed range to [-(2^53) + 1, (2^53) - 1]
    // https://www.rfc-editor.org/rfc/rfc7493#section-2.2
    fn is_out_of_range(&self, start: usize, state: NumberState) -> bool {
        let slice = &self.buffer[start..self.pos];

        if state.decimal_point || state.scientific_notation {
            return number::is_out_of_range_f64(slice);
        } else {
            match number::is_out_of_range_i64(slice) {
                true if state.negative => return true,
                // positive and overflow for i64, try u64
                true => return number::is_out_of_range_u64(slice),
                false => (),
            };
        }
        false
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
                    return Err(StringError { kind: StringErrorKind::InvalidControlCharacter { byte: current }, pos: self.pos });
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
    fn read_boolean(&mut self) -> Result<(), JsonError> {
        let target = if self.buffer[self.pos] == b't' { "true".as_bytes() } else { "false".as_bytes() };
        Ok(self.read_literal(target)?)
    }

    fn read_null(&mut self) -> Result<(), JsonError> { Ok(self.read_literal(b"null")?) }

    // before we return we call self.pos -= 1; self.pos is at the 1st character after a valid literal
    // and the control returns back to tokenize(), back to peek() and eventually advance() is called
    // which moves self.pos, and we skip that character
    fn read_literal(&mut self, target: &[u8]) -> Result<(), JsonError> {
        let remaining = &self.buffer[self.pos..];

        if target.len() > remaining.len() {
            return Err(JsonError::new(JsonErrorKind::UnexpectedEof, Some(self.buffer.len() - 1)));
        }

        for byte in target.iter() {
            if self.buffer[self.pos] != *byte {
                return Err(JsonError::new(JsonErrorKind::UnexpectedCharacter { byte: self.buffer[self.pos] }, Some(self.pos)));
            }
            self.pos += 1;
        }
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
            (b"0", LexerToken::new(0, 1, LexerTokenType::Number)),
            (b"-0", LexerToken::new(0, 2, LexerTokenType::Number)),
            (b"123", LexerToken::new(0, 3, LexerTokenType::Number)),
            (b"-45", LexerToken::new(0, 3, LexerTokenType::Number)),
            (b"123.45", LexerToken::new(0, 6, LexerTokenType::Number)),
            (b"1e10", LexerToken::new(0, 4, LexerTokenType::Number)),
            (b"-0.1e-2", LexerToken::new(0, 7, LexerTokenType::Number)),
        ]
    }

    fn invalid_numbers() -> Vec<(&'static [u8], JsonError)> {
        let mut entries: Vec<(&'static [u8], JsonError)> =  vec![
            (b"+", JsonError::new(JsonErrorKind::UnexpectedCharacter { byte: b'+' }, Some(0))),
            (b"+a", JsonError::new(JsonErrorKind::UnexpectedCharacter { byte: b'+' }, Some(0))),
            (b"+9", JsonError::new(JsonErrorKind::InvalidNumber { message: "json specification prohibits numbers from being prefixed with a plus sign" }, Some(0))),
            (b"-a", JsonError::new(JsonErrorKind::InvalidNumber { message: "a valid numeric value requires a digit (0-9) after the minus sign" }, Some(0))),
            (b"06", JsonError::new(JsonErrorKind::InvalidNumber { message: "leading zeros are not allowed" }, Some(0))),
            (b"1.2.3", JsonError::new(JsonErrorKind::InvalidNumber { message: "double decimal point found" }, Some(3))),
            (b"1.", JsonError::new(JsonErrorKind::InvalidNumber { message: "decimal point must be followed by a digit" }, Some(1))),
            (b"1.a", JsonError::new(JsonErrorKind::InvalidNumber { message: "decimal point must be followed by a digit" }, Some(1))),
            (b"4e5.1", JsonError::new(JsonErrorKind::InvalidNumber { message: "decimal point is not allowed after exponential notation" }, Some(3))),
            (b"1e2e5", JsonError::new(JsonErrorKind::InvalidNumber { message: "double exponential notation('e' or 'E') found" }, Some(3))),
            (b"1e", JsonError::new(JsonErrorKind::InvalidNumber { message: "exponential notation must be followed by a digit or a sign" }, Some(1))),
            (b"246Ef", JsonError::new(JsonErrorKind::InvalidNumber { message: "exponential notation must be followed by a digit or a sign" }, Some(3))),
            (b"83+1", JsonError::new(JsonErrorKind::InvalidNumber { message: "sign ('+' or '-') is only allowed as part of exponential notation" }, Some(2))),
            (b"1e+", JsonError::new(JsonErrorKind::InvalidNumber { message: "exponential notation must be followed by a digit" }, Some(2))),
            (b"1e+f", JsonError::new(JsonErrorKind::InvalidNumber { message: "exponential notation must be followed by a digit" }, Some(2)))
        ];
        #[cfg(not(feature = "big_decimal"))]
        {
            entries.push((b"1.8e308", JsonError::new(JsonErrorKind::InvalidNumber { message: "number out of range" }, Some(0))));
            entries.push((b"-9223372036854775809", JsonError::new(JsonErrorKind::InvalidNumber { message: "number out of range" }, Some(0))));
            entries.push((b"18446744073709551616", JsonError::new(JsonErrorKind::InvalidNumber { message: "number out of range" }, Some(0))));
        }

        entries
    }

    fn valid_strings() -> Vec<(&'static [u8], LexerToken)> {
        vec![
            (b"\"abc\"", LexerToken::new(0, 5, LexerTokenType::String)),
            (b"\"A\\uD83D\\uDE00B\"", LexerToken::new(0, 16, LexerTokenType::String)),
            (b"\"b\\n\"", LexerToken::new(0, 5, LexerTokenType::String)),
            (b"\"\\u00E9\"", LexerToken::new(0, 8, LexerTokenType::String)),
            // 2-byte sequence same as (b"\"\xC3\xA9\"", 0, 4)
            ("\"é\"".as_bytes(), LexerToken::new(0, 4, LexerTokenType::String)),
        ]
    }

    fn invalid_strings() -> Vec<(&'static [u8], JsonError)> {
        vec![
            // raw byte control character
            (b"\"\x00", JsonError::from(StringError { kind: StringErrorKind::InvalidControlCharacter { byte: 0 }, pos: 1 })),
            // unpaired escaped
            (b"\"\\", JsonError::from(StringError { kind: StringErrorKind::UnexpectedEndOf, pos: 1 })),
            // unknown escaped
            (b"\"\\g\"", JsonError::from(StringError { kind: StringErrorKind::UnknownEscapedCharacter { byte: b'g' }, pos: 2 })),
            // incomplete unicode
            (b"\"\\u", JsonError::from(StringError { kind: StringErrorKind::UnexpectedEndOf, pos: 2 })),
            // // not enough bytes to form a low surrogate
            (b"\"\\uD83D\"", JsonError::from(StringError { kind: StringErrorKind::UnexpectedEndOf, pos: 7 })),
            // // high not followed by low
            (b"\"\\uD83Dabcdef\"", JsonError::from(StringError { kind: StringErrorKind::InvalidSurrogate, pos: 1 })),
            // // high followed by high
            (b"\"\\uD83D\\uD83D\"", JsonError::from(StringError { kind: StringErrorKind::InvalidSurrogate, pos: 1 })),
            // // low surrogate
            (b"\"\\uDC00\"", JsonError::from(StringError { kind: StringErrorKind::InvalidSurrogate, pos: 1 })),
            (b"\"abc", JsonError::from(StringError { kind: StringErrorKind::UnexpectedEndOf, pos: 3 })),
        ]
    }

    fn valid_literals() -> Vec<(&'static [u8], LexerToken)> {
        vec![
            (b"false", LexerToken::new(0, 5, LexerTokenType::Boolean)),
            (b"true", LexerToken::new(0, 4, LexerTokenType::Boolean)),
            (b"null", LexerToken::new(0, 4, LexerTokenType::Null)),
        ]
    }

    // 1 mismatch and 1 not enough characters is enough
    fn invalid_literals() -> Vec<(&'static [u8], JsonError)> {
        vec![
            (b"falte", JsonError::new(JsonErrorKind::UnexpectedCharacter { byte: b't' }, Some(3))),
            (b"tru", JsonError::new(JsonErrorKind::UnexpectedEof, Some(2))),
        ]
    }

    #[test]
    fn test_valid_numbers() {
        for (buffer, token) in valid_numbers() {
            let mut lexer = Lexer::new(buffer);
            let t = lexer.peek().unwrap().unwrap();

            assert_eq!(t, token);
        }
    }

    #[test]
    fn test_invalid_numbers() {
        for (buffer, error) in invalid_numbers() {
            let mut lexer = Lexer::new(buffer);
            let result = lexer.lex();

            assert_eq!(result, Err(error), "failed to tokenize: {buffer:?}");
        }
    }

    #[test]
    fn test_valid_strings() {
        for (buffer, token) in valid_strings() {
            let mut lexer = Lexer::new(buffer);
            let t = lexer.peek().unwrap().unwrap();

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
            let t = lexer.peek().unwrap().unwrap();

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
        let error = JsonError::new(JsonErrorKind::UnexpectedCharacter { byte: b'@' }, Some(0));

        assert_eq!(result, Err(error));
    }
}