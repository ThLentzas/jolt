use crate::error::{MalformedStringError, TokenizerError};
use crate::utils::{byte_utils, numeric_utils, utf8_utils};

// much better approach than passing boolean flags around, foo(true, true, false) is hard to understand
struct NumberState {
    decimal_point: bool,
    scientific_notation: bool,
    negative: bool
}

#[derive(Debug, PartialEq)]
pub enum TokenizerTokenType {
    LeftCurlyBracket,
    RightCurlyBracket,
    LeftSquareBracket,
    RightSquareBracket,
    Colon,
    Comma,
    Number,
    String,
    Boolean,
    Null,
}

#[derive(Debug, PartialEq)]
pub struct TokenizerToken {
    start_index: usize,
    offset: u32,
    token_type: TokenizerTokenType, // type is reserved
}


impl TokenizerToken {
    fn new(start_index: usize, offset: u32, token_type: TokenizerTokenType) -> Self {
        Self { start_index, offset, token_type }
    }

    pub fn start_index(&self) -> usize {
        self.start_index
    }

    pub fn offset(&self) -> u32 {
        self.offset
    }

    pub fn token_type(&self) -> &TokenizerTokenType {
        &self.token_type
    }
}

pub struct Tokenizer<'a> {
    buffer: &'a [u8],
    pos: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(buffer: &'a [u8]) -> Self {
        Self { buffer, pos: 0 }
    }

    pub fn tokenize(&mut self) -> Result<Vec<TokenizerToken>, TokenizerError> {
        let mut tokens: Vec<TokenizerToken> = Vec::new();
        let len = self.buffer.len();

        // Byte Order Mark
        utf8_utils::ignore_bom_if_present(&mut self.pos, &self.buffer);

        while self.pos < len {
            self.skip_white_spaces()?;
            if self.pos >= len {
                return Ok(tokens);
            }

            let current = self.buffer[self.pos];
            if current.is_ascii() {
                match current {
                    b'{' => tokens.push(TokenizerToken::new(self.pos, 1, TokenizerTokenType::LeftCurlyBracket)),
                    b'}' => tokens.push(TokenizerToken::new(self.pos, 1, TokenizerTokenType::RightCurlyBracket)),
                    b']' => tokens.push(TokenizerToken::new(self.pos, 1, TokenizerTokenType::RightSquareBracket)),
                    b'[' => tokens.push(TokenizerToken::new(self.pos, 1, TokenizerTokenType::LeftSquareBracket)),
                    b':' => tokens.push(TokenizerToken::new(self.pos, 1, TokenizerTokenType::Colon)),
                    b',' => tokens.push(TokenizerToken::new(self.pos, 1, TokenizerTokenType::Comma)),
                    b'-' | b'+' | b'0'..=b'9' => {
                        let start = self.pos;
                        self.scan_number()?;
                        tokens.push(TokenizerToken::new(start, (self.pos - start + 1) as u32, TokenizerTokenType::Number));
                    }
                    b'"' => {
                        let start = self.pos;
                        self.scan_string()?;
                        tokens.push(TokenizerToken::new(start, (self.pos - start + 1) as u32, TokenizerTokenType::String));
                    }
                    b't' | b'f' => {
                        let start = self.pos;
                        self.scan_boolean()?;
                        tokens.push(TokenizerToken::new(start, (self.pos - start + 1) as u32, TokenizerTokenType::Boolean));
                    }
                    b'n' => {
                        let start = self.pos;
                        self.scan_null()?;
                        tokens.push(TokenizerToken::new(start, (self.pos - start + 1) as u32, TokenizerTokenType::Null));
                    }
                    _ => return Err(TokenizerError::UnrecognizedCharacter { byte: Some(current), pos: self.pos })
                }
                self.pos += 1;
            } else {
                return Err(TokenizerError::UnrecognizedCharacter { byte: Some(current), pos: self.pos });
            }
        }
        // reset the position, so the next tokenize call with the same buffer has the same output
        self.pos = 0;

        Ok(tokens)
    }

    fn scan_number(&mut self) -> Result<(), TokenizerError> {
        let len = self.buffer.len();
        let start = self.pos;
        let mut current = self.buffer[self.pos];
        let next = self.buffer.get(self.pos + 1);

        match(current, next) {
            (b'+' | b'-', None) => return Err(TokenizerError::UnrecognizedCharacter { byte: Some(current), pos: self.pos }),
            // +9
            (b'+', Some(next_byte)) if next_byte.is_ascii_digit() => return Err(TokenizerError::InvalidNumber {
                message: "json specification prohibits numbers from being prefixed with a plus sign",
                pos: self.pos
            }),
            // +a
            (b'+', Some(_)) => return Err(TokenizerError::UnrecognizedCharacter { byte: Some(current), pos: self.pos }),
            // -0 is valid
            (b'-', Some(next_byte)) if !next_byte.is_ascii_digit() => return Err(TokenizerError::InvalidNumber {
                message: "a valid numeric value requires a digit (0-9) after the minus sign",
                pos: self.pos
            }),
            // 05 not allowed
            (b'0', Some(next_byte)) if next_byte.is_ascii_digit() => return Err(TokenizerError::InvalidNumber {
                message: "leading zeros are not allowed",
                pos: self.pos
            }),
            _ => (),
        }

        // Advance before the check. For single digit cases next is always none, if we return
        // before advancing, self.position is still at 0, control returns to tokenize and because
        // self.position < len is true it enters an infinite loop
        self.pos += 1;
        if next.is_none() {
            self.pos -= 1;
            return Ok(());
        }

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
        if self.is_out_of_range(start, state) {
            return Err(TokenizerError::InvalidNumber { message: "number out of range", pos: start });
        }

        self.pos -= 1;
        Ok(())
    }

    fn check_decimal_point(&self, state: &mut NumberState) -> Result<(), TokenizerError> {
        // 1.2.3
        if state.decimal_point {
            return Err(TokenizerError::InvalidNumber { message: "double decimal point found", pos: self.pos });
        }
        // 1. 2.g
        if self.pos + 1 >= self.buffer.len() || !self.buffer[self.pos + 1].is_ascii_digit() {
            return Err(TokenizerError::InvalidNumber { message: "decimal point must be followed by a digit", pos: self.pos });
        }
        // 1e4.5
        if state.scientific_notation {
            return Err(TokenizerError::InvalidNumber { message: "decimal point is not allowed after exponential notation", pos: self.pos });
        }
        state.decimal_point = true;

        Ok(())
    }

    fn check_scientific_notation(&self, state: &mut NumberState) -> Result<(), TokenizerError> {
        let current = self.buffer[self.pos];

        match current {
            b'e' | b'E' => {
                // 1e2E3
                if state.scientific_notation {
                    return Err(TokenizerError::InvalidNumber { message: "double exponential notation('e' or 'E') found", pos: self.pos });
                }
                // 1e 1eg
                if self.pos + 1 >= self.buffer.len() || !matches!(self.buffer[self.pos + 1], b'-' | b'+' | b'0'..b'9') {
                    return Err(TokenizerError::InvalidNumber { message: "exponential notation must be followed by a digit or a sign", pos: self.pos });
                }
                // Leading zeros are allowed on the exponent 1e005 evaluates to 100000
                state.scientific_notation = true;
            }
            b'+' | b'-' => {
                // 1+2
                if !matches!(self.buffer[self.pos - 1], b'e' | b'E') {
                    return Err(TokenizerError::InvalidNumber { message: "sign ('+' or '-') is only allowed as part of exponential notation", pos: self.pos });
                }
                // 1E+g
                if self.pos + 1 >= self.buffer.len()  || !self.buffer[self.pos + 1].is_ascii_digit() {
                    return Err(TokenizerError::InvalidNumber { message: "exponential notation must be followed by a digit", pos: self.pos });
                }
            }
            _ => unreachable!("Called with {} instead of 'e', 'E', '+', or '-'", current)
        }
        Ok(())
    }

    fn is_out_of_range(&self, start: usize, state: NumberState) -> bool {
        let slice =  &self.buffer[start..self.pos];

        if state.decimal_point || state.scientific_notation {
            return numeric_utils::is_out_of_range_f64(slice);
        } else {
            match numeric_utils::is_out_of_range_i64(slice) {
                true if state.negative => return false,
                // positive and overflow for i64, try u64
                true => return numeric_utils::is_out_of_range_u64(slice),
                false => (),
            };
        }
        false
    }

    fn scan_string(&mut self) -> Result<(), MalformedStringError> {
        let len = self.buffer.len();
        self.pos += 1;

        while self.pos < len && self.buffer[self.pos] != b'"' {
            let current = self.buffer[self.pos];
            // raw control characters are not allowed
            // [34, 10, 34] is invalid the control character is passed as raw byte, and it is unescaped
            // but [34, 92, 110, 34] should be considered valid as a new line character
            if current.is_ascii_control() {
                return Err(MalformedStringError::InvalidControlCharacter { byte: current, pos: self.pos });
            }
            if !current.is_ascii() {
                utf8_utils::validate_utf8_sequence(&mut self.pos, &self.buffer)?;
            }
            if current == b'\\' {
                self.pos += 1;
                if self.pos >= len {
                    return Err(MalformedStringError::InvalidEscapeSequence { pos: self.pos - 1 });
                }

                let next = self.buffer[self.pos];
                if !matches!(next, b'\\' | b'"' | b'/' | b'b' | b'f' | b'n' | b'r' | b't' | b'u') {
                    return Err(MalformedStringError::InvalidEscapeSequence { pos: self.pos });
                }
                if next == b'u' {
                    self.check_unicode_escape()?;
                }
            }
            self.pos += 1;
        }
        if self.pos == len {
            return Err(MalformedStringError::UnterminatedString { pos: self.pos - 1 });
        }
        Ok(())
    }

    fn check_unicode_escape(&mut self) -> Result<(), MalformedStringError> {
        // Need 4 hex digits
        if self.pos + 4 >= self.buffer.len() {
            return Err(MalformedStringError::InvalidEscapeSequence { pos: self.pos });
        }

        // self.pos is at the first digit of the hex sequence
        self.pos += 1;
        let val = match numeric_utils::hex_to_u16(&self.buffer[self.pos..self.pos + 4]) {
            Ok(hex) => hex,
            Err(_) => return Err(MalformedStringError::InvalidEscapeSequence { pos: self.pos - 2 })
        };
        // move past the hex digits
        self.pos += 3;
        if utf8_utils::is_surrogate(val) {
            utf8_utils::validate_surrogate(&mut self.pos, &self.buffer, val)?;
        }

        Ok(())
    }

    fn scan_boolean(&mut self) -> Result<(), TokenizerError> {
        let remaining = &self.buffer[self.pos..];

        match remaining {
            s if s.starts_with(b"true") => self.pos += 3,
            s if s.starts_with(b"false") => self.pos += 4,
            _ => return Err(TokenizerError::UnexpectedValue { pos: self.pos }),
        }
        Ok(())
    }

    fn scan_null(&mut self) -> Result<(), TokenizerError> {
        if self.buffer[self.pos..].starts_with(b"null") {
            self.pos += 3;
        } else {
            return Err(TokenizerError::UnexpectedValue { pos: self.pos });
        }
        Ok(())
    }

    fn skip_white_spaces(&mut self) -> Result<(), TokenizerError> {
        while self.pos < self.buffer.len() {
            let current = self.buffer[self.pos];
            // The only control characters that are allowed as raw bytes according to rfc are '\t', '\n', '\r'
            // ' '(space) is not a control character
            if current.is_ascii_control() && !byte_utils::is_rfc_whitespace(current) {
                return Err(TokenizerError::UnrecognizedCharacter { byte: Some(current), pos: self.pos });
            }
            if byte_utils::is_rfc_whitespace(current) {
                self.pos += 1;
            } else {
                break;
            }
        }
        Ok(())
    }
}

// We declare tokenizer as let mut tokenizer because tokenize takes a mutable reference to self
#[cfg(test)]
mod tests {
    use crate::error::Utf8Error;
    use super::*;

    fn valid_numbers() -> Vec<(&'static [u8], TokenizerToken)> {
        vec![
            (b"0", TokenizerToken::new(0, 1, TokenizerTokenType::Number)),
            (b"-0", TokenizerToken::new(0, 2, TokenizerTokenType::Number)),
            (b"123", TokenizerToken::new(0, 3, TokenizerTokenType::Number)),
            (b"-45", TokenizerToken::new(0, 3, TokenizerTokenType::Number)),
            (b"123.45", TokenizerToken::new(0, 6, TokenizerTokenType::Number)),
            (b"1e10", TokenizerToken::new(0, 4, TokenizerTokenType::Number)),
            (b"-0.1e-2", TokenizerToken::new(0, 7, TokenizerTokenType::Number)),
        ]
    }

    fn invalid_numbers() -> Vec<(&'static [u8], TokenizerError)> {
        vec![
            (b"+", TokenizerError::UnrecognizedCharacter { byte: Some(43), pos: 0 }),
            (b"+a", TokenizerError::UnrecognizedCharacter { byte: Some(43), pos: 0 }),
            (b"+9", TokenizerError::InvalidNumber { message: "json specification prohibits numbers from being prefixed with a plus sign", pos: 0 }),
            (b"-a", TokenizerError::InvalidNumber { message: "a valid numeric value requires a digit (0-9) after the minus sign", pos: 0 }),
            (b"06", TokenizerError::InvalidNumber { message: "leading zeros are not allowed", pos: 0 }),
            (b"1.2.3", TokenizerError::InvalidNumber { message: "double decimal point found", pos: 3 }),
            (b"1.", TokenizerError::InvalidNumber { message: "decimal point must be followed by a digit", pos: 1 }),
            (b"1.a", TokenizerError::InvalidNumber { message: "decimal point must be followed by a digit", pos: 1 }),
            (b"4e5.1", TokenizerError::InvalidNumber { message: "decimal point is not allowed after exponential notation", pos: 3 }),
            (b"1e2e5", TokenizerError::InvalidNumber { message: "double exponential notation('e' or 'E') found", pos: 3 }),
            (b"1e", TokenizerError::InvalidNumber { message: "exponential notation must be followed by a digit or a sign", pos: 1 }),
            (b"246Ef", TokenizerError::InvalidNumber { message: "exponential notation must be followed by a digit or a sign", pos: 3 }),
            (b"83+1", TokenizerError::InvalidNumber { message: "sign ('+' or '-') is only allowed as part of exponential notation", pos: 2 }),
            (b"1e+", TokenizerError::InvalidNumber { message: "exponential notation must be followed by a digit", pos: 2 }),
            (b"1e+f", TokenizerError::InvalidNumber { message: "exponential notation must be followed by a digit", pos: 2 }),
            (b"1.8e308", TokenizerError::InvalidNumber { message: "number out of range", pos: 0 }), // overflow for f64
            (b"-9223372036854775809", TokenizerError::InvalidNumber { message: "number out of range", pos: 0 }), // overflow for i64
            (b"18446744073709551616", TokenizerError::InvalidNumber { message: "number out of range", pos: 0 }), // overflow for u64
        ]
    }

    fn valid_strings() -> Vec<(&'static [u8], TokenizerToken)> {
        vec![
            (b"\"abc\"", TokenizerToken::new(0, 5, TokenizerTokenType::String)),
            (b"\"A\\uD83D\\uDE00B\"", TokenizerToken::new(0, 16, TokenizerTokenType::String)),
            (b"\"b\\n\"", TokenizerToken::new(0, 5, TokenizerTokenType::String)),
            (b"\"\\u00E9\"", TokenizerToken::new(0, 8, TokenizerTokenType::String)),
            // 2-byte sequence same as (b"\"\xC3\xA9\"", 0, 4)
            ("\"é\"".as_bytes(), TokenizerToken::new(0, 4, TokenizerTokenType::String)),
        ]
    }

    fn invalid_strings() -> Vec<(&'static [u8], TokenizerError)> {
        vec![
            // raw byte control character
            (b"\"\x00", TokenizerError::InvalidString(MalformedStringError::InvalidControlCharacter { byte: 0, pos: 1 })),
            (b"\"\\", TokenizerError::InvalidString(MalformedStringError::InvalidEscapeSequence { pos: 1 })),
            (b"\"\\g\"", TokenizerError::InvalidString(MalformedStringError::InvalidEscapeSequence { pos: 2 })),
            (b"\"\\u\"", TokenizerError::InvalidString(MalformedStringError::InvalidEscapeSequence { pos: 2 })),
            // // not enough bytes to form a low surrogate
            (b"\"\\uD83D\"", TokenizerError::InvalidString(MalformedStringError::InvalidUtf8(Utf8Error::InvalidSurrogate { pos: 1 }))),
            // high not followed by low
            (b"\"\\uD83Dabcdef\"", TokenizerError::InvalidString(MalformedStringError::InvalidUtf8(Utf8Error::InvalidSurrogate { pos: 1 }))),
            // high followed by high
            (b"\"\\uD83D\\uD83D\"", TokenizerError::InvalidString(MalformedStringError::InvalidUtf8(Utf8Error::InvalidSurrogate { pos: 1 }))),
            // low surrogate
            (b"\"\\uDC00\"", TokenizerError::InvalidString(MalformedStringError::InvalidUtf8(Utf8Error::InvalidSurrogate { pos: 1 }))),
            (b"\"abc", TokenizerError::InvalidString(MalformedStringError::UnterminatedString { pos: 3 })),
        ]
    }

    fn valid_boolean() -> Vec<(&'static [u8], TokenizerToken)> {
        vec![
            (b"false", TokenizerToken::new(0, 5, TokenizerTokenType::Boolean)),
            (b"true", TokenizerToken::new(0, 4, TokenizerTokenType::Boolean)),
        ]
    }

    fn invalid_boolean() -> Vec<(&'static [u8], TokenizerError)> {
        vec![
            (b"falte", TokenizerError::UnexpectedValue { pos: 0}),
            (b"trur", TokenizerError::UnexpectedValue { pos: 0}),
        ]
    }

    #[test]
    fn empty_input() {
        // []
        let buffer = [];
        let mut tokenizer = Tokenizer::new(&buffer);
        let tokens = tokenizer.tokenize().unwrap();

        assert!(tokens.is_empty());
    }

    #[test]
    fn skip_whitespaces() {
        // \t, \n, \r, ' '
        let buffer: [u8; 4] = [9, 10, 13, 32];
        let mut tokenizer = Tokenizer::new(&buffer);
        let tokens = tokenizer.tokenize().unwrap();

        assert!(tokens.is_empty());
    }

    #[test]
    fn test_valid_numbers() {
        for (buffer, token) in valid_numbers() {
            let mut tokenizer = Tokenizer::new(buffer);
            let tokens = tokenizer.tokenize().unwrap();

            assert_eq!(tokens.len(), 1);
            assert_eq!(tokens[0], token);
        }
    }

    #[test]
    fn test_invalid_numbers() {
        for (buffer, error) in invalid_numbers() {
            let mut tokenizer = Tokenizer::new(buffer);
            let result = tokenizer.tokenize();

            if let Err(e) = result {
                assert_eq!(e, error);
            }
        }
    }

    #[test]
    fn test_valid_strings() {
        for (buffer, token) in valid_strings() {
            let mut tokenizer = Tokenizer::new(buffer);
            let tokens = tokenizer.tokenize().unwrap();

            assert_eq!(tokens.len(), 1);
            assert_eq!(tokens[0], token);
        }
    }

    #[test]
    fn test_invalid_strings() {
        for (buffer, error) in invalid_strings() {
            let mut tokenizer = Tokenizer::new(buffer);
            let result = tokenizer.tokenize();

            if let Err(e) = result {
                assert_eq!(e, error);
            }
        }
    }

    #[test]
    fn test_valid_boolean() {
        for (buffer, token) in valid_boolean() {
            let mut tokenizer = Tokenizer::new(buffer);
            let tokens = tokenizer.tokenize().unwrap();

            assert_eq!(tokens.len(), 1);
            assert_eq!(tokens[0], token);
        }
    }

    #[test]
    fn test_invalid_boolean() {
        for (buffer, error) in invalid_boolean() {
            let mut tokenizer = Tokenizer::new(buffer);
            let result = tokenizer.tokenize();

            if let Err(e) = result {
                assert_eq!(e, error);
            }
        }
    }

    #[test]
    fn test_valid_null() {
        let buffer: [u8; 4] = [110, 117, 108, 108];
        let mut tokenizer = Tokenizer::new(&buffer);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], TokenizerToken::new(0, 4, TokenizerTokenType::Null));
    }

    #[test]
    fn test_invalid_null() {
        let buffer: [u8; 4] = [110, 117, 108, 107];
        let mut tokenizer = Tokenizer::new(&buffer);
        let result = tokenizer.tokenize();

        if let Err(e) = result {
            assert_eq!(e, TokenizerError::UnexpectedValue { pos: 0 });
        }
    }

    #[test]
    fn test_unrecognized_character() {
        // @
        let mut tokenizer = Tokenizer::new(&[64]);

        assert!(tokenizer.tokenize().is_err());
    }
}