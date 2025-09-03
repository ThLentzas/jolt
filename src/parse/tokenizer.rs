use crate::error::TokenizerError;
use crate::utils::{byte_utils, numeric_utils, utf8_utils};

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

pub struct TokenizerToken {
    start_index: usize,
    offset: u32,
    token_type: TokenizerTokenType,
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
    position: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(buffer: &'a [u8]) -> Self {
        Self { buffer, position: 0 }
    }

    pub fn tokenize(&mut self) -> Result<Vec<TokenizerToken>, TokenizerError> {
        let mut tokens: Vec<TokenizerToken> = Vec::new();
        let len = self.buffer.len();

        // Byte Order Mark
        utf8_utils::ignore_bom_if_present(&mut self.position, & self.buffer);

        while self.position < len {
            self.skip_white_spaces()?;
            if self.position >= len {
                return Ok(tokens);
            }

            let byte = self.buffer[self.position];
            if byte.is_ascii() {
                match byte {
                    b'{' => tokens.push(TokenizerToken::new(self.position, 1, TokenizerTokenType::LeftCurlyBracket)),
                    b'}' => tokens.push(TokenizerToken::new(self.position, 1, TokenizerTokenType::RightCurlyBracket)),
                    b']' => tokens.push(TokenizerToken::new(self.position, 1, TokenizerTokenType::RightSquareBracket)),
                    b'[' => tokens.push(TokenizerToken::new(self.position, 1, TokenizerTokenType::LeftSquareBracket)),
                    b':' => tokens.push(TokenizerToken::new(self.position, 1, TokenizerTokenType::Colon)),
                    b',' => tokens.push(TokenizerToken::new(self.position, 1, TokenizerTokenType::Comma)),
                    b'-' | b'+' | b'0'..=b'9' => {
                        let start = self.position;
                        self.scan_number()?;
                        tokens.push(TokenizerToken::new(start, (self.position - start) as u32, TokenizerTokenType::Number))
                    }
                    b'"' => {
                        let start = self.position;
                        self.scan_string()?;
                        tokens.push(TokenizerToken::new(start, (self.position - start) as u32, TokenizerTokenType::String))
                    }
                    b't' | b'f' => {
                        let start = self.position;
                        self.scan_boolean()?;
                        tokens.push(TokenizerToken::new(start, (self.position - start) as u32, TokenizerTokenType::Boolean))
                    }
                    b'n' => {
                        let start = self.position;
                        self.scan_null()?;
                        tokens.push(TokenizerToken::new(start, (self.position - start) as u32, TokenizerTokenType::Null))
                    }
                    // toDo: handle unexpected character: false5 -> check if the stack is empty and there are more tokens in the list on the parser
                    _ => return Err(TokenizerError::UnrecognizedCharacter { byte: Some(byte), position: self.position })
                }
            } else {
                return Err(TokenizerError::UnrecognizedCharacter { byte: Some(byte), position: self.position });
            }
        }
        Ok(tokens)
    }

    fn scan_number(&mut self) -> Result<(), TokenizerError> {
        let len = self.buffer.len();
        let mut byte = self.buffer[self.position];
        let next_byte = self.buffer.get(self.position + 1);

        match(byte, next_byte) {
            (b'+' | b'-', None) => return Err(TokenizerError::UnrecognizedCharacter { byte: Some(byte), position: self.position }),
            (b'+', Some(next_byte)) if next_byte.is_ascii_digit() => return Err(TokenizerError::InvalidNumber {
                message: "JSON specification prohibits numbers from being prefixed with a plus sign",
                position: self.position
            }),
            // -0 is valid
            (b'-', Some(next_byte)) if !next_byte.is_ascii_digit() => return Err(TokenizerError::InvalidNumber {
                message: "A valid numeric value requires a digit (0-9) after the minus sign",
                position: self.position
            }),
            (b'0', Some(next_byte)) if next_byte.is_ascii_digit() => return Err(TokenizerError::InvalidNumber {
                message: "Leading zeros are not allowed",
                position: self.position
            }),
            _ => (),
        }

        // Advance before the check. For single digit cases next_byte is always none, if we return
        // before advancing, self.position is still at 0, control returns to tokenize and because
        // self.position < len is true it enters an infinite loop
        self.position += 1;
        if next_byte.is_none() {
            return Ok(());
        }

        byte = *next_byte.unwrap();
        let mut has_decimal_point = false;
        let mut has_exponential_notation = false;
        while self.position < len {
                match byte {
                    b'0'..=b'9' => (),
                    b'.' => {
                        // 1.2.3
                        if has_decimal_point {
                            return Err(TokenizerError::InvalidNumber { message: "Double decimal point found", position: self.position });
                        }
                        // 1. 2.g
                        if self.position + 1 >= len || !self.buffer[self.position + 1].is_ascii_digit() {
                            return Err(TokenizerError::InvalidNumber { message: "Decimal point must be followed by a digit", position: self.position });
                        }
                        // 1e4.5
                        if has_exponential_notation {
                            return Err(TokenizerError::InvalidNumber { message: "Decimal point is not allowed after exponential notation", position: self.position });
                        }
                        has_decimal_point = true;
                    }
                    b'e' | b'E' => {
                        // 1e2E3
                        if has_exponential_notation {
                            return Err(TokenizerError::InvalidNumber { message: "Double exponential notation('e' or 'E') found", position: self.position });
                        }
                        // 1e 1eg
                        if self.position + 1 >= len || !matches!(self.buffer[self.position + 1], b'-' | b'+' | b'0'..b'9') {
                            return Err(TokenizerError::InvalidNumber { message: "Exponential notation must be followed by a digit or a sign", position: self.position });
                        }
                        has_exponential_notation = true;
                    }
                    b'-' | b'+' => {
                        // 1+2
                        if !matches!(self.buffer[self.position - 1], b'e' | b'E') {
                            return Err(TokenizerError::InvalidNumber { message: "Sign ('+' or '-') is only allowed as part of exponential notation", position: self.position });
                        }
                        // 1E+g
                        if self.position + 1 >= len  || !self.buffer[self.position + 1].is_ascii_digit() {
                            return Err(TokenizerError::InvalidNumber { message: "Exponential notation must be followed by a digit", position: self.position });
                        }
                    }
                    _ => break
            }
            self.position += 1;
            if self.position >= len {
                break;
            }
            byte = self.buffer[self.position];
        }
        Ok(())
    }
    
    fn scan_string(&mut self) -> Result<(), TokenizerError> {
        let len = self.buffer.len();
        self.position += 1;

        while self.position < len && self.buffer[self.position] != b'"' {
            let byte = self.buffer[self.position];
            // raw control characters are not allowed
            if byte.is_ascii_control() {
                return Err(TokenizerError::InvalidControlCharacter { byte, position: self.position });
            }
            if !byte.is_ascii() {
                utf8_utils::validate_utf8_sequence(&mut self.position, &self.buffer)?;
            }
            if byte == b'\\' {
                self.position += 1;
                if self.position >= len {
                    return Err(TokenizerError::IncompleteEscapeSequence { position: self.position - 1 });
                }

                let next_byte = self.buffer[self.position];
                if !matches!(next_byte, b'\\' | b'"' | b'/' | b'b' | b'f' | b'n' | b'r' | b't' | b'u') {
                    return Err(TokenizerError::UnknownEscapeCharacter{ position: self.position });
                }
                if next_byte == b'u' {
                    // Need 4 hex digits
                    if self.position + 4 >= len {
                        return Err(TokenizerError::IncompleteUnicodeSequence { position: self.position });
                    }

                    // self.position is at the 1 digit of the hex sequence
                    self.position += 1;
                    let val = numeric_utils::hex_to_u16((&self.buffer[self.position..self.position + 4]).try_into().unwrap())?;
                    // move past the hex digits
                    self.position += 3;
                    if utf8_utils::is_surrogate(val) {
                        self.validate_surrogate(val)?
                    }
                }
            }
            self.position += 1;
        }
        if self.position == len {
            return Err(TokenizerError::UnterminatedString { position: self.position - 1 });
        }
        // move to the next character past the closing quotation mark
        self.position += 1;

        Ok(())
    }

    fn validate_surrogate(&mut self, val: u16) -> Result<(), TokenizerError>{
        let len = self.buffer.len();

        if utf8_utils::is_high_surrogate(val) {
            if self.position + 6 >= len {
                // self.position - 6 is the index at the start of the Unicode sequence
                return Err(TokenizerError::UnpairedSurrogate { val, position: self.position - 6 });
            }

            self.position += 1;
            match (self.buffer[self.position], self.buffer[self.position + 1]) {
                (b'\\', b'u') => {
                    self.position += 2;
                }
                _ => return Err(TokenizerError::UnpairedSurrogate { val, position: self.position })
            };

            let low = numeric_utils::hex_to_u16((&self.buffer[self.position..self.position + 4]).try_into().unwrap())?;
            if !utf8_utils::is_low_surrogate(low) {
                return Err(TokenizerError::UnpairedSurrogate { val, position: self.position - 6});
            }
        } else {
            // surrogate pairs do not start with low surrogate, it's always high-low
            return Err(TokenizerError::InvalidSurrogate { val, position: self.position - 6 });
        }
        Ok(())
    }

    fn scan_boolean(&mut self) -> Result<(), TokenizerError> {
        let remaining = &self.buffer[self.position..];

        match remaining {
            s if s.starts_with(b"false") => self.position += 5,
            s if s.starts_with(b"true") => self.position += 4,
            _ => {
                match self.buffer[self.position] {
                    b'f' | b't' => return Err(TokenizerError::UnexpectedValue { position: self.position }),
                    _ => ()
                }
            }
        }
        Ok(())
    }

    fn scan_null(&mut self) -> Result<(), TokenizerError> {
        if self.buffer[self.position..].starts_with(b"null") {
            self.position += 4;
        } else {
            return Err(TokenizerError::UnexpectedValue { position: self.position });
        }
        Ok(())
    }

    fn skip_white_spaces(&mut self) -> Result<(), TokenizerError> {
        while self.position < self.buffer.len() {
            let byte = self.buffer[self.position];
            // The only control characters that are allowed as raw bytes according to rfc are '\t', '\n', '\r'
            // ' '(space) is not a control character
            if byte.is_ascii_control() && !byte_utils::is_rfc_whitespace(byte) {
                return Err(TokenizerError::InvalidControlCharacter { byte, position: self.position});
            }
            if byte_utils::is_rfc_whitespace(byte) {
                self.position += 1;
            } else {
                break;
            }
        }
        Ok(())
    }

    // reset the input buffer
    pub fn reset(&mut self, buffer: &'a [u8]) {
        self.buffer = buffer;
        self.position = 0;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn valid_numbers() -> Vec<(&'static [u8], usize, u32)> {
        vec![
            (b"0", 0, 1),
            (b"-0", 0, 2),
            (b"123", 0, 3),
            (b"-45", 0, 3),
            (b"123.45", 0, 6),
            (b"1e10", 0, 4),
            (b"-0.1e-2", 0, 7),
        ]
    }

    fn invalid_numbers() -> Vec<&'static [u8]> {
        vec![
            b"+", b"-", b"+9", b"-a", b"06",
            b"1.", b"1.a", b"1.2.3", b"4e5.1",
            b"1e2e5", b"246Ef", b"83+1", b"1e+", b"1e"
        ]
    }

    fn valid_strings() -> Vec<(&'static [u8], usize, u32)> {
        vec![
            (b"\"abc\"", 0, 5),
            (b"\"A\\uD83D\\uDE00B\"", 0, 16),
            (b"\"b\\n\"", 0, 5),
            (b"\"\\u00E9\"", 0, 8),
            // 2-byte sequence
            // same as (b"\"\xC3\xA9\"", 0, 4)
            ("\"é\"".as_bytes(), 0, 4),
        ]
    }

    fn invalid_strings() -> Vec<&'static [u8]> {
        vec![
            // raw byte control character
            b"\"\x00",
            // incomplete escape sequence
            b"\"\\",
            // unknown escape character
            b"\"\\g\"",
            // incomplete unicode sequence
            b"\"\\u\"",
            // unpaired high surrogate, not enough bytes to form a low surrogate
            b"\"\\uD83D\"",
            // unpaired high surrogate, high not followed by low
            b"\"\\uD83Dabcdef\"",
            // low surrogate
            b"\"\\uDE00\"",
            // unterminated string
            b"\"abc",
        ]
    }

    fn valid_boolean() -> Vec<(&'static [u8], usize, u32)> {
        vec![
            (b"false", 0, 5),
            (b"true", 0, 4)
        ]
    }

    fn invalid_boolean() -> Vec<&'static [u8]> {
        vec![
            b"falte",
            b"trur"
        ]
    }

    #[test]
    fn test_valid_numbers() {
        for (sequence, start_index, offset) in valid_numbers() {
            let mut tokenizer = Tokenizer::new(sequence);
            let tokens = tokenizer.tokenize().unwrap();

            assert_eq!(tokens.len(), 1);
            assert_eq!(tokens[0].start_index, start_index);
            assert_eq!(tokens[0].offset, offset);
            assert_eq!(tokens[0].token_type, TokenizerTokenType::Number);
        }
    }

    #[test]
    fn test_invalid_numbers() {
        for sequence in invalid_numbers() {
            let mut tokenizer = Tokenizer::new(sequence);
            assert!(tokenizer.tokenize().is_err());
        }
    }

    #[test]
    fn test_valid_strings() {
        for (sequence, start_index, offset) in valid_strings() {
            let mut tokenizer = Tokenizer::new(sequence);
            let tokens = tokenizer.tokenize().unwrap();

            assert_eq!(tokens.len(), 1);
            assert_eq!(tokens[0].start_index, start_index);
            assert_eq!(tokens[0].offset, offset);
            assert_eq!(tokens[0].token_type, TokenizerTokenType::String);
        }
    }

    #[test]
    fn test_invalid_strings() {
        for sequence in invalid_strings() {
            let mut tokenizer = Tokenizer::new(sequence);
            assert!(tokenizer.tokenize().is_err());
        }
    }

    #[test]
    fn test_valid_boolean() {
        for (sequence, start_index, offset) in valid_boolean() {
            let mut tokenizer = Tokenizer::new(sequence);
            let tokens = tokenizer.tokenize().unwrap();

            assert_eq!(tokens.len(), 1);
            assert_eq!(tokens[0].start_index, start_index);
            assert_eq!(tokens[0].offset, offset);
            assert_eq!(tokens[0].token_type, TokenizerTokenType::Boolean);
        }
    }

    #[test]
    fn test_invalid_boolean() {
        for sequence in invalid_boolean() {
            let mut tokenizer = Tokenizer::new(sequence);
            assert!(tokenizer.tokenize().is_err());
        }
    }

    #[test]
    fn test_valid_null() {
        let mut tokenizer = Tokenizer::new(&[110, 117, 108, 108]);
        let tokens = tokenizer.tokenize().unwrap();

        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].start_index, 0);
        assert_eq!(tokens[0].offset, 4);
        assert_eq!(tokens[0].token_type, TokenizerTokenType::Null);
    }

    #[test]
    fn test_invalid_null() {
        let mut tokenizer = Tokenizer::new(&[110, 117, 108, 107]);

        assert!(tokenizer.tokenize().is_err());
    }

    #[test]
    fn test_unrecognized_character() {
        // @
        let mut tokenizer = Tokenizer::new(&[64]);

        assert!(tokenizer.tokenize().is_err());
    }
}