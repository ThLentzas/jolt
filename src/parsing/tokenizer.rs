use crate::parsing::error::{JsonErrorKind, JsonError};
use crate::parsing::{escapes, number, utf8};

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

    pub(super) fn start_index(&self) -> usize {
        self.start_index
    }

    pub(super) fn offset(&self) -> u32 {
        self.offset
    }

    pub(super) fn token_type(&self) -> &TokenizerTokenType {
        &self.token_type
    }
}

pub(super) struct Tokenizer<'a> {
    buffer: &'a [u8],
    pos: usize,
}

impl<'a> Tokenizer<'a> {
    pub(super) fn new(buffer: &'a [u8]) -> Self {
        Self { buffer, pos: 0 }
    }

    pub(super) fn tokenize(&mut self) -> Result<Vec<TokenizerToken>, JsonError> {
        let mut tokens: Vec<TokenizerToken> = Vec::new();
        let len = self.buffer.len();

        self.skip_white_spaces()?;
        // [](empty input buffer) or [\n, \t, '\r', ' '](only ws) are not allowed
        // Json text = ws value ws
        if self.buffer.is_empty() || self.pos >= len {
            let i = if self.pos == 0 { len } else { len - 1 };
            return Err(JsonError::new(JsonErrorKind::UnexpectedEof, Some(i)));
        }

        // skip Byte Order Mark
        if utf8::is_bom_present(&self.buffer) {
            self.pos = 3;
        }

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
                    // unknown ascii has text representation we pass Some()
                    _ => return Err(JsonError::new(JsonErrorKind::UnexpectedCharacter { byte: current }, Some(self.pos)))
                }
                self.pos += 1;
            } else {
                return Err(JsonError::new(JsonErrorKind::UnexpectedCharacter { byte: current }, Some(self.pos)));
            }
        }

        Ok(tokens)
    }

    fn scan_number(&mut self) -> Result<(), JsonError> {
        let len = self.buffer.len();
        let start = self.pos;
        let mut current = self.buffer[self.pos];
        let next = self.buffer.get(self.pos + 1);

        match(current, next) {
            (b'+' | b'-', None) => return Err(JsonError::new(JsonErrorKind::UnexpectedCharacter { byte: current }, Some(self.pos))),
            // +9
            (b'+', Some(next_byte)) if next_byte.is_ascii_digit() => return Err(JsonError::new(JsonErrorKind::InvalidNumber {
                message: "json specification prohibits numbers from being prefixed with a plus sign" }, Some(self.pos))),
            // +a
            (b'+', Some(_)) => return Err(JsonError::new(JsonErrorKind::UnexpectedCharacter { byte: current }, Some(self.pos))),
            // -0 is valid
            (b'-', Some(next_byte)) if !next_byte.is_ascii_digit() => return Err(JsonError::new(JsonErrorKind::InvalidNumber {
                message: "a valid numeric value requires a digit (0-9) after the minus sign" }, Some(self.pos))),
            // 05 not allowed
            (b'0', Some(next_byte)) if next_byte.is_ascii_digit() => return Err(JsonError::new(JsonErrorKind::InvalidNumber {
                message: "leading zeros are not allowed" }, Some(self.pos))),
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

    // toDo: consider adding a limit for the length of the string
    fn scan_string(&mut self) -> Result<(), JsonError> {
        let len = self.buffer.len();
        self.pos += 1;

        while self.pos < len && self.buffer[self.pos] != b'"' {
            let current = self.buffer[self.pos];
            // raw control characters are not allowed
            // [34, 10, 34] is invalid the control character is passed as raw byte, and it is unescaped
            // but [34, 92, 110, 34] should be considered valid as a new line character
            if current.is_ascii_control() {
                return Err(JsonError::new(JsonErrorKind::InvalidControlCharacter { byte: current }, Some(self.pos)));
            }
            if !current.is_ascii() {
                utf8::check_utf8_sequence(&self.buffer, self.pos)?;
                self.pos += utf8::utf8_char_width(current) - 1;
            }
            if current == b'\\' {
                escapes::check_escape_character(&self.buffer, self.pos)?;
                self.pos += escapes::len(&self.buffer, self.pos) - 1;
            }
            self.pos += 1;
        }
        if self.pos == len {
            return Err(JsonError::new(JsonErrorKind::UnexpectedEof, Some(self.pos - 1)));
        }
        Ok(())
    }

    // An approach with starts_with() could work but in the case of mismatch we won't know the index
    fn scan_boolean(&mut self) -> Result<(), JsonError> {
        let target = if self.buffer[self.pos] == b't' { "true".as_bytes() } else { "false".as_bytes() };
        self.scan_literal(target)?;

        Ok(())
    }

    fn scan_null(&mut self) -> Result<(), JsonError> {
        self.scan_literal(b"null")?;
        Ok(())
    }

    fn scan_literal(&mut self, target: &[u8]) -> Result<(), JsonError> {
        let remaining = &self.buffer[self.pos..];

        if target.len() > remaining.len() {
            return Err(JsonError::new(JsonErrorKind::UnexpectedEof, Some(self.buffer.len() - 1)));
        }

        for &byte in target.iter() {
            if self.buffer[self.pos] != byte {
                return Err(JsonError::new(JsonErrorKind::UnexpectedCharacter { byte: self.buffer[self.pos] }, Some(self.pos)));
            }
            self.pos += 1;
        }
        self.pos -= 1;

        Ok(())
    }

    fn skip_white_spaces(&mut self) -> Result<(), JsonError> {
        while self.pos < self.buffer.len() {
            let current = self.buffer[self.pos];
            // The only control characters that are allowed as raw bytes according to rfc are '\t', '\n', '\r'
            // ' '(space) is not a control character
            if current.is_ascii_control() && !matches!(current, b'\t' | b'\n' | b'\r' | b' ') { // rfc whitespaces
                return Err(JsonError::new(JsonErrorKind::UnexpectedCharacter { byte: current }, Some(self.pos)));
            }
            if matches!(current, b'\t' | b'\n' | b'\r' | b' ') {
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

    fn invalid_numbers() -> Vec<(&'static [u8], JsonError)> {
        vec![
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
            (b"1e+f", JsonError::new(JsonErrorKind::InvalidNumber { message: "exponential notation must be followed by a digit" }, Some(2))),
            (b"1.8e308", JsonError::new(JsonErrorKind::InvalidNumber { message: "number out of range" }, Some(0))),
            (b"-9223372036854775809", JsonError::new(JsonErrorKind::InvalidNumber { message: "number out of range" }, Some(0))),
            (b"18446744073709551616", JsonError::new(JsonErrorKind::InvalidNumber { message: "number out of range" }, Some(0))),
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

    fn invalid_strings() -> Vec<(&'static [u8], JsonError)> {
        vec![
            // raw byte control character
            (b"\"\x00", JsonError::new(JsonErrorKind::InvalidControlCharacter { byte: 0 }, Some(1))),
            // unpaired escaped
            (b"\"\\", JsonError::new(JsonErrorKind::UnexpectedEof, Some(1))),
            // unknown escaped
            (b"\"\\g\"", JsonError::new(JsonErrorKind::UnknownEscapedCharacter { byte: b'g' }, Some(2))),
            // incomplete unicode
            (b"\"\\u\"", JsonError::new(JsonErrorKind::UnexpectedEof, Some(3))),
            // // not enough bytes to form a low surrogate
            (b"\"\\uD83D\"", JsonError::new(JsonErrorKind::UnexpectedEof, Some(7))),
            // // high not followed by low
            (b"\"\\uD83Dabcdef\"", JsonError::new(JsonErrorKind::InvalidSurrogate, Some(1))),
            // // high followed by high
            (b"\"\\uD83D\\uD83D\"", JsonError::new(JsonErrorKind::InvalidSurrogate, Some(1))),
            // // low surrogate
            (b"\"\\uDC00\"", JsonError::new(JsonErrorKind::InvalidSurrogate, Some(1))),
            (b"\"abc", JsonError::new(JsonErrorKind::UnexpectedEof, Some(3))),
        ]
    }

    fn valid_literals() -> Vec<(&'static [u8], TokenizerToken)> {
        vec![
            (b"false", TokenizerToken::new(0, 5, TokenizerTokenType::Boolean)),
            (b"true", TokenizerToken::new(0, 4, TokenizerTokenType::Boolean)),
            (b"null", TokenizerToken::new(0, 4, TokenizerTokenType::Null)),
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
    fn empty_input() {
        let buffer = [];
        let mut tokenizer = Tokenizer::new(&buffer);
        let result = tokenizer.tokenize();
        let error = JsonError::new(JsonErrorKind::UnexpectedEof, Some(0));

        assert_eq!(result, Err(error));
    }

    #[test]
    fn skip_whitespaces() {
        // \t, \n, \r, ' '
        let buffer: [u8; 4] = [9, 10, 13, 32];
        let mut tokenizer = Tokenizer::new(&buffer);
        let result = tokenizer.tokenize();
        let error = JsonError::new(JsonErrorKind::UnexpectedEof, Some(3));

        assert_eq!(result, Err(error));
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

            assert_eq!(result, Err(error), "failed to tokenize: {buffer:?}");
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

            assert_eq!(result, Err(error), "failed to tokenize: {buffer:?}");
        }
    }

    #[test]
    fn test_valid_literals() {
        for (buffer, token) in valid_literals() {
            let mut tokenizer = Tokenizer::new(buffer);
            let tokens = tokenizer.tokenize().unwrap();

            assert_eq!(tokens.len(), 1);
            assert_eq!(tokens[0], token);
        }
    }

    #[test]
    fn test_invalid_literals() {
        for (buffer, error) in invalid_literals() {
            let mut tokenizer = Tokenizer::new(buffer);
            let result = tokenizer.tokenize();

            assert_eq!(result, Err(error), "failed to tokenize: {buffer:?}");
        }
    }

    #[test]
    fn test_unexpected_character() {
        // @
        let mut tokenizer = Tokenizer::new(&[64]);
        let result = tokenizer.tokenize();
        let error = JsonError::new(JsonErrorKind::UnexpectedCharacter { byte: b'@' }, Some(0));

        assert_eq!(result, Err(error));
    }
}