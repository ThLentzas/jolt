use std::{error, fmt};

#[derive(Debug, PartialEq)]
pub struct Number (NumberKind);

#[derive(Debug, PartialEq)]
enum NumberKind {
    I64(i64),
    U64(u64),
    F64(f64),
}

// toDo: implement BigDecimal as feature
impl Number {
    pub fn from_i64(val: i64) -> Self {
        Number(NumberKind::I64(val))
    }

    pub fn from_u64(val: u64) -> Self {
        Number(NumberKind::U64(val))
    }

    pub fn from_f64(val: f64) -> Self {
        Number(NumberKind::F64(val))
    }

    pub fn as_i64(&self) -> Option<i64> {
        match self.0 {
            NumberKind::I64(v) => Some(v),
            _ => None,
        }
    }

    pub fn as_u64(&self) -> Option<u64> {
        match self.0 {
            NumberKind::U64(v) => Some(v),
            _ => None,
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        match self.0 {
            NumberKind::F64(v) => Some(v),
            _ => None,
        }
    }
}

// We return u16 because the valid range for the Unicode sequences are 0x0000-0xFFFF(0-65535)
// 0-65535 is the range for u16
pub(super) fn hex_to_u16(hex_bytes: &[u8]) -> Result<u16, NumericError> {
    let mut val: u16 = 0;

    // Logic similar to base 10 seen on Leetcode problems
    // digit - b'a' returns a value in the [0,25] range but in base 16 a/A is 10 not 0, b/B is 11
    // + 10 brings into that range
    for (index, &byte) in hex_bytes.iter().enumerate() {
        let hex_val = match byte {
            b'0'..=b'9' => byte - b'0',
            b'a'..=b'f' => byte - b'a' + 10,
            b'A'..=b'F' => byte - b'A' + 10,
            _ => return Err(NumericError::InvalidHexDigit { digit: byte, pos: index })
        };
        val = val * 16 + hex_val as u16;
    }
    Ok(val)
}

pub(super) fn is_out_of_range_u64(buffer: &[u8]) -> bool {
    let s = str::from_utf8(buffer).unwrap();
    s.parse::<u64>().is_err()
    // review underflow
}

// toDo: review loops and patterns in loops, check default values for primitives
pub(super) fn is_out_of_range_i64(buffer: &[u8]) -> bool {
    let s = str::from_utf8(buffer).unwrap();
    s.parse::<i64>().is_err()
}

// Overflow: Parsing a number that exceeds f64::MAX/MIN will cause parsing::<f64>() to return inf/-inf
// Based on that handling normal numbers is straightforward. If the result of parsing is inf/-inf since
// infinity and nan are not supported in the json rfc we can safely say that the number is out of range
//
// If we actually had to parsing parts of the number and check for over/under flow look at dec2flt() at src/num/dec2flt/mod.rs
pub(super) fn is_out_of_range_f64(buffer: &[u8]) -> bool {
    let s = str::from_utf8(buffer).unwrap();
    let val = s.parse::<f64>().unwrap();

    val.is_infinite()
}

#[derive(Debug, PartialEq)]
pub(super) enum NumericError {
    InvalidHexDigit { digit: u8, pos: usize }
}

impl fmt::Display for NumericError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl error::Error for NumericError {}

#[cfg(test)]
mod tests {
    use super::*;

    fn overflow_f64() -> Vec<&'static [u8]> {
        vec![
            b"1e309",
            b"-1e309",
        ]
    }

    fn overflow_i64() -> Vec<&'static [u8]> {
        vec![
            b"9223372036854775808", // MAX + 1
            b"-9223372036854775809", // MIN - 1
        ]
    }

    #[test]
    fn out_of_range_f64() {
        for num in overflow_f64() {
            assert!(is_out_of_range_f64(num));
        }
    }

    #[test]
    fn out_of_range_i64() {
        for num in overflow_i64() {
            assert!(is_out_of_range_i64(num));
        }
    }

    #[test]
    fn out_of_range_u64() {
        let num = b"18446744073709551616"; // MAX + 1
        assert!(is_out_of_range_u64(num));

    }

    #[test]
    fn convert_to_u16() {
        // "12aB"
        // unwrap() uses the Debug trait, not Display
        assert_eq!(hex_to_u16(&[0x31, 0x32, 0x61, 0x42]).unwrap(), 0x12ab);
    }

    #[test]
    fn invalid_hex_digit() {
        // "12gB"
        let result = hex_to_u16(&[0x31, 0x32, 0x67, 0x42]);
        if let Err(e) = result {
            assert_eq!(e, NumericError::InvalidHexDigit { digit: 0x67, pos: 2});
        }
    }
}
