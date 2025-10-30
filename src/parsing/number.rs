use std::{error, fmt};
#[cfg(feature = "big_decimal")]
use bigdecimal::BigDecimal;
#[cfg(feature = "big_decimal")]
use bigdecimal::num_bigint::BigInt;

#[derive(Debug, PartialEq)]
pub struct Number (NumberKind);

// The way BigDecimal works is by storing every digit of the number as BigInt, (unscaled value, 
// an arbitrary-precision integer) and also the number of decimal places as int(scale)
// 
//  The number 123.45 is stored as:
//      Unscaled value: 12345
//      Scale: 2
//      Precision: 5 (number of digits)
//
// The actual value is calculated as: unscaled value * 10^(-scale)
//
// To store the unscaled value as an arbitrary precision integer BigInt uses a Vec<u32/u64> under the hood

// In base 10: 1234 = 1 * 10^3 + 2 * 10^2 + 3 * 10^1 + 4 * 10^0
// In base 2^32: each u32 is a coefficient for powers of 2^32

// 2^32 = 4,294,967,296
// 10,000,000,000,000 / 4,294,967,296 = 2328 remainder 1,316,134,912

// So it's stored as:
// vec![1_316_134_912, 2328]
// Meaning: 1_316_134_912 * (2^32)^0 + 2328 * (2^32)^1
//
// The least significant "digit" (the remainder) goes in index 0, and more significant digits go in
// higher indices. This is called "little-endian" order.
//
// 50,000,000,000,000,000,000 (50 quintillion)
//
// Step 1: Divide by 2^32
//  50,000,000,000,000,000,000 ÷ 4,294,967,296 = 11,641,532,182
//  Remainder: 1,695,547,392
//
// Step 2: That quotient is still > 2^32, so divide again
//  11,641,532,182 / 4,294,967,296 = 2
//  Remainder: 3,051,597,590
//
//  Step 3: Final quotient is 2 (fits in u32)
//
//  So our Vec<u32> is: vec![1_695_547_392, 3_051_597_590, 2]
//  1,695,547,392 * (2^32)^0 + 3,051,597,590 * (2^32)^1 + 2 * (2^32)^2 = 50,000,000,000,000,000,000
//
// In parser.rs for the test valid_array() the number 340282366920938463463374607431768211456
// is vec![0, 0, 1] for u64 and vec![0, 0, 0, 0, 1]
//
// 10^40
//
// With u32:
// vec![1661992960, 1808227885, 3721402093, 4028081056, 542101086]
//
//With u64:
// vec![7766279631452241920, 542101086035307936, 2]
#[derive(Debug, PartialEq)]
enum NumberKind {
    I64(i64),
    U64(u64),
    F64(f64),
    #[cfg(feature = "big_decimal")]
    BigInt(BigInt),
    #[cfg(feature = "big_decimal")]
    BigDecimal(BigDecimal),
}

impl Number {
    // for i64, u64, f64 self.0 will copy v because all those types implement Copy; BigDecimal/Int does
    // not, so we need to borrow it and clone it, similar to &self.kind in error.rs
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

    #[cfg(feature = "big_decimal")]
    pub fn as_big_decimal(&self) -> Option<BigDecimal> {
        match &self.0 {
            NumberKind::BigDecimal(v) => Some(v.clone()),
            _ => None,
        }
    }

    #[cfg(feature = "big_decimal")]
    pub fn as_big_int(&self) -> Option<BigInt> {
        match &self.0 {
            NumberKind::BigInt(v) => Some(v.clone()),
            _ => None,
        }
    }
}

impl From<i64> for Number {
    fn from(val: i64) -> Self {
        Number(NumberKind::I64(val))
    }
}

impl From<u64> for Number {
    fn from(val: u64) -> Self {
        Number(NumberKind::U64(val))
    }
}

impl From<f64> for Number {
    fn from(val: f64) -> Self {
        Number(NumberKind::F64(val))
    }
}

#[cfg(feature = "big_decimal")]
impl From<BigDecimal> for Number {
    fn from(val: BigDecimal) -> Self {
        Number(NumberKind::BigDecimal(val))
    }
}

#[cfg(feature = "big_decimal")]
impl From<BigInt> for Number {
    fn from(val: BigInt) -> Self {
        Number(NumberKind::BigInt(val))
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

// from_utf8() calls in the is_out_of_range() functions are always called in valid numbers
pub(super) fn is_out_of_range_u64(buffer: &[u8]) -> bool {
    let s = str::from_utf8(buffer).unwrap();
    s.parse::<u64>().is_err()
}

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
        match self {
            // self is auto deref based on rust ergonomics; for digit and pos we have a reference as well,
            // but we don't need to deref when we pass into the write!()
            Self::InvalidHexDigit { digit, pos }  => write!(f, "Invalid hex digit (0x{:02X}) at index {}", digit, pos)
        }
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
