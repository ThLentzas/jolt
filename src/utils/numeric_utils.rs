use crate::error::{NumericError};

#[derive(Debug, PartialEq)]
pub struct Number (NumberKind);

#[derive(Debug, PartialEq)]
enum NumberKind {
    I64(i64),
    U64(u64),
    F64(f64),
}

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
}

// We return u16 because the valid range for the Unicode sequences are 0x0000-0xFFFF(0-65535)
// 0-65535 is the range for u16
pub fn hex_to_u16(hex_bytes: &[u8]) -> Result<u16, NumericError> {
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

pub fn is_out_of_range_u64(buffer: &[u8]) -> bool {
    let s = str::from_utf8(buffer).unwrap();
    s.parse::<u64>().is_err()
    // review underflow
}

// toDo: review loops and patterns in loops, check default values for primitives
pub fn is_out_of_range_i64(buffer: &[u8]) -> bool {
    let s = str::from_utf8(buffer).unwrap();
    s.parse::<i64>().is_err()
}

// Overflow: Parsing a number that exceeds f64::MAX/MIN will cause parse::<f64>() to return inf/-inf
// Based on that handling normal numbers is straightforward. If the result of parse is inf/-inf since
// infinity and nan are not supported in the json rfc we can safely say that the number is out of range
// Underflow: This is the tricky part. Calling parse::<f64>() on a number that is outside the subnormal
// range will return 0.0 due to a round down. This causes problems because we need to distinguish
// between 0.0 returned by an underflow or because we actually parsed 0.(any number of 0s)
//
// We can write any float as: mantissa * 10^exponent
//
// If the number contains a scientific notation, s.rfind(|c| c == 'e' || c == 'E') will return the
// index. At this point, the number is always valid so if the number has a scientific notation
// it will be the only one(either 'e' or 'E')
//
// s = &s[..pos]; creates the mantissa. If we want to be strict we would have to normalize it in the
// range of [1, 10) but this is not needed in this case.
// If the mantissa before parsing was 0.(any number of 0s) it would always result in 0.0 after parsing
// because 0 * 10^anything = 0, always zero, no matter the exponent. We can be sure that the return
// value of s.parse::<f64>() is not an overflow.
//
// If the mantissa though has any non-zero digit, mantissa * 10^exponent will never result in 0.0, it
// can get very close to zero but never zero, which means that if the call to s.parse::<f64>() returned
// 0.0 it must have been because it overflowed.
//
// What if the number does not have a scientific notation?
//
// We could try to write the number in the form of mantissa * 10^exponent
//      0.0001 -> 1 * 10^-3
//      127.8 -> 1.27 * 10^2
// Search for the 1st non-zero digit and adjust the exponent
// If none is found it means the mantissa would have been zero, and it falls into this case 0 * 10^anything = 0
// otherwise non-zero mantissa and zero returned value from s.parse::<f64>() means underflow
//
// Negative numbers use the same ranges, the sign is stored separately. The sign bit is independent
// of the magnitude representation. Subnormal range also applies to both positive and negative.
// We don't need special handling for negative numbers, they overflow/underflow at the same
// magnitudes as positive numbers, just with opposite sign
//
// If we actually had to parse parts of the number and check for over/under flow look at dec2flt() at src/num/dec2flt/mod.rs
pub fn is_out_of_range_f64(buffer: &[u8]) -> bool {
    let mut s = str::from_utf8(buffer).unwrap();
    let val = s.parse::<f64>().unwrap();

    if val.is_infinite() {
        return true;
    }

    if let Some(pos) = s.rfind(|c| c == 'e' || c == 'E') {
        s = &s[..pos];
    }
    if val == 0.0 {
        return s.contains(|c: char| c != '0' && c.is_ascii_digit());
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn convert_to_u16() {
        // "12aB"
        // unwrap() uses the Debug trait, not Display
        assert_eq!(hex_to_u16(&[0x31, 0x32, 0x61, 0x42]).unwrap(), 0x12ab);
    }

    #[test]
    fn invalid_hex_digit() {
        // "12gB"
        assert!(hex_to_u16(&[0x31, 0x32, 0x67, 0x42]).is_err());
    }
}
