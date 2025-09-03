use crate::error::NumericError;

// We return u16 because the valid range for the Unicode sequences are 0x0000-0xFFFF(0-65535)
// 0-65535 is the range for u16
pub fn hex_to_u16(hex_bytes: &[u8; 4]) -> Result<u16, NumericError> {
    let mut val: u16 = 0;

    // Logic similar to base 10 seen on Leetcode problems
    // digit - b'a' returns a value in the [0,25] range but in base 16 a/A is 10 not 0, b/b is 11
    // + 10 brings into that range
    for (index, &digit) in hex_bytes.iter().enumerate() {
        let hex_val = match digit {
            b'0'..=b'9' => digit - b'0',
            b'a'..=b'f' => digit - b'a' + 10,
            b'A'..=b'F' => digit - b'A' + 10,
            _ => return Err(NumericError::InvalidHexDigit { digit, position: index })
        };
        val = val * 16 + hex_val as u16;
    }
    Ok(val)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn convert_to_u16() {
        // "12aB"
        // unwrap() uses the Debug trait, not Display
        assert_eq!(hex_to_u16(&[0x31, 0x32, 0x61, 0x42]).unwrap(), 0x12ab)
    }

    #[test]
    fn invalid_hex_digit() {
        // "12gB"
        assert!(hex_to_u16(&[0x31, 0x32, 0x67, 0x42]).is_err())
    }
}
