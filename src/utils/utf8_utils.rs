use crate::error::{Utf8Error};
use crate::utils::{numeric_utils};

// Returns the number of bytes in an utf8 sequence based on the value of the leading byte
// https://en.wikipedia.org/wiki/UTF-8

// Index 0-127 (0x00-0x7F): ASCII = 1 byte
// Index 128-191 (0x80-0xBF): Continuation bytes = 0 (invalid as first byte)
// Index 192-223 (0xC0-0xDF): 2-byte sequence starters = 2, C0-C1 are invalid (0)
// Index 224-239 (0xE0-0xEF): 3-byte sequence starters = 3
// Index 240-244 (0xF0-0xF4): 4-byte sequence starters = 4
// Index 245-255 (0xF5-0xFF): Invalid = 0
const UTF8_CHAR_WIDTH: [u8; 256] = [
    // 1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 0
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 1
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 2
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 3
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 4
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 5
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 6
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 7
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 8
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 9
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // A
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // B
    0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // C
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // D
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, // E
    4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // F
];

pub fn validate_utf8_sequence(index: &mut usize, buffer: &[u8]) -> Result<(), Utf8Error> {
    let first = buffer[*index];
    let width = UTF8_CHAR_WIDTH[first as usize];
    let start = *index;

    // https://datatracker.ietf.org/doc/html/rfc3629
    //
    //  UTF8-1      = %x00-7F
    //  UTF8-2      = %xC2-DF UTF8-tail
    //  UTF8-3      = %xE0 %xA0-BF UTF8-tail / %xE1-EC 2( UTF8-tail ) /
    //                %xED %x80-9F UTF8-tail / %xEE-EF 2( UTF8-tail )
    //  UTF8-4      = %xF0 %x90-BF 2( UTF8-tail ) / %xF1-F3 3( UTF8-tail ) /
    //                %xF4 %x80-8F 2( UTF8-tail )
    //  UTF8-tail   = %x80-BF
    //
    //  The above reads as:
    //
    //      1 byte: ASCII range
    //      2 byte sequence: 1st byte in C2-DF range and a tail byte
    //      3 byte sequence: 1st byte E0, 2nd byte in range of A0-BF and a tail byte | 1st byte in
    //      the E1-EC range and 2 tail bytes | 1st byte ED, 2nd byte in 80-9F range and a tail byte |
    //      1st byte in the EE-EF range and 2 tail bytes(4 different cases)
    //      Same logic applies for 4 byte sequence
    match width {
        2 => {
            let Some(second) = next(index, buffer) else {
              return Err(Utf8Error::InvalidByteSequence { len: 2, pos: start });
            };
            // Clever way the Rust team checks for trailing bytes
            // A valid utf8 tail byte is in the range 0x80-0xBF (128-191)
            // If we cast that number to i8(-128, 127) it will overflow and Rust wraps around it
            // 128 - 256 = -128 in i8, 191 - 256 = -95 so for the value to be valid it has to be
            // in the range -128 to -65, any number greater than -64 is not
            if second as i8 >= -64 {
                return Err(Utf8Error::InvalidByteSequence { len: 2, pos: start });
            }
        }
        3 => {
            let Some(second) = next(index, buffer) else {
                return Err(Utf8Error::InvalidByteSequence { len: 3, pos: start });
            };

            match (first, second) {
                (0xE0, 0xA0..=0xBF)
                | (0xE1..=0xEC, 0x80..=0xBF)
                | (0xED, 0x80..=0x9F)
                | (0xEE..=0xEF, 0x80..=0xBF) => {}
                _ => return Err(Utf8Error::InvalidByteSequence { len: 3, pos: start })
            }
            let Some(third) = next(index, buffer) else {
                return Err(Utf8Error::InvalidByteSequence { len: 3, pos: start });
            };
            if third as i8 >= -64 {
                return Err(Utf8Error::InvalidByteSequence { len: 3, pos: start });
            }
        }
        4 => {
            let Some(second) = next(index, buffer) else {
                return Err(Utf8Error::InvalidByteSequence { len: 4, pos: start });
            };

            match (first, second) {
                (0xF0, 0x90..=0xBF) | (0xF1..=0xF3, 0x80..=0xBF) | (0xF4, 0x80..=0x8F) => {}
                _ => return Err(Utf8Error::InvalidByteSequence { len: 4, pos: start }),
            }

            // 2 trailing bytes
            for _ in 0..2 {
                let Some(next) = next(index, buffer) else {
                    return Err(Utf8Error::InvalidByteSequence { len: 4, pos: start });
                };
                if next as i8 >= -64 {
                    return Err(Utf8Error::InvalidByteSequence { len: 4, pos: start });
                }
            }
        }
        _ => return Err(Utf8Error::InvalidByteSequence { len: 1, pos: start }),
    }
    Ok(())
}

// https://en.wikipedia.org/wiki/UTF-16#U+D800_to_U+DFFF_(surrogates)
// Read: To decode...
pub fn decode_surrogate_pair(high: u16, low: u16) -> u32 {
    (high as u32 - 0xD800) * 0x400 + low as u32 - 0xDC00 + 0x10000
}

pub fn is_surrogate(val: u16) -> bool {
    matches!(val, 0xD800..=0xDFFF)
}

pub fn is_high_surrogate(val: u16) -> bool { matches!(val, 0xD800..=0xDBFF) }

pub fn is_low_surrogate(val: u16) -> bool { matches!(val, 0xDC00..=0xDFFF) }

pub fn validate_surrogate(index: &mut usize, buffer: &[u8], hex_sequence: u16) -> Result<(), Utf8Error> {
    let len = buffer.len();
    // *index - 5 is the index at the start of the Unicode sequence
    let start = *index - 5;

    if is_high_surrogate(hex_sequence) {
        if *index + 6 >= len {
            return Err(Utf8Error::InvalidSurrogate { pos: start });
        }

        // move to the next value after the last digit of the high surrogate sequence
        *index += 1;
        match (buffer[*index], buffer[*index + 1]) {
            (b'\\', b'u') => {
                *index += 2;
            }
            _ => return Err(Utf8Error::InvalidSurrogate { pos: start })
        };

        // safe to call, it will never be out of bounds
        let next = match numeric_utils::hex_to_u16(&buffer[*index..*index + 4]) {
            Ok(low) => low,
            Err(_) => return Err(Utf8Error::InvalidSurrogate { pos: start })
        };

        *index += 3;
        if !is_low_surrogate(next) {
            return Err(Utf8Error::InvalidSurrogate { pos: start });
        }
    } else {
        // surrogate pairs do not start with low surrogate, it's always high-low
        return Err(Utf8Error::InvalidSurrogate { pos: start });
    }
    Ok(())
}

// returns the width(number of bytes) of an utf8-sequence
pub fn utf8_char_width(byte: u8) -> usize {
    UTF8_CHAR_WIDTH[byte as usize] as usize
}

// https://www.rfc-editor.org/rfc/rfc8259#section-8.1
pub fn ignore_bom_if_present(index: &mut usize, buffer: &[u8]) {
    if buffer.len() < 3 {
        return;
    }

    match(buffer[*index], buffer[*index + 1], buffer[*index + 2]) {
        (0xEF, 0xBB, 0xBF) => *index += 3,
        _ => ()
    }
}

fn next(index: &mut usize, bytes: &[u8]) -> Option<u8> {
    *index += 1;

    if *index >= bytes.len() {
        return None;
    }
    Some(bytes[*index])
}

// toDo: complete good cases for 4 byte, group them in 1 method as valid sequences and create test cases for invalid
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next_out_of_bounds() {
        let bytes = &[72, 101, 108, 108, 111];
        let mut pos = 4;

        assert!(next(&mut pos, bytes).is_none());
    }

    #[test]
    fn ignore_bom() {
        let bytes = &[0xEF, 0xBB, 0xBF];
        let mut pos = 0;

        ignore_bom_if_present(&mut pos, bytes);

        assert_eq!(pos, 3);
    }

    #[test]
    fn valid_2_byte_sequence() {
        let bytes = &[0xC2, 0x90];
        let mut pos = 0;

        assert!(validate_utf8_sequence(&mut pos, bytes).is_ok())
    }

    #[test]
    fn invalid_2_byte_sequence() {
        // no trailing byte after C2
        let bytes = &[0xC2, 0x3F];
        let mut pos = 0;

        assert!(validate_utf8_sequence(&mut pos, bytes).is_err())
    }

    #[test]
    fn valid_3_byte_sequence() {
        // 1st: E0, 2nd: A0...BF, 3rd: 80...BF (trail)
        let bytes = &[0xE0, 0xBC, 0xB5];
        let mut pos = 0;

        assert!(validate_utf8_sequence(&mut pos, bytes).is_ok())
    }

    #[test]
    fn valid_3_byte_sequence_1() {
        // 1st: E1...EC, 2nd: 80...BF, 3rd: 80...BF (2 trailing)
        let bytes = &[0xE8, 0xA0, 0xB1];
        let mut pos = 0;

        assert!(validate_utf8_sequence(&mut pos, bytes).is_ok())
    }

    #[test]
    fn valid_3_byte_sequence_2() {
        // 1st: ED, 2nd: 80...9F, 3rd: 80...BF (trail)
        let bytes = &[0xED, 0x91, 0x87];
        let mut pos = 0;

        assert!(validate_utf8_sequence(&mut pos, bytes).is_ok())
    }

    #[test]
    fn valid_3_byte_sequence_3() {
        // 1st: ED, 2nd: 80...9F, 3rd: 80...BF (trail)
        let bytes = &[0xED, 0x91, 0x87];
        let mut pos = 0;

        assert!(validate_utf8_sequence(&mut pos, bytes).is_ok())
    }
}
