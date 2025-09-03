use crate::error::{Utf8Error};

// Returns the number of bytes in an utf8 sequence based on the value of the leading byte

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

pub fn validate_utf8_sequence(position: &mut usize, bytes: &[u8]) -> Result<(), Utf8Error> {
    let first = bytes[*position];
    let width = UTF8_CHAR_WIDTH[first as usize];
    let tmp = *position;

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
            // Clever way the Rust team checks for trailing bytes
            // A valid utf8 tail byte is in the range 0x80-0xBF (128-191)
            // If we cast that number to i8(-128, 127) it will overflow and Rust wraps around it
            // 128 - 256 = -128 in i8, 191 - 256 = -95 so for the value to be valid it has to be
            // in the range -128 to -65 so any number greater than -64 is not
            if next(position, bytes)? as i8 >= -64 {
                return Err(Utf8Error::InvalidSequence {len: 2, position: tmp });
            }
        }
        3 => {
            match (first, next(position, bytes)?) {
                (0xE0, 0xA0..=0xBF)
                | (0xE1..=0xEC, 0x80..=0xBF)
                | (0xED, 0x80..=0x9F)
                | (0xEE..=0xEF, 0x80..=0xBF) => {}
                _ => return Err(Utf8Error::InvalidSequence { len: 3, position: tmp })
            }
            if next(position, bytes)? as i8 >= -64 {
                return Err(Utf8Error::InvalidSequence { len: 3, position: tmp });
            }
        }
        4 => {
            match (first, next(position, bytes)?) {
                (0xF0, 0x90..=0xBF) | (0xF1..=0xF3, 0x80..=0xBF) | (0xF4, 0x80..=0x8F) => {}
                _ => return Err(Utf8Error::InvalidSequence { len: 4, position: tmp }),
            }
            // 2 trailing bytes
            if next(position, bytes)? as i8 >= -64 {
                return Err(Utf8Error::InvalidSequence { len: 3, position: tmp });
            }
            if next(position, bytes)? as i8 >= -64 {
                return Err(Utf8Error::InvalidSequence { len: 3, position: tmp });
            }
        }
        _ => return Err(Utf8Error::InvalidSequence { len: 4, position: tmp } ),
    }
    Ok(())
}

pub fn is_surrogate(val: u16) -> bool {
    (0xD800..=0xDFFF).contains(&val)
}

pub fn is_high_surrogate(val: u16) -> bool {
    (0xD800..=0xDBFF).contains(&val)
}

pub fn is_low_surrogate(val: u16) -> bool {
    (0xDC00..=0xDFFF).contains(&val)
}

fn next(position: &mut usize, bytes: &[u8]) -> Result<u8, Utf8Error> {
    *position += 1;

    if *position >= bytes.len() {
        return Err(Utf8Error::IncompleteSequence  { position: *position - 1 });
    }
    Ok(bytes[*position])
}

// https://www.rfc-editor.org/rfc/rfc8259#section-8.1
pub fn ignore_bom_if_present(position: &mut usize, bytes: &[u8]) {
    if bytes.len() < 3 {
        return;
    }

    match(bytes[*position], bytes[*position + 1], bytes[*position + 2]) {
        (0xEF, 0xBB, 0xBF) => *position += 3,
        _ => ()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next_out_of_bounds() {
        let bytes = &[72, 101, 108, 108, 111];
        let mut pos = 4;

        assert!(next(&mut pos, bytes).is_err());
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
