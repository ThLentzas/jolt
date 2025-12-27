use std::{error, fmt};
// Returns the number of bytes in an utf8 sequence based on the value of the leading byte
// https://en.wikipedia.org/wiki/UTF-8
//
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

pub(super) fn check_utf8_sequence(buffer: &[u8], pos: usize) -> Result<(), Utf8Error> {
    let mut i = pos;
    let first = buffer[i];
    let width = UTF8_CHAR_WIDTH[first as usize];

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
            let Some(second) = next(buffer, &mut i) else {
                return Err(Utf8Error { len: 2, pos });
            };
            // Clever way the Rust team checks for trailing bytes
            // A valid utf8 tail byte is in the range 0x80-0xBF (128-191)
            // If we cast that number to i8(-128, 127) it will overflow and Rust wraps around it
            // 128 - 256 = -128 in i8, 191 - 256 = -65 so for the value to be valid it has to be
            // in the range -128 to -65, any number greater than -64 is not
            if second as i8 >= -64 {
                return Err(Utf8Error { len: 2, pos });
            }
        }
        3 => {
            let Some(second) = next(buffer, &mut i) else {
                return Err(Utf8Error { len: 3, pos });
            };
            match (first, second) {
                (0xE0, 0xA0..=0xBF)
                | (0xE1..=0xEC, 0x80..=0xBF)
                | (0xED, 0x80..=0x9F)
                | (0xEE..=0xEF, 0x80..=0xBF) => {}
                _ => return Err(Utf8Error { len: 3, pos }),
            }
            let Some(third) = next(buffer, &mut i) else {
                return Err(Utf8Error { len: 3, pos });
            };
            // 1 tail byte
            if third as i8 >= -64 {
                return Err(Utf8Error { len: 3, pos });
            }
        }
        4 => {
            let Some(second) = next(buffer, &mut i) else {
                return Err(Utf8Error { len: 4, pos });
            };
            match (first, second) {
                (0xF0, 0x90..=0xBF) | (0xF1..=0xF3, 0x80..=0xBF) | (0xF4, 0x80..=0x8F) => {}
                _ => return Err(Utf8Error { len: 4, pos }),
            }
            // 2 tail bytes
            for _ in 0..2 {
                let Some(next) = next(buffer, &mut i) else {
                    return Err(Utf8Error { len: 4, pos });
                };
                if next as i8 >= -64 {
                    return Err(Utf8Error { len: 4, pos });
                }
            }
        }
        _ => return Err(Utf8Error { len: 1, pos }),
    }
    Ok(())
}

// returns the width(number of bytes) of a sequence
pub(super) fn utf8_char_width(byte: u8) -> usize {
    UTF8_CHAR_WIDTH[byte as usize] as usize
}

pub(super) fn is_bom_present(buffer: &[u8]) -> bool {
    buffer.len() >= 3 && (buffer[0], buffer[1], buffer[2]) == (0xEF, 0xBB, 0xBF)
}

pub(super) fn read_utf8_char(buffer: &[u8], pos: usize) -> char {
    let width = utf8_char_width(buffer[pos]);
    // SAFETY: always called on a valid sequence
    unsafe {
        str::from_utf8_unchecked(&buffer[pos..pos + width])
            .chars()
            .next()
            .unwrap()
    }
}

// only 1 kind of Utf8Error, InvalidByteSequence
#[derive(Debug, PartialEq)]
pub(super) struct Utf8Error {
    pub(super) len: u8,
    pub(super) pos: usize,
}

impl error::Error for Utf8Error {}

impl fmt::Display for Utf8Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "invalid {} byte utf-8 sequence from index {}",
            self.len, self.pos
        )
    }
}

fn next(bytes: &[u8], pos: &mut usize) -> Option<u8> {
    *pos += 1;
    if *pos >= bytes.len() {
        return None;
    }
    Some(bytes[*pos])
}

#[cfg(test)]
mod tests {
    use super::*;

    fn valid_sequences() -> Vec<&'static [u8]> {
        vec![
            &[0xC2, 0x90],
            &[0xE0, 0xBC, 0xB5],
            &[0xE8, 0xA0, 0xB1],
            &[0xED, 0x91, 0x87],
            &[0xEF, 0x94, 0xA2],
            &[0xF0, 0xBA, 0x9A, 0xB3],
            &[0xF2, 0xBE, 0x88, 0x93],
            &[0xF4, 0x83, 0x86, 0xB4],
        ]
    }

    fn invalid_sequences() -> Vec<(&'static [u8], Utf8Error)> {
        vec![
            // tail byte as start
            (&[0x82], Utf8Error { len: 1, pos: 0 }),
            // invalid start byte
            (&[0xFF], Utf8Error { len: 1, pos: 0 }),
            // incomplete 2-byte
            (&[0xC7], Utf8Error { len: 2, pos: 0 }),
            // invalid tail
            (&[0xC2, 0x7F], Utf8Error { len: 2, pos: 0 }),
            // incomplete 3-byte
            (&[0xE0], Utf8Error { len: 3, pos: 0 }),
            (&[0xE0, 0xA2], Utf8Error { len: 3, pos: 0 }),
            // E0 needs A0-BF as 2nd byte
            (&[0xE0, 0xC1, 0x82], Utf8Error { len: 3, pos: 0 }),
            // E1..=EC needs 0x80..=0xBF as 2nd byte
            (&[0xE8, 0x72, 0x89], Utf8Error { len: 3, pos: 0 }),
            // E0 needs 80-9F as 2nd byte
            (&[0xED, 0xB1, 0x83], Utf8Error { len: 3, pos: 0 }),
            // EE..=EF needs 0x80..=0xBF as 2nd byte
            (&[0xEE, 0x63, 0x94], Utf8Error { len: 3, pos: 0 }),
            // 3rd byte must always be tail no matter the first two, 0x80..=0xBF
            (&[0xE0, 0xA7, 0xD1], Utf8Error { len: 3, pos: 0 }),
            // incomplete 4-byte
            (&[0xF0], Utf8Error { len: 4, pos: 0 }),
            (&[0xF2, 0x99], Utf8Error { len: 4, pos: 0 }),
            (&[0xF4, 0x81, 0x82], Utf8Error { len: 4, pos: 0 }),
            // F0 needs 90-BF as 2nd byte
            (&[0xF0, 0x41, 0xBD, 0xAF], Utf8Error { len: 4, pos: 0 }),
            // 0xF1..=0xF3 needs 0x80..=0xBF as 2nd byte
            (&[0xF2, 0xD2, 0x92, 0x98], Utf8Error { len: 4, pos: 0 }),
            // 0xF4, needs 0x80..=0x8F as 2nd byte
            (&[0xF4, 0xAB, 0xB3, 0xB5], Utf8Error { len: 4, pos: 0 }),
            // any 4-byte sequence needs 2 trail bytes
            // out of range 3rd byte
            (&[0xF3, 0x97, 0x52, 0x8D], Utf8Error { len: 4, pos: 0 }),
            // out of range 4th byte
            (&[0xF3, 0x97, 0xBB, 0x022], Utf8Error { len: 4, pos: 0 }),
        ]
    }

    #[test]
    fn next_out_of_bounds() {
        let buffer = &[72, 101, 108, 108, 111];
        let mut pos = 4;

        assert!(next(buffer, &mut pos).is_none());
    }

    #[test]
    fn ignore_bom() {
        let buffer = &[0xEF, 0xBB, 0xBF];
        assert!(is_bom_present(buffer));
    }

    #[test]
    fn test_valid_sequences() {
        let pos = 0;
        for buffer in valid_sequences() {
            assert!(
                check_utf8_sequence(buffer, pos).is_ok(),
                "failed to validate: {buffer:?}"
            )
        }
    }

    #[test]
    fn test_invalid_sequences() {
        let pos = 0;
        for (buffer, error) in invalid_sequences() {
            let result = check_utf8_sequence(buffer, pos);
            assert_eq!(result, Err(error), "failed to validate: {buffer:?}")
        }
    }
}
