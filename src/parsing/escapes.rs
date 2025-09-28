use std::{error, fmt};
use crate::parsing::number;

// maybe we could use composition for ErrorKind?
#[derive(Debug, PartialEq)]
pub(super) enum EscapeError {
    UnknownEscapedCharacter { byte: u8, pos: usize },
    UnexpectedEof { pos: usize },
    InvalidUnicodeSequence { digit: u8, pos: usize },
    InvalidSurrogate { pos: usize }
}

impl error::Error for EscapeError {}

impl fmt::Display for EscapeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnknownEscapedCharacter { byte, pos } => {
                match byte {
                    // can be a byte from a utf8 sequence or a character with no text representation
                    b if b.is_ascii_graphic() => write!(f, "unknown escape character {} at index {}", *b as char, pos),
                    _ => write!(f, "unknown escape character (0x{:02X}) at index {}", byte, pos),
                }
            }
            Self::UnexpectedEof{ pos } => write!(f, "unexpected end of input at index {}", pos),
            Self::InvalidUnicodeSequence { digit, pos } => write!(f, "invalid hex digit '{}' at index {}", *digit as char, pos),
            Self::InvalidSurrogate { pos } => write!(f, "invalid surrogate pair at index {}", pos)
        }
    }
}

pub(super) fn check_escape_character(buffer: &[u8], pos: usize) -> Result<(), EscapeError> {
    let len = buffer.len();
    let mut i = pos;

    i += 1;
    if i >= len {
        return Err(EscapeError::UnexpectedEof { pos: i - 1 });
    }

    let next = buffer[i];
    if !matches!(next, b'\\' | b'"' | b'/' | b'b' | b'f' | b'n' | b'r' | b't' | b'u') {
        return Err(EscapeError::UnknownEscapedCharacter { byte: next, pos: i });
    }
    if next == b'u' {
        check_unicode_escape(buffer, i)?;
    }

    Ok(())
}

pub(super) fn map_escape_character(buffer: &[u8], pos: usize) -> char {
    match buffer[pos + 1] {
        b'\\' => '\\',
        b'"' => '"',
        b'/' => '/',
        b'b' => '\x08',
        b'f' => '\x0C',
        b'n' => '\n',
        b'r' => '\r',
        b't' => '\t',
        b'u' => decode_unicode(buffer, pos),
        _ => unreachable!("backslash not followed by an escape character"),
    }
}

// returns the length of an escape sequence, it is always called on valid escape sequences
pub(super) fn len(buffer: &[u8], pos: usize) -> usize {
    let mut i = pos + 1;
    if matches!(buffer[i], b'\\' | b'"' | b'/' | b'b' | b'f' | b'n' | b'r' | b't') {
        return 2;
    }

    i += 1;
    let val = number::hex_to_u16(&buffer[i..i + 4]).unwrap();
    if is_surrogate(val) { 12 } else { 6 }
}

fn check_unicode_escape(buffer: &[u8], pos: usize) -> Result<(), EscapeError> {
    // pos is at 'u', need 4 hex digits
    if pos + 4 >= buffer.len() {
        return Err(EscapeError::UnexpectedEof { pos: buffer.len() - 1 });
    }

    let mut i = pos;
    i += 1; // move to the 1st hex digit
    let val = number::hex_to_u16(&buffer[i..i + 4])?;
    i += 3; // move past the hex digits
    if is_surrogate(val) {
        validate_surrogate(buffer, i, val)?;
    }

    Ok(())
}

fn decode_unicode(buffer: &[u8], pos: usize) -> char {
    let mut i = pos;
    i += 2; // skip '\u'
    let code_unit = number::hex_to_u16(&buffer[i..i + 4]).unwrap();
    let ch: char;

    if is_surrogate(code_unit) {
        let high = code_unit;
        i += 6; // 4 hex digits + \u
        let low = number::hex_to_u16(&buffer[i..i + 4]).unwrap();
        let code_point = decode_surrogate_pair(high as u32, low as u32);
        ch = char::from_u32(code_point).unwrap();
    } else {
        ch = char::from_u32(code_unit as u32).unwrap();
    }

    ch
}

fn is_surrogate(val: u16) -> bool {
    matches!(val, 0xD800..=0xDFFF)
}

fn is_high_surrogate(val: u16) -> bool { matches!(val, 0xD800..=0xDBFF) }

fn is_low_surrogate(val: u16) -> bool { matches!(val, 0xDC00..=0xDFFF) }

fn validate_surrogate(buffer: &[u8], pos: usize, hex_sequence: u16) -> Result<(), EscapeError> {
    let len = buffer.len();
    let start = pos - 5; // pos - 5 is the index at the start of the surrogate

    if is_high_surrogate(hex_sequence) {
        if pos + 6 >= len {
            return Err(EscapeError::UnexpectedEof { pos: buffer.len() - 1 });
        }

        let mut i = pos;
        i += 1;  // move to the next value after the last digit of the high surrogate
        match (buffer[i], buffer[i + 1]) {
            (b'\\', b'u') => {
                i += 2;
            }
            _ => return Err(EscapeError::InvalidSurrogate { pos: start })
        };

        // safe to call, it will never be out of bounds
        let next = number::hex_to_u16(&buffer[i..i + 4])?;
        if !is_low_surrogate(next) {
            return Err(EscapeError::InvalidSurrogate { pos: start });
        }
    } else {
        // surrogate pairs do not start with low surrogate, it's always high-low
        return Err(EscapeError::InvalidSurrogate { pos: start });
    }
    Ok(())
}

// https://en.wikipedia.org/wiki/UTF-16#U+D800_to_U+DFFF_(surrogates)
// Read: To decode...
fn decode_surrogate_pair(high: u32, low: u32) -> u32 {
    (high - 0xD800) * 0x400 + low - 0xDC00 + 0x10000
}