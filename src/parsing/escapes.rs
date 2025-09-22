use crate::parsing::error::MalformedStringError;
use crate::parsing::{number, utf8};

pub(super) fn check_escape_character(buffer: &[u8], index: &mut usize) -> Result<(), MalformedStringError> {
    let current = buffer[*index];
    let len = buffer.len();
    
    if current == b'\\' {
        *index += 1;
        if *index >= len {
            return Err(MalformedStringError::InvalidEscapeSequence { pos: *index - 1 });
        }

        let next = buffer[*index];
        if !matches!(next, b'\\' | b'"' | b'/' | b'b' | b'f' | b'n' | b'r' | b't' | b'u') {
            return Err(MalformedStringError::InvalidEscapeSequence { pos: *index });
        }
        if next == b'u' {
            check_unicode_escape(buffer, index)?;
        }
    }
    Ok(())
}

pub(super) fn map_escape_character(buffer: &[u8], index: &mut usize) -> char {
    match buffer[*index] {
        b'\\' => '\\',
        b'"' => '"',
        b'/' => '/',
        b'b' => '\x08',
        b'f' => '\x0C',
        b'n' => '\n',
        b'r' => '\r',
        b't' => '\t',
        b'u' => decode_unicode(buffer, index),
        _ => unreachable!("backslash not followed by an escape character"),
    }
}

fn check_unicode_escape(buffer: &[u8], index: &mut usize) -> Result<(), MalformedStringError> {
    // Need 4 hex digits
    if *index + 4 >= buffer.len() {
        return Err(MalformedStringError::InvalidEscapeSequence { pos: *index });
    }

    // self.pos is at the first digit of the hex sequence
    *index += 1;
    let val = match number::hex_to_u16(&buffer[*index..*index + 4]) {
        Ok(hex) => hex,
        Err(_) => return Err(MalformedStringError::InvalidEscapeSequence { pos: *index - 2 })
    };
    // move past the hex digits
    *index += 3;
    if utf8::is_surrogate(val) {
        utf8::validate_surrogate(index, buffer, val)?;
    }

    Ok(())
}

fn decode_unicode(buffer: &[u8], index: &mut usize) -> char {
    *index += 1;
    let code_unit = number::hex_to_u16(&buffer[*index..*index + 4]).unwrap();
    let ch: char;

    if utf8::is_surrogate(code_unit) {
        let high = code_unit;
        *index += 6; // 4 hex digits + \u
        let low = number::hex_to_u16(&buffer[*index..*index + 4]).unwrap();
        let code_point = utf8::decode_surrogate_pair(high as u32, low as u32);
        ch = char::from_u32(code_point).unwrap();
    } else {
        ch = char::from_u32(code_unit as u32).unwrap();
    }
    *index += 3; // Move to the last hex digit
    ch
}