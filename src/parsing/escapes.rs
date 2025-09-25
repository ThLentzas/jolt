use crate::parsing::error::MalformedStringError;
use crate::parsing::{number, utf8};

pub(super) fn check_escape_character(buffer: &[u8], index: usize) -> Result<(), MalformedStringError> {
    let len = buffer.len();
    let mut i = index;

    i += 1;
    if i >= len {
        return Err(MalformedStringError::InvalidEscapeSequence { pos: i - 1 });
    }

    let next = buffer[i];
    if !matches!(next, b'\\' | b'"' | b'/' | b'b' | b'f' | b'n' | b'r' | b't' | b'u') {
        return Err(MalformedStringError::InvalidEscapeSequence { pos: i });
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

// returns the length of an escape sequence
pub(super) fn len(buffer: &[u8], pos: usize) -> usize {
    let mut i = pos + 1;
    if matches!(buffer[i], b'\\' | b'"' | b'/' | b'b' | b'f' | b'n' | b'r' | b't') {
        return 2;
    }

    i += 1;
    let val = number::hex_to_u16(&buffer[i..i + 4]).unwrap();
    if utf8::is_surrogate(val) { 12 } else { 6 }
}

fn check_unicode_escape(buffer: &[u8], pos: usize) -> Result<(), MalformedStringError> {
    // Need 4 hex digits
    if pos + 4 >= buffer.len() {
        return Err(MalformedStringError::InvalidEscapeSequence { pos: pos - 1 });
    }

    let mut i = pos;
    i += 1; // move to the 1st hex digit
    let val = match number::hex_to_u16(&buffer[i..i + 4]) {
        Ok(hex) => hex,
        Err(_) => return Err(MalformedStringError::InvalidEscapeSequence { pos: i - 2 })
    };
    // move past the hex digits
    i += 3;
    if utf8::is_surrogate(val) {
        utf8::validate_surrogate(i, buffer, val)?;
    }

    Ok(())
}

fn decode_unicode(buffer: &[u8], pos: usize) -> char {
    let mut i = pos;
    i += 2; // skip '\u'
    let code_unit = number::hex_to_u16(&buffer[i..i + 4]).unwrap();
    let ch: char;

    if utf8::is_surrogate(code_unit) {
        let high = code_unit;
        i += 6; // 4 hex digits + \u
        let low = number::hex_to_u16(&buffer[i..i + 4]).unwrap();
        let code_point = utf8::decode_surrogate_pair(high as u32, low as u32);
        ch = char::from_u32(code_point).unwrap();
    } else {
        ch = char::from_u32(code_unit as u32).unwrap();
    }

    ch
}