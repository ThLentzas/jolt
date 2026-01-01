use crate::parsing::error::{KeywordError, KeywordErrorKind, StringError, StringErrorKind};
use crate::parsing::{error::ParserError, parser::Parser, value::Value};

pub(super) mod error;
mod escapes;
mod lexer;
mod map;
pub(super) mod number;
mod parser;
mod utf8;
pub(super) mod value;

//implementation limits: https://www.ibm.com/docs/en/datapower-gateway/10.6.0?topic=20-json-parser-limits
const INPUT_BUFFER_LIMIT: usize = 4_194_304; // also mentioned as Document size, 4MB
// this is the length of the [u8] representation of the string after parsing
// the input buffer can be longer than 8192 bytes because of escape,utf8 sequences
// in the worst case, where we have only Unicode sequences, the length of the input buffer is roughly 49_000 bytes
const STRING_VALUE_LENGTH_LIMIT: usize = 8192;
const NESTING_DEPTH_LIMIT: u16 = 128;

pub(super) fn parse(buffer: &[u8]) -> Result<Value, ParserError> {
    Parser::new(buffer).parse()
}

// used to read true, false, null during parsing and functions name of filter selectors
fn read_keyword(buffer: &[u8], pos: &mut usize, keyword: &[u8]) -> Result<(), KeywordError> {
    let remaining = &buffer[*pos..];

    if keyword.len() > remaining.len() {
        return Err(KeywordError {
            kind: KeywordErrorKind::UnexpectedEndOf,
            pos: buffer.len() - 1,
        });
    }

    for byte in keyword.iter() {
        if buffer[*pos] != *byte {
            return Err(KeywordError {
                kind: KeywordErrorKind::UnexpectedCharacter { byte: buffer[*pos] },
                pos: *pos,
            });
        }
        *pos += 1;
    }
    Ok(())
}

fn is_rfc_whitespace(byte: u8) -> bool {
    matches!(byte, b'\t' | b'\n' | b'\r' | b' ')
}

fn skip_whitespaces(buffer: &[u8], pos: &mut usize) {
    while *pos < buffer.len() {
        if is_rfc_whitespace(buffer[*pos]) {
            *pos += 1;
        } else {
            break;
        }
    }
}

fn to_jstr(text: &str) -> Result<String, StringError> {
    let buffer = text.as_bytes();
    let mut val = String::new();
    let mut pos = 0;

    while pos < buffer.len() {
        let current = buffer[pos];
        match current {
            c if c.is_ascii_control() => {
                return Err(StringError {
                    kind: StringErrorKind::InvalidControlCharacter { byte: current },
                    pos,
                });
            }
            c if !c.is_ascii() => {
                val.push(utf8::read_utf8_char(buffer, pos));
                pos += utf8::utf8_char_width(current);
            }
            b'\\' => {
                escapes::check_escape_character(buffer, pos)?;
                val.push(escapes::map_escape_character(buffer, pos));
                pos += escapes::len(buffer, pos);
            }
            _ => {
                val.push(current as char);
                pos += 1;
            }
        }

        if val.len() > STRING_VALUE_LENGTH_LIMIT {
            return Err(StringError {
                kind: StringErrorKind::StringValueLengthLimitExceed {
                    len: STRING_VALUE_LENGTH_LIMIT,
                },
                pos,
            });
        }
    }
    Ok(val)
}