use crate::parsing::{error::ParserError, parser::Parser, value::Value};
use crate::parsing::error::{KeywordError, KeywordErrorKind};

pub(super) mod error;
pub(super) mod value;
mod escapes;
mod lexer;
mod number;
mod parser;
mod utf8;

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

fn read_boolean_or_null(buffer: &[u8], pos: &mut usize, target: &[u8]) -> Result<(), KeywordError> {
    let remaining = &buffer[*pos..];

    if target.len() > remaining.len() {
        return Err(KeywordError {
            kind: KeywordErrorKind::UnexpectedEndOf,
            pos: buffer.len() - 1
        });
    }

    for byte in target.iter() {
        if buffer[*pos] != *byte {
            return Err(KeywordError {
                kind: KeywordErrorKind::UnexpectedCharacter { byte: buffer[*pos] },
                pos: *pos
            });
        }
        *pos += 1;
    }
    Ok(())
}

fn is_rfc_whitespace(byte: u8)  -> bool {
    matches!(byte, b'\t' | b'\n' | b'\r' | b' ')
}

//         while self.pos < self.buffer.len() {
//             let current = self.buffer[self.pos];
//             // The only control characters that are allowed as raw bytes according to rfc are '\t', '\n', '\r'
//             // ' '(space) is not a control character
//             if current.is_ascii_control() && !matches!(current, b'\t' | b'\n' | b'\r' | b' ') { // rfc whitespaces
//                 return Err(JsonError::new(JsonErrorKind::UnexpectedCharacter { byte: current }, Some(self.pos)));
//             }
//             if matches!(current, b'\t' | b'\n' | b'\r' | b' ') {
//                 self.pos += 1;
//             } else {
//                 break;
//             }
//         }
//         Ok(())
//     }
// Initially I had the above code but the check for control characters was not needed because
// if the current byte is not a whitespace, will fail to match any arm in lex, and we would
// return an UnexpectedChar error
fn skip_whitespaces(buffer: &[u8], pos: &mut usize)  {
    while *pos < buffer.len() {
        if is_rfc_whitespace(buffer[*pos]) {
            *pos += 1;
        } else {
            break;
        }
    }
}