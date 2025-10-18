use crate::parsing::{error::JsonError, parser::Parser, value::Value};

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

pub(super) fn parse(buffer: &[u8]) -> Result<Value, JsonError> {
    Parser::new(buffer).parse()
}
