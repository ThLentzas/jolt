use crate::parsing::error::JsonError;
use crate::parsing::value::Value;
use crate::parsing::parser::Parser;

mod tokenizer;
mod parser;
pub(super) mod value;
mod number;
mod escapes;
mod utf8;
pub(super) mod error;

//implementation limits: https://www.ibm.com/docs/en/datapower-gateway/10.6.0?topic=20-json-parser-limits
// also mentioned as Document size, 4MB
const INPUT_BUFFER_LIMIT: usize = 4_194_304;
// this is the length of the [u8] representation of the string after parsing
// the input buffer can be longer than 8192 bytes because of escape,utf8 sequences
// in the worst case, where we have only Unicode sequences, the length of the input buffer is roughly 49_000 bytes
const STRING_VALUE_LENGTH_LIMIT: usize = 8192;
const NESTING_DEPTH_LIMIT: u16 = 128;

pub(super) fn parse(buffer: &[u8]) -> Result<Value, JsonError> {
    Parser::new(buffer).parse()
}