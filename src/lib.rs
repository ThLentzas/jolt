use crate::parsing::error::FileParseError;
use crate::parsing::{error::JsonError, value::Value};
use std::fs;

mod macros;
mod parsing;

// have a way for them to specify the case they want when they deserialize
// maybe rename to nobu(trust)
// check zero copy deserialization
pub fn from_bytes(buffer: &[u8]) -> Result<Value, JsonError> {
    parsing::parse(buffer)
}

pub fn from_str(text: &str) -> Result<Value, JsonError> {
    parsing::parse(text.as_bytes())
}

// fs::read() pre-allocates the vector by calling with_capacity based on the size of the file
pub fn from_file(path: &str) -> Result<Value, FileParseError> {
    let buffer = fs::read(path)
        .map_err(|e| FileParseError::IoError(e))?;
    parsing::parse(&buffer)
        .map_err(|e| FileParseError::ParserError(e))
}
