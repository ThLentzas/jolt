use crate::parsing::error::{FileParseError, ParserError};
use crate::parsing::value::Value;
use std::fs;

mod macros;
mod parsing;

// have a way for them to specify the case they want when they deserialize
// maybe rename to nobu(trust) or vilya
// check zero copy deserialization

pub fn from_bytes(buffer: &[u8]) -> Result<Value, ParserError> {
    parsing::parse(buffer)
}

pub fn from_str(text: &str) -> Result<Value, ParserError> {
    parsing::parse(text.as_bytes())
}

// fs::read() pre-allocates the vector by calling with_capacity based on the size of the file
pub fn from_file(path: &str) -> Result<Value, FileParseError> {
    let buffer = fs::read(path)
        .map_err(|e| FileParseError::IoError(e))?;
    parsing::parse(&buffer)
        .map_err(|e| FileParseError::ParserError(e))
}

// toDo: write a method validate(reader: Reader) that just scans the reader and doesn't create the ast
// it just checks if it is valid json or not

// pub fn to_pointer_path(npath: &str) -> String {
//
// }