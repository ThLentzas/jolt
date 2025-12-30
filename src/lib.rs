use std::fs;

mod macros;
mod parsing;

// Any type that appears in a public function signature must be accessible to users
pub use parsing::error::{FileParseError, ParserError};
pub use parsing::value::Value;

pub fn from_bytes(buffer: &[u8]) -> Result<Value, ParserError> {
    parsing::parse(buffer)
}

pub fn from_str(text: &str) -> Result<Value, ParserError> {
    parsing::parse(text.as_bytes())
}

// fs::read() pre-allocates the vector by calling with_capacity based on the size of the file
pub fn from_file(path: &str) -> Result<Value, FileParseError> {
    let buffer = fs::read(path).map_err(|e| FileParseError::IoError(e))?;
    parsing::parse(&buffer).map_err(|e| FileParseError::ParserError(e))
}

// toDo: write a method validate(reader: Reader) that just scans the reader and doesn't create the ast
// it just checks if it is valid json or not
// toDo: add in the Readme that if the user tries to create a value on the fly by calling Value::from()
// or directly by passing a string they can have unexpected behaviour since there is no validation
// done. numbers out of range and invalid json strings can lead to such behavior
