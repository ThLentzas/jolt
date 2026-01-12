use std::fs;

mod macros;
mod parsing;

// Any type that appears in a public function signature must be accessible to users
pub use parsing::error::{FileParseError, ParseError};
pub use parsing::value::Value;

pub fn from_slice(buffer: &[u8]) -> Result<Value, ParseError> {
    parsing::parse(buffer)
}

pub fn from_str(text: &str) -> Result<Value, ParseError> {
    parsing::parse(text.as_bytes())
}

// fs::read() pre-allocates the vector by calling with_capacity based on the size of the file
// toDo: maybe remove this? review all comments check the std thing
pub fn from_file(path: &str) -> Result<Value, FileParseError> {
    let buffer = fs::read(path).map_err(|e| FileParseError::IoError(e))?;
    parsing::parse(&buffer).map_err(|e| FileParseError::ParserError(e))
}
