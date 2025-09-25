use crate::parsing::error::ParserError;
use crate::parsing::value::Value;

mod parsing;

pub fn from_bytes(buffer: &[u8]) -> Result<Value, ParserError> {

}

pub fn from_str(json: &str) -> Result<Value, ParserError> {

}