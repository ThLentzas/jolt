use crate::parsing::error::JsonError;
use crate::parsing::value::Value;

mod parsing;

pub fn from_bytes(buffer: &[u8]) -> Result<Value, JsonError> {

}

pub fn from_str(json: &str) -> Result<Value, JsonError> {

}