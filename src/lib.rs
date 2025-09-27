use crate::parsing::error::JsonError;
use crate::parsing::value::Value;

mod parsing;

pub fn from_bytes(buffer: &[u8]) -> Result<Value, JsonError> {
    let tokens = parsing::tokenize(buffer)?;
    parsing::parse(buffer, &tokens)
}

pub fn from_str(json: &str) -> Result<Value, JsonError> {
    let buffer = json.as_bytes();
    let tokens = parsing::tokenize(buffer)?;

    parsing::parse(buffer, &tokens)
}