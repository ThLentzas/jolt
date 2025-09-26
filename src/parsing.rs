use crate::parsing::error::JsonError;
use crate::parsing::value::Value;
use crate::parsing::parser::Parser;
use crate::parsing::tokenizer::{Tokenizer, TokenizerToken};

mod tokenizer;
mod parser;
pub(super) mod value;
mod number;
mod escapes;
mod utf8;
pub(super) mod error;

// maybe a from() method so someone could let val = from(some byte buffer);, from_str() also an option, or even a macro!
// where there is a call to as.bytes() and then just call from()
// maybe even make the input a Reader? or a Reader trait
// make the tokenizer error as variant of the parser error
pub(super) fn tokenize(buffer: &[u8]) -> Result<Vec<TokenizerToken>, JsonError> {
    Tokenizer::new(buffer).tokenize()
}

pub(super) fn parse(buffer: &[u8], tokens: &Vec<TokenizerToken>) -> Result<Value, JsonError> {
    Parser::new(buffer, tokens).parse()
}