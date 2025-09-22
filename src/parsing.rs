use crate::parsing::error::{ParserError, TokenizerError};
use crate::parsing::value::Value;
use crate::parsing::parser::Parser;
use crate::parsing::tokenizer::{Tokenizer, TokenizerToken};

mod tokenizer;
mod parser;
mod value;
mod number;
pub mod error;
mod escapes;
mod utf8;

// toDo: rename parsing.rs to a noun
pub fn tokenize(buffer: &[u8]) -> Result<Vec<TokenizerToken>, TokenizerError> {
    Tokenizer::new(buffer).tokenize()
}

pub fn parse(buffer: &[u8], tokens: &Vec<TokenizerToken>) -> Result<Option<Value>, ParserError> {
    Parser::new(buffer, tokens).parse()
}