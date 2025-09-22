use crate::error::{ParserError, TokenizerError};
use crate::parse::node::Node;
use crate::parse::parser::Parser;
use crate::parse::tokenizer::{Tokenizer, TokenizerToken};

mod tokenizer;
mod parser;
mod node;

// toDo: rename parse.rs to a noun
pub fn tokenize(buffer: &[u8]) -> Result<Vec<TokenizerToken>, TokenizerError> {
    Tokenizer::new(buffer).tokenize()
}

pub fn parse(buffer: &[u8], tokens: &Vec<TokenizerToken>) -> Result<Node, ParserError> {
    Parser::new(buffer, tokens).parse()
}