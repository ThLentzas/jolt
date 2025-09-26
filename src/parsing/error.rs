use std::{error, fmt};
use crate::parsing::number::NumericError;
use crate::parsing::utf8::Utf8Error;

#[derive(Debug, PartialEq)]
pub(super) enum ErrorKind {
    InvalidByteSequence { len: u8,},
    InvalidSurrogate,
    UnknownEscapedCharacter { byte: u8 },// the next byte value might be a utf8 sequence, we can conditionally render it in display by calling is ascii
    InvalidUnicodeSequence { digit: u8 }, // incomplete sequence is just unexpectedEoF
    InvalidControlCharacter { byte: u8 },
    UnexpectedEof,
    UnexpectedCharacter { byte: u8 },
    InvalidNumber { message: &'static str }, // toDo: Why not String
    DuplicateName { name: String },
    UnexpectedToken { expected: Option<&'static str> },
    DepthLimitExceeded { depth: u16 }
}

#[derive(Debug, PartialEq)]
pub struct JsonError {
    pub kind: ErrorKind,
    pub pos: Option<usize>
}

impl JsonError {
    pub(super) fn new(kind: ErrorKind, pos: Option<usize>)  -> Self {
        JsonError { kind, pos }
    }
}

#[derive(Debug, PartialEq)]
pub enum EscapeError {
    UnknownEscapedCharacter { byte: u8, pos: usize },
    UnexpectedEof { pos: usize },
    InvalidUnicodeSequence { digit: u8, pos: usize },
    InvalidSurrogate { pos: usize }
}

#[derive(Debug, PartialEq)]
pub enum PointerError {
    InvalidPathSyntax { pos: usize },
    InvalidControlCharacter { byte: u8, pos: usize }, // toDo: maybe change to IllegalControlCharacter
    InvalidEscapeSequence { pos: usize },
    // can't use &'static str, we concatenate the index syntax
    InvalidIndex { message: String, pos: usize }
}

impl fmt::Display for JsonError {
    // because self is borrowed everything inside it is also borrowed
    // ErrorKind::DuplicateName { name } => // This tries to move name out and we can not move owned data out of share context
    // we end up with &self.kind
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            // len, byte, digit are copied, we don't need to dereference them
            // toDo: review this and how dereferencing works, what takes ownership and what happens if we had match self.kind
            ErrorKind::InvalidByteSequence { len } => write!(f, "invalid {} byte utf-8 sequence from index {}", len, self.pos.unwrap()),
            ErrorKind::InvalidSurrogate => write!(f, "invalid surrogate pair at index {}", self.pos.unwrap()),
            ErrorKind::UnknownEscapedCharacter { byte } => {
                match byte {
                    b if b.is_ascii_graphic() => write!(f, "unknown escape character {} at index {}", *b as char, self.pos.unwrap()),
                    _  => write!(f, "unknown escape character (0x{:02X}) at index {}", byte, self.pos.unwrap()), // can be a byte from a utf8 sequence or a character with no text representation
                }
            }
            ErrorKind::InvalidUnicodeSequence { digit } => write!(f, "invalid hex digit '{}' at index {}", *digit as char, self.pos.unwrap()),
            ErrorKind::InvalidControlCharacter { byte } => write!(f, "invalid control character (0x{:02X}) at index {}", byte, self.pos.unwrap()),
            ErrorKind::UnexpectedEof => write!(f, "unexpected end of input at index {}", self.pos.unwrap()),
            ErrorKind::UnexpectedCharacter { byte } => {
                match byte {
                    b if b.is_ascii_graphic() => write!(f, "unexpected character {} at index {}", *b as char, self.pos.unwrap()),
                    _  => write!(f, "unexpected character (0x{:02X}) at index {}", byte, self.pos.unwrap()), // can be a byte from a utf8 sequence or a character with no text representation
                }
            }
            ErrorKind::InvalidNumber { message } => write!(f, "{} at index {}", message, self.pos.unwrap()),
            ErrorKind::DuplicateName { name } => write!(f, "duplicate object name {} at index {}", name, self.pos.unwrap()),
            ErrorKind::UnexpectedToken { expected } => {
                match expected {
                    Some(token) => write!(f, "unexpected token at index {}. Expected {}", self.pos.unwrap(), token),
                    None => write!(f, "unexpected token at index {}", self.pos.unwrap()),
                }
            }
            ErrorKind::DepthLimitExceeded { depth } => write!(f, "nesting depth exceeded limit of {}", depth)
        }
    }
}

impl fmt::Display for PointerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl From<NumericError> for EscapeError {
    fn from(err: NumericError) -> Self {
        match err {
            NumericError::InvalidHexDigit { digit, pos} => EscapeError::InvalidUnicodeSequence { digit, pos }
        }
    }
}

impl From<Utf8Error> for JsonError {
    fn from(err: Utf8Error) -> Self {
        match err {
            Utf8Error::InvalidByteSequence { len, pos} => JsonError::new(ErrorKind::InvalidByteSequence { len }, Some(pos)),
        }
    }
}

impl From<EscapeError> for JsonError {
    fn from(err: EscapeError) -> Self {
        match err {
            EscapeError::UnknownEscapedCharacter { byte, pos} => JsonError::new(ErrorKind::UnknownEscapedCharacter { byte }, Some(pos)),
            EscapeError::UnexpectedEof { pos} => JsonError::new(ErrorKind::UnexpectedEof, Some(pos)),
            EscapeError::InvalidUnicodeSequence { digit, pos} => JsonError::new(ErrorKind::InvalidUnicodeSequence { digit }, Some(pos)),
            EscapeError::InvalidSurrogate { pos} => JsonError::new(ErrorKind::InvalidSurrogate, Some(pos))
        }
    }
}

impl error::Error for PointerError {}
impl error::Error for JsonError {}


