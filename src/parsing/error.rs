use std::{error, fmt};
use crate::parsing::number::NumericError;
use crate::parsing::utf8::Utf8Error;

#[derive(Debug, PartialEq)]
pub enum JsonErrorKind {
    InvalidByteSequence { len: u8 },
    InvalidSurrogate,
    UnknownEscapedCharacter { byte: u8 },// the next byte value might be an utf8 sequence, we can conditionally render it in display by calling is ascii
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
    pub kind: JsonErrorKind,
    pub pos: Option<usize> // For DepthLimitExceeded position is pointless
}

impl JsonError {
    pub(super) fn new(kind: JsonErrorKind, pos: Option<usize>) -> Self {
        JsonError { kind, pos }
    }
}

// maybe we could use composition for ErrorKind?
#[derive(Debug, PartialEq)]
pub enum EscapeError {
    UnknownEscapedCharacter { byte: u8, pos: usize },
    UnexpectedEof { pos: usize },
    InvalidUnicodeSequence { digit: u8, pos: usize },
    InvalidSurrogate { pos: usize }
}

#[derive(Debug, PartialEq)]
pub enum PointerErrorKind {
    InvalidPathSyntax,
    InvalidControlCharacter { byte: u8 }, // toDo: maybe change to IllegalControlCharacter
    UnknownEscapedCharacter { byte: u8 },
    UnexpectedEof,
    InvalidUnicodeSequence { digit: u8 },
    InvalidIndex { message: String },// can't use &'static str, we concatenate the index syntax
    InvalidSurrogate
}

#[derive(Debug, PartialEq)]
pub struct PointerError {
    pub kind: PointerErrorKind,
    pub pos: usize
}

impl PointerError {
    pub(super) fn new(kind: PointerErrorKind, pos: usize) -> Self {
        PointerError { kind, pos }
    }
}

impl fmt::Display for JsonError {
    // because self is borrowed everything inside it is also borrowed
    // ErrorKind::DuplicateName { name } => // This tries to move name out, and we can not move owned data out of share context
    // we end up with &self.kind
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            // len, byte, digit are copied, we don't need to dereference them
            // toDo: review this and how dereferencing works, what takes ownership and what happens if we had match self.kind
            JsonErrorKind::InvalidByteSequence { len } => write!(f, "invalid {} byte utf-8 sequence from index {}", len, self.pos.unwrap()),
            JsonErrorKind::InvalidSurrogate => write!(f, "invalid surrogate pair at index {}", self.pos.unwrap()),
            JsonErrorKind::UnknownEscapedCharacter { byte } => {
                match byte {
                    b if b.is_ascii_graphic() => write!(f, "unknown escape character {} at index {}", *b as char, self.pos.unwrap()),
                    _  => write!(f, "unknown escape character (0x{:02X}) at index {}", byte, self.pos.unwrap()), // can be a byte from a utf8 sequence or a character with no text representation
                }
            }
            JsonErrorKind::InvalidUnicodeSequence { digit } => write!(f, "invalid hex digit '{}' at index {}", *digit as char, self.pos.unwrap()),
            JsonErrorKind::InvalidControlCharacter { byte } => write!(f, "invalid control character (0x{:02X}) at index {}", byte, self.pos.unwrap()),
            JsonErrorKind::UnexpectedEof => write!(f, "unexpected end of input at index {}", self.pos.unwrap()),
            JsonErrorKind::UnexpectedCharacter { byte } => {
                match byte {
                    b if b.is_ascii_graphic() => write!(f, "unexpected character {} at index {}", *b as char, self.pos.unwrap()),
                    _  => write!(f, "unexpected character (0x{:02X}) at index {}", byte, self.pos.unwrap()), // can be a byte from a utf8 sequence or a character with no text representation
                }
            }
            JsonErrorKind::InvalidNumber { message } => write!(f, "{} at index {}", message, self.pos.unwrap()),
            JsonErrorKind::DuplicateName { name } => write!(f, "duplicate object name {} at index {}", name, self.pos.unwrap()),
            JsonErrorKind::UnexpectedToken { expected } => {
                match expected {
                    Some(token) => write!(f, "unexpected token at index {}. Expected {}", self.pos.unwrap(), token),
                    None => write!(f, "unexpected token at index {}", self.pos.unwrap()),
                }
            }
            JsonErrorKind::DepthLimitExceeded { depth } => write!(f, "nesting depth exceeded limit of {}", depth)
        }
    }
}

impl fmt::Display for PointerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            PointerErrorKind::InvalidPathSyntax => write!(f, "pointer paths must be prefixed with '/'syntax at index {}", self.pos),
            PointerErrorKind::InvalidControlCharacter { byte } => write!(f, "invalid control character (0x{:02X}) at index {}", byte, self.pos),
            PointerErrorKind::UnknownEscapedCharacter { byte } => {
                match byte {
                    b if b.is_ascii_graphic() => write!(f, "unknown escape character {} at index {}", *b as char, self.pos),
                    _ => write!(f, "unknown escape character (0x{:02X}) at index {}", byte, self.pos)
                }
            }
            PointerErrorKind::UnexpectedEof => write!(f, "unexpected end of input at index {}", self.pos),
            PointerErrorKind::InvalidUnicodeSequence { digit } => write!(f, "invalid hex digit '{}' at index {}", *digit as char, self.pos),
            PointerErrorKind::InvalidIndex { message } => write!(f, "{} at index {}", message, self.pos),
            PointerErrorKind::InvalidSurrogate => write!(f, "invalid surrogate pair at index {}", self.pos)
        }
    }
}

impl From<NumericError> for EscapeError {
    fn from(err: NumericError) -> Self {
        match err {
            NumericError::InvalidHexDigit { digit, pos } => EscapeError::InvalidUnicodeSequence { digit, pos }
        }
    }
}

impl From<Utf8Error> for JsonError {
    fn from(err: Utf8Error) -> Self {
        match err {
            Utf8Error::InvalidByteSequence { len, pos} => JsonError::new(JsonErrorKind::InvalidByteSequence { len }, Some(pos)),
        }
    }
}

impl From<EscapeError> for JsonError {
    fn from(err: EscapeError) -> Self {
        match err {
            EscapeError::UnknownEscapedCharacter { byte, pos } => JsonError::new(JsonErrorKind::UnknownEscapedCharacter { byte }, Some(pos)),
            EscapeError::UnexpectedEof { pos } => JsonError::new(JsonErrorKind::UnexpectedEof, Some(pos)),
            EscapeError::InvalidUnicodeSequence { digit, pos } => JsonError::new(JsonErrorKind::InvalidUnicodeSequence { digit }, Some(pos)),
            EscapeError::InvalidSurrogate { pos } => JsonError::new(JsonErrorKind::InvalidSurrogate, Some(pos))
        }
    }
}

impl From<EscapeError> for PointerError {
    fn from(err: EscapeError) -> Self {
        match err {
            EscapeError::UnknownEscapedCharacter { byte, pos } => PointerError::new(PointerErrorKind::UnknownEscapedCharacter { byte }, pos),
            EscapeError::UnexpectedEof { pos } => PointerError::new(PointerErrorKind::UnexpectedEof, pos),
            EscapeError::InvalidUnicodeSequence { digit, pos } => PointerError::new(PointerErrorKind::InvalidUnicodeSequence { digit }, pos),
            EscapeError::InvalidSurrogate { pos } => PointerError::new(PointerErrorKind::InvalidSurrogate, pos)
        }
    }
}

impl error::Error for PointerError {}
impl error::Error for JsonError {}


