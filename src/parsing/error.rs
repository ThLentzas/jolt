use std::{error, fmt};

#[derive(Debug, PartialEq)]
pub(super) enum NumericError {
    InvalidHexDigit { digit: u8, pos: usize }
}

#[derive(Debug, PartialEq)]
pub enum Utf8Error {
    InvalidByteSequence { len: u8, pos: usize },
    InvalidSurrogate { pos: usize },
}

#[derive(Debug, PartialEq)]
pub enum MalformedStringError {
    InvalidUtf8(Utf8Error),
    UnterminatedString { pos: usize }, // this should be unreachable() in the pointer case
    InvalidControlCharacter { byte: u8, pos: usize },
    InvalidEscapeSequence { pos: usize },
}

#[derive(Debug, PartialEq)]
pub enum TokenizerError {
    UnexpectedEndOfInput { pos: usize },
    InvalidString(MalformedStringError),
    UnrecognizedCharacter { byte: Option<u8>, pos: usize },
    InvalidNumber { message: &'static str, pos: usize }, // toDo: Why not String
    UnexpectedValue { pos: usize } // toDo: explain why we can't show the actual value(we don't if the value has characters with text representation(control characters) or
    // maybe an invalid utf8 sequence)
}

#[derive(Debug, PartialEq)]
pub enum ParserError {
    DuplicateName { name: String, pos: usize },
    UnexpectedToken { token: Option<&'static str>, pos: usize },
    UnexpectedEndOfInput { pos: usize },
    DepthLimitExceeded { depth: u16 }
}

#[derive(Debug, PartialEq)]
pub enum PointerError {
    InvalidPathSyntax { pos: usize },
    InvalidControlCharacter { byte: u8, pos: usize }, // toDo: maybe change to IllegalControlCharacter
    InvalidEscapeSequence { pos: usize },
    // can't use &'static str, we concatenate the index syntax
    InvalidIndex { message: String, pos: usize }
}

impl fmt::Display for Utf8Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidByteSequence { len, pos } => write!(f, "invalid {} byte utf-8 sequence from index {}", len, *pos),
            Self::InvalidSurrogate { pos } => write!(f, "invalid surrogate pair at index {}", *pos),
        }
    }
}

impl fmt::Display for MalformedStringError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidUtf8(err) => write!(f, "{}", err),
            Self::UnterminatedString { pos } => write!(f, "unterminated String at index {}", *pos),
            Self::InvalidControlCharacter { byte, pos } => write!(f, "invalid control character (0x{:02X}) at index {}", *byte, *pos),
            Self::InvalidEscapeSequence { pos } => write!(f, "invalid escape sequence at index {}", *pos)
        }
    }
}

impl fmt::Display for TokenizerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedEndOfInput { pos } => write!(f, "unexpected end of input at index {}", *pos),
            Self::InvalidString(err) => write!(f, "{}", err),
            Self::UnrecognizedCharacter { byte, pos } => {
                match byte {
                    Some(byte) if byte.is_ascii() => {
                        write!(f, "unrecognized character '{}' at index {}", *byte as char, *pos)
                    }
                    Some(byte) if byte.is_ascii_control() => {
                        write!(f, "unrecognized character (0x{:02X}) at index {}", *byte, *pos)
                    }
                    // utf8 sequence
                    _ => write!(f, "unrecognized character at index {}", *pos)
                }
            }
            Self::InvalidNumber { message, pos } => {
                write!(f, "{} at index {}", message, *pos)
            }
            Self::UnexpectedValue { pos } => {
                write!(f, "unexpected value  at index {}", *pos)
            },
        }
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DuplicateName { name, pos} => write!(f, "duplicate object name {} at index {}", name, *pos),
            Self::UnexpectedToken { token, pos } => {
                match token {
                    Some(token) => write!(f, "unexpected token at index {}. Expected {}", *pos, token),
                    None => write!(f, "unexpected token at index {}", *pos),
                }
            },
            Self:: UnexpectedEndOfInput { pos } => write!(f, "unexpected end of input at index {}", *pos),
            Self::DepthLimitExceeded { depth } => write!(f, "nesting depth exceeded limit of {}", depth)
        }
    }
}

impl fmt::Display for NumericError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl fmt::Display for PointerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl From<Utf8Error> for MalformedStringError {
    fn from(err: Utf8Error) -> Self {
        MalformedStringError::InvalidUtf8(err)
    }
}

impl From<MalformedStringError> for TokenizerError {
    fn from(err: MalformedStringError) -> Self {
        TokenizerError::InvalidString(err)
    }
}

impl From<MalformedStringError> for PointerError {
    fn from(err: MalformedStringError) -> Self {
        match err {
            MalformedStringError::InvalidEscapeSequence{ pos } => { PointerError::InvalidEscapeSequence { pos } }
            _ => unreachable!()
        }
    }
}

impl error::Error for Utf8Error {}
impl error::Error for MalformedStringError {}
impl error::Error for TokenizerError {}
impl error::Error for ParserError {}
impl error::Error for NumericError {}
impl error::Error for PointerError {}


