use std::{error, fmt};
use crate::utils::byte_utils;

#[derive(Debug)]
pub enum NumericError {
    InvalidHexDigit { digit: u8, position: usize }
}

#[derive(Debug)]
pub enum TokenizerError {
    InvalidControlCharacter { byte: u8, position: usize },
    InvalidUtf8Sequence { len: u8, position: usize },
    IncompleteUtf8Sequence { position: usize },
    IncompleteEscapeSequence { position: usize },
    UnknownEscapeCharacter { position: usize },
    InvalidSurrogate { val: u16, position: usize },
    UnpairedSurrogate{ val: u16, position: usize },
    UnrecognizedCharacter { byte: Option<u8>, position: usize },
    UnterminatedString { position: usize },
    IncompleteUnicodeSequence { position: usize},
    InvalidUnicodeSequence { digit: u8, position: usize },
    InvalidNumber { message: &'static str, position: usize }, // toDo: Why not String
    UnexpectedValue { position: usize }
}

#[derive(Debug)]
pub enum Utf8Error {
    IncompleteSequence { position: usize },
    InvalidSequence { len: u8, position: usize },
}

impl fmt::Display for TokenizerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidControlCharacter { byte, position } => {
                write!(f, "Invalid control character {} (0x{:02X}) at position {}", byte_utils::map_to_text(*byte), *byte, position)
            }
            Self::InvalidUtf8Sequence { len, position } => {
                write!(f, "Invalid {} byte utf-8 sequence from index {}", len, position)
            }
            Self::IncompleteUtf8Sequence { position } => {
                write!(f, "Incomplete utf-8 sequence from index {}", position)
            }
            Self::InvalidUnicodeSequence { digit, position } => {
                write!(f, "Invalid unicode sequence, invalid hex digit '{}' at index {}", digit, position)
            }
            Self::IncompleteUnicodeSequence {position } => {
                write!(f, "Incomplete unicode sequence at index {}", position)
            }
            Self::IncompleteEscapeSequence {position } => {
                write!(f, "Incomplete character escape sequence at index {}", position)
            }
            Self::UnknownEscapeCharacter {position } => {
                write!(f, "Unknown escape character at index {}", position)
            }
            Self::InvalidSurrogate {val, position} => {
                write!(f, "Low surrogate \\u{:04X} at index {}. Surrogate pair order is always high low", val, position)
            }
            Self::UnpairedSurrogate {val, position} => {
                write!(f, "Unpaired high surrogate \\u{:04X} at index {}", val, position)
            }
            Self::UnrecognizedCharacter {byte, position} => {
                match byte {
                    Some(byte) if byte.is_ascii() => {
                        write!(f, "Unrecognized character '{}' at position {}", *byte as char, position)
                    }
                    Some(byte) if byte.is_ascii_control() => {
                        write!(f, "Unrecognized character {} (0x{:02X}) at position {}", byte_utils::map_to_text(*byte), *byte, position)
                    }
                    // utf8 sequence
                    _ => write!(f, "Unrecognized character at position {}", position)
                }
            }
            Self::UnterminatedString {position} => {
                write!(f, "Unterminated String at index {}", position)
            }
            Self::InvalidNumber {message, position} => {
                write!(f, "{} at index {}", message, position)
            }
            Self::UnexpectedValue {position} => {
                write!(f, "Unexpected value  at index {}", position)
            }
        }
    }
}

impl fmt::Display for NumericError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidHexDigit {digit, position} => write!(f, "Invalid hex digit (byte 0x{:02x}) at index {}", digit, position)
        }
    }
}

impl fmt::Display for Utf8Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::IncompleteSequence { position } => write!(f, "Incomplete utf-8 sequence from index {}", position),
            Self::InvalidSequence { len, position } => write!(f, "Invalid {} byte utf-8 sequence from index {}", len, position)
        }
    }
}

impl From<Utf8Error> for TokenizerError {
    fn from(err: Utf8Error) -> Self {
        match err {
            Utf8Error::InvalidSequence { len, position } => TokenizerError::InvalidUtf8Sequence { len, position },
            Utf8Error::IncompleteSequence { position } => TokenizerError::IncompleteUtf8Sequence { position }
        }
    }
}

impl From<NumericError> for TokenizerError {
    fn from(err: NumericError) -> Self {
        match err {
            NumericError::InvalidHexDigit { digit, position } => TokenizerError::InvalidUnicodeSequence {digit, position}
        }
    }
}

impl error::Error for TokenizerError {}
impl error::Error for Utf8Error {}
impl error::Error for NumericError {}

