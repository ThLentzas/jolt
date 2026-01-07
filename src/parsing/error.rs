use crate::parsing::escapes::{EscapeError, EscapeErrorKind};
use crate::parsing::number::NumericError;
use crate::parsing::utf8::Utf8Error;
use std::{error, fmt, io};

// toDo: make a Lexer error
#[derive(Debug, PartialEq)]
pub enum StringErrorKind {
    UnexpectedEndOf,
    InvalidByteSequence { len: u8 },
    InvalidSurrogate,
    // the byte value might be an utf8 sequence, we can conditionally render it in display
    UnknownEscapedCharacter { byte: u8 },
    // incomplete sequence is just unexpectedEoF
    InvalidUnicodeSequence { digit: u8 },
    InvalidControlCharacter { byte: u8 },
    StringValueLengthLimitExceed { len: usize },
}

#[derive(Debug, PartialEq)]
pub struct StringError {
    pub kind: StringErrorKind,
    pub pos: usize,
}

impl error::Error for StringError {}

impl fmt::Display for StringError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            StringErrorKind::UnexpectedEndOf => {
                write!(f, "unexpected end of input, unterminated string")
            }
            StringErrorKind::InvalidByteSequence { len } => {
                write!(
                    f,
                    "invalid {} byte utf-8 sequence from index {}",
                    len, self.pos
                )
            }
            StringErrorKind::InvalidSurrogate => {
                write!(f, "invalid surrogate pair at index {}", self.pos)
            }
            StringErrorKind::UnknownEscapedCharacter { byte } => {
                if byte.is_ascii_graphic() {
                    write!(f, "unknown escape character {} at index {}", byte, self.pos)
                } else {
                    write!(
                        f,
                        "unknown escape character (0x{:02X}) at index {}",
                        byte, self.pos
                    )
                }
            }
            StringErrorKind::InvalidUnicodeSequence { digit } => {
                write!(f, "invalid hex digit '{}' at index {}", digit, self.pos)
            }
            StringErrorKind::InvalidControlCharacter { byte } => {
                write!(
                    f,
                    "invalid control character (0x{:02X}) at index {}",
                    byte, self.pos
                )
            }
            StringErrorKind::StringValueLengthLimitExceed { len } => {
                write!(f, "string value exceeded limit of {} bytes", len)
            }
        }
    }
}

impl From<Utf8Error> for StringError {
    fn from(err: Utf8Error) -> Self {
        match err {
            Utf8Error { len, pos } => StringError {
                kind: StringErrorKind::InvalidByteSequence { len },
                pos,
            },
        }
    }
}

impl From<EscapeError> for StringError {
    fn from(err: EscapeError) -> Self {
        match err.kind {
            EscapeErrorKind::UnknownEscapedCharacter { byte } => StringError {
                kind: StringErrorKind::UnknownEscapedCharacter { byte },
                pos: err.pos,
            },
            EscapeErrorKind::UnexpectedEof => StringError {
                kind: StringErrorKind::UnexpectedEndOf,
                pos: err.pos,
            },
            EscapeErrorKind::InvalidUnicodeSequence { digit } => StringError {
                kind: StringErrorKind::InvalidUnicodeSequence { digit },
                pos: err.pos,
            },
            EscapeErrorKind::InvalidSurrogate => StringError {
                kind: StringErrorKind::InvalidSurrogate,
                pos: err.pos,
            },
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ParserErrorKind {
    MalformedString(StringErrorKind),
    UnexpectedEof,
    UnexpectedCharacter { byte: u8 },
    InvalidNumber(NumericError),
    DuplicateName { name: String },
    UnexpectedToken { expected: Option<&'static str> },
    NestingDepthLimitExceeded { depth: u16 },
    InputBufferLimitExceeded { len: usize },
}

#[derive(Debug, PartialEq)]
pub struct ParserError {
    pub kind: ParserErrorKind,
    pub pos: Option<usize>, // For NestingDepthLimitExceeded, InputBufferLimitExceeded position is pointless
}

impl error::Error for ParserError {}

impl fmt::Display for ParserError {
    // macros like write!, println! and so on do autoderef
    // we access kind via a reference to self and because match takes ownership and kind holds
    // variants of String that do not implement copy we pass a reference to it instead; similar to
    // self.kind in Number for the partial_cmp() imp
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            // this delegates to the Display of StringError
            ParserErrorKind::MalformedString(err) => write!(f, "{} ", err),
            ParserErrorKind::UnexpectedEof => {
                write!(f, "unexpected end of input at index {}", self.pos.unwrap())
            }
            ParserErrorKind::UnexpectedCharacter { byte } => {
                match byte {
                    // can be a byte from a utf8 sequence or a character with no text representation
                    b if b.is_ascii_graphic() => {
                        write!(
                            f,
                            "unexpected character {} at index {}",
                            *b as char,
                            self.pos.unwrap()
                        )
                    }
                    _ => write!(
                        f,
                        "unexpected character (0x{:02X}) at index {}",
                        byte,
                        self.pos.unwrap()
                    ),
                }
            }
            ParserErrorKind::InvalidNumber(err) => write!(f, "{}", err),
            ParserErrorKind::DuplicateName { name } => {
                write!(
                    f,
                    "duplicate object name {} at index {}",
                    name,
                    self.pos.unwrap()
                )
            }
            ParserErrorKind::UnexpectedToken { expected } => match expected {
                Some(token) => {
                    write!(
                        f,
                        "unexpected token at index {}. Expected {}",
                        self.pos.unwrap(),
                        token
                    )
                }
                None => write!(f, "unexpected token at index {}", self.pos.unwrap()),
            },
            ParserErrorKind::NestingDepthLimitExceeded { depth } => {
                write!(f, "nesting depth exceeded limit of {}", depth)
            }
            ParserErrorKind::InputBufferLimitExceeded { len } => {
                write!(f, "input size exceeded limit of {} bytes", len)
            }
        }
    }
}

impl From<StringError> for ParserError {
    fn from(err: StringError) -> Self {
        let pos = err.pos;
        ParserError {
            kind: ParserErrorKind::MalformedString(err.kind),
            pos: Some(pos),
        }
    }
}

impl From<KeywordError> for ParserError {
    fn from(err: KeywordError) -> Self {
        match err.kind {
            KeywordErrorKind::UnexpectedCharacter { byte } => ParserError {
                kind: ParserErrorKind::UnexpectedCharacter { byte },
                pos: Some(err.pos),
            },
            KeywordErrorKind::UnexpectedEndOf => ParserError {
                kind: ParserErrorKind::UnexpectedEof,
                pos: Some(err.pos),
            },
        }
    }
}

#[derive(Debug)]
pub enum FileParseError {
    IoError(io::Error),
    ParserError(ParserError),
}

impl error::Error for FileParseError {}

impl fmt::Display for FileParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // delegate
            FileParseError::IoError(err) => write!(f, "{} ", err),
            FileParseError::ParserError(err) => write!(f, "{} ", err),
        }
    }
}

// this error occurs when we try to parse true, false, null
// didn't think of a better name and needed to return an Err in read_boolean_or_null()
// LiteralError does not make sense because all the possible values are literals(string literal,
// number etc..)
#[derive(Debug)]
pub(super) struct KeywordError {
    pub(super) kind: KeywordErrorKind,
    pub(super) pos: usize,
}
#[derive(Debug)]
pub(super) enum KeywordErrorKind {
    UnexpectedEndOf,
    UnexpectedCharacter { byte: u8 },
}
