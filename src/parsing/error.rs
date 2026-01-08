use crate::parsing::escapes::{EscapeError, EscapeErrorKind};
use crate::parsing::number::{NumericError, NumericErrorKind};
use crate::parsing::utf8::Utf8Error;
use std::{error, fmt, io};

#[derive(Debug, PartialEq)]
pub enum
StringErrorKind {
    UnexpectedEndOf,
    InvalidByteSequence { len: u8 },
    InvalidSurrogate,
    // the byte value might be an utf8 sequence, we can conditionally render it in display
    UnknownEscapedCharacter { byte: u8 },
    // incomplete sequence is just unexpectedEoF
    InvalidUnicodeSequence { digit: u8 },
    InvalidControlCharacter { byte: u8 },
    LengthLimitExceeded { len: usize },
}

impl fmt::Display for StringErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StringErrorKind::UnexpectedEndOf => {
                write!(f, "unexpected end of input, unterminated string")
            }
            StringErrorKind::InvalidByteSequence { len } => {
                write!(f, "invalid {} byte utf-8 sequence", len)
            }
            StringErrorKind::InvalidSurrogate => {
                write!(f, "invalid surrogate pair")
            }
            StringErrorKind::UnknownEscapedCharacter { byte } => {
                if byte.is_ascii_graphic() {
                    write!(f, "unknown escape character '{}'", *byte as char)
                } else {
                    write!(f, "unknown escape character (0x{:02X})", byte)
                }
            }
            StringErrorKind::InvalidUnicodeSequence { digit } => {
                write!(f, "invalid hex digit '{}'", digit)
            }
            StringErrorKind::InvalidControlCharacter { byte } => {
                write!(f, "invalid control character (0x{:02X})", byte)
            }
            StringErrorKind::LengthLimitExceeded { len } => {
                write!(f, "string value exceeded limit of  {} characters", len)
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct StringError {
    pub kind: StringErrorKind,
    pub pos: usize,
}

impl error::Error for StringError {}

impl fmt::Display for StringError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at index {}", self.kind, self.pos)
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
pub enum LexErrorKind {
    MalformedString(StringErrorKind),
    UnexpectedEof,
    UnexpectedCharacter { byte: u8 },
    Numeric(NumericErrorKind),
}

impl fmt::Display for LexErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexErrorKind::MalformedString(err) => write!(f, "{}", err),
            LexErrorKind::UnexpectedEof => write!(f, "unexpected end of input"),
            LexErrorKind::UnexpectedCharacter { byte } => {
                if byte.is_ascii_graphic() {
                    write!(f, "unexpected character '{}'", *byte as char)
                } else {
                    write!(f, "unexpected character (0x{:02X})", byte)
                }
            }
            LexErrorKind::Numeric(err) => write!(f, "{}", err),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub pos: usize
}

impl error::Error for LexError {}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at index {}", self.kind, self.pos)
    }
}

impl From<StringError> for LexError {
    fn from(err: StringError) -> Self {
        LexError {
            kind: LexErrorKind::MalformedString(err.kind),
            pos: err.pos,
        }
    }
}

impl From<KeywordError> for LexError {
    fn from(err: KeywordError) -> Self {
        match err.kind {
            KeywordErrorKind::UnexpectedCharacter { byte } => LexError {
                kind: LexErrorKind::UnexpectedCharacter { byte },
                pos: err.pos,
            },
            KeywordErrorKind::UnexpectedEndOf => LexError {
                kind: LexErrorKind::UnexpectedEof,
                pos: err.pos,
            },
        }
    }
}

impl From<NumericError> for LexError {
    fn from(err: NumericError) -> Self {
        LexError {
            kind: LexErrorKind::Numeric(err.kind),
            pos: err.pos,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseErrorKind {
    Lexical(LexErrorKind),
    UnexpectedEof,
    DuplicateName { name: String },
    UnexpectedToken { expected: Option<&'static str> },
    NestingDepthLimitExceeded { depth: u16 },
    InputBufferLimitExceeded { len: usize },
    StringLengthLimitExceeded { len: usize }
}

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseErrorKind::Lexical(err) => write!(f, "{}", err),
            ParseErrorKind::UnexpectedEof => {
                write!(f, "unexpected end of input")
            }
            ParseErrorKind::DuplicateName { name } => {
                write!(f, "duplicate object name '{}'", name)
            }
            ParseErrorKind::UnexpectedToken { expected } => match expected {
                Some(token) => write!(f, "unexpected token, expected {}", token),
                None => write!(f, "unexpected token"),
            },
            ParseErrorKind::NestingDepthLimitExceeded { depth } => {
                write!(f, "nesting depth exceeded limit of {}", depth)
            }
            ParseErrorKind::InputBufferLimitExceeded { len } => {
                write!(f, "input size exceeded limit of {} bytes", len)
            }
            ParseErrorKind::StringLengthLimitExceeded { len } => {
                write!(f, "string value exceeded limit of  {} characters", len)
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub pos: usize
}

impl error::Error for ParseError {}

impl fmt::Display for ParseError {
    // macros like write!, println! and so on do autoderef
    // we access kind via a reference to self and because match takes ownership and kind holds
    // variants of String that do not implement copy we pass a reference to it instead; similar to
    // self.kind in Number for the partial_cmp() imp
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at index {}", self.kind, self.pos)
    }
}

impl From<LexError> for ParseError {
    fn from(err: LexError) -> Self {
        ParseError {
            kind: ParseErrorKind::Lexical(err.kind),
            pos: err.pos
        }
    }
}

#[derive(Debug)]
pub enum FileParseError {
    IoError(io::Error),
    ParserError(ParseError),
}

impl error::Error for FileParseError {}

impl fmt::Display for FileParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
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
