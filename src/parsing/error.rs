use std::{error, fmt, io};
use crate::parsing::escapes::EscapeError;
use crate::parsing::utf8::Utf8Error;

#[derive(Debug, PartialEq)]
pub enum StringErrorKind {
    UnexpectedEndOf,
    InvalidByteSequence { len: u8 },
    InvalidSurrogate,
    UnknownEscapedCharacter { byte: u8 },// the byte value might be an utf8 sequence, we can conditionally render it in display
    InvalidUnicodeSequence { digit: u8 }, // incomplete sequence is just unexpectedEoF
    InvalidControlCharacter { byte: u8 },
    StringValueLengthLimitExceed { len: usize }
}

#[derive(Debug, PartialEq)]
pub struct StringError {
    pub kind: StringErrorKind,
    pub pos: usize
}

#[derive(Debug, PartialEq)]
pub enum JsonErrorKind {
    MalformedString(StringError),
    UnexpectedEof,
    UnexpectedCharacter { byte: u8 },
    InvalidNumber { message: &'static str }, // toDo: Why not String
    DuplicateName { name: String },
    UnexpectedToken { expected: Option<&'static str> },
    NestingDepthLimitExceeded { depth: u16 },
    InputBufferLimitExceeded { len: usize },
}

#[derive(Debug, PartialEq)]
pub struct JsonError {
    pub kind: JsonErrorKind,
    pub pos: Option<usize> // For NestingDepthLimitExceeded, InputBufferLimitExceeded position is pointless
}

impl JsonError {
    pub(super) fn new(kind: JsonErrorKind, pos: Option<usize>) -> Self {
        JsonError { kind, pos }
    }
}

#[derive(Debug)]
pub enum FileParseError {
    IoError(io::Error),
    ParserError(JsonError),
}

impl fmt::Display for JsonError {
    // because self is borrowed everything inside it is also borrowed
    // ErrorKind::DuplicateName { name } => // This tries to move name out, and we can not move owned data out of share context
    // we end up with &self.kind
    // len, byte, digit are copied, we don't need to dereference them
    // toDo: review this and how dereferencing works, what takes ownership and what happens if we had match self.kind
    // macros like write!, println! and so on do autoderef
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            // this delegates to the Display of StringError
            JsonErrorKind::MalformedString(str_err) => write!(f, "{} ", str_err),
            JsonErrorKind::UnexpectedEof => write!(f, "unexpected end of input at index {}", self.pos.unwrap()),
            JsonErrorKind::UnexpectedCharacter { byte } => {
                match byte {
                    // can be a byte from a utf8 sequence or a character with no text representation
                    b if b.is_ascii_graphic() => write!(f, "unexpected character {} at index {}", *b as char, self.pos.unwrap()),
                    _  => write!(f, "unexpected character (0x{:02X}) at index {}", byte, self.pos.unwrap()),
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
            JsonErrorKind::NestingDepthLimitExceeded { depth } => write!(f, "nesting depth exceeded limit of {}", depth),
            JsonErrorKind::InputBufferLimitExceeded { len } => write!(f, "input size exceeded limit of {} bytes", len),
        }
    }
}

impl fmt::Display for StringError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            StringErrorKind::UnexpectedEndOf => write!(f, "unexpected end of input at index {}", self.pos),
            StringErrorKind::InvalidByteSequence { len} => write!(f, "invalid {} byte utf-8 sequence from index {}", len, self.pos),
            StringErrorKind::InvalidSurrogate => write!(f, "invalid surrogate pair at index {}", self.pos),
            StringErrorKind::UnknownEscapedCharacter { byte } => {
                if byte.is_ascii_graphic() {
                    write!(f, "unknown escape character {} at index {}", byte, self.pos)
                } else {
                    write!(f, "unknown escape character (0x{:02X}) at index {}", byte, self.pos)
                }
            }
            StringErrorKind::InvalidUnicodeSequence { digit } => write!(f, "invalid hex digit '{}' at index {}", digit, self.pos),
            StringErrorKind::InvalidControlCharacter { byte } => write!(f, "invalid control character (0x{:02X}) at index {}", byte, self.pos),
            StringErrorKind::StringValueLengthLimitExceed { len } => write!(f, "string value exceeded limit of {} bytes", len),
        }
    }
}

impl From<Utf8Error> for StringError {
    fn from(err: Utf8Error) -> Self {
        match err {
            Utf8Error::InvalidByteSequence { len, pos } => StringError { kind: StringErrorKind::InvalidByteSequence { len }, pos }
        }
    }
}

impl From<StringError> for JsonError {
    // we can not call, JsonError { kind: JsonErrorKind::MalformedString(err), err.pos }
    // because err is already move to MalformedString, and we are trying to access pos via err which
    // is no longer valid
    // it is similar to
    // let foo = Foo { bar: Bar };
    // let x = foo;
    // let y = foo.bar;
    // we can no longer access bar via because foo is no longer valid
    // http://doc.rust-lang.org/book/ch04-01-what-is-ownership.html above figure 4.4
    fn from(err: StringError) -> Self {
        let pos = err.pos;
        JsonError { kind: JsonErrorKind::MalformedString(err), pos: Some(pos) }
    }
}

impl From<EscapeError> for StringError {
    fn from(err: EscapeError) -> Self {
        match err {
            EscapeError::UnknownEscapedCharacter { byte, pos } => StringError { kind: StringErrorKind::UnknownEscapedCharacter { byte }, pos },
            EscapeError::UnexpectedEof { pos } => StringError { kind: StringErrorKind::UnexpectedEndOf, pos },
            EscapeError::InvalidUnicodeSequence { digit, pos } => StringError { kind: StringErrorKind::InvalidUnicodeSequence { digit }, pos },
            EscapeError::InvalidSurrogate { pos } => StringError { kind: StringErrorKind::InvalidSurrogate, pos }
        }
    }
}

impl error::Error for JsonError {}
impl error::Error for StringError {}


