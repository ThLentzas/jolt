use crate::parsing::error::{KeywordError, KeywordErrorKind, StringError};
use crate::parsing::number::{NumericError, OutOfRangeError};
use crate::parsing::value::path::filter::function::FnExprError;
use std::error::Error;
use std::fmt;
use crate::ParserError;

#[derive(Debug, PartialEq)]
pub enum PointerErrorKind {
    InvalidPointerSyntax,
    MalformedString(StringError),
    InvalidIndex { message: &'static str },
}

#[derive(Debug, PartialEq)]
pub struct PointerError {
    pub kind: PointerErrorKind,
    pub pos: usize,
}

impl Error for PointerError {}

impl fmt::Display for PointerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            // macros like write!, println! and so on do autoderef
            PointerErrorKind::InvalidPointerSyntax => {
                write!(
                    f,
                    "pointer paths must be prefixed with '/' at index {}",
                    self.pos
                )
            }
            PointerErrorKind::MalformedString(err) => write!(f, "{}", err),
            PointerErrorKind::InvalidIndex { message } => {
                write!(f, "{} at index {}", message, self.pos)
            }
        }
    }
}

impl From<StringError> for PointerError {
    // we can not call, PathError { kind: PathErrorKind::MalformedString(err), err.pos }
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
        PointerError {
            kind: PointerErrorKind::MalformedString(err),
            pos,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum PathErrorKind {
    UnexpectedEndOf,
    MalformedString(StringError),
    UnexpectedCharacter { byte: u8 },
    InvalidIndex { message: String },
    Numeric(NumericError),
    FnExpr(FnExprError),
}

#[derive(Debug, PartialEq)]
pub struct PathError {
    pub kind: PathErrorKind,
    pub pos: usize,
}

impl Error for PathError {}

impl fmt::Display for PathError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            PathErrorKind::UnexpectedEndOf => {
                write!(f, "unexpected end of input at index {}", self.pos)
            }
            PathErrorKind::MalformedString(err) => {
                write!(f, "{} ", err)
            }
            PathErrorKind::UnexpectedCharacter { byte } => {
                match byte {
                    // can be a byte from a utf8 sequence or a character with no text representation
                    b if b.is_ascii_graphic() => {
                        write!(
                            f,
                            "unexpected character {} at index {}",
                            *b as char, self.pos
                        )
                    }
                    _ => write!(
                        f,
                        "unexpected character (0x{:02X}) at index {}",
                        byte, self.pos
                    ),
                }
            }
            PathErrorKind::InvalidIndex { message } => {
                write!(f, "{} at index {}", message, self.pos)
            }
            PathErrorKind::Numeric(err) => {
                write!(f, "{} ", err)
            }
            PathErrorKind::FnExpr(err) => {
                write!(f, "{} ", err)
            }
        }
    }
}

impl From<StringError> for PathError {
    // we can not call, PathError { kind: PathErrorKind::MalformedString(err), err.pos }
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
        PathError {
            kind: PathErrorKind::MalformedString(err),
            pos,
        }
    }
}

impl From<KeywordError> for PathError {
    fn from(err: KeywordError) -> Self {
        match err.kind {
            KeywordErrorKind::UnexpectedCharacter { byte } => PathError {
                kind: PathErrorKind::UnexpectedCharacter { byte },
                pos: err.pos,
            },
            KeywordErrorKind::UnexpectedEndOf => PathError {
                kind: PathErrorKind::UnexpectedEndOf,
                pos: err.pos,
            },
        }
    }
}

impl From<OutOfRangeError> for PathError {
    fn from(err: OutOfRangeError) -> Self {
        PathError {
            kind: PathErrorKind::InvalidIndex {
                message: format!("overflow, index exceeds: {}", err.bound)
            },
            pos: err.pos,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum PatchError {
    ParserError(ParserError),
    UnexpectedValue { expected: &'static str },
    // usize is the index of the operation within the array
    OpError(OpError, usize),
}

#[derive(Debug, PartialEq)]
pub enum OpError {
    MissingMember { member: &'static str },
    PointerError(PointerError),
    UnexpectedValue { field: &'static str, expected: &'static str },
    PathNotFound { path: String, depth: usize },
    IndexOutOfBounds { path: String, depth: usize, index: usize, len: usize },
    InvalidRootOp { op: &'static str },
    NotEqual
}

impl From<ParserError> for PatchError {
    fn from(err: ParserError) -> Self {
        PatchError::ParserError(err)
    }
}

impl From<PointerError> for OpError {
    fn from(err: PointerError) -> Self {
        OpError::PointerError(err)
    }
}