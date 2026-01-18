use crate::ParseError;
use crate::parsing::error::{KeywordError, KeywordErrorKind, StringError, StringErrorKind};
use crate::parsing::number::{NumericErrorKind, OutOfRangeError};
use crate::parsing::value::path::filter::function::FnExprError;
use std::{error, fmt};

#[derive(Debug, PartialEq)]
pub(super) enum PointerErrorKind {
    InvalidPointerSyntax,
    MalformedString(StringErrorKind),
    InvalidIndex { message: &'static str },
}

impl fmt::Display for PointerErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PointerErrorKind::InvalidPointerSyntax => {
                write!(f, "pointer paths must be prefixed with '/'")
            }
            PointerErrorKind::MalformedString(err) => write!(f, "{}", err),
            PointerErrorKind::InvalidIndex { message } => {
                write!(f, "{}", message)
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct PointerError {
    pub(super) kind: PointerErrorKind,
    pub(super) pos: usize,
}

impl error::Error for PointerError {}

impl fmt::Display for PointerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at index {}", self.kind, self.pos)
    }
}

impl From<StringError> for PointerError {
    fn from(err: StringError) -> Self {
        PointerError {
            kind: PointerErrorKind::MalformedString(err.kind),
            pos: err.pos,
        }
    }
}

#[derive(Debug, PartialEq)]
pub(super) enum PathErrorKind {
    UnexpectedEndOf,
    MalformedString(StringErrorKind),
    UnexpectedCharacter { byte: u8 },
    // index/slice selectors
    InvalidIndex { message: String },
    // can occur when we try to parse literals
    Numeric(NumericErrorKind),
    // we use map_err() to convert
    FnExpr(FnExprError),
}

#[derive(Debug, PartialEq)]
pub struct PathError {
    pub(super) kind: PathErrorKind,
    pub(super) pos: usize,
}

impl fmt::Display for PathErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathErrorKind::UnexpectedEndOf => {
                write!(f, "unexpected end of input")
            }
            PathErrorKind::MalformedString(err) => write!(f, "{}", err),
            PathErrorKind::UnexpectedCharacter { byte } => {
                if byte.is_ascii_graphic() {
                    write!(f, "unexpected character '{}'", *byte as char)
                } else {
                    write!(f, "unexpected character (0x{:02X})", byte)
                }
            }
            PathErrorKind::InvalidIndex { message } => {
                write!(f, "{}", message)
            }
            PathErrorKind::Numeric(err) => write!(f, "{}", err),
            PathErrorKind::FnExpr(err) => write!(f, "{}", err),
        }
    }
}

impl error::Error for PathError {}

impl fmt::Display for PathError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at index {}", self.kind, self.pos)
    }
}

impl From<StringError> for PathError {
    fn from(err: StringError) -> Self {
        PathError {
            kind: PathErrorKind::MalformedString(err.kind),
            pos: err.pos,
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
                message: format!("overflow, index exceeds: {}", err.bound),
            },
            pos: err.pos,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum PatchError {
    // in previous cases, we used to pass ParseErrorKind as the type to avoid having pos twice, 1
    // for the inner type and one of the outer type, but now PatchError is an enum, not a struct
    ParseError(ParseError),
    UnexpectedValue { expected: &'static str },
    // usize is the index of the operation within the array
    InvalidOp(OpError, usize),
}

impl error::Error for PatchError {}

impl fmt::Display for PatchError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PatchError::ParseError(err) => write!(f, "{}", err),
            PatchError::UnexpectedValue { expected } => {
                write!(f, "expected {}", expected)
            }
            PatchError::InvalidOp(err, index) => {
                write!(f, "operation {} failed: {}", index, err)
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum OpError {
    MissingMember {
        member: &'static str,
    },
    Pointer(PointerError),
    UnexpectedValue {
        field: &'static str,
        expected: &'static str,
    },
    PathNotFound {
        path: String,
        depth: usize,
    },
    IndexOutOfBounds {
        path: String,
        depth: usize,
        index: usize,
        len: usize,
    },
    InvalidRootOp {
        op: &'static str,
    },
    NotEqual,
}

impl fmt::Display for OpError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OpError::MissingMember { member } => {
                write!(f, "missing required member '{}'", member)
            }
            OpError::Pointer(err) => write!(f, "{}", err),
            OpError::UnexpectedValue { field, expected } => {
                write!(f, "expected {} for '{}'", expected, field)
            }
            OpError::PathNotFound { path, depth } => {
                write!(f, "path '{}' not found at depth {}", path, depth)
            }
            OpError::IndexOutOfBounds {
                path,
                depth,
                index,
                len,
            } => {
                write!(
                    f,
                    "index {} out of bounds (length {}) for path '{}' at depth {}",
                    index, len, path, depth
                )
            }
            OpError::InvalidRootOp { op } => {
                write!(f, "cannot {} root document", op)
            }
            OpError::NotEqual => {
                write!(f, "values not equal")
            }
        }
    }
}

impl From<ParseError> for PatchError {
    fn from(err: ParseError) -> Self {
        PatchError::ParseError(err)
    }
}

impl From<PointerError> for OpError {
    fn from(err: PointerError) -> Self {
        OpError::Pointer(err)
    }
}
