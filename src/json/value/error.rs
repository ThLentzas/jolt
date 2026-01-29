use crate::ParseError;
use crate::json::error::{KeywordError, KeywordErrorKind, StringError, StringErrorKind};
use crate::json::number::{NumericErrorKind, OutOfRangeError};
use crate::json::value::path::filter::function::FnExprError;
use std::{error, fmt};

#[derive(Debug, PartialEq,Eq)]
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

/// Represents an error that can occur when parsing malformed JSON Pointer paths.
#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub(super) enum PathErrorKind {
    UnexpectedEof,
    MalformedString(StringErrorKind),
    UnexpectedCharacter { byte: u8 },
    // index/slice selectors
    InvalidIndex { message: String },
    // can occur when we try to parse literals
    Numeric(NumericErrorKind),
    // we use map_err() to convert
    FnExpr(FnExprError),
}

/// Represents an error that can occur when parsing JSON Path expressions.
#[derive(Debug, PartialEq, Eq)]
pub struct PathError {
    pub(super) kind: PathErrorKind,
    pub(super) pos: usize,
}

impl fmt::Display for PathErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathErrorKind::UnexpectedEof => {
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
            KeywordErrorKind::UnexpectedEof => PathError {
                kind: PathErrorKind::UnexpectedEof,
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

#[derive(Debug, PartialEq, Eq)]
pub(super) enum OpError {
    MissingMember {
        member: &'static str,
    },
    Pointer(PointerError),
    UnexpectedField {
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

impl error::Error for OpError {}

impl fmt::Display for OpError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OpError::MissingMember { member } => {
                write!(f, "missing required member '{}'", member)
            }
            OpError::Pointer(err) => write!(f, "{}", err),
            OpError::UnexpectedField { field, expected } => {
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

impl From<PointerError> for OpError {
    fn from(err: PointerError) -> Self {
        OpError::Pointer(err)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(super) enum PatchErrorKind {
    // in previous cases, we used to pass ParseErrorKind as the type to avoid having pos twice, 1
    // for the inner type and one of the outer type, but now PatchError is an enum, not a struct
    ParseError(ParseError),
    UnexpectedValue { expected: &'static str },
    InvalidOp(OpError),
}

impl fmt::Display for PatchErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PatchErrorKind::ParseError(err) => write!(f, "{}", err),
            PatchErrorKind::UnexpectedValue { expected } => {
                write!(f, "expected {}", expected)
            }
            // Just print the underlying OpError.
            // The "operation X failed" prefix is now handled by the parent struct.
            PatchErrorKind::InvalidOp(err) => write!(f, "{}", err),
        }
    }
}

/// Represents an error that can occur when processing JSON Patch operations.
#[derive(Debug, PartialEq, Eq)]
pub struct PatchError {
    pub(super) kind: PatchErrorKind,
    // usize is the index of the operation within the array
    pub(super) index: Option<usize>
}

impl error::Error for PatchError {}

impl fmt::Display for PatchError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(idx) = self.index {
            // we have an index, wrap the inner error with context
            write!(f, "operation {} failed: {}", idx, self.kind)
        } else {
            // no index, just delegate to the inner error
            write!(f, "{}", self.kind)
        }
    }
}
