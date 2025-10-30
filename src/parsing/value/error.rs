use std::error::Error;
use std::fmt;
use std::fmt::{Display, Formatter};
use crate::parsing::error::StringError;

#[derive(Debug, PartialEq)]
pub struct PointerError {
    pub kind: PointerErrorKind,
    pub pos: usize
}

#[derive(Debug, PartialEq)]
pub enum PointerErrorKind {
    InvalidPathSyntax,
    MalformedString(StringError),
    UnexpectedEof,
    InvalidIndex { message: String },// can't use &'static str, we concatenate the index syntax
}

impl PointerError {
    pub(super) fn new(kind: PointerErrorKind, pos: usize) -> Self {
        PointerError { kind, pos }
    }
}

impl fmt::Display for PointerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            // macros like write!, println! and so on do autoderef
            PointerErrorKind::InvalidPathSyntax => write!(f, "pointer paths must be prefixed with '/'syntax at index {}", self.pos),
            PointerErrorKind::MalformedString(err) => write!(f, "{}", err),
            PointerErrorKind::UnexpectedEof => write!(f, "unexpected end of input at index {}", self.pos),
            PointerErrorKind::InvalidIndex { message } => write!(f, "{} at index {}", message, self.pos),
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
        PointerError { kind: PointerErrorKind::MalformedString(err), pos }
    }
}

#[derive(Debug, PartialEq)]
pub struct PathError {
    pub kind: PathErrorKind,
    pub pos: usize
}

#[derive(Debug, PartialEq)]
pub enum PathErrorKind {
    InvalidNameShorthandCharacter { byte: u8 },
    UnexpectedEndOf,
    MalformedString(StringError),
    UnexpectedCharacter { byte: u8 },
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
        PathError { kind: PathErrorKind::MalformedString(err), pos }
    }
}

impl Error for PointerError {}

impl Display for PathError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl Error for PathError {}