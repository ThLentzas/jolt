use crate::parsing::error::{StringError, StringErrorKind};
use crate::parsing::{escapes, utf8};
use crate::parsing::value::error::{PathError, PathErrorKind};

// fn read_name_shorthand(buffer: &[u8], pos: &mut usize) -> Result<String, PathError>
// fn read_double(buffer: &[u8], pos: &mut usize) -> Result<String, StringError>
// fn read_single(buffer: &[u8], pos: &mut usize) -> Result<String, StringError>
// fn read_name(buffer: &[u8], pos: &mut usize) -> Result<String, StringError>
// fn read_bracketed_selection(buffer: &[u8], pos: &mut usize, kind: SegmentKind) -> Result<Segment, PathError>
// fn read_descendant_seg(buffer: &[u8], pos: &mut usize) -> Result<Segment, PathError>
// fn read_child_seg(buffer: &[u8], pos: &mut usize) -> Result<Segment, PathError>
// pub(super) fn read_seg(buffer: &[u8], pos: &mut usize) -> Result<Segment, PathError>
//
// Initially I had the above structure which made it obvious that I needed a struct; both passing
// the struct as argument or implement the methods for the struct is fine.
pub(super) struct Query<'a> {
    buffer: &'a [u8],
    pos: usize
}

#[derive(Debug, PartialEq)]
pub(super) struct Segment {
    kind: SegmentKind,
    selectors: Vec<Selector>
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub(super)enum SegmentKind {
    Child,
    Descendant
}

#[derive(Debug, PartialEq)]
pub(super) enum Selector {
    Name(String),
    WildCard,
    Index,
    ArraySlice { start: Option<i32>, end: Option<i32>, step: Option<i32> },
    FilterExpression
}

impl Segment {
    fn new(kind: SegmentKind) -> Self {
        Self { kind, selectors: Vec::new() }
    }
}

impl<'a> Query<'a> {
    pub(super) fn new(buffer: &'a [u8]) -> Self {
        Self { buffer, pos: 0 }
    }

    // we have already checked for empty input
    // the query(json path expression) is not a json string like pointer, which means that we won't
    // check a Unicode sequence could map to '$'
    pub(super) fn check_root(&mut self) -> Result<(), PathError> {
        let root = self.buffer[self.pos];
        if root != b'$' {
            return Err(PathError { kind: PathErrorKind::UnexpectedCharacter { byte: root }, pos: self.pos });
        }

        self.pos += 1;
        Ok(())
    }

    pub(super) fn read_seg(&mut self) -> Result<Option<Segment>, PathError> {
        if self.pos >= self.buffer.len() {
            return Ok(None);
        }

        let current = self.buffer[self.pos];
        let next = self.buffer.get(self.pos + 1);

        match(current, next) {
            (b'.', Some(b'.')) if self.buffer.get(self.pos + 2).is_none() => Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos + 1 }),
            (b'.', Some(b'.')) => Ok(Some(self.read_descendant_seg()?)),
            (b'.' | b'[', None) => Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos }),
            (b'.' | b'[', Some(_)) => Ok(Some(self.read_child_seg()?)),
            // a segment always starts with '.', '..' or '['
            _ => Err(PathError { kind: PathErrorKind::UnexpectedCharacter { byte: current }, pos: self.pos })
        }
    }

    // child-segment = bracketed-selection / ("." (wildcard-selector / member-name-shorthand))
    // we have either bracketed selection or '.' followed by either a wildcard or a member-name-shorthand
    fn read_child_seg(&mut self) -> Result<Segment, PathError> {
        self.read_notation(SegmentKind::Child)
    }

    // descendant-segment  = ".." (bracketed-selection / wildcard-selector / member-name-shorthand)
    // we have 2 dots(..) followed by either a bracketed-selection or a wildcard or a member name shorthand
    fn read_descendant_seg(&mut self) -> Result<Segment, PathError> {
        self.pos += 2; // move past ..
        self.read_notation(SegmentKind::Descendant)
    }

    fn read_notation(&mut self, kind: SegmentKind) -> Result<Segment, PathError> {
        if self.buffer[self.pos] == b'[' {
            Ok(self.read_bracket(kind)?)
        } else {
            Ok(self.read_shorthand(kind)?)
        }
    }

    // also called bracketed-selection
    fn read_bracket(&mut self, kind: SegmentKind) -> Result<Segment, PathError> {
        self.pos += 1;// move past '['
        let len = self.buffer.len();
        let mut segment = Segment::new(kind);

        while self.pos < self.buffer.len() {
            crate::parsing::skip_whitespaces(self.buffer, &mut self.pos);
            let current = self.buffer.get(self.pos);

            match current {
                Some(c) if *c == b'\'' || *c == b'\"' => {
                    let name = self.read_name()?;
                    segment.selectors.push(Selector::Name(name));
                }
                None => return Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos - 1 }),
                _ => return Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos - 1 }), //toDo: these should be the remaining cases of selectors
            }
            crate::parsing::skip_whitespaces(self.buffer, &mut self.pos);

            if self.pos < len && self.buffer[self.pos] == b','{
                self.pos += 1;
            }
            if self.pos < len && self.buffer[self.pos] == b']'{
                self.pos += 1;
                break;
            }
            self.pos += 1;
        }
        Ok(segment)
    }

    // when this method gets called pos is at the 1st character after . or ..
    // member name shorthand is still a name selector with more strict syntax
    fn read_shorthand(&mut self, kind: SegmentKind) -> Result<Segment, PathError> {
        let current = self.buffer[self.pos];
        let mut segment = Segment::new(kind);

        match current {
            b'*' => segment.selectors.push(Selector::WildCard),
            _ => {
                let name = self.read_name_shorthand()?;
                segment.selectors.push(Selector::Name(name));
            }
        }
        Ok(segment)
    }

    // We never check if name exceeds the StringValueLengthLimit because even if it does, no key
    // will ever map to that and we will return None
    fn read_name(&mut self) -> Result<String, StringError> {
        if self.buffer[self.pos] == b'"' { Ok(self.read_double()?) } else { Ok(self.read_single()?) }
    }

    // create a JsonStringError, it is repeated in Pointer as well
    // maybe we could write a read_quoted and pass the delimiter?
    fn read_single(&mut self) -> Result<String, StringError> {
        let len = self.buffer.len();
        let mut name = String::new();
        self.pos += 1; // skip opening '

        while self.pos < len && self.buffer[self.pos] != b'\'' {
            let current = self.buffer[self.pos];
            match current {
                c if c.is_ascii_control() => return Err(StringError { kind: StringErrorKind::InvalidControlCharacter { byte: current }, pos: self.pos }),
                c if !c.is_ascii() => {
                    name.push_str(utf8::read_utf8_char(self.buffer, self.pos));
                    self.pos += utf8::utf8_char_width(current) - 1;
                }
                b'\\' => {
                    let next = self.buffer.get(self.pos + 1);
                    match next {
                        // This is the extra edge case where ' has to be escaped when the name is in single quotes
                        // Not part of the escape characters defined for json string
                        Some(b'\'') => name.push('\''),
                        _ => {
                            escapes::check_escape_character(self.buffer, self.pos)?;
                            self.pos += escapes::len(self.buffer, self.pos) - 1;
                        }
                    }
                }
                _ => name.push(current as char)
            }
            self.pos += 1;
        }
        if self.pos == len {
            return Err(StringError { kind: StringErrorKind::UnexpectedEndOf, pos: self.pos - 1 });
        }
        Ok(name)
    }

    fn read_double(&mut self) -> Result<String, StringError> {
        let len = self.buffer.len();
        let mut name = String::new();
        self.pos += 1; // skip opening '

        while self.pos < len && self.buffer[self.pos] != b'"' {
            let current = self.buffer[self.pos];
            match current {
                c if c.is_ascii_control() => return Err(StringError { kind: StringErrorKind::InvalidControlCharacter { byte: current }, pos: self.pos }),
                c if !c.is_ascii() => {
                    name.push_str(utf8::read_utf8_char(self.buffer, self.pos));
                    self.pos += utf8::utf8_char_width(current) - 1;
                }
                b'\\' => {
                    escapes::check_escape_character(self.buffer, self.pos)?;
                    self.pos += escapes::len(self.buffer, self.pos) - 1;
                }
                _ => name.push(current as char)
            }
            self.pos += 1;
        }
        if self.pos == len {
            return Err(StringError { kind: StringErrorKind::UnexpectedEndOf, pos: self.pos - 1 });
        }
        Ok(name)
    }

    // we don't have to run any utf8 validation because buffer is the underlying vector of the input query
    // and since it is &str Rust won't allow an invalid utf8 sequence; it is exactly the same as pointer
    // the 1st character of member name shorthand is a special case because it can not be a digit
    fn read_name_shorthand(&mut self) -> Result<String, PathError> {
        // term used by rfc; name_first the 1st character of member_name_shorthand
        // name_char is every other character; the only difference is that name_first can't be a digit
        let len = self.buffer.len();
        let mut name = String::new();
        let start = self.pos;
        let mut current = self.buffer[self.pos];

        // . and  [ will indicate the start of a new segment
        while self.pos < len && current != b'.' && current != b'[' {
            match current {
                c if !c.is_ascii() => name.push_str(utf8::read_utf8_char(self.buffer, self.pos)),
                // name_first
                c if start == self.pos => {
                    if !is_valid_name_first(c) || c.is_ascii_digit() {
                        return Err(PathError { kind: PathErrorKind::InvalidNameShorthandCharacter { byte: c }, pos: self.pos });
                    }
                    name.push(c as char);
                    self.pos += 1;
                }
                c => {
                    if !is_valid_name_char(c) {
                        return Err(PathError { kind: PathErrorKind::InvalidNameShorthandCharacter { byte: c }, pos: self.pos });
                    }
                    name.push(c as char);
                    self.pos += 1;
                }
            }
            if self.pos < len {
                current = self.buffer[self.pos];
            }
        }

        Ok(name)
    }
}

// !matches!(byte, 0x00..=0x1F | 0x20..=0x2F | 0x3A..=0x40 | 0x5C..=0x5E | 0x60 | 0x7B..=0x7E)
// the above one would work as well, it covers all the cases mentioned in the not_allowed section
// in the comment above
fn is_valid_name_first(byte: u8) -> bool {
    byte.is_ascii_alphabetic() || byte == b'_'
}

// name-char = name-first / DIGIT
fn is_valid_name_char(byte: u8) -> bool {
    is_valid_name_first(byte) || byte.is_ascii_digit()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn invalid_queries() -> Vec<(&'static str, PathError)> {
        vec![
            ("$.", PathError { kind: PathErrorKind::UnexpectedEndOf, pos: 1 }),
            ("$[", PathError { kind: PathErrorKind::UnexpectedEndOf, pos: 1 }),
            ("$..", PathError { kind: PathErrorKind::UnexpectedEndOf, pos: 2 }),
            ("$a", PathError { kind: PathErrorKind::UnexpectedCharacter { byte: b'a' }, pos: 1 }),
        ]
    }

    #[test]
    fn test_invalid_queries() {
        for (path_expr, err) in invalid_queries() {
            let mut query = Query::new(path_expr.as_bytes());
            query.pos += 1;// this happens by calling check_root() but we simplify it for this case
            let result = query.read_seg();

            assert_eq!(result, Err(err), "invalid path_expr: {path_expr}")
        }
    }

    #[test]
    fn invalid_start() {
        let path_expr = "a";
        let mut query = Query::new(path_expr.as_bytes());
        let result = query.check_root();

        assert_eq!(result, Err(PathError { kind: PathErrorKind::UnexpectedCharacter { byte: b'a' }, pos: 0 }));
    }

}