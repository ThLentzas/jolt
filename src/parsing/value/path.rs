use crate::parsing::error::{StringError, StringErrorKind};
use crate::parsing::{self, escapes, number, utf8};
use crate::parsing::value::error::{PathError, PathErrorKind};
use std::cmp;

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

// considering caching for each value, like a map where we store path/pointer -> value but we need a way to consider
// if the user mutated val, maybe not possible ?
#[derive(Debug, PartialEq)]
pub(super) struct Segment {
    pub(super) kind: SegmentKind,
    pub(super) selectors: Vec<Selector>
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub(super)enum SegmentKind {
    Child,
    Descendant
}

// this is similar to NumberState, otherwise we would have to pass Option<..> 3 times every time we
// need a slice
#[derive(Debug, PartialEq)]
pub(super) struct Slice {
    pub(super) start: Option<i64>,
    pub(super) end: Option<i64>,
    pub(super) step: Option<i64>
}

#[derive(Debug, PartialEq)]
pub(super) enum Selector {
    Name(String),
    WildCard,
    // i64 covers the range [-(2^53)+1, (2^53)-1], we don't need to use Number
    Index(i64),
    ArraySlice(Slice),
    FilterExpression
}

#[derive(Debug, PartialEq)]
pub(super) struct Range {
    // current can become negative when traversing from right to left(negative step)
    // current = 1, step = -2 -> current becomes -1
    pub(super) current: isize,
    // end can also be negative when we iterate in reverse order the lower bound is exclusive
    // len = 5, current = 4, end = -1, step = -1 gives us all elements in reverse order (-1, 4]
    pub(super) end: isize,
    pub(super) step: i64
}

impl Range {
    // negative indices on slices
    // len = 10, start = -1, end = -6, step = 1
    //
    // first we do normalization: n_start = 9, n_end = 4
    //
    // then we have to address the issue mentioned on index where if the number is negative and its
    // absolute value is greater than the length of the array, len + i still results in a negative
    // number; for index selectors that is just out of bounds but for range we need to adjust it
    //
    // setting the bounds:
    //
    //  lower = min(max(9, 0), 10) => 9
    //  upper = min(max(4, 0), 10) => 4
    //
    //  in this case where lower > upper we return an empty vector
    //
    //  len = 5, start = -60, end = 70
    //  n_start = -55, n_end = 65
    //
    //  lower = min(max(-55, 0), 10) => 0
    //  upper = min(max(-65, 0), 10) => 10
    //
    // what we observe is that when n_start, n_end is < 0, n_start rounds up to 0 and n_end rounds
    // down to len; logic is reversed when step is negative
    //
    pub(super) fn new(slice: &Slice, len: i64)  -> Self {
        let start;
        let end;
        // equivalent to: if step.is_none() { 1 } else { step.unwrap() }
        let step = slice.step.unwrap_or(1);

        if step >= 0 {
            start = slice.start.unwrap_or(0);
            end = slice.end.unwrap_or(len);
        } else {
            start = slice.start.unwrap_or(len - 1);
            end = slice.end.unwrap_or(-len - 1);
        }

        let n_start = normalize_index(start, len);
        let n_end = normalize_index(end, len);
        let lower;
        let upper;

        if step >= 0 {
            lower = cmp::min(cmp::max(n_start, 0), len);
            upper = cmp::min(cmp::max(n_end, 0), len);
        } else {
            upper = cmp::min(cmp::max(n_start, -1), len - 1);
            lower = cmp::min(cmp::max(n_end, -1), len - 1);
        }

        Self {
            // if step is negative, the first element we visit is at upper, otherwise is lower
            current: if step >= 0 { lower as isize } else { upper as isize },
            // end keeps track of where our range ends, if step is negative it is the lower bound else the upper
            end: if step >= 0 { upper as isize } else { lower as isize },
            step
        }
    }
}

impl Iterator for Range {
    // why an associated type instead of a generic? https://www.youtube.com/watch?v=yozQ9C69pNs
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        if self.step == 0 {
            return None;
        }

        // in the left to right case, the upper bound is exclusive
        if self.step > 0 && self.current >= self.end {
            return None;
        }

        // in the right to left case, the lower bound is exclusive
        if self.step < 0 && self.current <= self.end {
            return None;
        }

        let index = self.current;
        self.current += self.step as isize;
        Some(index as usize)
    }
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
    // check if a Unicode sequence could map to '$'
    // jsonpath-query = root-identifier *(S segment)
    //
    // the syntax means it must start with the root identifier('$') followed by zero or more segments
    pub(super) fn check_root(&mut self) -> Result<(), PathError> {
        let root = self.buffer[self.pos];

        if root != b'$' {
            return Err(PathError { kind: PathErrorKind::UnexpectedCharacter { byte: root }, pos: self.pos });
        }

        self.pos += 1; // move past '$'
        parsing::skip_whitespaces(self.buffer, &mut self.pos);
        // whitespaces are allowed after '$' only when followed by a segment
        // " $", "$ " neither is allowed
        if self.pos >= self.buffer.len() {
            return Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos - 1 });
        }
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
            (b'.', Some(b'.')) => {
                // descendant-segment  = ".." (bracketed-selection / wildcard-selector / member-name-shorthand)
                // we have 2 dots(..) followed by either a bracketed-selection or a wildcard or a member name shorthand
                // move past ..
                self.pos += 2;
                Ok(Some(self.read_notation(SegmentKind::Descendant)?))
            },
            (b'.' | b'[', None) => Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos }),
            // child-segment = bracketed-selection / ("." (wildcard-selector / member-name-shorthand))
            // we have either bracketed selection or '.' followed by either a wildcard or a member-name-shorthand
            (b'.' | b'[', Some(_)) => Ok(Some(self.read_notation(SegmentKind::Child)?)),
            // a segment always starts with '.', '..' or '['
            _ => Err(PathError { kind: PathErrorKind::UnexpectedCharacter { byte: current }, pos: self.pos })
        }
    }

    fn read_notation(&mut self, kind: SegmentKind) -> Result<Segment, PathError> {
        if self.buffer[self.pos] == b'[' {
            Ok(self.read_bracket(kind)?)
        } else {
            Ok(self.read_shorthand(kind)?)
        }
    }

    // also called bracketed-selection
    //
    // rfc syntax: bracketed-selection = "[" S selector *(S "," S selector) S "]"
    // S = *B (optional blank space, zero or more)
    // B = '\t', '\n', '\r', ' '
    fn read_bracket(&mut self, kind: SegmentKind) -> Result<Segment, PathError> {
        let len = self.buffer.len();
        let mut segment = Segment::new(kind);
        self.pos += 1;// move past '[';

        while self.pos < len && self.buffer[self.pos] != b']'{
            segment.selectors.push(self.read_selector()?);
            parsing::skip_whitespaces(self.buffer, &mut self.pos);

            match self.pos < len {
                true if self.buffer[self.pos] == b']' => {
                    // skip pass the closing bracket ']'
                    self.pos += 1;
                    break;
                }
                // after processing selector if we don't encounter ']', we expect comma to separate
                // multiple selectors.
                true if self.buffer[self.pos] != b',' => return Err(PathError { kind: PathErrorKind::UnexpectedCharacter {
                    byte: self.buffer[self.pos] },
                    pos: self.pos
                }),
                // got a comma skip it
                true => self.pos += 1,
                // we didn't encounter ']'
                false => return Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos - 1 }),
            }
        }

        // 1 edge case to consider: [<selector>,]
        // at this point we exited the loop  because we encountered ']' but if the exact previous
        // character is ',' we have invalid syntax; after comma we expect another selector
        // this is different from [<selector>,+S]
        // if we have at least 1 whitespace read_selector() would skip it and then try to read ']'
        // which is not a valid start for any selector
        if self.buffer[self.pos - 1] == b',' {
            return Err(PathError { kind: PathErrorKind::UnexpectedCharacter { byte: self.buffer[self.pos] }, pos: self.pos });
        }

        Ok(segment)
    }

    // gets called from read_bracket after '[' or ','
    fn read_selector(&mut self) -> Result<Selector, PathError> {
        parsing::skip_whitespaces(self.buffer, &mut self.pos);

        match self.buffer.get(self.pos) {
            Some(c) if *c == b'\'' || *c == b'\"' => Ok(Selector::Name(self.read_name()?)),
            Some(b'*') => {
                self.pos += 1;
                Ok(Selector::WildCard)
            },
            Some(b'-' | b'0'..=b'9' | b':') => self.read_numeric(),
            None => Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos - 1 }),
            Some(n) => Err(PathError { kind: PathErrorKind::UnexpectedCharacter { byte: *n }, pos: self.pos })
        }
    }

    // when we encounter ':' it is a slice selector, but for '-' or any digit we can't know yet, we
    // have to process the number and then check if we encounter ':'; if we do, we call read_slice()
    // otherwise it is an index
    fn read_numeric(&mut self) -> Result<Selector, PathError> {
        let num;

        if self.buffer[self.pos] == b':' {
            return self.read_slice(None);
        }

        num = self.read_index()?;
        // from the slice syntax: start *S ":" *S end *S ":" *S step  we need to skip whitespaces
        // between first index and ':'. If we don't encounter ':' we have an index case not a slice
        parsing::skip_whitespaces(self.buffer, &mut self.pos);
        if self.pos < self.buffer.len() && self.buffer[self.pos] == b':' {
            return self.read_slice(Some(num));
        }
        Ok(Selector::Index(num))
    }

    // when this method gets called self.buffer[self.pos] is at ':'
    // rfc syntax: start *S ":" *S end *S ":" *S step
    fn read_slice(&mut self, start: Option<i64>) -> Result<Selector, PathError> {
        let len = self.buffer.len();
        let mut step: Option<i64> = Some(1);// default step value
        let mut end: Option<i64> = None;
        // skip the first ':'(we encountered that during the read_numeric() and read_slice() was called)
        self.pos += 1;

        parsing::skip_whitespaces(self.buffer, &mut self.pos);

        // 1: or :
        if self.pos >= len {
            return Ok(Selector::ArraySlice(Slice { start, end, step }));
        }

        // the cases where end and step are omitted(: or 1:) still need a right square bracket
        // after to be valid [:] or [1:] and those will be checked when control returns to the
        // read_bracket(), the problem is not with the slice itself rather that with the bracket
        // syntax
        match self.buffer[self.pos] {
            // 0:: or ::
            b':' => self.pos += 1,
            // set end when present
            c if c == b'-' || c.is_ascii_digit() => end = Some(self.read_index()?),
            _ =>  return Ok(Selector::ArraySlice(Slice { start, end, step })),
        }

        parsing::skip_whitespaces(self.buffer, &mut self.pos);
        // 1::, ::, 1:5
        if self.pos > len {
            return Ok(Selector::ArraySlice(Slice { start, end, step }));
        }
        // 1:2: or :2:
        if end.is_some() && self.buffer[self.pos] == b':'{
            self.pos += 1; // skip the second ':'
            parsing::skip_whitespaces(self.buffer, &mut self.pos);
        }

        match self.buffer.get(self.pos) {
            // set step when present
            Some(c) if *c == b'-' || c.is_ascii_digit() => step = Some(self.read_index()?),
            // 1::, 1::(*S any number of whitespaces), 1:2 or 1:2(*S), similar to the : and 1: cases
            // above all are valid slice selectors it is the bracket syntax that is incorrect
            _ => return Ok(Selector::ArraySlice(Slice { start, end, step })),
        }
        Ok(Selector::ArraySlice(Slice { start, end, step }))
    }

    fn read_index(&mut self) -> Result<i64, PathError> {
        let current = self.buffer[self.pos];
        let next = self.buffer.get(self.pos + 1);

        match (current, next) {
            // -a
            (b'-', Some(n)) if !n.is_ascii_digit() || *n == b'0' => return Err(PathError { kind: PathErrorKind::InvalidIndex {
                message: "minus sign must be followed by a non-zero digit"},
                pos: self.pos
            }),
            // -
            (b'-', None) =>  return Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos }),
            (b'0', Some(n)) if n.is_ascii_digit() => return Err(PathError { kind: PathErrorKind::InvalidIndex {
                message: "leading zeros are not allowed"},
                pos: self.pos
            }),
            _ => ()
        }
        Ok(number::atoi(self.buffer, &mut self.pos)?)
    }

    // when this method gets called pos is at the 1st character after . or ..
    // member name shorthand is still a name selector with more strict syntax
    fn read_shorthand(&mut self, kind: SegmentKind) -> Result<Segment, PathError> {
        self.pos += 1;
        let current = self.buffer[self.pos];
        let mut segment = Segment::new(kind);

        match current {
            b'*' => {
                segment.selectors.push(Selector::WildCard);
                self.pos += 1;
            },
            _ => segment.selectors.push(Selector::Name(self.read_name_shorthand()?))
        }
        Ok(segment)
    }

    // We never check if name exceeds the StringValueLengthLimit because even if it does, no key
    // will ever map to that and we will return None
    // toDo: when we check if the name exists we DO NOT do any normalization prior to comparison
    // rfc forbids it, we MUST check the underlying Unicode scalar values that they are the exact same
    fn read_name(&mut self) -> Result<String, StringError> {
        let quote = self.buffer[self.pos];
        let len = self.buffer.len();
        let mut name = String::new();
        self.pos += 1; // skip opening quote

        while self.pos < len && self.buffer[self.pos] != quote {
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
                        Some(b'\'') if quote == b'\''=> {
                            name.push('\'');
                            // pos is at \, next is ', move pos to ' so when we exit this arm and
                            // self.pos += 1 will move pos past '. If we don't do this increment
                            // pos would be moved to after exiting the arm, and our loop will end
                            // because incorrectly will treat the escaped ' as a closing quote
                            self.pos += 1;
                        },
                        _ => {
                            escapes::check_escape_character(self.buffer, self.pos)?;
                            name.push(escapes::map_escape_character(self.buffer, self.pos));
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
        self.pos += 1;// skip closing quote

        Ok(name)
    }

    // we don't have to run any utf8 validation because buffer is the underlying vector of the input query
    // and since it is &str Rust won't allow an invalid utf8 sequence; it is exactly the same as pointer
    // the 1st character of member name shorthand is a special case because it can not be a digit
    fn read_name_shorthand(&mut self) -> Result<String, PathError> {
        // term used by rfc; name_first, the 1st character of member_name_shorthand
        // name_char is every other character; the only difference is that name_first can't be a digit
        let len = self.buffer.len();
        let mut name = String::new();
        let start = self.pos;
        let mut current = self.buffer[self.pos];

        // . and  [ will indicate the start of a new segment
        while self.pos < len && current != b'.' && current != b'[' {
            match current {
                c if !c.is_ascii() => {
                    name.push_str(utf8::read_utf8_char(self.buffer, self.pos));
                    self.pos += utf8::utf8_char_width(current) - 1;
                },
                // name_first check
                //
                // name_first/char can be any non ascii character in the range 0x80..=0xD7FF | 0xE000..=0x10FFFF
                // but in is_valid_name_first() we only check: byte.is_ascii_alphabetic() || byte == b'_', this
                // is because we already checked for a non-ascii character, it is the 1st arm so no matter if it
                // is the first character or not it will always be valid.
                //
                // if we didn't follow this logic we would have to pass the 1st character to the function not
                // just the byte value
                c if start == self.pos => {
                    if !is_valid_name_first(c) || c.is_ascii_digit() {
                        return Err(PathError { kind: PathErrorKind::InvalidNameShorthandSyntax { byte: c }, pos: self.pos });
                    }
                    name.push(c as char);
                }
                c => {
                    if !is_valid_name_char(c) {
                        return Err(PathError { kind: PathErrorKind::InvalidNameShorthandSyntax { byte: c }, pos: self.pos });
                    }
                    name.push(c as char);
                }
            }
            self.pos += 1;
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

pub(super) fn normalize_index(index: i64, len: i64) -> i64 {
    if index >= 0 { index } else { len + index }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn invalid_root() -> Vec<(&'static str, PathError)> {
        vec![
            // mismatch
            (" ", PathError { kind: PathErrorKind::UnexpectedCharacter { byte: b' ' }, pos: 0 }),
            // whitespaces not followed by a segment
            ("$ \n\r", PathError { kind: PathErrorKind::UnexpectedEndOf, pos: 3 }),
        ]
    }

    fn invalid_notations() -> Vec<(&'static str, PathError)> {
        vec![
            ("$.", PathError { kind: PathErrorKind::UnexpectedEndOf, pos: 1 }),
            ("$[", PathError { kind: PathErrorKind::UnexpectedEndOf, pos: 1 }),
            ("$..", PathError { kind: PathErrorKind::UnexpectedEndOf, pos: 2 }),
            ("$a", PathError { kind: PathErrorKind::UnexpectedCharacter { byte: b'a' }, pos: 1 }),
        ]
    }

    // Name selectors return member names which are json strings. We have already covered the invalid
    // cases in lexer.rs(invalid_strings())
    fn invalid_name_selectors() -> Vec<(&'static str, PathError)> {
        vec![
            // name-first from name-shorthand notation can not start with a digit
            ("$.2a", PathError { kind: PathErrorKind::InvalidNameShorthandSyntax { byte: b'2' }, pos: 2 }),
            // name-char from name-shorthand notation can not contain punctuation symbols
            ("$.f!o", PathError { kind: PathErrorKind::InvalidNameShorthandSyntax { byte: b'!' }, pos: 3 }),
            // unterminated single quoted string
            ("$['foo]", PathError::from(StringError { kind: StringErrorKind::UnexpectedEndOf, pos: 6 })),
            ("$[\"foo]", PathError::from(StringError { kind: StringErrorKind::UnexpectedEndOf, pos: 6 })),
        ]
    }

    fn invalid_indices() -> Vec<(&'static str, PathError)> {
        vec![
            ("$[-", PathError { kind: PathErrorKind::UnexpectedEndOf, pos: 2 }),
            ("$[-a]", PathError { kind: PathErrorKind::InvalidIndex { message: "minus sign must be followed by a non-zero digit" }, pos: 2 }),
            ("$[-01]", PathError { kind: PathErrorKind::InvalidIndex { message: "minus sign must be followed by a non-zero digit" }, pos: 2 }),
            ("$[02]", PathError { kind: PathErrorKind::InvalidIndex { message: "leading zeros are not allowed" }, pos: 2 }),
            ("$[-9223372036854775809]", PathError { kind: PathErrorKind::InvalidIndex { message: "number out of range" }, pos: 3 }),
            ("$[9223372036854775808]", PathError { kind: PathErrorKind::InvalidIndex { message: "number out of range" }, pos: 2 }),
        ]
    }

    #[test]
    fn test_invalid_notations() {
        for (path_expr, err) in invalid_notations() {
            let mut query = Query::new(path_expr.as_bytes());
            query.pos += 1;// this happens by calling check_root() but we simplify it for this case
            let result = query.read_seg();

            assert_eq!(result, Err(err), "invalid path_expr: {path_expr}")
        }
    }

    #[test]
    fn test_invalid_root() {
        for(path_expr, err) in invalid_root() {
            let mut query = Query::new(path_expr.as_bytes());
            let result = query.check_root();

            assert_eq!(result, Err(err));
        }
    }

    #[test]
    fn test_invalid_name_selectors() {
        for (path_expr, err) in invalid_name_selectors() {
            let mut query = Query::new(path_expr.as_bytes());
            query.pos += 1;// this happens by calling check_root() but we simplify it for this case
            let result = query.read_seg();

            assert_eq!(result, Err(err), "invalid path_expr: {path_expr}")
        }
    }

    #[test]
    fn invalid_bracketed_selection_syntax() {
        let path_expr = "$['foo''bar']";
        let mut query = Query::new(path_expr.as_bytes());
        query.pos += 1;// this happens by calling check_root() but we simplify it for this case
        let result = query.read_seg();

        assert_eq!(result, Err(PathError { kind: PathErrorKind::UnexpectedCharacter { byte: b'\'' }, pos: 7 }));
    }

    #[test]
    fn test_invalid_index_selectors() {
        for (path_expr, err) in invalid_indices() {
            let mut query = Query::new(path_expr.as_bytes());
            query.pos += 1;// this happens by calling check_root() but we simplify it for this case
            let result = query.read_seg();

            assert_eq!(result, Err(err), "invalid path_expr: {path_expr}")
        }
    }
}