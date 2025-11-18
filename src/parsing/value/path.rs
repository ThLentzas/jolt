use crate::parsing::error::{StringError, StringErrorKind};
use crate::parsing::{self, escapes, number, utf8};
use crate::parsing::value::error::{PathError, PathErrorKind};
use std::cmp;
use std::collections::VecDeque;
use crate::parsing::value::path::filter::{Comparable, ComparisonExpr, ComparisonOp, EmbeddedQuery, LogicalExpression, QueryType, TestExpr, Type};
use crate::parsing::value::Value;

// toDo: as_pointer() -> converts an npath to pointer

mod filter;

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
pub(super) struct Query<'a, 'v> {
    buffer: &'a [u8],
    pos: usize,
    depth: i64,
    root: &'v Value,
    // the values need to live as long as the root value
   pub(super) nodelist: VecDeque<&'v Value>
}

// considering caching for each value, like a map where we store path/pointer -> value, but we need a way to consider
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
    Slice(Slice),
    Filter(LogicalExpression)
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

impl<'a, 'v> Query<'a, 'v> {
    pub(super) fn new(buffer: &'a [u8], root: &'v Value) -> Self {
        let mut nodelist = VecDeque::new();
        nodelist.push_back(root);

        Self {
            buffer,
            pos: 0,
            depth: 0,
            root,
            nodelist
        }
    }

    pub(super) fn parse(&mut self) -> Result<(), PathError> {
        self.check_root()?;

        // note that this is the only place that we get an error, when we process the expression,
        // any other case, calling a selector to a value that can't be applied we return an empty list
        while let Some(segment) = self.read_seg()? {
            match segment.kind {
                SegmentKind::Child => self.apply_child_seg(&segment),
                SegmentKind::Descendant => self.apply_descendant_seg(&segment)
            }
        }
        Ok(())
    }

    // we have already checked for empty input
    // the query(json path expression) is not a json string like pointer, which means that we won't
    // check if a Unicode sequence could map to '$'
    // jsonpath-query = root-identifier *(S segment)
    //
    // the syntax means it must start with the root identifier('$') followed by zero or more segments
    fn check_root(&mut self) -> Result<(), PathError> {
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

    fn read_seg(&mut self) -> Result<Option<Segment>, PathError> {
        // before reading the next segment skip the whitespaces
        parsing::skip_whitespaces(self.buffer, &mut self.pos);

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
            // toDo: explain how we parse subqueries
            (b']', _) if !self.depth > 0 => Ok(None) ,
            // a segment always starts with '.', '..' or '['
            _ => Err(PathError { kind: PathErrorKind::UnexpectedCharacter { byte: current }, pos: self.pos })
        }
    }

    fn read_notation(&mut self, kind: SegmentKind) -> Result<Segment, PathError> {
        let res;

        if self.buffer[self.pos] == b'[' {
            self.depth += 1;
            res = Ok(self.read_bracket(kind)?);
            self.depth -= 1;
        } else {
            res = Ok(self.read_shorthand(kind)?);
        }
        res
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
                true if self.buffer[self.pos] != b',' => return Err(PathError {
                    kind: PathErrorKind::UnexpectedCharacter { byte: self.buffer[self.pos] },
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
            _ => {
                // we can not have shorthand notation inside a bracketed selection
                // if we need either a wildcard, or a name selector we need to use the full syntax
                let name = self.read_name_shorthand(|c| {
                    matches!(c, | b'.' | b'[' ) || parsing::is_rfc_whitespace(c)
                })?;
                segment.selectors.push(Selector::Name(name));
            }
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
            Some(b'?') => self.read_filter(),
            None => Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos - 1 }),
            Some(n) => Err(PathError { kind: PathErrorKind::UnexpectedCharacter { byte: *n }, pos: self.pos })
        }
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
                c if c.is_ascii_control() => return Err(StringError {
                    kind: StringErrorKind::InvalidControlCharacter { byte: current },
                    pos: self.pos
                }),
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
    //
    // we pass a predicate which is nothing but a closure; it has a specific name because it takes
    // 1 argument and returns a boolean Fn(T) -> bool. We need it because member-name shorthand has
    // different ending conditions between filter selector and name shorthand; in the 1st case we
    // need to include parenthesis and comparison operators. Otherwise, we would have to extract
    // the common logic to a method and create 2 methods with different loop conditions
    //
    // Rust's iterator API (filter, take_while, skip_while all take predicates)
    // toDo: write this comment better
    fn read_name_shorthand<P>(&mut self, predicate: P) -> Result<String, PathError>
    where
        P: Fn(u8) -> bool
    {
        // term used by rfc; name_first, the 1st character of member_name_shorthand
        // name_char is every other character; the only difference is that name_first can't be a digit
        let len = self.buffer.len();
        let mut name = String::new();
        let start = self.pos;
        let mut current = self.buffer[self.pos];

        // . and  [ will indicate the start of a new segment while ',' will indicate multi selectors
        // on the current segment
        while self.pos < len && !predicate(current) {
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
                        return Err(PathError {
                            kind: PathErrorKind::UnexpectedCharacter { byte: c },
                            pos: self.pos
                        });
                    }
                    name.push(c as char);
                }
                c => {
                    if !is_valid_name_char(c) {
                        return Err(PathError {
                            kind: PathErrorKind::UnexpectedCharacter { byte: c },
                            pos: self.pos
                        });
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

    fn read_index(&mut self) -> Result<i64, PathError> {
        let current = self.buffer[self.pos];
        let next = self.buffer.get(self.pos + 1);

        match (current, next) {
            // -a
            (b'-', Some(n)) if !n.is_ascii_digit() || *n == b'0' => return Err(PathError {
                kind: PathErrorKind::InvalidIndex {
                    message: "minus sign must be followed by a non-zero digit"
                },
                pos: self.pos
            }),
            // -
            (b'-', None) =>  return Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos }),
            (b'0', Some(n)) if n.is_ascii_digit() => return Err(PathError {
                kind: PathErrorKind::InvalidIndex { message: "leading zeros are not allowed" },
                pos: self.pos
            }),
            _ => ()
        }
        Ok(number::atoi(self.buffer, &mut self.pos)?)
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
            return Ok(Selector::Slice(Slice { start, end, step }));
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
            _ =>  return Ok(Selector::Slice(Slice { start, end, step })),
        }

        parsing::skip_whitespaces(self.buffer, &mut self.pos);
        // 1::, ::, 1:5
        if self.pos > len {
            return Ok(Selector::Slice(Slice { start, end, step }));
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
            _ => return Ok(Selector::Slice(Slice { start, end, step })),
        }
        Ok(Selector::Slice(Slice { start, end, step }))
    }
    // test expression test for existence of a node designated by an embedded query
    //
    // because test expression can evaluate jsonpath-query what matters is if the return list is empty
    // or not:
    //  A query by itself in a logical context is an existence test that yields true if the query
    //  selects at least one node and yields false if the query does not select any nodes
    //
    // this is different from comparison expressions where queries either relative or absolute are
    // singular queries meaning we can not encounter multi-selectors like slice and wildcard(*)
    // singular-query = rel-singular-query / abs-singular-query

    // filter-selector = "?" S logical-expr
    //
    // rfc example:
    //
    // {
    //   "a": [3, 5, 1, 2, 4, 6,
    //         {"b": "j"},
    //         {"b": "k"},
    //         {"b": {}},
    //         {"b": "kilo"}
    //        ],
    //   "o": {"p": 1, "q": 2, "r": 3, "s": 5, "t": {"u": 6}},
    //   "e": "f"
    // }
    //
    // $.a[?@.b == $.x] -> result: 3, 5, 1, 2, 4, 6
    //
    // .a -> returns the array
    // what confused me at first was that we even get a result because we try to apply @.b on an array,
    // but I was wrong; we apply the filter selector on the input list of .a; we iterate over the values
    // of the array as per the filter selector, and then we try to apply the filter selector, meaning
    // for each element in the array we apply @.b == $.x
    // for 3, 5, 1, 2, 4, 6 @.b returns an empty list because a name selector is not applicable to
    // json number, the right side has a json path query which returns nothing because the root object
    // does not have a key named 'x'; we are comparing 2 empty lists(.b on numbers and .x on root)
    // those return true which means that the numbers are added to the output nodelist; this is why
    // the result is: 3, 5, 1, 2, 4, 6. The remaining elements of the array have a key 'b' which
    // results in a non-empty list and the comparison with .x returns false so no nodes added in the
    // list
    //
    // toDo: cache the subquery during evaluation? if the rhs or the lhs is a subquery the nodelist will not change in any iteration
    fn read_filter(&mut self) -> Result<Selector, PathError> {
        self.pos += 1; // skip '?'
        parsing::skip_whitespaces(self.buffer, &mut self.pos);

        // when this method gets called self.pos is at '?', we need to handle a case like "? S" where
        // '?' is not followed by a logical-expr
        if self.pos >= self.buffer.len() {
            return Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos - 1 });
        }

        Ok(Selector::Filter(self.read_logical_and_expr()?))
    }

    // Parse expressions separated by ||
    fn read_logical_or_expr(&mut self) -> Result<LogicalExpression, PathError> {
        let mut lhs = self.read_logical_and_expr()?;

        loop {
            parsing::skip_whitespaces(self.buffer, &mut self.pos);
            if self.pos >= self.buffer.len() {
                return Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos - 1 });
            }

            if self.pos + 1 < self.buffer.len()
                && self.buffer[self.pos] == b'|'
                && self.buffer[self.pos + 1] == b'|' {
                self.pos += 2;
                parsing::skip_whitespaces(self.buffer, &mut self.pos);
                if self.pos >= self.buffer.len() {
                    return Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos - 1 });
                }

                let rhs = self.read_logical_and_expr()?;
                lhs = LogicalExpression::Or(Box::new(lhs), Box::new(rhs));
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn read_logical_and_expr(&mut self) -> Result<LogicalExpression, PathError> {
        let mut lhs = self.read_basic_expr()?;

        loop {
            parsing::skip_whitespaces(self.buffer, &mut self.pos);
            if self.pos >= self.buffer.len() {
                return Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos - 1 });
            }

            if self.pos + 1 < self.buffer.len()
                && self.buffer[self.pos] == b'&'
                && self.buffer[self.pos + 1] == b'&' {
                self.pos += 2;
                parsing::skip_whitespaces(self.buffer, &mut self.pos);
                if self.pos >= self.buffer.len() {
                    return Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos - 1 });
                }

                let rhs = self.read_basic_expr()?;
                lhs = LogicalExpression::And(Box::new(lhs), Box::new(rhs));
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn read_basic_expr(&mut self) -> Result<LogicalExpression, PathError> {
        parsing::skip_whitespaces(self.buffer, &mut self.pos);
        if self.pos >= self.buffer.len() {
            return Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos - 1 });
        }

        if self.buffer[self.pos] == b'!' {
            self.pos += 1; // skip '!'
            parsing::skip_whitespaces(self.buffer, &mut self.pos);
            if self.pos >= self.buffer.len() {
                return Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos - 1 });
            }
            let expr = self.read_basic_expr()?;
            return Ok(LogicalExpression::Not(Box::new(expr)));
        }

        if self.buffer[self.pos] == b'(' {
            self.pos += 1; // skip '('

            parsing::skip_whitespaces(self.buffer, &mut self.pos);
            if self.pos >= self.buffer.len() {
                return Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos - 1 });
            }
            let expr = self.read_logical_or_expr()?;

            if self.pos >= self.buffer.len() {
                return Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos - 1 });
            }

            if self.buffer[self.pos] != b')' {
                return Err(PathError {
                    kind: PathErrorKind::UnexpectedCharacter { byte: self.buffer[self.pos] },
                    pos: self.pos
                });
            }
            self.pos += 1; // skip ')'
            // in the case of ! we returned Ok(LogicalExpression::Not(Box::new(expr)));
            // but now we just return the expression because it was just parenthesized
            return Ok(expr);
        }

        // it must be a comparison or test expression
        self.read_comparison_or_test()
    }

    fn read_comparison_or_test(&mut self) -> Result<LogicalExpression, PathError> {
        parsing::skip_whitespaces(self.buffer, &mut self.pos);
        if self.pos >= self.buffer.len() {
            return Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos - 1 });
        }

        let lhs = self.read_comparable()?;
        parsing::skip_whitespaces(self.buffer, &mut self.pos);
        if self.pos >= self.buffer.len() {
            return Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos - 1 });
        }

        let current = self.buffer[self.pos];
        let next = self.buffer.get(self.pos + 1);

        let comp_op = match(current, next)  {
            (b'=', Some(b'=')) => Some(ComparisonOp::Equal),
            (b'!', Some(b'=')) => Some(ComparisonOp::NotEqual),
            (b'<', Some(b'=')) => Some(ComparisonOp::LessThanOrEqual),
            (b'<', _) => Some(ComparisonOp::LessThan),
            (b'>', Some(b'=')) => Some(ComparisonOp::GreaterThanOrEqual),
            (b'>', _) => Some(ComparisonOp::GreaterThan),
            _ => None,
        };

        match comp_op {
            Some(op) => {
                parsing::skip_whitespaces(self.buffer, &mut self.pos);
                if self.pos >= self.buffer.len() {
                    return Err(PathError { kind: PathErrorKind::UnexpectedEndOf, pos: self.pos - 1 });
                }
                let rhs = self.read_comparable()?;
                Ok(LogicalExpression::Comparison(ComparisonExpr { lhs, op, rhs }))
            }
            None => {
                match lhs {
                    // query is moved to the TestExpr and we can no longer access lhs after
                    Comparable::Query(query) => {
                        Ok(LogicalExpression::Test(TestExpr { query }))
                    }
                    Comparable::Literal(_) => todo!(),
                    _ => todo!() // toDo: add function expressions
                }
            }
        }
    }

    fn read_comparable(&mut self) -> Result<Comparable, PathError> {
        let current = self.buffer[self.pos];

        match current {
            b'-' | b'0'..=b'9' => {
                let start = self.pos;
                number::read(self.buffer, &mut self.pos)
                    // why not from? This the only case where we have to convert from a NumericError to
                    // a PathError, so I thought to do it via map_err(). The body of the closure would
                    // be the body of the from() impl.
                    .map_err(|err| {
                        let pos = err.pos;
                        PathError { kind: PathErrorKind::Numeric(err), pos }
                    })?;
                let t = Type::Number(number::parse(&self.buffer[start..self.pos]));
                Ok(Comparable::Literal(t))
            }
            b'"' | b'\'' => {
                let t = Type::String(self.read_name()?);
                Ok(Comparable::Literal(t))
            }
            b't' | b'f' => {
                let first = current;
                let target = if first == b't' { "true".as_bytes() } else { "false".as_bytes() };

                parsing::read_boolean_or_null(self.buffer, &mut self.pos, target)?;
                let t = Type::Boolean(first == b't');
                Ok(Comparable::Literal(t))
            }
            b'n' => {
                parsing::read_boolean_or_null(self.buffer, &mut self.pos, "null".as_bytes())?;
                let t = Type::Null;
                Ok(Comparable::Literal(t))
            }
            // For embedded queries we just parse for now, evaluation will happen when applying the filter selector
            b'@' => {
                self.pos += 1;
                let mut segments = Vec::new();
                while let Some(seg) = self.read_seg()? {
                    segments.push(seg);
                }
                Ok(Comparable::Query(EmbeddedQuery { query_type: QueryType::Relative, segments }))
            }
            b'$' => {
                self.pos += 1;
                let mut segments = Vec::new();
                while let Some(seg) = self.read_seg()? {
                    segments.push(seg);
                }
                Ok(Comparable::Query(EmbeddedQuery { query_type: QueryType::Absolute, segments }))
            }
            _ => Err(PathError {
                kind: PathErrorKind::UnexpectedCharacter { byte: current },
                pos: self.pos
            })
        }
    }

    // child segments
    //
    //{
    //   "users": [
    //     {
    //       "name": "Alice",
    //       "age": 30,
    //       "city": "NYC"
    //     },
    //     {
    //       "name": "Bob",
    //       "age": 25,
    //       "city": "LA"
    //     },
    //     {
    //       "name": "Charlie",
    //       "age": 35,
    //       "city": "Chicago"
    //     }
    //   ]
    // }
    //
    // $.users[*]['name', 'city']
    //
    // 1. .users returns the array
    // 2. [*] is called on the array, and it returns all the elements
    // 3. for every element in the nodelist the input list we apply each selector, in this case we have
    // 2 name selectors.
    // output: ["]Alice", "NYC", "Bob", "LA", "Charlie", "Chicago"]
    // It is important to note that from the output we see that we applied all the selector for the 1st
    // element we got back "Alice", "NYC" and so on
    //
    // applying each segment essentially increases the depth by 1 level
    //
    // note that the selectors are only applied to the input nodelist, the initial_count keeps track
    // of that, no selector is applied to a node added by a previous selector
    fn apply_child_seg(&mut self, segment: &Segment) {
        if self.nodelist.is_empty() {
            return;
        }

        let initial_count = self.nodelist.len();
        for i in 0..initial_count  {
            for selector in &segment.selectors {
                self.apply_selector(selector, self.nodelist[i]);
            }
        }
        // this is similar to iterating from 0 to initial_count and calling pop_front()
        self.nodelist.drain(..initial_count);
    }

    // {
    //   "store": {
    //     "book": [
    //       {
    //         "title": "Book 1",
    //         "price": 10,
    //         "author": {
    //           "name": "Alice",
    //           "price": 5
    //         }
    //       },
    //       {
    //         "title": "Book 2",
    //         "price": 15
    //       }
    //     ],
    //     "bicycle": {
    //       "price": 100
    //     }
    //   }
    // }
    //
    // $..price
    //
    // price is a member name shorthand selector, we look if our object has any keys that match
    // If so we push the value of the price key in nodelist, then we dfs for EVERY value in current object
    //
    // no key matches price and at the root object, iterate through the values and call dfs
    //
    // values of store(only key in the root object) gives us store and book, try to apply the selector
    // again on the keys of store, no match, dfs again for all values of store
    // book is an array, can't apply name selector, dfs into its values, we have our first match 10,
    // we add to the list the moment we visit it(have to according to the rfc) and recurse again for
    // book[0], author is an object that name selector can be applied, has a key price, visit it and recurse
    // for the values of author, no container nodes return
    //
    // now we check book[1], match, add 15, so far we have 10, 5, 15
    // recurse into each values, no container nodes and recursion returns to the level where no we have
    // to visit the 2nd key of store, 'bicycle' again check its values we have a match price, 10,5,15,100
    // recurse again no more nodes we return at the start
    //
    // it is dfs on the values of the node, and if a match is found as we visit them for the 1st time(preorder)
    // not when recursion backtracks(postorder) we append them in the list
    //
    // in the end we apply the same logic as child segment remove from the front queue
    fn apply_descendant_seg(&mut self, segment: &Segment) {
        if self.nodelist.is_empty() {
            return;
        }

        let initial_count = self.nodelist.len();
        for i in 0..initial_count {
            for selector in &segment.selectors {
                self.dfs(self.nodelist[i], selector);
            }
        }
        // this is similar to iterating from 0 to initial_count and calling pop_front()
        self.nodelist.drain(..initial_count);
    }

    fn dfs(&mut self, root: &'v Value, selector: &Selector) {
        if !root.is_object() && !root.is_array() {
            return;
        }

        // toDo: why recursive desc needs lifetimes  before calling apply_selector? why child/desc segm dont?
        self.apply_selector(selector, root);
        match root {
            Value::Object(map) => {
                for entry in map.values() {
                    self.dfs(entry, selector);
                }
            }
            Value::Array(arr) => {
                for elem in arr {
                    self.dfs(elem, selector);
                }
            }
            _ => unreachable!("recursive_descendant() was called in a non container node")
        }
    }

    // If we don't specify the lifetime, Rust due to lifetime elision will assign lifetimes to the references,
    // but we will get an error when we try to do nodelist.extend(map.values()); with an error 'lifetime may not live long enough'
    // Without explicitly specifying the lifetime, our function signature does not require the passed in
    // value to live as long as the values referenced by nodelist
    //
    // note that not all references get that lifetime, just the contents of VecDeque and value, because
    // those are the two we want to tie together
    //
    // a simplified version

    // let mut vec: Vec<&str> = vec![];
    //
    //     {
    //         let s = String::from("hello");
    //         let s_ref = s.as_str();
    //         vec.push(s_ref);  // s doesn't live long enough
    //     }  // s drops here
    //
    //     // vec would contain dangling reference
    //
    // this was the signature before: fn apply_selector<'a>(nodelist: &mut VecDeque<&'a Value>, selector: &Selector, value: &'a Value)
    // now we already define v
    fn apply_selector(&mut self, selector: &Selector, value: &'v Value) {
        match (value, selector) {
            (Value::Object(map), Selector::Name(name)) => {
                if let Some(val) = map.get(name) {
                    self.nodelist.push_back(val);
                }
            }
            // map.values() returns an iterator over &Value, extend() adds all items from the iterator to values
            (Value::Object(map), Selector::WildCard) => {
                self.nodelist.extend(map.values());
            }
            // arr.iter() returns an iterator over &Value, extend() adds all items to values
            (Value::Array(arr), Selector::WildCard) => {
                self.nodelist.extend(arr.iter());
            }
            (Value::Array(arr), Selector::Index(index)) => {
                // if index is negative and its absolute value is greater than length, the n_idx can
                // still be negative, and we can not call get() with anything other than usize
                // len = 2, index = -4 => n_idx = -2
                // toDo: add explanation why we can cast len to i64 without any loss
                let n_idx = normalize_index(*index, arr.len() as i64);
                if let Some(val) = usize::try_from(n_idx)
                    .ok()
                    .and_then(|idx| arr.get(idx)) {
                    self.nodelist.push_back(val);
                }
            }
            (Value::Array(arr), Selector::Slice(slice)) => {
                let range = Range::new(slice, arr.len() as i64);

                for i in range {
                    if let Some(val) = arr.get(i) {
                        self.nodelist.push_back(val);
                    }
                }
            }
            // selectors can only be applied to containers(object, array)
            _ => (),
        }
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
    use crate::parsing::value::IndexMap;
    use crate::macros::json;
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
            ("$.2a", PathError { kind: PathErrorKind::UnexpectedCharacter { byte: b'2' }, pos: 2 }),
            // name-char from name-shorthand notation can not contain punctuation symbols
            ("$.f!o", PathError { kind: PathErrorKind::UnexpectedCharacter { byte: b'!' }, pos: 3 }),
            // unterminated single quoted string
            ("$['foo]", PathError::from(StringError {
                kind: StringErrorKind::UnexpectedEndOf,
                pos: 6
            })),
            ("$[\"foo]", PathError::from(StringError {
                kind: StringErrorKind::UnexpectedEndOf,
                pos: 6
            })),
        ]
    }

    fn invalid_indices() -> Vec<(&'static str, PathError)> {
        vec![
            ("$[-", PathError { kind: PathErrorKind::UnexpectedEndOf, pos: 2 }),
            ("$[-a]", PathError {
                kind: PathErrorKind::InvalidIndex {
                    message: "minus sign must be followed by a non-zero digit"
                },
                pos: 2
            }),
            ("$[-01]", PathError {
                kind: PathErrorKind::InvalidIndex {
                    message: "minus sign must be followed by a non-zero digit"
                },
                pos: 2
            }),
            ("$[02]", PathError {
                kind: PathErrorKind::InvalidIndex {
                    message: "leading zeros are not allowed"
                },
                pos: 2
            }),
            ("$[-9223372036854775809]", PathError {
                kind: PathErrorKind::InvalidIndex {
                    message: "number out of range"
                },
                pos: 3
            }),
            ("$[9223372036854775808]", PathError {
                kind: PathErrorKind::InvalidIndex {
                    message: "number out of range"
                },
                pos: 2
            }),
        ]
    }

    #[test]
    fn test_invalid_notations() {
        for (path_expr, err) in invalid_notations() {
            let root = json!({});
            let mut query = Query::new(path_expr.as_bytes(), &root);
            query.pos += 1;// this happens by calling check_root() but we simplify it for this case
            let result = query.read_seg();

            assert_eq!(result, Err(err), "invalid path_expr: {path_expr}")
        }
    }

    #[test]
    fn test_invalid_root() {
        for(path_expr, err) in invalid_root() {
            let root = json!({});
            let mut query = Query::new(path_expr.as_bytes(), &root);
            let result = query.check_root();

            assert_eq!(result, Err(err));
        }
    }

    #[test]
    fn test_invalid_name_selectors() {
        for (path_expr, err) in invalid_name_selectors() {
            let root = json!({});
            let mut query = Query::new(path_expr.as_bytes(), &root);
            query.pos += 1;// this happens by calling check_root() but we simplify it for this case
            let result = query.read_seg();

            assert_eq!(result, Err(err), "invalid path_expr: {path_expr}")
        }
    }

    #[test]
    fn invalid_bracketed_selection_syntax() {
        let root = json!({});
        let path_expr = "$['foo''bar']";
        let mut query = Query::new(path_expr.as_bytes(), &root);
        query.pos += 1;// this happens by calling check_root() but we simplify it for this case
        let result = query.read_seg();

        assert_eq!(result, Err(PathError { kind: PathErrorKind::UnexpectedCharacter { byte: b'\'' }, pos: 7 }));
    }

    #[test]
    fn test_invalid_index_selectors() {
        for (path_expr, err) in invalid_indices() {
            let root = json!({});
            let mut query = Query::new(path_expr.as_bytes(), &root);
            query.pos += 1;// this happens by calling check_root() but we simplify it for this case
            let result = query.read_seg();

            assert_eq!(result, Err(err), "invalid path_expr: {path_expr}")
        }
    }
}