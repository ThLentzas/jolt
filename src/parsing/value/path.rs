use crate::parsing::error::{StringError, StringErrorKind};
use crate::parsing::number::{self, Atoi};
use crate::parsing::value::Value;
use crate::parsing::value::error::{PathError, PathErrorKind};
use crate::parsing::value::path::filter::function::{FnExpr, FnExprArg};
use crate::parsing::value::path::filter::{
    Comparable, ComparisonExpr, ComparisonOp, EmbeddedQuery, EmbeddedQueryType, LogicalExpr,
    TestExpr,
};
use crate::parsing::value::path::tracker::{PathNode, Step, Tracker};
use crate::parsing::{self, escapes, utf8};
use std::cmp;
use std::collections::HashMap;

pub(super) mod filter;
pub(super) mod tracker;

#[derive(Debug, PartialEq, Eq, Hash)]
enum Selector {
    Name(String),
    WildCard,
    // i64 covers the range [-(2^53)+1, (2^53)-1], we don't need to use Number
    Index(i64),
    Slice(Slice),
    Filter(LogicalExpr),
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
enum SegmentKind {
    Child,
    Descendant,
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Segment {
    kind: SegmentKind,
    selectors: Vec<Selector>,
}

impl Segment {
    fn new(kind: SegmentKind) -> Self {
        Self {
            kind,
            selectors: Vec::new(),
        }
    }

    fn apply<'r, T: Tracker<'r>>(
        &self,
        context: &mut EvalContext<'r>,
        reader: &Vec<PathNode<'r, T::Trace>>,
        writer: &mut Vec<PathNode<'r, T::Trace>>,
    ) {
        match self.kind {
            SegmentKind::Child => self.apply_child::<T>(context, &reader, writer),
            SegmentKind::Descendant => self.apply_desc::<T>(context, &reader, writer),
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
    // 2. [*] returns all the elements
    // 3. for every element in the nodelist, the input list, we apply each selector, in this case we have
    // 2 name selectors.
    // output: ["]Alice", "NYC", "Bob", "LA", "Charlie", "Chicago"]
    // It is important to note that from the output we see that we applied all the selector for the 1st
    // element we got back "Alice", "NYC" and so on
    //
    // applying each segment essentially increases the depth by 1 level
    //
    // lifetimes: context and path nodes hold references to values that live in root so they share
    // the same lifetime; the lifetime of context, reader, writer as references is not tied to self,
    // they're independent borrows passed into the function; for T read parse()
    fn apply_child<'r, T: Tracker<'r>>(
        &self,
        context: &mut EvalContext<'r>,
        reader: &Vec<PathNode<'r, T::Trace>>,
        writer: &mut Vec<PathNode<'r, T::Trace>>,
    ) {
        if reader.is_empty() {
            return;
        }

        for v in reader.iter() {
            for selector in &self.selectors {
                selector.apply::<T>(context, writer, v);
            }
        }
    }

    // read the comment above the test_desc_seg_and_multi_selector() method in value.rs
    //
    // it is dfs on the values of the node, and if a match is found as we visit them for the 1st time(preorder)
    // not when recursion backtracks(postorder) we append them in the list
    //
    // lifetimes: context and path nodes hold references to values that live in root so they share
    // the same lifetime; the lifetime of context, reader, writer as references is not tied to self,
    // they're independent borrows passed into the function; for T read parse()
    fn apply_desc<'r, T: Tracker<'r>>(
        &self,
        context: &mut EvalContext<'r>,
        reader: &Vec<PathNode<'r, T::Trace>>,
        writer: &mut Vec<PathNode<'r, T::Trace>>,
    ) {
        if reader.is_empty() {
            return;
        }

        for v in reader.iter() {
            for selector in &self.selectors {
                dfs::<T>(context, v, writer, selector);
            }
        }
    }

    fn is_singular(&self) -> bool {
        match self.kind {
            SegmentKind::Descendant => false,
            SegmentKind::Child => self.selectors.len() == 1 && self.selectors[0].is_singular(),
        }
    }
}

impl Selector {
    fn is_singular(&self) -> bool {
        match self {
            Selector::Name(_) => true,
            Selector::WildCard => false,
            Selector::Index(_) => true,
            Selector::Slice(_) => false,
            Selector::Filter(_) => false,
        }
    }

    fn apply<'r, T: Tracker<'r>>(
        &self,
        context: &mut EvalContext<'r>,
        writer: &mut Vec<PathNode<'r, T::Trace>>,
        current: &PathNode<'r, T::Trace>,
    ) {
        match (current.val, self) {
            (Value::Object(map), Selector::Name(name)) => {
                if let Some((key, val)) = map.get_key_value(name) {
                    let trace = T::descend(&current.trace, Step::Key(key));
                    writer.push(PathNode { trace, val });
                }
            }
            (Value::Object(map), Selector::WildCard) => {
                for (key, val) in map.iter() {
                    let trace = T::descend(&current.trace, Step::Key(key));
                    writer.push(PathNode { trace, val });
                }
            }
            (Value::Array(arr), Selector::WildCard) => {
                for (i, val) in arr.iter().enumerate() {
                    let trace = T::descend(&current.trace, Step::Index(i));
                    writer.push(PathNode { trace, val });
                }
            }
            (Value::Array(arr), Selector::Index(index)) => {
                let n_idx = normalize_index(*index, arr.len() as i64);
                if let Ok(i) = usize::try_from(n_idx) {
                    if let Some(val) = arr.get(i) {
                        let trace = T::descend(&current.trace, Step::Index(i));
                        writer.push(PathNode { trace, val });
                    }
                }
            }
            (Value::Array(arr), Selector::Slice(slice)) => {
                let range = Range::new(slice, arr.len() as i64);
                for i in range {
                    if let Some(val) = arr.get(i) {
                        let trace = T::descend(&current.trace, Step::Index(i));
                        writer.push(PathNode { trace, val });
                    }
                }
            }
            (Value::Object(map), Selector::Filter(expr)) => {
                for (key, val) in map.iter() {
                    if expr.evaluate(context, val) {
                        let trace = T::descend(&current.trace, Step::Key(key));
                        writer.push(PathNode { trace, val });
                    }
                }
            }
            (Value::Array(arr), Selector::Filter(expr)) => {
                for (i, elem) in arr.iter().enumerate() {
                    if expr.evaluate(context, elem) {
                        let trace = T::descend(&current.trace, Step::Index(i));
                        writer.push(PathNode { trace, val: elem });
                    }
                }
            }
            _ => (),
        }
    }
}

// this is similar to NumberState, otherwise we would have to pass Option<..> 3 times every time we
// need a slice
#[derive(Debug, PartialEq, Eq, Hash)]
struct Slice {
    start: Option<i64>,
    end: Option<i64>,
    step: Option<i64>,
}

#[derive(Debug, PartialEq)]
struct Range {
    // current can become negative when traversing from right to left(negative step)
    // current = 1, step = -2 -> current becomes -1
    current: isize,
    // end can also be negative when we iterate in reverse order, the lower bound is exclusive
    // len = 5, current = 4, end = -1, step = -1 gives us all elements in reverse order (-1, 4]
    end: isize,
    step: i64,
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
    fn new(slice: &Slice, len: i64) -> Self {
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
            // if step is negative, the first element we visit is at upper, otherwise is at lower
            current: if step >= 0 {
                lower as isize
            } else {
                upper as isize
            },
            // end keeps track of where our range ends, if step is negative it is the lower bound
            // else the upper
            end: if step >= 0 {
                upper as isize
            } else {
                lower as isize
            },
            step,
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

// lifetimes: EvalContext holds a reference to root and the map that we use to cache the result of
// evaluating absolute path embedded queries, that result is a vector of references to values that
// live in root, so they share the same lifetime
struct EvalContext<'r> {
    root: &'r Value,
    cache: HashMap<usize, Vec<&'r Value>>,
}

impl<'r> EvalContext<'r> {
    fn new(root: &'r Value) -> Self {
        Self {
            root,
            cache: HashMap::new(),
        }
    }
}

// lifetimes: buffer originates from calling as_bytes() on the path arg that was passed in select()
// root is the root of the document
pub(super) struct Parser<'a, 'r> {
    buffer: &'a [u8],
    pos: usize,
    root: &'r Value,
    eq_id: usize,
}

impl<'a, 'r> Parser<'a, 'r> {
    pub(super) fn new(buffer: &'a [u8], root: &'r Value) -> Self {
        Self {
            buffer,
            pos: 0,
            root,
            eq_id: 0,
        }
    }

    // json path requires validation of the entire query before returning results. A malformed query
    // must always return an error, regardless of whether earlier segments produce an empty nodelist.
    //
    // path: "$[1]['foo]", root: "3"
    //
    // The index selector [1] yields an empty list (string is not indexable), but we cannot return
    // early, the unterminated string 'foo] makes the query invalid. We must parse all segments to
    // detect this error.
    //
    // 'r as lifetime for the Tracker because the "keys" will live as long as the root, since they
    // exist in the root
    //
    // we need to be explicit with the lifetime for PathNode otherwise it would tie to self due to
    // the 3rd rule
    // rust ties the lifetime of PathNode to the lifetime of the parser and parser was a local variable
    // that was getting dropped at the end of scope causing issues when calling value.select().
    //
    // fn parse<T>(&mut self) -> Result<Vec<PathNode<'_, T::Trace>>, PathError>
    //
    // we don't return a reference but owned data that hold a reference; elision rules are applied
    // the same. If no lifetime is specified, it is always tied to self.
    pub(super) fn parse<T: Tracker<'r>>(
        &mut self,
    ) -> Result<Vec<PathNode<'r, T::Trace>>, PathError> {
        self.parse_root()?;

        let root_trace = PathNode {
            trace: T::root(),
            val: self.root,
        };
        // reader and writer own the Nodes, we don't pass references to nodes. The nodes are
        // created locally, they do reference values from the root, but we can't pass references
        // to something that was created locally it would be a dangling reference once it goes out
        // of scope
        let mut reader: Vec<PathNode<'r, T::Trace>> = vec![root_trace];
        let mut writer: Vec<PathNode<'r, T::Trace>> = Vec::new();
        let mut context = EvalContext::new(self.root);
        while let Some(segment) = self.parse_seg()? {
            writer.clear();
            segment.apply::<T>(&mut context, &reader, &mut writer);
            std::mem::swap(&mut reader, &mut writer);
        }
        // the final nodelist is in reader after the last swap
        Ok(reader)
    }

    // unlike pointer, the query is not a json string, so we don't need to handle Unicode escapes
    // like \u0024 for '$'
    fn parse_root(&mut self) -> Result<(), PathError> {
        if self.buffer.is_empty() {
            return Err(PathError {
                kind: PathErrorKind::UnexpectedEndOf,
                pos: self.pos,
            });
        }

        let identifier = self.buffer[self.pos];
        if identifier != b'$' {
            return Err(PathError {
                kind: PathErrorKind::UnexpectedCharacter { byte: identifier },
                pos: self.pos,
            });
        }
        self.pos += 1; // consume '$'
        // Case: "$"
        // advancing pos and calling skip_ws() would result in an error because pos is already out
        // of bounds
        if self.buffer.len() == 1 {
            return Ok(());
        }
        // whitespaces are allowed after '$' only when followed by a segment
        // " $", "$ " neither is allowed
        self.skip_ws()?;
        Ok(())
    }

    fn parse_seg(&mut self) -> Result<Option<Segment>, PathError> {
        parsing::skip_whitespaces(self.buffer, &mut self.pos);
        if self.pos >= self.buffer.len() {
            return Ok(None);
        }

        let current = self.buffer[self.pos];
        let next = self.buffer.get(self.pos + 1);
        match (current, next) {
            (b'.', Some(b'.')) => {
                // descendant-segment  = ".." (bracketed-selection / wildcard-selector / member-name-shorthand)
                // we have 2 dots(..) followed by, either a bracketed-selection or a wildcard or a member name shorthand
                // if what follows '..' is not '[', we call parse_shorthand(), if the current character
                // is '*', it's a wildcard shorthand, otherwise it is treated as the start of name-shorthand
                // whitespaces are not allowed in between
                self.pos += 1;
                Ok(Some(self.parse_notation(SegmentKind::Descendant)?))
            }
            // child-segment = bracketed-selection / ("." (wildcard-selector / member-name-shorthand))
            // we have either bracketed selection or '.' followed by either a wildcard or a member-name-shorthand
            (b'.' | b'[', _) => Ok(Some(self.parse_notation(SegmentKind::Child)?)),
            // a segment always starts with '.', '..' or '['
            _ => Err(PathError {
                kind: PathErrorKind::UnexpectedCharacter { byte: current },
                pos: self.pos,
            }),
        }
    }

    // pos is either at '.' or '['
    // child-segment = bracketed-selection / ("." (wildcard-selector / member-name-shorthand))
    // descendant-segment  = ".." (bracketed-selection / wildcard-selector / member-name-shorthand)
    //
    // if pos is at '.' we are not sure what kind of segment we are parsing; it can be the shorthand
    // syntax for a child segment, .foo, or it can be a descendant segment that we don't know if
    // it is represented with a bracketed selection or the shorthand syntax. We need to peek to determine
    // the notation
    //   if current is '.', previous is also '.' and next is '[' it is a descendant segment using
    //   bracketed selection syntax. In any other case, it is shorthand
    fn parse_notation(&mut self, kind: SegmentKind) -> Result<Segment, PathError> {
        if self.buffer[self.pos] == b'.' && self.buffer[self.pos - 1] == b'.' {
            if let Some(b'[') = self.buffer.get(self.pos + 1) {
                self.pos += 1;
            }
        }

        if self.buffer[self.pos] == b'[' {
            Ok(self.parse_bracket(kind)?)
        } else {
            Ok(self.parse_shorthand(kind)?)
        }
    }

    // bracketed-selection = "[" S selector *(S "," S selector) S "]"
    // S = *B (optional blank space, zero or more)
    // B = '\t', '\n', '\r', ' '
    fn parse_bracket(&mut self, kind: SegmentKind) -> Result<Segment, PathError> {
        let len = self.buffer.len();
        let mut segment = Segment::new(kind);
        self.pos += 1; // consume '['

        while self.pos < len && self.buffer[self.pos] != b']' {
            segment.selectors.push(self.parse_selector()?);
            parsing::skip_whitespaces(self.buffer, &mut self.pos);

            match self.pos < len {
                true if self.buffer[self.pos] == b']' => {
                    break;
                }
                // after parsing a selector, if we don't encounter ']', we expect comma to separate
                // multiple selectors.
                true if self.buffer[self.pos] != b',' => {
                    return Err(PathError {
                        kind: PathErrorKind::UnexpectedCharacter {
                            byte: self.buffer[self.pos],
                        },
                        pos: self.pos,
                    });
                }
                // got a comma skip it, read next selector
                true => self.pos += 1,
                // we didn't encounter ']'
                false => {
                    return Err(PathError {
                        kind: PathErrorKind::UnexpectedEndOf,
                        pos: self.pos - 1,
                    });
                }
            }
        }

        // handles the $[ case where after consuming '[' we never enter the loop
        if self.pos >= len {
            return Err(PathError {
                kind: PathErrorKind::UnexpectedEndOf,
                pos: self.pos - 1,
            });
        }

        // Case: [<selector>,]
        // at this point we exited the loop  because we encountered ']' but if the exact previous
        // character is ',' we have invalid syntax; after comma we expect another selector
        // this is different from [<selector>,+S]
        // if we have at least 1 whitespace parse_selector() would skip it and then try to read ']'
        // which is not a valid start for any selector
        if self.buffer[self.pos - 1] == b',' {
            return Err(PathError {
                kind: PathErrorKind::UnexpectedCharacter {
                    byte: self.buffer[self.pos - 1],
                },
                pos: self.pos - 1,
            });
        }
        self.pos += 1; // consume closing bracket ']'

        Ok(segment)
    }

    // member name shorthand is still treated as a name selector with more strict syntax
    fn parse_shorthand(&mut self, kind: SegmentKind) -> Result<Segment, PathError> {
        self.pos += 1; // consume .
        let mut segment = Segment::new(kind);

        match self.buffer.get(self.pos) {
            Some(b'*') => {
                segment.selectors.push(Selector::WildCard);
                self.pos += 1;
            }
            Some(_) => {
                // we can not have shorthand notation inside a bracketed selection
                // if we need either a wildcard, or a name selector in a multi-selector segment
                // we need to use the full syntax
                segment
                    .selectors
                    .push(Selector::Name(self.parse_name_shorthand()?));
            }
            None => {
                return Err(PathError {
                    kind: PathErrorKind::UnexpectedEndOf,
                    pos: self.pos - 1,
                });
            }
        }
        Ok(segment)
    }

    // gets called from parse_bracket after '[' or ','
    fn parse_selector(&mut self) -> Result<Selector, PathError> {
        parsing::skip_whitespaces(self.buffer, &mut self.pos);

        match self.buffer.get(self.pos) {
            Some(c) if *c == b'\'' || *c == b'\"' => Ok(Selector::Name(self.parse_name()?)),
            Some(b'*') => {
                self.pos += 1;
                Ok(Selector::WildCard)
            }
            Some(b'-' | b'0'..=b'9' | b':') => self.parse_numeric(),
            Some(b'?') => self.parse_filter(),
            None => Err(PathError {
                kind: PathErrorKind::UnexpectedEndOf,
                pos: self.pos - 1,
            }),
            Some(n) => Err(PathError {
                kind: PathErrorKind::UnexpectedCharacter { byte: *n },
                pos: self.pos,
            }),
        }
    }

    fn parse_name(&mut self) -> Result<String, StringError> {
        let quote = self.buffer[self.pos];
        let len = self.buffer.len();
        self.pos += 1; // skip opening quote(' or ")

        let end = parsing::find(&self.buffer[self.pos..], quote, |b| {
            b == quote || escapes::is_escape(b)
        })
        .map(|i| self.pos + i)
        .ok_or(StringError {
            kind: StringErrorKind::UnexpectedEndOf,
            pos: len - 1,
        })?;

        let slice = &self.buffer[self.pos..end];
        // calls iter.any() internally
        if !slice.contains(&b'\\') {
            // SAFETY: buffer represents &str
            let val = unsafe { String::from_utf8_unchecked(Vec::from(slice)) };
            if val.len() > parsing::STRING_LENGTH_LIMIT {
                return Err(StringError {
                    kind: StringErrorKind::LengthLimitExceeded {
                        len: parsing::STRING_LENGTH_LIMIT,
                    },
                    // at the index of opening quote
                    pos: self.pos - 1,
                });
            }
            self.pos = end + 1;
            return Ok(val);
        }

        let mut name = String::with_capacity(slice.len());
        let mut i = 0;

        while i < slice.len() {
            let j = i;
            while i < slice.len() && slice[i] != b'\\' && slice[i].is_ascii() {
                if slice[i].is_ascii_control() {
                    return Err(StringError {
                        kind: StringErrorKind::InvalidControlCharacter { byte: slice[i] },
                        // convert back to buffer index for error
                        pos: self.pos + i,
                    });
                }
                i += 1;
            }
            name.push_str(unsafe { str::from_utf8_unchecked(&slice[j..i]) });
            if i >= slice.len() {
                break;
            }
            match slice[i] {
                b'\\' => {
                    let next = slice.get(i + 1);
                    match next {
                        // single quotes must be escaped within single-quoted strings.
                        // json path specific, not part of standard json escapes.
                        Some(b'\'') if quote == b'\'' => {
                            name.push('\'');
                            // prevents the escaped quote from being treated as a closing delimiter
                            i += 2;
                        }
                        _ => {
                            escapes::check_escape_character(slice, i)?;
                            name.push(escapes::map_escape_character(slice, i));
                            i += escapes::len(slice, i);
                        }
                    }
                }
                _ => {
                    name.push(utf8::read_utf8_char(slice, i));
                    i += utf8::utf8_char_width(slice[i]);
                }
            }

            if name.len() > parsing::STRING_LENGTH_LIMIT {
                return Err(StringError {
                    kind: StringErrorKind::LengthLimitExceeded {
                        len: parsing::STRING_LENGTH_LIMIT,
                    },
                    // at the index of opening quote
                    pos: self.pos - 1,
                });
            }
        }
        self.pos = end + 1;
        Ok(name)
    }

    // we follow the same logic as the previous cases where we had to parse strings; don't do it 1
    // by 1 calling push() each time, create a slice by tracking the starting position
    fn parse_name_shorthand(&mut self) -> Result<String, PathError> {
        let len = self.buffer.len();
        let mut current = self.buffer[self.pos];
        let start = self.pos;

        if !is_valid_name_first(current) {
            return Err(PathError {
                kind: PathErrorKind::UnexpectedCharacter { byte: current },
                pos: self.pos,
            });
        }

        if current.is_ascii() {
            self.pos += 1;
        } else {
            self.pos += utf8::utf8_char_width(current);
        }

        // We stop when we encounter 1 of the following characters without necessarily having an
        // invalid name
        //
        //   '.', '[' -> start of next segment
        //   '<', '>', '=', '&', '|' -> comparison/logical operators in filter expressions
        //   ')', ']' -> end of parenthesized expression or filter selector
        //   ',' -> function argument separator
        //   whitespace -> delimiter
        //
        // '!' is missing because no valid expression can start in that case,
        // @.name!... (nothing valid follows)
        //
        // if we have a case like "$.price< ", parsing will fail later when we attempt to parse the
        // next segment
        while self.pos < len {
            current = self.buffer[self.pos];

            if matches!(
                current,
                b'.' | b'[' | b'<' | b'>' | b'=' | b'&' | b'|' | b')' | b']' | b','
            ) || parsing::is_rfc_whitespace(current)
            {
                break;
            }
            if !is_valid_name_char(current) {
                return Err(PathError {
                    kind: PathErrorKind::UnexpectedCharacter { byte: current },
                    pos: self.pos,
                });
            }
            if current.is_ascii() {
                self.pos += 1;
            } else {
                self.pos += utf8::utf8_char_width(current);
            }
        }
        let name = unsafe { str::from_utf8_unchecked(&self.buffer[start..self.pos]) }.to_string();

        Ok(name)
    }

    // when we encounter ':' it is a slice selector, but for '-' or any digit we can't know yet, we
    // have to process the number and then if we encounter ':', we call parse_slice() otherwise it
    // is an index
    fn parse_numeric(&mut self) -> Result<Selector, PathError> {
        let num;

        if self.buffer[self.pos] == b':' {
            return self.parse_slice(None);
        }
        num = self.parse_index()?;
        // from the slice syntax: start *S ":" *S end *S ":" *S step  we need to skip whitespaces
        // between first index and ':'
        parsing::skip_whitespaces(self.buffer, &mut self.pos);
        if self.pos < self.buffer.len() && self.buffer[self.pos] == b':' {
            return self.parse_slice(Some(num));
        }
        Ok(Selector::Index(num))
    }

    fn parse_index(&mut self) -> Result<i64, PathError> {
        let current = self.buffer[self.pos];
        let next = self.buffer.get(self.pos + 1);

        match (current, next) {
            // -a
            (b'-', Some(n)) if !n.is_ascii_digit() || *n == b'0' => {
                return Err(PathError {
                    kind: PathErrorKind::InvalidIndex {
                        message: String::from("minus sign must be followed by a non-zero digit"),
                    },
                    pos: self.pos,
                });
            }
            // -
            (b'-', None) => {
                return Err(PathError {
                    kind: PathErrorKind::UnexpectedEndOf,
                    pos: self.pos,
                });
            }
            (b'0', Some(n)) if n.is_ascii_digit() => {
                return Err(PathError {
                    kind: PathErrorKind::InvalidIndex {
                        message: String::from("leading zeros are not allowed"),
                    },
                    pos: self.pos,
                });
            }
            _ => (),
        }
        // can infer the type, we don't need to do i64::atoi
        Ok(Atoi::atoi(self.buffer, &mut self.pos)?)
    }

    // when this method gets called self.buffer[self.pos] is at ':'
    // rfc syntax: start *S ":" *S end *S ":" *S step
    fn parse_slice(&mut self, start: Option<i64>) -> Result<Selector, PathError> {
        let len = self.buffer.len();
        let mut step: Option<i64> = Some(1); // default step value
        let mut end: Option<i64> = None;
        self.pos += 1; // consume ':'

        parsing::skip_whitespaces(self.buffer, &mut self.pos);
        // 1: or :
        if self.pos >= len {
            return Ok(Selector::Slice(Slice { start, end, step }));
        }

        // the cases where end and step are omitted(: or 1:) still need a right square bracket
        // after to be valid [:] or [1:] and those will be checked when control returns to the
        // parse_bracket(), the problem is not with the slice itself rather that with the bracket
        // syntax
        match self.buffer[self.pos] {
            // 0:: or ::
            b':' => self.pos += 1,
            // set end when present
            c if c == b'-' || c.is_ascii_digit() => end = Some(self.parse_index()?),
            _ => return Ok(Selector::Slice(Slice { start, end, step })),
        }

        parsing::skip_whitespaces(self.buffer, &mut self.pos);
        // 1::, ::, 1:5
        if self.pos > len {
            return Ok(Selector::Slice(Slice { start, end, step }));
        }
        // 1:2: or :2:
        if end.is_some() && self.buffer[self.pos] == b':' {
            self.pos += 1; // skip the second ':'
            parsing::skip_whitespaces(self.buffer, &mut self.pos);
        }

        match self.buffer.get(self.pos) {
            // set step when present
            Some(c) if *c == b'-' || c.is_ascii_digit() => step = Some(self.parse_index()?),
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
    fn parse_filter(&mut self) -> Result<Selector, PathError> {
        self.pos += 1; // consume '?'
        // "? *S"
        self.skip_ws()?;
        Ok(Selector::Filter(self.parse_logical_or()?))
    }

    // Precedence
    //
    // from the rfc table 10: we know that conjunction (&&) has higher precedence than
    // and disjunction (||) (level 2 vs level 1); it binds tighter
    //
    // "Highest Precedence" -> "Binds Tightest"
    // x || y && z should be evaluated as x || (y && z) and not as (x || y) && z
    // it is the same as math 1 + 2 * 3 is evaluated as 1 + (2 * 3) and not as (1 + 2) * 3
    //
    //        [ OR ]
    //       /      \
    //     [x]     [ AND ]
    //             /     \
    //           [y]     [z]
    //
    // Precedence is all about grouping not evaluation it builds the tree, and then we walk
    // it starting from root. Evaluation happens left-to-right with short-circuiting
    //
    // To handle precedence we create a Function Call Hierarchy. Each method invokes the function
    // for the Next Highest Precedence level. Functions called first (top of the chain) handle
    // the lowest precedence operators. Functions called last (deepest in the chain) handle the
    // highest precedence operators
    //
    // 1. logical_or() calls logical_and(). It waits for AND to finish grouping things tightly before
    // it even considers grouping things with OR due to lower precedence.
    // 2. logical_and() calls basic_expr(). This is the point where we handle NOT and grouping that
    // have higher precedence than AND. This is also the point where recursive calls can happen due
    // to nested logical expressions(parenthesized expressions). When we encounter '(' we make the
    // call to logical_or() to start the chain again.
    // 3. if no NOT or () expression we call parse_comparable() that parses an atom. Now that we
    // return back to logical_and() we call tail to handle left associativity as described in the
    // method itself and then back to logical_or(). We fully processed all methods with higher
    // precedence before returning to or.
    //
    // why  we need the tail methods and don't write the logic directly on the body of logical_or
    // and logical_and, read the comment in parse_fn_args()
    fn parse_logical_or(&mut self) -> Result<LogicalExpr, PathError> {
        let lhs = self.parse_logical_and()?;
        self.parse_logical_or_tail(lhs)
    }

    // loop handles associativity
    // x || y || z
    // the logical expression is (x || y) || z
    // pass x, rhs is y, then (x || y) is the lhs for || z
    // read the comment inside parse_fn_args() on why we need this method
    fn parse_logical_or_tail(&mut self, mut lhs: LogicalExpr) -> Result<LogicalExpr, PathError> {
        loop {
            self.skip_ws()?;
            if self.pos + 1 < self.buffer.len()
                && self.buffer[self.pos] == b'|'
                && self.buffer[self.pos + 1] == b'|'
            {
                self.pos += 2; // consume ||
                self.skip_ws()?;
                let rhs = self.parse_logical_and()?;
                lhs = LogicalExpr::Or(Box::new(lhs), Box::new(rhs));
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn parse_logical_and(&mut self) -> Result<LogicalExpr, PathError> {
        let lhs = self.parse_basic_expr()?;
        self.parse_logical_and_tail(lhs)
    }

    // loop handles associativity
    //
    // x && y && z
    // the logical expression is (x && y) && z
    // pass x, rhs is y, then (x && y) is the lhs for && z
    // read the comment inside parse_fn_args() on why we need this method
    fn parse_logical_and_tail(&mut self, mut lhs: LogicalExpr) -> Result<LogicalExpr, PathError> {
        loop {
            self.skip_ws()?;
            if self.pos + 1 < self.buffer.len()
                && self.buffer[self.pos] == b'&'
                && self.buffer[self.pos + 1] == b'&'
            {
                self.pos += 2; // consume &&
                self.skip_ws()?;
                let rhs = self.parse_basic_expr()?;
                lhs = LogicalExpr::And(Box::new(lhs), Box::new(rhs));
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn parse_basic_expr(&mut self) -> Result<LogicalExpr, PathError> {
        self.skip_ws()?;

        // for logical not operator we need to look ahead and consider only the 3 valid cases
        // 1. parenthesized expr
        // 2. filter-query
        // 3. function expr
        //
        // we can not have a case like !! or ! followed by a comparison expression
        //
        // "$[?!$[0].foo > 2 || @[1] <= 4]"
        // this results to an UnexpectedCharacter error for '>'
        // we look after '!' and we treat '$[0].foo' as a test expression and not the lhs of a
        // comparison expression
        if self.buffer[self.pos] == b'!' {
            self.pos += 1; // consume '!'
            self.skip_ws()?;

            let current = self.buffer[self.pos];
            let expr = match current {
                b'(' => self.parse_parenthesized()?,
                b'@' | b'$' => {
                    LogicalExpr::Test(TestExpr::EmbeddedQuery(self.parse_embedded_query()?))
                }
                b'l' | b'c' | b'm' | b's' | b'v' => {
                    LogicalExpr::Test(TestExpr::FnExpr(self.parse_fn_expr()?))
                }
                _ => {
                    return Err(PathError {
                        kind: PathErrorKind::UnexpectedCharacter { byte: current },
                        pos: self.pos,
                    });
                }
            };
            return Ok(LogicalExpr::Not(Box::new(expr)));
        }

        if self.buffer[self.pos] == b'(' {
            return Ok(self.parse_parenthesized()?);
        }

        let lhs = self.parse_comparable()?;
        // it must be a comparison or test expression
        self.parse_comparison_tail(lhs)
    }

    fn parse_parenthesized(&mut self) -> Result<LogicalExpr, PathError> {
        self.pos += 1; // consume '('
        self.skip_ws()?;
        // paren-expr = [logical-not-op S] "(" S logical-expr S ")"
        // start the chain again
        let expr = self.parse_logical_or()?;

        self.skip_ws()?;
        if self.buffer[self.pos] != b')' {
            return Err(PathError {
                kind: PathErrorKind::UnexpectedCharacter {
                    byte: self.buffer[self.pos],
                },
                pos: self.pos,
            });
        }
        self.pos += 1; // consume ')'
        Ok(expr)
    }

    // read the comment inside parse_fn_args() on why we need this method
    fn parse_comparison_tail(&mut self, lhs: Comparable) -> Result<LogicalExpr, PathError> {
        self.skip_ws()?;

        let current = self.buffer[self.pos];
        let next = self.buffer.get(self.pos + 1);
        let comp_op = match (current, next) {
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
                self.pos += op.len();
                self.skip_ws()?;
                let rhs = self.parse_comparable()?;
                Ok(LogicalExpr::Comparison(ComparisonExpr { lhs, op, rhs }))
            }
            // no operator, we treat the lhs as a test expression, and it can only be an embedded query
            // or fn expr
            None => match lhs {
                Comparable::EmbeddedQuery(query) => {
                    Ok(LogicalExpr::Test(TestExpr::EmbeddedQuery(query)))
                }
                Comparable::FnExpr(expr) => Ok(LogicalExpr::Test(TestExpr::FnExpr(expr))),
                // $[?42]
                // this is invalid, we just have lhs being a literal not followed by a comparison
                // operator
                Comparable::Literal(_) => Err(PathError {
                    kind: PathErrorKind::UnexpectedCharacter { byte: current },
                    pos: self.pos,
                }),
            },
        }
    }

    fn parse_comparable(&mut self) -> Result<Comparable, PathError> {
        let current = self.buffer[self.pos];

        match current {
            // literals as defined in the rfc (number / string-literal / true / false / null)
            b'-' | b'0'..=b'9' | b'"' | b'\'' | b't' | b'f' | b'n' => {
                Ok(Comparable::Literal(self.parse_literal()?))
            }
            // for embedded queries we just parse for now, evaluation will happen when applying the
            // filter selector
            b'@' | b'$' => Ok(Comparable::EmbeddedQuery(self.parse_embedded_query()?)),
            b'l' | b'c' | b'm' | b's' | b'v' => Ok(Comparable::FnExpr(self.parse_fn_expr()?)),
            _ => Err(PathError {
                kind: PathErrorKind::UnexpectedCharacter { byte: current },
                pos: self.pos,
            }),
        }
    }

    // We can't call parse_seg() directly for embedded queries because we have different boundaries
    // Calling parse_seg() the 1st time would read segments until the end of input; embedded queries
    // consist of segments included in the bracketed selection range of the filter selector.
    //
    // path = $[?@.price>10]
    //
    // we try to parse the embedded relative query and the 1st segment is a name shorthand; following
    // the syntax for name-shorthand we know that encountering '>' would result in an error; for example:
    // $.price> is an invalid, because it is not part of a filter selector, but in the
    // filter selector case we need to return a child segment with a single selector and let the
    // call of parse_embedded_seg() handle the next character; we update the ending conditions in
    // parse_name_shorthand() and let the caller handle it.
    //
    // parse_name_shorthand() -> returns price to parse_shorthand()
    // parse_shorthand() -> returns a segment with a single selector to parse_notation()
    // parse_notation() -> returns to parse_seg()
    // parse_seg() -> returns to parse_embedded_seg()
    // parse_embedded_seg() -> returns a segment for the embedded query from parse_comparable() and
    // is called again.
    // This is the key point it needs to look for any expression related character. If we didn't have
    // that check, calling parse_seg() directly would result in an error because no segment starts
    // with '>'. Note that we don't check for the full operators(<=, >=, !=, &&, ||). We encounter '<'
    // and we know we processed all the segments for the subquery. The control returns to the
    // parse_comparison_tail() which checks if the current, next is a valid comparison operator.
    // If not, control returns to parse_basic_expr() which will handle parenthesized expression and
    // then back to parse_logical_and/or().
    // Those 2 methods will handle logical operators. But what if we have $[?@.price&* 10]?
    // &* will not matching to anything in the parse_filter() call chain and the control returns to
    // parse_selector() and then to parse_bracket() which expects after processing the current selector
    // a comma to indicate multiple selectors or ']' to stop processing the current one, but it
    // encounters an unpaired '&' and we get a PathError
    //
    // an alternative would be to pass the boundaries as a predicate everytime we call parse_seg();
    // something similar to how we initially did it for name-shorthand at
    // ab674d202994ac40e061f7f31662919615e85956 commit
    fn parse_embedded_query(&mut self) -> Result<EmbeddedQuery, PathError> {
        let query_type = if self.buffer[self.pos] == b'@' {
            EmbeddedQueryType::Relative
        } else {
            EmbeddedQueryType::Absolute
        };
        self.pos += 1;
        let mut segments = Vec::new();

        loop {
            if self.pos >= self.buffer.len() {
                break;
            }
            self.skip_ws()?;
            if matches!(
                self.buffer[self.pos],
                // as mentioned above:
                //  '<', '>', '=', '!' part of comparison expr
                //  '!', '&', '|' part of logical expr
                //  ')' end of parenthesized expression or function call
                // ']' end of filter selector
                // ',' as argument on a function, count(@.*, @.*)
                b'<' | b'>' | b'=' | b'!' | b'&' | b'|' | b')' | b']' | b','
            ) {
                break;
            }
            if let Some(seg) = self.parse_seg()? {
                segments.push(seg);
            } else {
                break;
            }
        }

        let eq = EmbeddedQuery {
            id: self.eq_id,
            query_type,
            segments,
        };
        self.eq_id += 1;
        Ok(eq)
    }

    // function-expr = function-name "(" S [function-argument *(S "," S function-argument)] S ")"
    fn parse_fn_expr(&mut self) -> Result<FnExpr, PathError> {
        let current = self.buffer[self.pos];
        let mut name = String::new();
        let start = self.pos;

        match current {
            b'l' => {
                parsing::read_keyword(self.buffer, &mut self.pos, "length".as_bytes())?;
                name.push_str("length");
            }
            b'c' => {
                parsing::read_keyword(self.buffer, &mut self.pos, "count".as_bytes())?;
                name.push_str("count");
            }
            b'm' => {
                parsing::read_keyword(self.buffer, &mut self.pos, "match".as_bytes())?;
                name.push_str("match");
            }
            b's' => {
                parsing::read_keyword(self.buffer, &mut self.pos, "search".as_bytes())?;
                name.push_str("search");
            }
            b'v' => {
                parsing::read_keyword(self.buffer, &mut self.pos, "value".as_bytes())?;
                name.push_str("value");
            }
            _ => unreachable!(
                "parse_fn_expr() called with invalid byte {} at index {}",
                current, self.pos
            ),
        }

        self.skip_ws()?;

        let expr = FnExpr {
            name,
            args: self.parse_fn_args()?,
        };
        expr.type_check().map_err(|err| PathError {
            kind: PathErrorKind::FnExpr(err),
            pos: start,
        })?;

        Ok(expr)
    }

    fn parse_fn_args(&mut self) -> Result<Vec<FnExprArg>, PathError> {
        let mut args = Vec::new();

        if self.buffer[self.pos] != b'(' {
            return Err(PathError {
                kind: PathErrorKind::UnexpectedCharacter {
                    byte: self.buffer[self.pos],
                },
                pos: self.pos,
            });
        }
        self.pos += 1; // consume '('
        self.skip_ws()?;

        while self.pos < self.buffer.len() && self.buffer[self.pos] != b')' {
            // starts with '!' or '(', it is a LogicalExpr.
            if self.buffer[self.pos] == b'!' || self.buffer[self.pos] == b'(' {
                args.push(FnExprArg::LogicalExpr(self.parse_logical_or()?));
            } else {
                // this is the ambiguous part. It is known as the "First set problem"
                //
                // The "first set problem" in parsing, especially for LL(1) grammars, refers to when
                // a non-terminal can derive multiple production rules that start with the exact
                // same terminal symbol, creating ambiguity for top-down parsers (like LL parsers)
                // that need to decide which rule to apply next based on the next input token.
                //
                // function-argument = literal / filter-query / logical-expr / function-expr
                //
                // reading the argument is not enough to know what type of argument we have.
                // 5 can be a literal or the lhs of a comparison expression
                // an embedded query can be a filter query or part of a logical expression. The point
                // is we don't know yet. We need to look ahead for the next token and determine if
                // we have some operator
                let arg = self.parse_comparable()?;
                self.skip_ws()?;

                let is_op = matches!(
                    self.buffer[self.pos],
                    b'=' | b'<' | b'>' | b'!' | b'&' | b'|'
                );
                if is_op {
                    // arg is now the lhs. parse_comparison_tail() will check for a comparison operator
                    // If the operator we encountered was one, it will try to parse the rhs if not
                    // we have an existence test and the operator we encountered must have been
                    // a logical one.
                    let mut expr = self.parse_comparison_tail(arg)?;
                    // we can't just call parse_logical_and/or
                    // we are not parsing a logical expression from the start like we do when we call
                    // parse_filter(). We have already parsed the lhs, we are at && or || we need to
                    // parse the remaining part of the expression. We need those tail methods to look
                    // after lhs. Calling parse_logical_and/or would look for lhs, encounter && and
                    // fail
                    //
                    // this why we pass lhs as argument, the methods knows the lhs, parses rhs and
                    // returns the expression
                    expr = self.parse_logical_and_tail(expr)?;
                    expr = self.parse_logical_or_tail(expr)?;
                    args.push(FnExprArg::LogicalExpr(expr));
                } else {
                    // we encountered no operator, map it as is
                    match arg {
                        Comparable::Literal(l) => args.push(FnExprArg::Literal(l)),
                        Comparable::EmbeddedQuery(q) => args.push(FnExprArg::EmbeddedQuery(q)),
                        Comparable::FnExpr(f) => args.push(FnExprArg::FnExpr(Box::new(f))),
                    }
                }
            }

            // exhausted the buffer without encountering ')'
            if self.pos >= self.buffer.len() {
                return Err(PathError {
                    kind: PathErrorKind::UnexpectedEndOf,
                    pos: self.pos - 1,
                });
            }

            self.skip_ws()?;
            if self.buffer[self.pos] == b',' {
                self.pos += 1;
                self.skip_ws()?;
            }
        }
        self.pos += 1; // consume ')'
        Ok(args)
    }

    fn parse_literal(&mut self) -> Result<Value, PathError> {
        let current = self.buffer[self.pos];

        match current {
            b'-' | b'0'..=b'9' => {
                // now we scan 3 times the same buffer. once for read and twice for parse
                // maybe we can make read return the current byte? so look at a byte at a time
                // we could have a method number::scan() that essentially parses the number without
                // knowing the span of the number in the buffer. This could be problematic because
                // we might have to parse floats? We did something similar in convertNumber() in
                // NumberNode on the java parser
                let start = self.pos;
                // why not from? This the only case where we have to convert from a NumericError to
                // a PathError, so I thought to do it via map_err(). The body of the closure would
                // be the body of the from() impl.
                number::read(self.buffer, &mut self.pos).map_err(|err| PathError {
                    kind: PathErrorKind::Numeric(err.kind),
                    pos: err.pos,
                })?;
                Ok(Value::Number(number::parse(&self.buffer[start..self.pos])))
            }
            b'"' | b'\'' => Ok(Value::String(self.parse_name()?)),
            b't' | b'f' => {
                let keyword = if current == b't' {
                    "true".as_bytes()
                } else {
                    "false".as_bytes()
                };
                parsing::read_keyword(self.buffer, &mut self.pos, keyword)?;
                Ok(Value::Boolean(current == b't'))
            }
            b'n' => {
                parsing::read_keyword(self.buffer, &mut self.pos, "null".as_bytes())?;
                Ok(Value::Null)
            }
            _ => unreachable!("parse_literal() called with invalid byte: {}", current),
        }
    }

    fn skip_ws(&mut self) -> Result<(), PathError> {
        parsing::skip_whitespaces(self.buffer, &mut self.pos);
        if self.pos >= self.buffer.len() {
            return Err(PathError {
                kind: PathErrorKind::UnexpectedEndOf,
                pos: self.pos - 1,
            });
        }
        Ok(())
    }
}

fn dfs<'r, T: Tracker<'r>>(
    context: &mut EvalContext<'r>,
    current: &PathNode<'r, T::Trace>,
    writer: &mut Vec<PathNode<'r, T::Trace>>,
    selector: &Selector,
) {
    if !current.val.is_object() && !current.val.is_array() {
        return;
    }

    selector.apply::<T>(context, writer, current);
    match current.val {
        Value::Object(map) => {
            for (key, val) in map.iter() {
                let trace = T::descend(&current.trace, Step::Key(key));
                let node = PathNode { val, trace };
                dfs::<T>(context, &node, writer, selector);
            }
        }
        Value::Array(arr) => {
            for (i, val) in arr.iter().enumerate() {
                let trace = T::descend(&current.trace, Step::Index(i));
                let node = PathNode { val, trace };
                dfs::<T>(context, &node, writer, selector);
            }
        }
        _ => unreachable!("dfs() was called in a non container node"),
    }
}

// !matches!(byte, 0x00..=0x1F | 0x20..=0x2F | 0x3A..=0x40 | 0x5C..=0x5E | 0x60 | 0x7B..=0x7E)
// the above one would work as well, it covers all the cases mentioned in the not_allowed section
// in the comment above
fn is_valid_name_first(b: u8) -> bool {
    b.is_ascii_alphabetic() || b == b'_' || !b.is_ascii()
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
    use crate::macros::json;
    use crate::parsing::value::IndexMap;
    use crate::parsing::value::path::filter::function::FnExprError;
    use crate::parsing::value::path::filter::function::FnType;
    use crate::parsing::value::path::tracker::NoOpTracker;

    fn invalid_root() -> Vec<(&'static str, PathError)> {
        vec![
            // mismatch
            (
                " ",
                PathError {
                    kind: PathErrorKind::UnexpectedCharacter { byte: b' ' },
                    pos: 0,
                },
            ),
            // whitespaces not followed by a segment
            (
                "$ \n\r",
                PathError {
                    kind: PathErrorKind::UnexpectedEndOf,
                    pos: 3,
                },
            ),
        ]
    }

    fn invalid_notations() -> Vec<(&'static str, PathError)> {
        vec![
            (
                "$.",
                PathError {
                    kind: PathErrorKind::UnexpectedEndOf,
                    pos: 1,
                },
            ),
            (
                "$[",
                PathError {
                    kind: PathErrorKind::UnexpectedEndOf,
                    pos: 1,
                },
            ),
            (
                "$..",
                PathError {
                    kind: PathErrorKind::UnexpectedEndOf,
                    pos: 2,
                },
            ),
            (
                "$a",
                PathError {
                    kind: PathErrorKind::UnexpectedCharacter { byte: b'a' },
                    pos: 1,
                },
            ),
            (
                "$['foo',]",
                PathError {
                    kind: PathErrorKind::UnexpectedCharacter { byte: b',' },
                    pos: 7,
                },
            ),
        ]
    }

    // Name selectors return member names which are json strings. We have already covered the invalid
    // cases in lexer.rs(invalid_strings())
    fn invalid_name_selectors() -> Vec<(&'static str, PathError)> {
        vec![
            // name-first from name-shorthand notation can not start with a digit
            (
                "$.2a",
                PathError {
                    kind: PathErrorKind::UnexpectedCharacter { byte: b'2' },
                    pos: 2,
                },
            ),
            // note this test is not invalid, when we encounter '!' we stop because it can be the
            // start of a comparison expression(!=). If that does not happen we get unexpected character
            // when we encounter o after !
            // (
            //     "$.f!o",
            //     PathError {
            //         kind: PathErrorKind::UnexpectedCharacter { byte: b'!' },
            //         pos: 3,
            //     },
            // ),
            // unterminated single quoted string
            (
                "$['foo]",
                PathError::from(StringError {
                    kind: StringErrorKind::UnexpectedEndOf,
                    pos: 6,
                }),
            ),
            (
                "$[\"foo]",
                PathError::from(StringError {
                    kind: StringErrorKind::UnexpectedEndOf,
                    pos: 6,
                }),
            ),
        ]
    }

    fn invalid_indices() -> Vec<(&'static str, PathError)> {
        vec![
            (
                "$[-",
                PathError {
                    kind: PathErrorKind::UnexpectedEndOf,
                    pos: 2,
                },
            ),
            (
                "$[-a]",
                PathError {
                    kind: PathErrorKind::InvalidIndex {
                        message: String::from("minus sign must be followed by a non-zero digit"),
                    },
                    pos: 2,
                },
            ),
            (
                "$[-01]",
                PathError {
                    kind: PathErrorKind::InvalidIndex {
                        message: String::from("minus sign must be followed by a non-zero digit"),
                    },
                    pos: 2,
                },
            ),
            (
                "$[02]",
                PathError {
                    kind: PathErrorKind::InvalidIndex {
                        message: String::from("leading zeros are not allowed"),
                    },
                    pos: 2,
                },
            ),
            (
                "$[-9223372036854775809]",
                PathError {
                    kind: PathErrorKind::InvalidIndex {
                        message: String::from("overflow, index exceeds: -9007199254740991"),
                    },
                    pos: 2,
                },
            ),
            (
                "$[9223372036854775808]",
                PathError {
                    kind: PathErrorKind::InvalidIndex {
                        message: String::from("overflow, index exceeds: 9007199254740991"),
                    },
                    pos: 2,
                },
            ),
        ]
    }

    // fn errors like arity/type mismatch are handled in a different test case
    fn invalid_filter() -> Vec<(&'static str, Value, PathError)> {
        vec![
            // None arm in the parse_comparison_tail() case
            (
                "$[?1]",
                json!({}),
                PathError {
                    kind: PathErrorKind::UnexpectedCharacter { byte: b']' },
                    pos: 4,
                },
            ),
            // Missing closing ')'
            (
                "$[?(@.a]",
                json!({}),
                PathError {
                    kind: PathErrorKind::UnexpectedCharacter { byte: b']' },
                    pos: 7,
                },
            ),
            (
                "$[?@.a)]",
                json!({}),
                PathError {
                    kind: PathErrorKind::UnexpectedCharacter { byte: b')' },
                    pos: 6,
                },
            ),
            // can't negate literals
            (
                "$[?!10]",
                json!({}),
                PathError {
                    kind: PathErrorKind::UnexpectedCharacter { byte: b'1' },
                    pos: 4,
                },
            ),
            (
                "$[?@.a == @.b == @.c]",
                json!({}),
                PathError {
                    kind: PathErrorKind::UnexpectedCharacter { byte: b'=' },
                    pos: 14,
                },
            ),
        ]
    }

    fn fn_arity_mismatch() -> Vec<(&'static str, Value, PathError)> {
        vec![
            (
                "$[?length() > 0]",
                json!([]),
                PathError {
                    kind: PathErrorKind::FnExpr(FnExprError::ArityMismatch {
                        expected: 1,
                        got: 0,
                    }),
                    pos: 3,
                },
            ),
            (
                "$[?count(@.*, @.*) > 0]",
                json!([]),
                PathError {
                    kind: PathErrorKind::FnExpr(FnExprError::ArityMismatch {
                        expected: 1,
                        got: 2,
                    }),
                    pos: 3,
                },
            ),
            (
                "$[?match(@.name)]",
                json!([]),
                PathError {
                    kind: PathErrorKind::FnExpr(FnExprError::ArityMismatch {
                        expected: 2,
                        got: 1,
                    }),
                    pos: 3,
                },
            ),
            (
                "$[?search()]",
                json!([]),
                PathError {
                    kind: PathErrorKind::FnExpr(FnExprError::ArityMismatch {
                        expected: 2,
                        got: 0,
                    }),
                    pos: 3,
                },
            ),
            (
                "$[?value() == 1]",
                json!([]),
                PathError {
                    kind: PathErrorKind::FnExpr(FnExprError::ArityMismatch {
                        expected: 1,
                        got: 0,
                    }),
                    pos: 3,
                },
            ),
        ]
    }

    fn fn_type_mismatch() -> Vec<(&'static str, Value, PathError)> {
        vec![
            (
                // non-singular query
                "$[?length(@.*) > 0]",
                json!([]),
                PathError {
                    kind: PathErrorKind::FnExpr(FnExprError::TypeMismatch {
                        expected: FnType::ValueType,
                        got: FnType::NodesType,
                    }),
                    pos: 3,
                },
            ),
            // expects NodesType
            (
                "$[?count(5) > 0]",
                json!([]),
                PathError {
                    kind: PathErrorKind::FnExpr(FnExprError::TypeMismatch {
                        // 5 is FnExprArg::Literal, and we do the conversion to ValueType
                        expected: FnType::NodesType,
                        got: FnType::ValueType,
                    }),
                    pos: 3,
                },
            ),
            // first arg non-singular,
            (
                "$[?match(@.*, 'pattern')]",
                json!([]),
                PathError {
                    kind: PathErrorKind::FnExpr(FnExprError::TypeMismatch {
                        expected: FnType::ValueType,
                        got: FnType::NodesType,
                    }),
                    pos: 3,
                },
            ),
            // logical_expr as argument evaluates to FnType::Logical
            (
                "$[?search(@.x && @.y, 'z')]",
                json!([]),
                PathError {
                    kind: PathErrorKind::FnExpr(FnExprError::TypeMismatch {
                        expected: FnType::ValueType,
                        got: FnType::Logical,
                    }),
                    pos: 3,
                },
            ),
            // value expects NodesType,  match return LogicalType
            (
                "$[?value(match(@.x, 'a')) == 5]",
                json!([]),
                PathError {
                    kind: PathErrorKind::FnExpr(FnExprError::TypeMismatch {
                        expected: FnType::NodesType,
                        got: FnType::Logical,
                    }),
                    pos: 3,
                },
            ),
        ]
    }

    #[test]
    fn test_empty_path() {
        let root = json!({});
        let err = PathError {
            kind: PathErrorKind::UnexpectedEndOf,
            pos: 0,
        };
        let res = root.select("");
        assert_eq!(res, Err(err));
    }

    #[test]
    fn test_invalid_notations() {
        for (path_expr, err) in invalid_notations() {
            let root = json!({});
            let mut query = Parser::new(path_expr.as_bytes(), &root);
            query.pos += 1; // this happens by calling check_root() but we simplify it for this case
            let result = query.parse_seg();

            assert_eq!(result, Err(err), "invalid path_expr: {path_expr}")
        }
    }

    #[test]
    fn test_invalid_root() {
        for (path_expr, err) in invalid_root() {
            let root = json!({});
            let mut query = Parser::new(path_expr.as_bytes(), &root);
            let result = query.parse::<NoOpTracker>();

            assert_eq!(result, Err(err));
        }
    }

    #[test]
    fn test_invalid_name_selectors() {
        for (path_expr, err) in invalid_name_selectors() {
            let root = json!({});
            let mut query = Parser::new(path_expr.as_bytes(), &root);
            let result = query.parse::<NoOpTracker>();

            assert_eq!(result, Err(err), "invalid path_expr: {path_expr}")
        }
    }

    #[test]
    fn invalid_bracketed_selection_syntax() {
        let root = json!({});
        let path_expr = "$['foo''bar']";
        let mut query = Parser::new(path_expr.as_bytes(), &root);
        let result = query.parse::<NoOpTracker>();

        assert_eq!(
            result,
            Err(PathError {
                kind: PathErrorKind::UnexpectedCharacter { byte: b'\'' },
                pos: 7
            })
        );
    }

    #[test]
    fn test_invalid_index_selectors() {
        for (path_expr, err) in invalid_indices() {
            let root = json!({});
            let mut query = Parser::new(path_expr.as_bytes(), &root);
            let result = query.parse::<NoOpTracker>();

            assert_eq!(result, Err(err), "invalid path_expr: {path_expr}")
        }
    }

    #[test]
    fn test_invalid_filter_selectors() {
        for (path_expr, root, err) in invalid_filter() {
            let mut query = Parser::new(path_expr.as_bytes(), &root);
            let result = query.parse::<NoOpTracker>();

            assert_eq!(result, Err(err), "invalid path_expr: {path_expr}")
        }
    }

    #[test]
    fn test_fn_arity_mismatch() {
        for (path_expr, root, err) in fn_arity_mismatch() {
            let mut query = Parser::new(path_expr.as_bytes(), &root);
            let result = query.parse::<NoOpTracker>();

            assert_eq!(result, Err(err), "invalid path_expr: {path_expr}")
        }
    }

    #[test]
    fn test_fn_type_mismatch() {
        for (path_expr, root, err) in fn_type_mismatch() {
            let mut query = Parser::new(path_expr.as_bytes(), &root);
            let result = query.parse::<NoOpTracker>();

            assert_eq!(result, Err(err), "invalid path_expr: {path_expr}")
        }
    }
}
