use crate::parsing::error::{StringError, StringErrorKind};
use crate::parsing::value::Value;
use crate::parsing::value::error::{PathError, PathErrorKind};
use crate::parsing::value::path::filter::function::{FnExpr, FnExprArg};
use crate::parsing::value::path::filter::{
    Comparable,
    ComparisonExpr,
    ComparisonOp,
    EmbeddedQuery,
    EmbeddedQueryType,
    LogicalExpr,
    TestExpr,
};
use crate::parsing::value::path::tracker::{PathNode, Step, Tracker};
use crate::parsing::{self, escapes, number, utf8};
use std::cmp;
use crate::parsing::number::Atoi;

pub(super) mod filter;
pub(crate) mod tracker;

pub(super) struct Parser<'a, 'v> {
    // toDo: this could also be chars() since we never have to worry about running into an invalid ut8
    buffer: &'a [u8],
    pos: usize,
    root: &'v Value,
}

#[derive(Debug, PartialEq)]
struct Segment {
    kind: SegmentKind,
    selectors: Vec<Selector>,
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum SegmentKind {
    Child,
    Descendant,
}

// this is similar to NumberState, otherwise we would have to pass Option<..> 3 times every time we
// need a slice
#[derive(Debug, PartialEq)]
struct Slice {
    start: Option<i64>,
    end: Option<i64>,
    step: Option<i64>,
}

#[derive(Debug, PartialEq)]
enum Selector {
    Name(String),
    WildCard,
    // i64 covers the range [-(2^53)+1, (2^53)-1], we don't need to use Number
    Index(i64),
    Slice(Slice),
    Filter(LogicalExpr),
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

impl Segment {
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

impl Segment {
    fn new(kind: SegmentKind) -> Self {
        Self {
            kind,
            selectors: Vec::new(),
        }
    }
}

impl<'a, 'v> Parser<'a, 'v> {
    pub(super) fn new(buffer: &'a [u8], root: &'v Value) -> Self {
        Self {
            buffer,
            pos: 0,
            root,
        }
    }

    // A JSONPath implementation MUST raise an error for any query that is not well-formed and
    // valid. The well-formedness and the validity of JSONPath queries are independent of the JSON
    // value the query is applied to. No further errors relating to the well-formedness and the
    // validity of a JSONPath query can be raised during application of the query to a value.
    //
    // As mentioned above invalid paths should always return an error which means even if we process
    // some segment and returned an empty list we can NOT stop and return immediately because the
    // path expression might still be invalid.
    //
    // path_expr = "$[1]['foo]"
    // val = "3"
    //
    // If we try to apply [1] to val it returns an empty list because it is not an array and in theory
    // we could return without processing the rest of the segments, but we return something from an
    // invalid path which is not allowed. Processing the next segment will result in an UnterminatedString
    // case.
    //
    // v as lifetime for the Tracker because the "keys" will live as long as the root, since they
    // exist in the root
    //
    // we need to be explicit with the lifetime for PathNode otherwise it would tie to ti Query due to
    // the 3rd rule
    // had this initially and caused issues in value.select(),
    // fn parse<T>(&mut self) -> Result<Vec<PathNode<T::Trace>>, PathError>
    //
    // rust ties the lifetime of PathNode to the lifetime of the query and query was a local variable
    // that was getting dropped at the end of scope causing issues when calling value.select().
    // From the Query's definition we know that it will never outlive the root, but it does not
    // mean that the root can not outlive the query, which is what happens in this case, query goes
    // out of scope, root still valid, returned values have the lifetime of root
    pub(super) fn parse<T: Tracker<'v>>(&mut self) -> Result<Vec<PathNode<'v, T::Trace>>, PathError> {
        self.check_root()?;

        let root_trace = PathNode {
            val: self.root,
            trace: T::root(),
        };
        // reader and writer own the Nodes, we don't pass references to nodes. The nodes are
        // created locally, they do reference values from the root, but we can't pass references
        // to something that was created locally it would be a dangling reference once it goes out
        // of scope
        let mut reader: Vec<PathNode<'v, T::Trace>> = vec![root_trace];
        let mut writer: Vec<PathNode<'v, T::Trace>> = Vec::new();

        // this is the only place that we get an error, when we process the expression,
        // any other case, calling a selector to a value that can't be applied we return an empty list
        // we follow the same logic as we did with lexer/parser.
        // parse don't validate; we don't get all the segments first and then try to apply them
        while let Some(segment) = self.parse_seg()? {
            writer.clear();
            match segment.kind {
                // it is self.root and not &self.root because root is already a reference
                SegmentKind::Child => {
                    apply_child_seg::<T>(self.root, &reader, &mut writer, &segment)
                }
                SegmentKind::Descendant => {
                    apply_descendant_seg::<T>(self.root, &reader, &mut writer, &segment)
                }
            }
            std::mem::swap(&mut reader, &mut writer);
        }

        // the final nodelist is in reader after the last swap
        Ok(reader)
    }

    // we have already checked for empty input
    // the query(json path expression) is not a json string like pointer, which means that we won't
    // check if a Unicode sequence could map to '$'
    // jsonpath-query = root-identifier *(S segment)
    //
    // root identifier('$') followed by zero or more segments
    fn check_root(&mut self) -> Result<(), PathError> {
        let root = self.buffer[self.pos];

        if root != b'$' {
            return Err(PathError {
                kind: PathErrorKind::UnexpectedCharacter { byte: root },
                pos: self.pos,
            });
        }

        self.pos += 1; // move past '$'
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
            (b'.', Some(b'.')) if self.buffer.get(self.pos + 2).is_none() => Err(PathError {
                kind: PathErrorKind::UnexpectedEndOf,
                pos: self.pos + 1,
            }),
            (b'.', Some(b'.')) => {
                // descendant-segment  = ".." (bracketed-selection / wildcard-selector / member-name-shorthand)
                // we have 2 dots(..) followed by either a bracketed-selection or a wildcard or a member name shorthand
                // if what follows '..' is not '[', we call parse_shorthand(), if the current character
                // is '*', it's a wildcard shorthand, otherwise it is treated as the start of name-shorthand
                // whitespaces are not allowed in between
                // consume ..
                self.pos += 2;
                Ok(Some(self.parse_notation(SegmentKind::Descendant)?))
            }
            (b'.' | b'[', None) => Err(PathError {
                kind: PathErrorKind::UnexpectedEndOf,
                pos: self.pos,
            }),
            // child-segment = bracketed-selection / ("." (wildcard-selector / member-name-shorthand))
            // we have either bracketed selection or '.' followed by either a wildcard or a member-name-shorthand
            (b'.' | b'[', Some(_)) => Ok(Some(self.parse_notation(SegmentKind::Child)?)),
            // a segment always starts with '.', '..' or '['
            _ => Err(PathError {
                kind: PathErrorKind::UnexpectedCharacter { byte: current },
                pos: self.pos,
            }),
        }
    }

    fn parse_notation(&mut self, kind: SegmentKind) -> Result<Segment, PathError> {
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
                    // skip pass the closing bracket ']'
                    self.pos += 1;
                    break;
                }
                // after parsing a selector if we don't encounter ']', we expect comma to separate
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

        // 1 edge case to consider: [<selector>,]
        // at this point we exited the loop  because we encountered ']' but if the exact previous
        // character is ',' we have invalid syntax; after comma we expect another selector
        // this is different from [<selector>,+S]
        // if we have at least 1 whitespace parse_selector() would skip it and then try to read ']'
        // which is not a valid start for any selector
        if self.buffer[self.pos - 1] == b',' {
            return Err(PathError {
                kind: PathErrorKind::UnexpectedCharacter {
                    byte: self.buffer[self.pos],
                },
                pos: self.pos,
            });
        }

        Ok(segment)
    }

    // when this method gets called pos is at the 1st character after . or ..
    // member name shorthand is still treated as a name selector with more strict syntax
    fn parse_shorthand(&mut self, kind: SegmentKind) -> Result<Segment, PathError> {
        self.pos += 1;
        let current = self.buffer[self.pos];
        let mut segment = Segment::new(kind);

        if current == b'*' {
            segment.selectors.push(Selector::WildCard);
            self.pos += 1;
        } else {
            // we can not have shorthand notation inside a bracketed selection
            // if we need either a wildcard, or a name selector in a multi-selector segment
            // we need to use the full syntax
            segment
                .selectors
                .push(Selector::Name(self.parse_name_shorthand()?));
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

    // We never check if name exceeds the StringValueLengthLimit because even if it does, no key
    // will ever map to that and we will return None
    // toDo: when we check if the name exists we DO NOT do any normalization prior to comparison
    // rfc forbids it, we MUST check the underlying Unicode scalar values that they are the exact same
    fn parse_name(&mut self) -> Result<String, StringError> {
        let quote = self.buffer[self.pos];
        let len = self.buffer.len();
        let mut name = String::new();
        self.pos += 1; // skip opening quote

        while self.pos < len && self.buffer[self.pos] != quote {
            let current = self.buffer[self.pos];
            match current {
                c if c.is_ascii_control() => {
                    return Err(StringError {
                        kind: StringErrorKind::InvalidControlCharacter { byte: current },
                        pos: self.pos,
                    });
                }
                c if !c.is_ascii() => {
                    name.push(utf8::read_utf8_char(self.buffer, self.pos));
                    self.pos += utf8::utf8_char_width(current) - 1;
                }
                b'\\' => {
                    let next = self.buffer.get(self.pos + 1);
                    match next {
                        // This is the extra edge case where ' has to be escaped when the name is in single quotes
                        // Not part of the escape characters defined for json string
                        Some(b'\'') if quote == b'\'' => {
                            name.push('\'');
                            // pos is at \, next is ', move pos to ' so when we exit this arm and
                            // self.pos += 1 will move pos past '. If we don't do this increment
                            // pos would be moved to after exiting the arm, and our loop will end
                            // because incorrectly will treat the escaped ' as a closing quote
                            self.pos += 1;
                        }
                        _ => {
                            escapes::check_escape_character(self.buffer, self.pos)?;
                            name.push(escapes::map_escape_character(self.buffer, self.pos));
                            self.pos += escapes::len(self.buffer, self.pos) - 1;
                        }
                    }
                }
                _ => name.push(current as char),
            }
            self.pos += 1;
        }
        if self.pos == len {
            return Err(StringError {
                kind: StringErrorKind::UnexpectedEndOf,
                pos: self.pos - 1,
            });
        }
        self.pos += 1; // skip closing quote

        Ok(name)
    }

    // we don't have to run any utf8 validation because buffer is the underlying vector of the input query
    // and since it is &str Rust won't allow an invalid utf8 sequence; it is exactly the same as pointer
    // the 1st character of member name shorthand is a special case because it can not be a digit
    fn parse_name_shorthand(&mut self) -> Result<String, PathError> {
        // term used by rfc; name_first, the 1st character of member_name_shorthand
        // name_char is every other character; the only difference is that name_first can't be a digit
        let len = self.buffer.len();
        let mut name = String::new();
        let start = self.pos;
        let mut current = self.buffer[self.pos];

        // . and [ will indicate the start of a new segment
        // comparison operators indicate that the name-shorthand could be part of a comparison
        // expression if syntax is valid
        // logical operators indicate the name-shorthand is part of a logical expression
        // ) and ] indicate that the name-shorthand is part of the parenthesized expression and part
        // of a filter selector respectively
        //
        // basically all cases other than . and [ handle the possible syntax of a filter selector
        // if it is not part of a filter selector we will try to parse the next segment, and we will
        // fail because '<' in '$.price < ' does not start a segment. Read the comment above
        // parse_embedded_query()
        // ',' is for fn_expr args
        while self.pos < len
            && !matches!(
                current,
                b'.' | b'[' | b'<' | b'>' | b'=' | b'!' | b'&' | b'|' | b')' | b']' | b','
            )
            && !parsing::is_rfc_whitespace(current)
        {
            match current {
                c if !c.is_ascii() => {
                    name.push(utf8::read_utf8_char(self.buffer, self.pos));
                    self.pos += utf8::utf8_char_width(current) - 1;
                }
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
                            pos: self.pos,
                        });
                    }
                    name.push(c as char);
                }
                c => {
                    if !is_valid_name_char(c) {
                        return Err(PathError {
                            kind: PathErrorKind::UnexpectedCharacter { byte: c },
                            pos: self.pos,
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
    // have to process the number and then check if we encounter ':', if we do, we call parse_slice()
    // otherwise it is an index
    fn parse_numeric(&mut self) -> Result<Selector, PathError> {
        let num;

        if self.buffer[self.pos] == b':' {
            return self.parse_slice(None);
        }

        num = self.parse_index()?;
        // from the slice syntax: start *S ":" *S end *S ":" *S step  we need to skip whitespaces
        // between first index and ':'. If we don't encounter ':' we have an index case not a slice
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
                        message: "minus sign must be followed by a non-zero digit",
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
                        message: "leading zeros are not allowed",
                    },
                    pos: self.pos,
                });
            }
            _ => (),
        }
        // todo: we need to find a way to adjust the bounds here because we don't want to accept anything
        // above the INT_LIMIT defined in number.rs; with the current implementation we accept up
        // to i64::MAX
        // can infer the type, we don't need to do i64::atoi
        Ok(Atoi::atoi(self.buffer, &mut self.pos)?)
    }

    // when this method gets called self.buffer[self.pos] is at ':'
    // rfc syntax: start *S ":" *S end *S ":" *S step
    fn parse_slice(&mut self, start: Option<i64>) -> Result<Selector, PathError> {
        let len = self.buffer.len();
        let mut step: Option<i64> = Some(1); // default step value
        let mut end: Option<i64> = None;
        // consume the first ':'(we encountered that during the parse_numeric() and parse_slice() was called)
        self.pos += 1;

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
    //
    // toDo: cache the subquery during evaluation? if the rhs or the lhs is a subquery the nodelist will not change in any iteration
    fn parse_filter(&mut self) -> Result<Selector, PathError> {
        self.pos += 1; // consume '?'
        // when this method gets called self.pos is at '?', we need to handle a case like "? S" where
        // '?' is not followed by a logical-expr
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
    // and logical_and read the comment in fn_args()
    fn parse_logical_or(&mut self) -> Result<LogicalExpr, PathError> {
        let lhs = self.parse_logical_and()?;
        self.parse_logical_or_tail(lhs)
    }

    // why we loop?
    // we handle cases where we have multiple AND cases with left to right associativity
    // x || y || z
    // the logical expression is (x || y) || z
    // pass x, rhs is y, then (x || y) is the lhs for || z
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

    // why we loop?
    // we handle cases where we have multiple AND cases with left to right associativity
    // x && y && z
    // the logical expression is (x && y) && z
    // pass x, rhs is y, then (x && y) is the lhs for && z
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

        // paren-expr = [logical-not-op S] "(" S logical-expr S ")"
        // test-expr  = [logical-not-op S] (filter-query / function-expr)
        //
        // for logical not operator we need to look ahead and consider only the 3 valid cases
        // 1. parenthesized expr
        // 2. filter-query
        // 3. function expr
        //
        // we can not have a case like !! or ! followed by a comparison expression
        // "$[?!$[0].foo > 2 || @[1] <= 4]"
        // this results to an UnexpectedCharacter error for '>'
        // we look after '!' and we treat '$[0].foo' as a Test expression and not the lhs of a comparison
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
            None => {
                match lhs {
                    // query is moved to the TestExpr and we can no longer access lhs after
                    Comparable::EmbeddedQuery(query) => {
                        Ok(LogicalExpr::Test(TestExpr::EmbeddedQuery(query)))
                    }
                    Comparable::Literal(_) => todo!(
                        "since no comp_operator found, it must be a test expr, and a test expr can only be fn_expr or filter query. Create an error variant"
                    ),
                    Comparable::FnExpr(expr) => Ok(LogicalExpr::Test(TestExpr::FnExpr(expr))),
                }
            }
        }
    }

    fn parse_comparable(&mut self) -> Result<Comparable, PathError> {
        let current = self.buffer[self.pos];

        match current {
            // literals as defined in the rfc (number / string-literal / true / false / null)
            b'-' | b'0'..=b'9' | b'"' | b'\'' | b't' | b'f' | b'n' => {
                Ok(Comparable::Literal(self.parse_literal()?))
            }
            // For embedded queries we just parse for now, evaluation will happen when applying the filter selector
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
    // $.price> is an invalid name shorthand because it is not part of a filter selector, but in the
    // filter selector case we need to return a child segment with a single selector and let the
    // call of parse_embedded_seg() handle the next character; we update the ending conditions in
    // parse_name_shorthand() and let the caller handle it.
    //
    // parse_name_shorthand() -> returns price to parse_shorthand()
    // parse_shorthand() -> returns a segment with a single selector(shorthand notation can't have multiple selectors) to parse_notation()
    // parse_notation() -> returns to parse_seg()
    // parse_seg() -> returns to parse_embedded_seg()
    // parse_embedded_seg() -> returns a segment for the embedded query from parse_comparable() and
    // is called again.
    // This is the key point it needs to look for any expression related character. If we didn't have
    // that check, calling parse_seg() directly would result in an error because no segment starts
    // with '<'. Note that we don't check for the full operators(<=, >=, !=, &&, ||). We encounter '<'
    // and we know we processed all the segments for the subquery. The control returns to the parse_comparison_or_test()
    // which checks if the current, next is a valid comparison operator. If not control returns to
    // parse_basic_expr() which will handle parenthesized expression and then back to parse_logical_and/or()
    // Those 2 methods will handle logical operators. But what if we have $[?@.price&* 10]?
    // &* will not matching to anything in the parse_filter() call chain and the control returns to
    // parse_selector() and then to parse_bracket() which expects after processing the current selector
    // a comma to indicate multiple selectors or ']' to stop processing the current one, but it encounters
    // an unpaired '&' and we get a PathError
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

        Ok(EmbeddedQuery {
            query_type,
            segments,
        })
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
            _ => unreachable!("parse_fn_expr() called with invalid byte {} at index {}", current, self.pos),
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
        // consume '('
        self.pos += 1;
        self.skip_ws()?;

        while self.pos < self.buffer.len() && self.buffer[self.pos] != b')' {
            // starts with '!' or '(', it is a LogicalExpr.
            if self.buffer[self.pos] == b'!' || self.buffer[self.pos] == b'(' {
                args.push(FnExprArg::LogicalExpr(self.parse_logical_or()?));
            } else {
                // This is the ambiguous part. It is known as the "First set problem"
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
                        Comparable::EmbeddedQuery(q) => {
                            args.push(FnExprArg::EmbeddedQuery(q))
                        }
                        Comparable::FnExpr(f) => {
                            args.push(FnExprArg::FnExpr(Box::new(f)))
                        },
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
        // consume ')'
        self.pos += 1;

        Ok(args)
    }

    // we read literals in 2 cases: filter selectors or function arguments
    fn parse_literal(&mut self) -> Result<Value, PathError> {
        let current = self.buffer[self.pos];

        match current {
            b'-' | b'0'..=b'9' => {
                // toDo: maybe this can be optimized to build the string as we read the number so we can call parse directly
                // now we scan 3 times the same buffer. once for read and twice for parse
                // maybe we can make read return the current byte? so look at a byte at a time
                let start = self.pos;
                // why not from? This the only case where we have to convert from a NumericError to
                // a PathError, so I thought to do it via map_err(). The body of the closure would
                // be the body of the from() impl.
                number::read(self.buffer, &mut self.pos).map_err(|err| {
                    let pos = err.pos;
                    PathError {
                        kind: PathErrorKind::Numeric(err),
                        pos,
                    }
                })?;
                Ok(Value::Number(number::parse(&self.buffer[start..self.pos])))
            }
            b'"' | b'\'' => {
                let string = self.parse_name()?;
                Ok(Value::String(string))
            }
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
// 3. for every element in the nodelist, the input list, we apply each selector, in this case we have
// 2 name selectors.
// output: ["]Alice", "NYC", "Bob", "LA", "Charlie", "Chicago"]
// It is important to note that from the output we see that we applied all the selector for the 1st
// element we got back "Alice", "NYC" and so on
//
// applying each segment essentially increases the depth by 1 level
fn apply_child_seg<'v, T: Tracker<'v>>(
    root: &'v Value,
    reader: &Vec<PathNode<'v, T::Trace>>,
    writer: &mut Vec<PathNode<'v, T::Trace>>,
    segment: &Segment,
) {
    if reader.is_empty() {
        return;
    }

    for v in reader.iter() {
        for selector in &segment.selectors {
            apply_selector::<T>(root, writer, selector, v);
        }
    }
}

// read the comment above the test_desc_seg_and_multi_selector() method in value.rs
//
// it is dfs on the values of the node, and if a match is found as we visit them for the 1st time(preorder)
// not when recursion backtracks(postorder) we append them in the list
fn apply_descendant_seg<'v, T: Tracker<'v>>(
    root: &'v Value,
    reader: &Vec<PathNode<'v, T::Trace>>,
    writer: &mut Vec<PathNode<'v, T::Trace>>,
    segment: &Segment,
) {
    if reader.is_empty() {
        return;
    }

    for v in reader.iter() {
        for selector in &segment.selectors {
            dfs::<T>(root, v, writer, selector);
        }
    }
}

fn dfs<'v, T: Tracker<'v>>(
    root: &'v Value,
    current: &PathNode<'v, T::Trace>,
    writer: &mut Vec<PathNode<'v, T::Trace>>,
    selector: &Selector,
) {
    if !current.val.is_object() && !current.val.is_array() {
        return;
    }

    apply_selector::<T>(root, writer, selector, current);
    match current.val {
        Value::Object(map) => {
            for (key, val) in map.iter() {
                let trace = T::descend(&current.trace, Step::Key(key));
                let node = PathNode { val, trace };
                dfs::<T>(root, &node, writer, selector);
            }
        }
        Value::Array(arr) => {
            for (i, val) in arr.iter().enumerate() {
                let trace = T::descend(&current.trace, Step::Index(i));
                let node = PathNode { val, trace };
                dfs::<T>(root, &node, writer, selector);
            }
        }
        _ => unreachable!("dfs() was called in a non container node"),
    }
}

fn apply_selector<'v, T: Tracker<'v>>(
    root: &'v Value,
    writer: &mut Vec<PathNode<'v, T::Trace>>,
    selector: &Selector,
    current: &PathNode<'v, T::Trace>,
) {
    match (current.val, selector) {
        (Value::Object(map), Selector::Name(name)) => {
            // very important to borrow key from map and not from the selector. If we chose to go
            // with the selector we would have lifetime problems that bubble up to parse() because
            // segment is a short-lived value that gets dropped immediately after the call to
            // apply it
            if let Some((key, val)) = map.get_key_value(name) {
                let trace = T::descend(&current.trace, Step::Key(key));
                writer.push(PathNode { val, trace });
            }
        }
        (Value::Object(map), Selector::WildCard) => {
            for (key, val) in map.iter() {
                let trace = T::descend(&current.trace, Step::Key(key));
                writer.push(PathNode { val, trace });
            }
        }
        (Value::Array(arr), Selector::WildCard) => {
            for (i, val) in arr.iter().enumerate() {
                let trace = T::descend(&current.trace, Step::Index(i));
                writer.push(PathNode { val, trace });
            }
        }
        (Value::Array(arr), Selector::Index(index)) => {
            // if index is negative and its absolute value is greater than length, the n_idx can
            // still be negative, and we can not call get() with anything other than usize
            // len = 2, index = -4 => n_idx = -2
            // toDo: add explanation why we can cast len to i64 without any loss
            let n_idx = normalize_index(*index, arr.len() as i64);
            if let Ok(i) = usize::try_from(n_idx) {
                if let Some(val) = arr.get(i) {
                    let trace = T::descend(&current.trace, Step::Index(i));
                    writer.push(PathNode { val, trace });
                }
            }
        }
        (Value::Array(arr), Selector::Slice(slice)) => {
            let range = Range::new(slice, arr.len() as i64);

            for i in range {
                // toDo: we should break in None?
                if let Some(val) = arr.get(i) {
                    let trace = T::descend(&current.trace, Step::Index(i));
                    writer.push(PathNode { val, trace });
                }
            }
        }
        // the expression is evaluated for every value
        // check the second to last entry in test_valid_selectors() in value.rs
        (Value::Object(map), Selector::Filter(expr)) => {
            for (key, val) in map.iter() {
                if expr.evaluate(root, val) {
                    let trace = T::descend(&current.trace, Step::Key(key));
                    writer.push(PathNode { val, trace });
                }
            }
        }
        // the expression is evaluated for every element
        (Value::Array(arr), Selector::Filter(expr)) => {
            for (i, elem) in arr.iter().enumerate() {
                if expr.evaluate(root, elem) {
                    let trace = T::descend(&current.trace, Step::Index(i));
                    writer.push(PathNode { val: elem, trace });
                }
            }
        }
        _ => (),
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
                        message: "minus sign must be followed by a non-zero digit",
                    },
                    pos: 2,
                },
            ),
            (
                "$[-01]",
                PathError {
                    kind: PathErrorKind::InvalidIndex {
                        message: "minus sign must be followed by a non-zero digit",
                    },
                    pos: 2,
                },
            ),
            (
                "$[02]",
                PathError {
                    kind: PathErrorKind::InvalidIndex {
                        message: "leading zeros are not allowed",
                    },
                    pos: 2,
                },
            ),
            (
                "$[-9223372036854775809]",
                PathError {
                    kind: PathErrorKind::InvalidIndex {
                        message: "number out of range",
                    },
                    pos: 3,
                },
            ),
            (
                "$[9223372036854775808]",
                PathError {
                    kind: PathErrorKind::InvalidIndex {
                        message: "number out of range",
                    },
                    pos: 2,
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
                        found: FnType::NodesType,
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
                        found: FnType::ValueType,
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
                        found: FnType::NodesType,
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
                        found: FnType::Logical,
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
                        found: FnType::Logical,
                    }),
                    pos: 3,
                },
            ),
        ]
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
            let result  = query.parse::<NoOpTracker>();

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
