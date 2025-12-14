use crate::parsing::number::{Atoi, OutOfRangeError};
use crate::parsing::utf8;

pub(super) fn full_match() {}
pub(super) fn partial_match() {}

#[derive(Debug, PartialEq)]
enum Regexp {
    Empty,
    Concat(usize, usize),
    Union(usize, usize),
    Star(usize),
    Atom(CharClass),
}

#[derive(Debug, PartialEq)]
enum CharClass {
    Literal(char),
    Dot,
    ClassExpr(ClassExpr),
    Property(Property),
}

#[derive(Debug, PartialEq)]
struct ClassExpr {
    negated: bool,
    items: Vec<ExprItem>,
}

#[derive(Debug, PartialEq)]
enum ExprItem {
    Literal(char),
    Property(Property),
    Range(char, char),
}

impl ExprItem {
    // extracts the char from a literal to form a range
    fn as_char(&self) -> Option<char> {
        match self {
            // char implements Copy, we don't try to move out of a borrowed value
            ExprItem::Literal(c) => Some(*c),
            _ => None,
        }
    }
}

// toDo: try this with Peekable<char> and working with chars directly
struct Parser<'a> {
    buffer: &'a [u8],
    pos: usize,
    nodes: Vec<Regexp>,
}

impl<'a> Parser<'a> {
    fn new(buffer: &'a [u8]) -> Self {
        Self {
            buffer,
            pos: 0,
            // toDo: explain the indices approach
            nodes: Vec::new(),
        }
    }

    fn parse(&mut self) -> Result<usize, RegexpError> {
        if self.buffer.is_empty() {
            return Ok(self.emit(Regexp::Empty));
        }
        Ok(self.parse_union()?)
    }

    // Much easier than parse_concat() because we can keep going as long as we encounter the
    // pipe operator. We use the loop for the left-right associativity as explained in
    // parse_logical_and/or() in path.rs
    fn parse_union(&mut self) -> Result<usize, RegexpError> {
        let mut lhs = self.parse_concat()?;

        while self.peek().copied() == Some(b'|') {
            self.consume(1);
            let rhs = self.parse_concat()?;
            lhs = self.emit(Regexp::Union(lhs, rhs));
        }
        Ok(lhs)
    }

    fn parse_concat(&mut self) -> Result<usize, RegexpError> {
        let mut lhs = self.parse_quantifier()?;
        // In theory this should be similar to parse_logical_and() where we follow left to right
        // associativity. It is not as straightforward though, because there is no concat operator
        // abc: ((ab)c) Regex::Concat(Regex::Concat(a,b), c). We use the loop for the left-right
        // associativity as explained in parse_logical_and/or() in path.rs
        //
        // Case: |
        //  abc|d
        // Case: )
        //  this case is caused by a nested expression; it is triggered when parse_atom() encounters
        //  '(' and calls parse_union()
        //  a(bcd)e: parse_atom() will return for d the node Regex::Concat(Regex::Concat(b,c), d)
        //  and then quantifier will peek, won't find anything and the control returns to
        //  parse_concat() which encounters ) and breaks, returns to parse_union() and now the
        //  recursive call returns back to parse_atom()
        // Case: None
        //  we exhausted the buffer, we parsed the entire expression
        //
        // we never consider quantifiers because by the time concat loops we have already processed
        // them if any.
        //
        // a?bb* -> parse_atom() returns a and then parse_quantifier() peeks and sees ?, expands it and
        // control returns back to parse_concat()
        loop {
            match self.peek() {
                Some(b'|') | Some(b')') | None => break,
                _ => {
                    let rhs = self.parse_quantifier()?;
                    lhs = self.emit(Regexp::Concat(lhs, rhs));
                }
            }
        }
        Ok(lhs)
    }

    fn parse_quantifier(&mut self) -> Result<usize, RegexpError> {
        let atom_idx = self.parse_atom()?;

        match self.peek() {
            Some(b'*') => {
                self.consume(1);
                let node = Regexp::Star(atom_idx);
                Ok(self.emit(node))
            }
            Some(b'+') => {
                // aa*
                self.consume(1);
                let star = Regexp::Star(atom_idx);
                let start_idx = self.emit(star);
                Ok(self.emit(Regexp::Concat(atom_idx, start_idx)))
            }
            Some(b'?') => {
                // a|ε
                self.consume(1);
                let empty = Regexp::Empty;
                let empty_idx = self.emit(empty);
                Ok(self.emit(Regexp::Union(atom_idx, empty_idx)))
            }
            Some(b'{') => {
                self.consume(1);
                let (min, max) = self.parse_quant_range()?;
                match max {
                    Some(m) => Ok(self.expand_bounded(atom_idx, min, m)?),
                    None => Ok(self.expand_unbounded(atom_idx, min)?),
                }
            }
            _ => Ok(atom_idx),
        }
    }

    // this was hard to understand; we need to expand the range the same way we did for the
    // other quantifiers.
    // {2,4} it means at least 2, at most 4
    // we need to accept: aa, aaa, aaaa
    //
    // the regex we want to quantify is the node at atom_idx; for the aa case is nothing but
    // concatenation of the node at atom_idx twice. Regex::Concat(atom_idx, atom_idx). That is
    // the case if we had {3,4}, aaa is the concatenation of the 1st two with the 3rd
    // concatenate Regex::Concat(atom_idx, atom_idx) -> get the new index
    // Regex::Concat(new_idx, atom_idx) or Regex::Concat(Regex::Concat(atom_idx, atom_idx), atom_idx)
    //
    // to cover all cases we can write it as aaa?a?
    // aa is aa and then 2 other terms are empty, aaεε
    // aaa is aa and then 1 term is a, the other empty, aaaε
    // aaaa is aa and then both terms are a, aaaa
    //
    // what we observe is that we need to create the a | ε expression which is what a? expands to
    // how many times we are going to work with that new node? max - min times
    //
    // Case: {3}
    //  we treat it as {3,3} which has a delta of 0; we create only min nodes, so a, aa, aaa
    //
    // the loop at the end is what builds the small ast for this use case.
    //  if we trace a{2,4}: parse_atom() calls emit to create the first node at 0 which will be
    //  an atom literal 'a'. We need at least "aa", which in theory we don't need 'a' but in order
    //  to create "aa" we need those intermediates nodes, then it's all about connecting nodes via
    //  indices we never allocate for "aa". This is true for a{3,5} we build 'a', then "aa" and then
    //  "aaa" which is just connecting the indices of "aa" and 'a'
    //
    fn expand_bounded(&mut self, atom_idx: usize, min: u8, max: u8) -> Result<usize, RegexpError> {
        // toDo: set bounds around 100 for min/max to prevent resource exhaustion
        let mut tmp = Vec::new();
        for _ in 0..min {
            tmp.push(atom_idx);
        }

        let delta = max - min;
        // Case: a{0,4}, a?a?a?a?
        //  the mandatory part, min, is 0 in this case, which means delta is max.
        if delta > 0 {
            let empty_idx = self.emit(Regexp::Empty);
            // creates a | ε
            let optional_idx = self.emit(Regexp::Union(atom_idx, empty_idx));

            for _ in 0..delta {
                tmp.push(optional_idx);
            }
        }
        // toDo: Specifically, range quantifiers (as in a{2,4}) provide particular challenges
        // for both existing and I-Regexp focused implementations. Implementations may therefore
        // limit range quantifiers in composability (disallowing nested range quantifiers such
        // as (a{2,4}){2,4}) or range (disallowing very large ranges such as a{20,200000}),
        // or detect and reject any excessive resource consumption caused by range quantifiers.
        //

        // at this point we need to link them
        // create aa then take its index and concat with the optional_idx to create aaa? and
        // then take that index and create aaa?a?
        let mut i = tmp[0];
        for node in tmp.into_iter().skip(1) {
            // we concat i with tmp[j] not j, i had this initially which was wrong Concat(i, j)
            // because my loop condition was for j in 1..tmp.len() so i was looking at indices
            // we want the value at the given index; the most sane solution is to iterate over the
            // values instead
            i = self.emit(Regexp::Concat(i, node));
        }
        Ok(i)
    }

    // a{2,} is nothing but a{2}a*
    // it means at least 2
    fn expand_unbounded(&mut self, atom_idx: usize, min: u8) -> Result<usize, RegexpError> {
        // toDo: set bounds for min/max to prevent resource exhaustion
        let star_idx = self.emit(Regexp::Star(atom_idx));
        // Case: a{0,} is just a*
        if min == 0 {
            return Ok(star_idx);
        }

        let mut i = atom_idx;
        for _ in 1..min {
            i = self.emit(Regexp::Concat(i, atom_idx));
        }

        Ok(self.emit(Regexp::Concat(i, star_idx)))
    }

    // parses a range quantifier
    // we return min, Optional<max>
    fn parse_quant_range(&mut self) -> Result<(u8, Option<u8>), RegexpError> {
        match self.peek() {
            Some(b) if !b.is_ascii_digit() => Err(RegexpError {
                kind: RegexpErrorKind::UnexpectedCharacter(*b),
                pos: self.pos,
            }),
            Some(_) => {
                // we can infer the type here, we don't need to be explicit and call u8::atoi()
                let min = Atoi::atoi(&mut self.buffer, &mut self.pos)?;
                match self.peek() {
                    Some(b',') => {
                        self.consume(1);
                        match self.peek() {
                            // {2,}
                            Some(b'}') => {
                                self.consume(1);
                                Ok((min, None))
                            }
                            Some(b) if !b.is_ascii_digit() => Err(RegexpError {
                                kind: RegexpErrorKind::UnexpectedCharacter(*b),
                                pos: self.pos,
                            }),
                            Some(_) => {
                                let max = Atoi::atoi(&mut self.buffer, &mut self.pos)?;
                                match self.peek() {
                                    Some(b'}') => {
                                        self.consume(1);
                                        Ok((min, Some(max)))
                                    }
                                    Some(b) => Err(RegexpError {
                                        kind: RegexpErrorKind::UnexpectedCharacter(*b),
                                        pos: self.pos,
                                    }),
                                    None => Err(RegexpError {
                                        kind: RegexpErrorKind::UnexpectedEof,
                                        pos: self.pos - 1,
                                    }),
                                }
                            }
                            None => Err(RegexpError {
                                kind: RegexpErrorKind::UnexpectedEof,
                                pos: self.pos - 1,
                            }),
                        }
                    }
                    // Case: {2}
                    // we can rewrite it as {2,2} which will allow us to expand it as {min, max}
                    // range. In both cases our range is bounded while in {2,} is not
                    Some(b'}') => {
                        self.consume(1);
                        Ok((min, Some(min)))
                    }
                    Some(b) => Err(RegexpError {
                        kind: RegexpErrorKind::UnexpectedCharacter(*b),
                        pos: self.pos,
                    }),
                    None => Err(RegexpError {
                        kind: RegexpErrorKind::UnexpectedEof,
                        pos: self.pos - 1,
                    }),
                }
            }
            None => Err(RegexpError {
                kind: RegexpErrorKind::UnexpectedEof,
                pos: self.pos - 1,
            }),
        }
    }

    // atom = NormalChar / charClass / ( "(" i-regexp ")" )
    // charClass = "." / SingleCharEsc / charClassEsc / charClassExpr
    //
    // if we merge them two: NormalChar / "." / SingleCharEsc / charClassEsc / charClassExpr
    // NormalChar and SingleCharEsc are treated as char literals
    //
    // '(' implies a nested regex, start the call chain from the top
    // '[', '.', '\' all indicate the start of char class
    // '.' is any char
    // '[' implies the start of character set(charClassExpr)
    // '\' describes either a single escaped character or the start of char property
    //
    // the last 2 arms are for NormalChar
    // if we get a character that must be escaped unescaped it is an error
    // in any other case, it is a char literal
    fn parse_atom(&mut self) -> Result<usize, RegexpError> {
        match self.peek() {
            Some(b'(') => {
                self.consume(1);
                let expr_idx = self.parse_union()?;
                match self.peek() {
                    Some(b')') => {
                        self.consume(1);
                        Ok(expr_idx)
                    }
                    Some(b) => Err(RegexpError {
                        kind: RegexpErrorKind::UnexpectedCharacter(*b),
                        pos: self.pos,
                    }),
                    None => Err(RegexpError {
                        kind: RegexpErrorKind::UnexpectedEof,
                        pos: self.pos - 1,
                    }),
                }
            }
            Some(b'[') | Some(b'.') | Some(b'\\') => {
                let atom = self.parse_class()?;
                Ok(self.emit(Regexp::Atom(atom)))
            }
            Some(b) if is_escape_char(*b) => Err(RegexpError {
                kind: RegexpErrorKind::UnexpectedCharacter(*b),
                pos: self.pos,
            }),
            Some(_) => {
                let char = self.parse_char();
                Ok(self.emit(Regexp::Atom(CharClass::Literal(char))))
            }
            None => Err(RegexpError {
                kind: RegexpErrorKind::UnexpectedEof,
                pos: self.pos - 1,
            }),
        }
    }

    // charClass = "." / SingleCharEsc / charClassEsc / charClassExpr
    // '.' is any char
    // '[' implies the start of character set(charClassExpr)
    // '\' describes either a single escaped character or the start of char property
    fn parse_class(&mut self) -> Result<CharClass, RegexpError> {
        match self.buffer[self.pos] {
            b'.' => Ok(CharClass::Dot),
            b'[' => Ok(CharClass::ClassExpr(self.parse_class_expr()?)),
            b'\\' => Ok(CharClass::from(self.parse_escape()?)),
            _ => unreachable!("parse_class() was called with {}", self.buffer[self.pos]),
        }
    }

    // parses a character set
    // no quantifiers or nested regex is allowed inside a set
    fn parse_class_expr(&mut self) -> Result<ClassExpr, RegexpError> {
        // consume '['
        self.consume(1);
        let mut items = Vec::new();
        let len = self.buffer.len();
        let mut negated = false;

        if let Some(b) = self.peek() {
            if *b == b'^' {
                negated = true;
                self.consume(1);
            }
        }

        if let Some(b'-') = self.peek() {
            self.consume(1);
            items.push(ExprItem::Literal('-'));
        }

        while self.pos < len && self.buffer[self.pos] != b']' {
            let start = self.pos;
            let item = self.parse_class_expr_item()?;
            if let Some(b) = self.peek() {
                // we have 4 cases to consider when we parse a hyphen unescaped
                // [^-...]
                // [-a] is treated as a literal
                // [a-] is also tread as a literal
                // [a-z] now we have a range because we encountered CCchar '-' CCchar
                // for the first three cases hyphen is treated as literal and for the last one as range
                //
                // in any other case, it must be escaped
                if *b == b'-' {
                    self.consume(1);
                    match self.peek() {
                        //[a-]
                        Some(b) if *b == b']' => {
                            // we don't have to consume ']' now; it is consumed when we exit the loop
                            items.push(item);
                            items.push(ExprItem::Literal('-'));
                            break;
                        }
                        Some(_) => {
                            let rhs = self.parse_class_expr_item()?;
                            if !is_valid_cs_range(&item, &rhs) {
                                return Err(RegexpError {
                                    kind: RegexpErrorKind::InvalidCsRange,
                                    pos: start,
                                });
                            }
                            // always safe to call after is_valid_cs_range()
                            items.push(ExprItem::Range(
                                item.as_char().unwrap(),
                                rhs.as_char().unwrap(),
                            ))
                        }
                        // we peeked, and we didn't get anything, we have an Eof case
                        // we have 2 choices, either we return an err here or do nothing exit
                        // the loop and catch it after
                        None => {
                            return Err(RegexpError {
                                kind: RegexpErrorKind::UnexpectedEof,
                                pos: self.pos - 1,
                            });
                        }
                    }
                } else {
                    items.push(item);
                }
            }
        }

        if self.pos >= len {
            return Err(RegexpError {
                kind: RegexpErrorKind::UnexpectedEof,
                pos: self.pos - 1,
            });
        }
        // consume ']'
        self.consume(1);

        Ok(ClassExpr { negated, items })
    }

    fn parse_class_expr_item(&mut self) -> Result<ExprItem, RegexpError> {
        match self.buffer[self.pos] {
            b'\\' => Ok(ExprItem::from(self.parse_escape()?)),
            // can't appear unescaped
            b => {
                if is_escape_char(b) {
                    return Err(RegexpError {
                        kind: RegexpErrorKind::UnexpectedCharacter(b),
                        pos: self.pos,
                    });
                }
                Ok(ExprItem::Literal(self.parse_char()))
            }
        }
    }

    fn parse_escape(&mut self) -> Result<Escape, RegexpError> {
        // consume '\'
        self.consume(1);
        match self.peek() {
            Some(b'p') | Some(b'P') => Ok(Escape::Property(self.parse_property()?)),
            Some(b) if !is_escape_char(*b) => Err(RegexpError {
                kind: RegexpErrorKind::UnexpectedCharacter(*b),
                pos: self.pos,
            }),
            Some(_) => Ok(Escape::Literal(self.map_escape_character())),
            None => Err(RegexpError {
                kind: RegexpErrorKind::UnexpectedEof,
                pos: self.pos - 1,
            }),
        }
    }

    fn parse_char(&mut self) -> char {
        let c;
        let current = self.buffer[self.pos];

        if !current.is_ascii() {
            c = utf8::read_utf8_char(self.buffer, self.pos);
            self.consume(utf8::utf8_char_width(current));
        } else {
            c = current as char;
            self.consume(1);
        }
        c
    }

    fn parse_property(&mut self) -> Result<Property, RegexpError> {
        let negated = self.buffer[self.pos] == b'P';
        self.consume(1);

        // need 3 or 4 characters after p/P: {..}
        // this won't work, we don't know if we have a major or a minor category
        // if self.pos + 4 >= self.buffer.len() {
        //     return Err(RegexpError {
        //         kind: RegexpErrorKind::UnexpectedEof,
        //         pos: self.pos - 1,
        //     });
        // }
        match self.peek() {
            Some(b) if *b == b'{' => self.consume(1),
            Some(b) => {
                return Err(RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(*b),
                    pos: self.pos,
                });
            }
            None => {
                return Err(RegexpError {
                    kind: RegexpErrorKind::UnexpectedEof,
                    pos: self.pos - 1,
                });
            }
        }

        let category = self.map_category()?;

        match self.peek() {
            Some(b) if *b == b'}' => self.consume(1),
            Some(b) => {
                return Err(RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(*b),
                    pos: self.pos,
                });
            }
            None => {
                return Err(RegexpError {
                    kind: RegexpErrorKind::UnexpectedEof,
                    pos: self.pos - 1,
                });
            }
        }
        Ok(Property { category, negated })
    }

    fn map_escape_character(&mut self) -> char {
        let c = match self.buffer[self.pos] {
            b'n' => '\n',
            b'r' => '\r',
            b't' => '\t',
            b => b as char,
        };
        self.consume(1);
        c
    }

    // the case where we get a major category into none like /p{L is handled by the caller
    // the category itself is not invalid, /p{ is and is handled by this method(2nd to last arm)
    fn map_category(&mut self) -> Result<GeneralCategory, RegexpError> {
        let current = self.peek();
        let next = self.peek_next();

        // major, minor order
        // after parsing a category we need to advance pos by the category's length, 1 for major,
        // 2 for minor
        let (category, n) = match (current, next) {
            (Some(b'L'), Some(b'}')) => (GeneralCategory::Letter, 1),
            // this is the /p{L case mentioned above, consume L let caller peek and get None
            (Some(b'L'), None) => (GeneralCategory::Letter, 1),
            (Some(b'L'), Some(b'l')) => (GeneralCategory::LetterLowercase, 2),
            (Some(b'L'), Some(b'm')) => (GeneralCategory::LetterModifier, 2),
            (Some(b'L'), Some(b'o')) => (GeneralCategory::LetterOther, 2),
            (Some(b'L'), Some(b't')) => (GeneralCategory::LetterTitlecase, 2),
            (Some(b'L'), Some(b'u')) => (GeneralCategory::LetterUppercase, 2),
            (Some(b'L'), Some(b)) => {
                return Err(RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(*b),
                    pos: self.pos + 1,
                });
            }

            (Some(b'M'), Some(b'}')) => (GeneralCategory::Mark, 1),
            // this is the /p{L case mentioned above, consume L let caller peek and get None
            (Some(b'M'), None) => (GeneralCategory::Mark, 1),
            (Some(b'M'), Some(b'c')) => (GeneralCategory::MarkSpacingCombining, 2),
            (Some(b'M'), Some(b'e')) => (GeneralCategory::MarkEnclosing, 2),
            (Some(b'M'), Some(b'n')) => (GeneralCategory::MarkNonSpacing, 2),
            (Some(b'M'), Some(b)) => {
                return Err(RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(*b),
                    pos: self.pos + 1,
                });
            }

            (Some(b'N'), Some(b'}')) => (GeneralCategory::Number, 1),
            // this is the /p{L case mentioned above, consume L let caller peek and get None
            (Some(b'N'), None) => (GeneralCategory::Number, 1),
            (Some(b'N'), Some(b'd')) => (GeneralCategory::NumberDecimalDigit, 2),
            (Some(b'N'), Some(b'l')) => (GeneralCategory::NumberLetter, 2),
            (Some(b'N'), Some(b'o')) => (GeneralCategory::NumberOther, 2),
            (Some(b'N'), Some(b)) => {
                return Err(RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(*b),
                    pos: self.pos + 1,
                });
            }

            (Some(b'P'), Some(b'}')) => (GeneralCategory::Punctuation, 1),
            // this is the /p{L case mentioned above, consume L let caller peek and get None
            (Some(b'P'), None) => (GeneralCategory::Punctuation, 1),
            (Some(b'P'), Some(b'c')) => (GeneralCategory::PunctuationConnector, 2),
            (Some(b'P'), Some(b'd')) => (GeneralCategory::PunctuationDash, 2),
            (Some(b'P'), Some(b'e')) => (GeneralCategory::PunctuationClose, 2),
            (Some(b'P'), Some(b'f')) => (GeneralCategory::PunctuationFinalQuote, 2),
            (Some(b'P'), Some(b'i')) => (GeneralCategory::PunctuationInitialQuote, 2),
            (Some(b'P'), Some(b's')) => (GeneralCategory::PunctuationOpen, 2),
            (Some(b'P'), Some(b'o')) => (GeneralCategory::PunctuationOther, 2),
            (Some(b'P'), Some(b)) => {
                return Err(RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(*b),
                    pos: self.pos + 1,
                });
            }

            (Some(b'Z'), Some(b'}')) => (GeneralCategory::Separator, 1),
            // this is the /p{L case mentioned above, consume L let caller peek and get None
            (Some(b'Z'), None) => (GeneralCategory::Separator, 1),
            (Some(b'Z'), Some(b'l')) => (GeneralCategory::SeparatorLine, 2),
            (Some(b'Z'), Some(b'p')) => (GeneralCategory::SeparatorParagraph, 2),
            (Some(b'Z'), Some(b's')) => (GeneralCategory::SeparatorSpace, 2),
            (Some(b'Z'), Some(b)) => {
                return Err(RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(*b),
                    pos: self.pos + 1,
                });
            }

            (Some(b'S'), Some(b'}')) => (GeneralCategory::Symbol, 1),
            // this is the /p{L case mentioned above, consume L let caller peek and get None
            (Some(b'S'), None) => (GeneralCategory::Symbol, 1),
            (Some(b'S'), Some(b'c')) => (GeneralCategory::SymbolCurrency, 2),
            (Some(b'S'), Some(b'k')) => (GeneralCategory::SymbolModifier, 2),
            (Some(b'S'), Some(b'm')) => (GeneralCategory::SymbolMath, 2),
            (Some(b'S'), Some(b'o')) => (GeneralCategory::SymbolOther, 2),
            (Some(b'S'), Some(b)) => {
                return Err(RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(*b),
                    pos: self.pos + 1,
                });
            }

            (Some(b'C'), Some(b'}')) => (GeneralCategory::Other, 1),
            // this is the /p{L case mentioned above, consume L let caller peek and get None
            (Some(b'C'), None) => (GeneralCategory::Other, 1),
            (Some(b'C'), Some(b'c')) => (GeneralCategory::OtherControl, 2),
            (Some(b'C'), Some(b'f')) => (GeneralCategory::OtherFormat, 2),
            (Some(b'C'), Some(b'n')) => (GeneralCategory::OtherNotAssigned, 2),
            (Some(b'C'), Some(b'o')) => (GeneralCategory::OtherPrivateUse, 2),
            (Some(b'C'), Some(b)) => {
                return Err(RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(*b),
                    pos: self.pos + 1,
                });
            }

            (None, _) => {
                return Err(RegexpError {
                    kind: RegexpErrorKind::UnexpectedEof,
                    pos: self.pos,
                });
            }
            (Some(b), _) => {
                return Err(RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(*b),
                    pos: self.pos,
                });
            }
        };

        self.consume(n);
        Ok(category)
    }

    fn emit(&mut self, node: Regexp) -> usize {
        self.nodes.push(node);
        self.nodes.len() - 1
    }

    fn peek(&self) -> Option<&u8> {
        self.buffer.get(self.pos)
    }

    fn peek_next(&self) -> Option<&u8> {
        self.buffer.get(self.pos + 1)
    }

    fn consume(&mut self, n: usize) {
        self.pos += n;
    }
}

fn is_escape_char(b: u8) -> bool {
    matches!(
        b,
        b'(' | b')'
            | b'*'
            | b'+'
            | b'-'
            | b'.'
            | b'?'
            | b'['
            | b'\\'
            | b']'
            | b'^'
            | b'n'
            | b'r'
            | b't'
            | b'{'
            | b'|'
            | b'}'
    )
}

// the range is fine as long both operands are characters
// 0-a is fine
// the rule is simple:
//  Both sides must be CCchar (single characters, not category escapes)
//  Start codepoint <= end codepoint
//
// the actual characters don't need to be "related" (like both letters or both digits). Any two
// characters can form a range as long as the first comes before or equals the second in Unicode
// order.
fn is_valid_cs_range(lhs: &ExprItem, rhs: &ExprItem) -> bool {
    match (lhs, rhs) {
        (ExprItem::Literal(s), ExprItem::Literal(e)) => {
            // z-a
            if *s > *e {
                return false;
            }
        }
        // this is the case where we got a-\p{L}, two operands that don't match
        _ => return false,
    }
    true
}

#[derive(Debug, PartialEq)]
struct Property {
    category: GeneralCategory,
    negated: bool,
}

// major, minor
#[derive(Debug, PartialEq)]
enum GeneralCategory {
    Letter,
    LetterLowercase,
    LetterModifier,
    LetterOther,
    LetterTitlecase,
    LetterUppercase,
    Mark,
    MarkSpacingCombining,
    MarkEnclosing,
    MarkNonSpacing,
    Number,
    NumberDecimalDigit,
    NumberLetter,
    NumberOther,
    Punctuation,
    PunctuationConnector,
    PunctuationDash,
    PunctuationClose,
    PunctuationFinalQuote,
    PunctuationInitialQuote,
    PunctuationOpen,
    PunctuationOther,
    Separator,
    SeparatorLine,
    SeparatorParagraph,
    SeparatorSpace,
    Symbol,
    SymbolCurrency,
    SymbolModifier,
    SymbolMath,
    SymbolOther,
    Other,
    // surrogates not allowed
    OtherControl,
    OtherFormat,
    OtherNotAssigned,
    OtherPrivateUse,
}

#[derive(Debug, PartialEq)]
struct RegexpError {
    kind: RegexpErrorKind,
    pos: usize,
}

impl From<OutOfRangeError> for RegexpError {
    fn from(err: OutOfRangeError) -> Self {
        RegexpError {
            kind: RegexpErrorKind::OutOfRange,
            pos: err.pos,
        }
    }
}

enum Escape {
    Literal(char),
    Property(Property),
}

impl From<Escape> for ExprItem {
    fn from(value: Escape) -> Self {
        match value {
            Escape::Literal(c) => ExprItem::Literal(c),
            Escape::Property(p) => ExprItem::Property(p),
        }
    }
}

impl From<Escape> for CharClass {
    fn from(value: Escape) -> Self {
        match value {
            Escape::Literal(c) => CharClass::Literal(c),
            Escape::Property(p) => CharClass::Property(p),
        }
    }
}

#[derive(Debug, PartialEq)]
enum RegexpErrorKind {
    // invalid character set range
    InvalidCsRange,
    // parsing ints for range quantifiers
    OutOfRange,
    UnexpectedCharacter(u8),
    UnexpectedEof,
}

#[cfg(test)]
mod test {
    use super::{CharClass, GeneralCategory, Parser, Regexp, RegexpError, RegexpErrorKind};

    // this test is more about not missing a case rather than testing the match call of map_category()
    fn valid_categories() -> Vec<(&'static str, GeneralCategory)> {
        vec![
            (r"\p{L}", GeneralCategory::Letter),
            (r"\p{Ll}", GeneralCategory::LetterLowercase),
            (r"\p{Lm}", GeneralCategory::LetterModifier),
            (r"\p{Lo}", GeneralCategory::LetterOther),
            (r"\p{Lt}", GeneralCategory::LetterTitlecase),
            (r"\p{Lu}", GeneralCategory::LetterUppercase),
            (r"\p{M}", GeneralCategory::Mark),
            (r"\p{Mc}", GeneralCategory::MarkSpacingCombining),
            (r"\p{Me}", GeneralCategory::MarkEnclosing),
            (r"\p{Mn}", GeneralCategory::MarkNonSpacing),
            (r"\p{N}", GeneralCategory::Number),
            (r"\p{Nd}", GeneralCategory::NumberDecimalDigit),
            (r"\p{Nl}", GeneralCategory::NumberLetter),
            (r"\p{No}", GeneralCategory::NumberOther),
            (r"\p{P}", GeneralCategory::Punctuation),
            (r"\p{Pc}", GeneralCategory::PunctuationConnector),
            (r"\p{Pd}", GeneralCategory::PunctuationDash),
            (r"\p{Pe}", GeneralCategory::PunctuationClose),
            (r"\p{Pf}", GeneralCategory::PunctuationFinalQuote),
            (r"\p{Pi}", GeneralCategory::PunctuationInitialQuote),
            (r"\p{Ps}", GeneralCategory::PunctuationOpen),
            (r"\p{Po}", GeneralCategory::PunctuationOther),
            (r"\p{Z}", GeneralCategory::Separator),
            (r"\p{Zl}", GeneralCategory::SeparatorLine),
            (r"\p{Zp}", GeneralCategory::SeparatorParagraph),
            (r"\p{Zs}", GeneralCategory::SeparatorSpace),
            (r"\p{S}", GeneralCategory::Symbol),
            (r"\p{Sc}", GeneralCategory::SymbolCurrency),
            (r"\p{Sk}", GeneralCategory::SymbolModifier),
            (r"\p{Sm}", GeneralCategory::SymbolMath),
            (r"\p{So}", GeneralCategory::SymbolOther),
            (r"\p{C}", GeneralCategory::Other),
            (r"\p{Cc}", GeneralCategory::OtherControl),
            (r"\p{Cf}", GeneralCategory::OtherFormat),
            (r"\p{Cn}", GeneralCategory::OtherNotAssigned),
            (r"\p{Co}", GeneralCategory::OtherPrivateUse),
        ]
    }

    fn invalid_categories() -> Vec<(&'static str, RegexpError)> {
        vec![
            (
                r"\p{La}",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(b'a'),
                    pos: 4,
                },
            ),
            (
                r"\p{Mq}",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(b'q'),
                    pos: 4,
                },
            ),
            (
                r"\p{Nx}",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(b'x'),
                    pos: 4,
                },
            ),
            (
                r"\p{Pz}",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(b'z'),
                    pos: 4,
                },
            ),
            (
                r"\p{Zo}",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(b'o'),
                    pos: 4,
                },
            ),
            (
                r"\p{Sl}",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(b'l'),
                    pos: 4,
                },
            ),
            (
                r"\p{Cp}",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(b'p'),
                    pos: 4,
                },
            ),
            (
                r"\p{aa}",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(b'a'),
                    pos: 3,
                },
            ),
            (
                r"\p{",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedEof,
                    pos: 3,
                },
            ),
        ]
    }

    fn invalid_properties() -> Vec<(&'static str, RegexpError)> {
        vec![
            (
                r"\p",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedEof,
                    pos: 1,
                },
            ),
            (
                r"\pa",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(b'a'),
                    pos: 2,
                },
            ),
            (
                r"\p{N",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedEof,
                    pos: 3,
                },
            ),
            (
                r"\p{Soa",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(b'a'),
                    pos: 5,
                },
            ),
        ]
    }

    // character sets, escapes are tested indirectly
    fn invalid_cs() -> Vec<(&'static str, RegexpError)> {
        vec![
            (
                "[a-z",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedEof,
                    pos: 3,
                },
            ),
            // hyphen at the start is fine, then it must be escaped
            (
                "[--a]",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(b'-'),
                    pos: 2,
                },
            ),
            // escaped char unescaped
            (
                "[a*]",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(b'*'),
                    pos: 2,
                },
            ),
            // unknown escaped char
            (
                "[a\\b]",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(b'b'),
                    pos: 3,
                },
            ),
            // start > end
            (
                "[ω-α]",
                RegexpError {
                    kind: RegexpErrorKind::InvalidCsRange,
                    pos: 1,
                },
            ),
            // mismatch, the lhs of the range is not a char but a property
            (
                r"[\p{L}-z]",
                RegexpError {
                    kind: RegexpErrorKind::InvalidCsRange,
                    pos: 1,
                },
            ),
        ]
    }

    fn valid_quantifiers() -> Vec<(&'static str, Vec<Regexp>)> {
        vec![
            (
                "a*",
                vec![Regexp::Atom(CharClass::Literal('a')), Regexp::Star(0)],
            ),
            // expands to aa*
            (
                "a+",
                vec![
                    Regexp::Atom(CharClass::Literal('a')),
                    Regexp::Star(0),
                    Regexp::Concat(0, 1),
                ],
            ),
            // expands to a|ε
            (
                "a?",
                vec![
                    Regexp::Atom(CharClass::Literal('a')),
                    Regexp::Empty,
                    Regexp::Union(0, 1),
                ],
            ),
            (
                "a{3,5}",
                vec![
                    // creates 'a' -> returns 0
                    Regexp::Atom(CharClass::Literal('a')),
                    // returns 1
                    Regexp::Empty,
                    // creates a? -> returns 2
                    Regexp::Union(0, 1),
                    // creates aa -> returns 3
                    Regexp::Concat(0, 0),
                    // creates aaa -> returns 4
                    Regexp::Concat(3, 0),
                    // creates aaaa? -> returns 5
                    Regexp::Concat(4, 2),
                    // creates aaaaa? -> returns 5
                    Regexp::Concat(5, 2),
                ],
            ),
            // a?a?a?a?
            (
                "a{0,4}",
                vec![
                    // creates 'a' -> returns 0
                    Regexp::Atom(CharClass::Literal('a')),
                    // returns 1
                    Regexp::Empty,
                    // creates a? -> returns 2
                    Regexp::Union(0, 1),
                    // creates a?a? -> returns 3
                    Regexp::Concat(2, 2),
                    // creates a?a?a? -> returns 4
                    Regexp::Concat(3, 2),
                    // creates a?a?a?a? -> returns 5
                    Regexp::Concat(4, 2),
                ],
            ),
            (
                "a{3}",
                vec![
                    // creates 'a' -> returns 0
                    Regexp::Atom(CharClass::Literal('a')),
                    // creates aa -> returns 1
                    Regexp::Concat(0, 0),
                    // creates aaa
                    Regexp::Concat(1, 0),
                ],
            ),
            // expands to aaaa*
            (
                "a{3,}",
                vec![
                    // creates 'a' -> returns 0
                    Regexp::Atom(CharClass::Literal('a')),
                    // creates a* -> returns 1
                    Regexp::Star(0),
                    // creates aa -> returns 2
                    Regexp::Concat(0, 0),
                    // creates aaa -> returns 3
                    Regexp::Concat(2, 0),
                    // creates aaaa*
                    Regexp::Concat(3, 1),
                ],
            ),
            // is just a*
            (
                "a{0,}",
                vec![
                    // creates 'a' -> returns 0
                    Regexp::Atom(CharClass::Literal('a')),
                    // creates a* -> returns 1
                    Regexp::Star(0),
                ],
            ),
        ]
    }

    fn invalid_quantifiers() -> Vec<(&'static str, RegexpError)> {
        vec![
            // expected digit got char
            (
                "z{a,}",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(b'a'),
                    pos: 2,
                },
            ),
            (
                "z{3",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedEof,
                    pos: 2,
                },
            ),
            // expected comma got char
            (
                "z{2c3}",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(b'c'),
                    pos: 3,
                },
            ),
            // expected digit got char
            (
                "z{2,c}",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(b'c'),
                    pos: 4,
                },
            ),
            (
                "z{2,",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedEof,
                    pos: 3,
                },
            ),
            // expected '}' got 'a'
            (
                "z{2,3a",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(b'a'),
                    pos: 5,
                },
            ),
            (
                "z{2,3",
                RegexpError {
                    kind: RegexpErrorKind::UnexpectedEof,
                    pos: 4,
                },
            ),
        ]
    }

    #[test]
    fn map_categories() {
        for (expr, category) in valid_categories() {
            let mut parser = Parser::new(expr.as_bytes());
            // skip \p{, this is done by the caller of map_category()
            parser.pos += 3;
            let result = parser.map_category();

            assert_eq!(result, Ok(category), "mismatch for input: {}", expr);
        }
    }

    #[test]
    fn test_invalid_categories() {
        for (expr, err) in invalid_categories() {
            let mut parser = Parser::new(expr.as_bytes());
            // skip \p{, this is done by the caller of map_category()
            parser.pos += 3;
            let result = parser.map_category();

            assert_eq!(result, Err(err));
        }
    }

    #[test]
    fn test_invalid_properties() {
        for (expr, err) in invalid_properties() {
            let mut parser = Parser::new(expr.as_bytes());
            // skip \, this is done by the caller of parse_property()
            parser.pos += 1;
            let result = parser.parse_property();

            assert_eq!(result, Err(err));
        }
    }

    #[test]
    fn test_invalid_cs() {
        for (expr, err) in invalid_cs() {
            let mut parser = Parser::new(expr.as_bytes());
            let result = parser.parse_class_expr();

            assert_eq!(result, Err(err));
        }
    }

    #[test]
    fn expand_quantifiers() {
        for (expr, nodes) in valid_quantifiers() {
            let mut parser = Parser::new(expr.as_bytes());
            let _result = parser.parse_quantifier();

            assert_eq!(parser.nodes, nodes, "mismatch for input: {}", expr);
        }
    }

    #[test]
    fn test_invalid_quantifiers() {
        for (expr, err) in invalid_quantifiers() {
            let mut parser = Parser::new(expr.as_bytes());
            let result = parser.parse_quantifier();

            assert_eq!(result, Err(err));
        }
    }
}
