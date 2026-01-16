use crate::parsing::number::{Atoi, OutOfRangeError};
use crate::parsing::utf8;
use crate::parsing::value::path::filter::nfa::{Nfa, NfaBuilder};
use crate::parsing::value::path::filter::table;

const QUANTIFIER_LIMIT: u8 = 100;
const MAX_NODES: u16 = 10_000;

pub(super) fn full_match(input: &str, pattern: &str) -> bool {
    let mut parser = Parser::new(pattern.as_bytes());
    if let Err(_) = parser.parse() {
        return false;
    }
    let mut nfa = parser.into_nfa();
    nfa.full_match(input)
}

// stops at the left-most match
pub(super) fn partial_match(input: &str, pattern: &str) -> bool {
    let mut parser = Parser::new(pattern.as_bytes());
    if let Err(_) = parser.parse() {
        return false;
    }
    let mut nfa = parser.into_nfa();
    nfa.partial_match(input)
}

#[derive(Debug, PartialEq)]
pub(super) enum Regex {
    Empty,
    Concat(usize, usize),
    Union(usize, usize),
    Star(usize),
    Plus(usize),
    Question(usize),
    Atom(usize),
}

#[derive(Debug, PartialEq)]
pub(super) enum CharClass {
    Literal(char),
    Dot,
    ClassExpr(ClassExpr),
    Property(Property),
}

impl CharClass {
    pub(super) fn matches(&self, t: char) -> bool {
        match self {
            CharClass::Literal(c) => *c == t,
            // https://www.regular-expressions.info/dot.html
            CharClass::Dot => t != '\n',
            CharClass::ClassExpr(expr) => expr.matches(t),
            CharClass::Property(p) => p.matches(t),
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
pub(super) struct ClassExpr {
    negated: bool,
    items: Vec<ExprItem>,
}

impl ClassExpr {
    // toDo: this is temporary will optimize it later
    fn matches(&self, t: char) -> bool {
        let matches = self.items.iter().any(|e| e.matches(t));
        if self.negated { !matches } else { matches }
    }
}

#[derive(Debug, PartialEq)]
pub(super) enum ExprItem {
    Literal(char),
    Property(Property),
    Range(char, char),
}

impl ExprItem {
    fn matches(&self, t: char) -> bool {
        match self {
            ExprItem::Literal(c) => *c == t,
            ExprItem::Property(p) => p.matches(t),
            ExprItem::Range(l, h) => *l <= t && t <= *h,
        }
    }

    // extracts the char from a literal to form a range
    fn as_char(&self) -> Option<char> {
        match self {
            // char implements Copy, we don't try to move out of a borrowed value
            ExprItem::Literal(c) => Some(*c),
            _ => None,
        }
    }
}

impl From<Escape> for ExprItem {
    fn from(value: Escape) -> Self {
        match value {
            Escape::Literal(c) => ExprItem::Literal(c),
            Escape::Property(p) => ExprItem::Property(p),
        }
    }
}

enum Escape {
    Literal(char),
    Property(Property),
}

// major, minor
#[derive(Debug, PartialEq)]
pub(super) enum GeneralCategory {
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

impl GeneralCategory {
    fn major(&self) -> GeneralCategory {
        match self {
            GeneralCategory::Letter
            | GeneralCategory::LetterLowercase
            | GeneralCategory::LetterModifier
            | GeneralCategory::LetterOther
            | GeneralCategory::LetterTitlecase
            | GeneralCategory::LetterUppercase => GeneralCategory::Letter,

            GeneralCategory::Mark
            | GeneralCategory::MarkSpacingCombining
            | GeneralCategory::MarkEnclosing
            | GeneralCategory::MarkNonSpacing => GeneralCategory::Mark,

            GeneralCategory::Number
            | GeneralCategory::NumberDecimalDigit
            | GeneralCategory::NumberLetter
            | GeneralCategory::NumberOther => GeneralCategory::Number,

            GeneralCategory::Punctuation
            | GeneralCategory::PunctuationConnector
            | GeneralCategory::PunctuationDash
            | GeneralCategory::PunctuationClose
            | GeneralCategory::PunctuationFinalQuote
            | GeneralCategory::PunctuationInitialQuote
            | GeneralCategory::PunctuationOpen
            | GeneralCategory::PunctuationOther => GeneralCategory::Punctuation,

            GeneralCategory::Separator
            | GeneralCategory::SeparatorLine
            | GeneralCategory::SeparatorParagraph
            | GeneralCategory::SeparatorSpace => GeneralCategory::Separator,

            GeneralCategory::Symbol
            | GeneralCategory::SymbolCurrency
            | GeneralCategory::SymbolModifier
            | GeneralCategory::SymbolMath
            | GeneralCategory::SymbolOther => GeneralCategory::Symbol,

            GeneralCategory::Other
            | GeneralCategory::OtherControl
            | GeneralCategory::OtherFormat
            | GeneralCategory::OtherNotAssigned
            | GeneralCategory::OtherPrivateUse => GeneralCategory::Other,
        }
    }

    fn is_major(&self) -> bool {
        matches!(
            self,
            GeneralCategory::Letter
                | GeneralCategory::Mark
                | GeneralCategory::Number
                | GeneralCategory::Punctuation
                | GeneralCategory::Separator
                | GeneralCategory::Symbol
                | GeneralCategory::Other
        )
    }
}

#[derive(Debug, PartialEq)]
pub(super) struct Property {
    category: GeneralCategory,
    negated: bool,
}

// tested via full_match()
impl Property {
    fn matches(&self, c: char) -> bool {
        let mut category = table::category(c);
        if self.category.is_major() {
            category = category.major();
        }

        if self.negated {
            !(category == self.category)
        } else {
            category == self.category
        }
    }
}

//
// Why we need nodes and classes?
//
// If we process aa, we need to create 'a' as char class twice because we have no idea that when
// we encounter it for the first time that we will encounter it again; it could have been ab, a plus
// anything. There is a case though that we know exactly how many a's we are going to get, range
// quantifiers. a{3}, a{3,6}, a{3,} will have at least 3 a's. If 'a' is big, a large vector(Vec<ExprItem>),
// we don't want to clone it. What I did in this approach was to create it once, let Atom() hold the
// value, Atom(CharClass), and push the node into this Vec<Regex>. Creating Regex::Atom(a) would
// return 0 and then aa is nothing but Concat(0, 0). It worked great for creating the AST, but we
// had the following problem
//
// The problem:
//
// In the next step where we have to convert the ast to an epsilon-nfa. What we want is to consume
// the ast, we no longer need it, and create an nfa. The compile() method in nfa.rs takes ownership
// of the vector nodes, and then we recursively walk the tree(bottom up) to create the states of the
// nfa. The key point here is the states and nfa in general shouldn't be aware of the regex at all,
// each state must own the value that will transition to the next state, which is also what we are
// trying to do bypassing ownership of nodes. We can't do that directly because walk() is a recursive
// function and needs &mut to nodes. The 1st approach to solve this was to call mem::replace() and
// replace those nodes that hold values(Regex::Atom(a)) with Regex::Empty, this could have worked
// if we only visited each node once, but for quantifiers we know that this is not true
//
// when walk() is called for Concat(0,0)
//
//             Regex::Concat(lhs, rhs) => {
//                 let f1 = self.walk(lhs, nodes);
//                 let f2 = self.walk(rhs, nodes);
//                 self.patch(&f1.outs, f2.start);
//                 Fragment {
//                     start: f1.start,
//                     outs: f2.outs,
//                 }
//             }
//
// lhs and rhs is the same index; lhs visits first replaces it with Empty and now when rhs visits
// the same node the value is different.
//
// The solution:
//
// Instead of having Regex::Atom(a) own a when building the ast, we store the char class into a
// vector and then let Regex::Atom(0) hold the index of 'a' in that vector. This actually solves both
// problems. During parsing nothing changes, we expand quantifiers the same that we did, for aa we
// still get Concat(0,0) that 0 refers to nodes[0] while Atom(0) refers to classes[0]. Now when
// walking the ast to create the automaton all we have to do is copy the index and pass ownership to
// the class vector
//
//             Regex::Atom(idx) => {
//                 let start = self.append(State::Atom(idx, None));
//                 Fragment {
//                     start,
//                     outs: vec![start],
//                 }
//             }
//
// the index that is copied to State::Atom(idx) is an index to the char class vector
struct Parser<'a> {
    buffer: &'a [u8],
    pos: usize,
    nodes: Vec<Regex>,
    classes: Vec<CharClass>,
}

impl<'a> Parser<'a> {
    fn new(buffer: &'a [u8]) -> Self {
        Self {
            buffer,
            pos: 0,
            nodes: Vec::new(),
            classes: Vec::new(),
        }
    }

    fn into_nfa(self) -> Nfa {
        NfaBuilder::new(self.nodes, self.classes).build()
    }

    fn parse(&mut self) -> Result<(), RegexError> {
        if self.buffer.is_empty() {
            self.push_node(Regex::Empty)?;
            return Ok(());
        }
        self.parse_union()?;
        Ok(())
    }

    // Much easier than parse_concat() because we can keep going as long as we encounter the
    // pipe operator. We use the loop for the left-right associativity as explained in
    // parse_logical_and/or() in path.rs
    fn parse_union(&mut self) -> Result<usize, RegexError> {
        let mut lhs = self.parse_concat()?;

        while self.buffer.get(self.pos) == Some(&b'|') {
            self.advance_by(1);
            let rhs = self.parse_concat()?;
            lhs = self.push_node(Regex::Union(lhs, rhs))?;
        }
        Ok(lhs)
    }

    fn parse_concat(&mut self) -> Result<usize, RegexError> {
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
            match self.buffer.get(self.pos) {
                Some(b'|') | Some(b')') | None => break,
                _ => {
                    let rhs = self.parse_quantifier()?;
                    lhs = self.push_node(Regex::Concat(lhs, rhs))?;
                }
            }
        }
        Ok(lhs)
    }

    fn parse_quantifier(&mut self) -> Result<usize, RegexError> {
        let atom_idx = self.parse_atom()?;

        match self.buffer.get(self.pos) {
            Some(b'*') => {
                self.advance_by(1);
                let node = Regex::Star(atom_idx);
                Ok(self.push_node(node)?)
            }
            // initially would map 'a+' to aa* and 'a?' to a|ε but it is not efficient when compiling
            // tried to keep as close to formal definition but would have performance issues when
            // compiling to nfa. It is more efficient to create the automata for a+ and a? than
            // aa* and a | ε. Especially in the first case if a is large we will have issues
            // due to duplication for a and a*
            //
            // Some(b'+') => {
            //     // aa*
            //     self.consume(1);
            //     let star = Regex::Star(atom_idx);
            //     let start_idx = self.emit(star);
            //     Ok(self.emit(Regex::Concat(atom_idx, start_idx)))
            // }
            // Some(b'?') => {
            //     // a|ε
            //     self.consume(1);
            //     let empty = Regex::Empty;
            //     let empty_idx = self.emit(empty);
            //     Ok(self.emit(Regex::Union(atom_idx, empty_idx)))
            // }
            Some(b'+') => {
                self.advance_by(1);
                Ok(self.push_node(Regex::Plus(atom_idx))?)
            }
            Some(b'?') => {
                self.advance_by(1);
                Ok(self.push_node(Regex::Question(atom_idx))?)
            }
            Some(b'{') => {
                self.advance_by(1);
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
    fn expand_bounded(&mut self, atom_idx: usize, min: u8, max: u8) -> Result<usize, RegexError> {
        let mut tmp = Vec::new();
        for _ in 0..min {
            tmp.push(atom_idx);
        }

        let delta = max - min;
        // Case: a{0,4}, a?a?a?a?
        //  the mandatory part, min, is 0 in this case, which means delta is max.
        if delta > 0 {
            let optional_idx = self.push_node(Regex::Question(atom_idx))?;

            for _ in 0..delta {
                tmp.push(optional_idx);
            }
        }

        // at this point we need to link them
        // create aa then take its index and concat with the optional_idx to create aaa? and
        // then take that index and create aaa?a?
        let mut i = tmp[0];
        for node in tmp.into_iter().skip(1) {
            // we concat i with tmp[j] not j, I had this initially which was wrong Concat(i, j)
            // because my loop condition was for j in 1..tmp.len() so i was looking at indices
            // we want the value at the given index; the most sane solution is to iterate over the
            // values instead
            i = self.push_node(Regex::Concat(i, node))?;
        }
        Ok(i)
    }

    // a{2,} is nothing but a{2}a*
    // it means at least 2
    fn expand_unbounded(&mut self, atom_idx: usize, min: u8) -> Result<usize, RegexError> {
        let star_idx = self.push_node(Regex::Star(atom_idx))?;
        // Case: a{0,} is just a*
        if min == 0 {
            return Ok(star_idx);
        }

        let mut i = atom_idx;
        for _ in 1..min {
            i = self.push_node(Regex::Concat(i, atom_idx))?;
        }

        Ok(self.push_node(Regex::Concat(i, star_idx))?)
    }

    // parses a range quantifier
    // we return min, Optional<max>
    //
    // In order to prevent resource exhaustion we need to set a limit to the quantifier value.
    // a{2, 20000},
    // a{100},
    // ((a{100}){100}){100} -> 1_000_000 nodes
    //
    // The max value we can have for a quantifier is 100, anything above results to an error.
    // Cases like ((a{100}){100}){100} are handled by keep tracking of the number of nodes in the
    // tree; when that number exceeds 10_000 we return an error. look at push_node()
    fn parse_quant_range(&mut self) -> Result<(u8, Option<u8>), RegexError> {
        match self.buffer.get(self.pos) {
            Some(b) if !b.is_ascii_digit() => Err(RegexError {
                kind: RegexErrorKind::UnexpectedCharacter(*b),
                pos: self.pos,
            }),
            Some(_) => {
                // we can infer the type here, we don't need to be explicit and call u8::atoi()
                let start = self.pos;
                let min = Atoi::atoi(&mut self.buffer, &mut self.pos)?;
                if min > QUANTIFIER_LIMIT {
                    return Err(RegexError {
                        kind: RegexErrorKind::QuantifierLimitExceeded(QUANTIFIER_LIMIT),
                        pos: start,
                    });
                }
                match self.buffer.get(self.pos) {
                    Some(b',') => {
                        self.advance_by(1);
                        match self.buffer.get(self.pos) {
                            // {2,}
                            Some(b'}') => {
                                self.advance_by(1);
                                Ok((min, None))
                            }
                            Some(b) if !b.is_ascii_digit() => Err(RegexError {
                                kind: RegexErrorKind::UnexpectedCharacter(*b),
                                pos: self.pos,
                            }),
                            Some(_) => {
                                let start = self.pos;
                                let max = Atoi::atoi(&mut self.buffer, &mut self.pos)?;
                                if max > QUANTIFIER_LIMIT {
                                    return Err(RegexError {
                                        kind: RegexErrorKind::QuantifierLimitExceeded(
                                            QUANTIFIER_LIMIT,
                                        ),
                                        pos: start,
                                    });
                                }
                                match self.buffer.get(self.pos) {
                                    Some(b'}') => {
                                        self.advance_by(1);
                                        Ok((min, Some(max)))
                                    }
                                    Some(b) => Err(RegexError {
                                        kind: RegexErrorKind::UnexpectedCharacter(*b),
                                        pos: self.pos,
                                    }),
                                    None => Err(RegexError {
                                        kind: RegexErrorKind::UnexpectedEof,
                                        pos: self.pos - 1,
                                    }),
                                }
                            }
                            None => Err(RegexError {
                                kind: RegexErrorKind::UnexpectedEof,
                                pos: self.pos - 1,
                            }),
                        }
                    }
                    // Case: {2}
                    // we can rewrite it as {2,2} which will allow us to expand it as {min, max}
                    // range. In both cases our range is bounded while in {2,} is not
                    Some(b'}') => {
                        self.advance_by(1);
                        Ok((min, Some(min)))
                    }
                    Some(b) => Err(RegexError {
                        kind: RegexErrorKind::UnexpectedCharacter(*b),
                        pos: self.pos,
                    }),
                    None => Err(RegexError {
                        kind: RegexErrorKind::UnexpectedEof,
                        pos: self.pos - 1,
                    }),
                }
            }
            None => Err(RegexError {
                kind: RegexErrorKind::UnexpectedEof,
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
    fn parse_atom(&mut self) -> Result<usize, RegexError> {
        match self.buffer.get(self.pos) {
            Some(b'(') => {
                self.advance_by(1);
                let expr_idx = self.parse_union()?;
                match self.buffer.get(self.pos) {
                    Some(b')') => {
                        self.advance_by(1);
                        Ok(expr_idx)
                    }
                    Some(b) => Err(RegexError {
                        kind: RegexErrorKind::UnexpectedCharacter(*b),
                        pos: self.pos,
                    }),
                    None => Err(RegexError {
                        kind: RegexErrorKind::UnexpectedEof,
                        pos: self.pos - 1,
                    }),
                }
            }
            // We need to consider the following cases according to the rfc syntax
            //
            // '[', '.', '\' when these characters are encountered unescaped have special meaning
            // '[' indicates the start of a character set
            // '.' maps to any character
            // '\' expect an escaped character
            //
            // any other metacharacter if appeared unescaped it is invalid
            //
            // Now for the escaped characters, the ones that can appear after '\' are the following
            // n, r, t -> those map to \n, \t, \r; this is how we did the mapping in json too.
            // any of the metacharacters maps to themselves: ( -> ( and so on
            Some(b'[') | Some(b'.') | Some(b'\\') => {
                let class = self.parse_class()?;
                let id = self.push_class(class);
                Ok(self.push_node(Regex::Atom(id))?)
            }
            Some(b)
                if matches!(
                    *b,
                    b'(' | b')' | b'*' | b'+' | b'-' | b'?' | b']' | b'{' | b'|' | b'}'
                ) =>
            {
                Err(RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(*b),
                    pos: self.pos,
                })
            }
            Some(_) => {
                let char = self.parse_char();
                let id = self.push_class(CharClass::Literal(char));
                Ok(self.push_node(Regex::Atom(id))?)
            }
            None => Err(RegexError {
                kind: RegexErrorKind::UnexpectedEof,
                pos: self.pos - 1,
            }),
        }
    }

    // charClass = "." / SingleCharEsc / charClassEsc / charClassExpr
    // '.' is any char
    // '[' implies the start of character set(charClassExpr)
    // '\' describes either a single escaped character or the start of char property
    fn parse_class(&mut self) -> Result<CharClass, RegexError> {
        match self.buffer[self.pos] {
            b'.' => {
                self.advance_by(1);
                Ok(CharClass::Dot)
            }
            b'[' => Ok(CharClass::ClassExpr(self.parse_class_expr()?)),
            b'\\' => Ok(CharClass::from(self.parse_escape()?)),
            _ => unreachable!("parse_class() was called with {}", self.buffer[self.pos]),
        }
    }

    // parses a character set
    // no quantifiers or nested regex is allowed inside a set
    fn parse_class_expr(&mut self) -> Result<ClassExpr, RegexError> {
        // consume '['
        self.advance_by(1);
        let mut items = Vec::new();
        let len = self.buffer.len();
        let mut negated = false;

        if let Some(b) = self.buffer.get(self.pos) {
            if *b == b'^' {
                negated = true;
                self.advance_by(1);
            }
        }

        if let Some(b'-') = self.buffer.get(self.pos) {
            self.advance_by(1);
            items.push(ExprItem::Literal('-'));
        }

        while self.pos < len && self.buffer[self.pos] != b']' {
            let start = self.pos;
            let item = self.parse_class_expr_item()?;
            if let Some(b) = self.buffer.get(self.pos) {
                // we have 4 cases to consider when we parse a hyphen unescaped
                // [^-...]
                // [-a] is treated as a literal
                // [a-] is also tread as a literal
                // [a-z] now we have a range because we encountered CCchar '-' CCchar
                // for the first three cases hyphen is treated as literal and for the last one as range
                //
                // in any other case, it must be escaped
                if *b == b'-' {
                    self.advance_by(1);
                    match self.buffer.get(self.pos) {
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
                                return Err(RegexError {
                                    kind: RegexErrorKind::InvalidCsRange,
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
                            return Err(RegexError {
                                kind: RegexErrorKind::UnexpectedEof,
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
            return Err(RegexError {
                kind: RegexErrorKind::UnexpectedEof,
                pos: self.pos - 1,
            });
        }
        // consume ']'
        self.advance_by(1);

        Ok(ClassExpr { negated, items })
    }

    fn parse_class_expr_item(&mut self) -> Result<ExprItem, RegexError> {
        match self.buffer[self.pos] {
            b'\\' => Ok(ExprItem::from(self.parse_escape()?)),
            // can't appear unescaped
            b => {
                if matches!(
                    b,
                    b'(' | b')'
                        | b'*'
                        | b'+'
                        | b'-'
                        | b'.'
                        | b'?'
                        | b'['
                        | b']'
                        | b'{'
                        | b'|'
                        | b'}'
                ) {
                    return Err(RegexError {
                        kind: RegexErrorKind::UnexpectedCharacter(b),
                        pos: self.pos,
                    });
                }
                Ok(ExprItem::Literal(self.parse_char()))
            }
        }
    }

    fn parse_escape(&mut self) -> Result<Escape, RegexError> {
        // consume '\'
        self.advance_by(1);
        match self.buffer.get(self.pos) {
            Some(b'p') | Some(b'P') => Ok(Escape::Property(self.parse_property()?)),
            Some(_) => Ok(Escape::Literal(self.map_escape_character()?)),
            None => Err(RegexError {
                kind: RegexErrorKind::UnexpectedEof,
                pos: self.pos - 1,
            }),
        }
    }

    fn parse_char(&mut self) -> char {
        let c = utf8::read_utf8_char(self.buffer, self.pos);
        self.advance_by(utf8::char_width(self.buffer[self.pos]));
        c
    }

    fn parse_property(&mut self) -> Result<Property, RegexError> {
        let negated = self.buffer[self.pos] == b'P';
        self.advance_by(1);

        // need 3 or 4 characters after p/P: {..}
        // this won't work, we don't know if we have a major or a minor category
        // if self.pos + 4 >= self.buffer.len() {
        //     return Err(RegexError {
        //         kind: RegexErrorKind::UnexpectedEof,
        //         pos: self.pos - 1,
        //     });
        // }
        match self.buffer.get(self.pos) {
            Some(b) if *b == b'{' => self.advance_by(1),
            Some(b) => {
                return Err(RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(*b),
                    pos: self.pos,
                });
            }
            None => {
                return Err(RegexError {
                    kind: RegexErrorKind::UnexpectedEof,
                    pos: self.pos - 1,
                });
            }
        }

        let category = self.parse_category()?;

        match self.buffer.get(self.pos) {
            Some(b) if *b == b'}' => self.advance_by(1),
            Some(b) => {
                return Err(RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(*b),
                    pos: self.pos,
                });
            }
            None => {
                return Err(RegexError {
                    kind: RegexErrorKind::UnexpectedEof,
                    pos: self.pos - 1,
                });
            }
        }
        Ok(Property { category, negated })
    }

    fn map_escape_character(&mut self) -> Result<char, RegexError> {
        let c = match self.buffer[self.pos] {
            b'n' => '\n',
            b'r' => '\r',
            b't' => '\t',
            b if !matches!(
                b,
                b'(' | b')' | b'*' | b'+' | b'-' | b'.' | b'?' | b'[' | b']' | b'{' | b'|' | b'}'
            ) =>
            {
                return Err(RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(b),
                    pos: self.pos,
                });
            }
            b => b as char,
        };
        self.advance_by(1);
        Ok(c)
    }

    // the case where we get a major category into none like /p{L is handled by the caller
    // the category itself is not invalid, /p{ is and is handled by this method(2nd to last arm)
    fn parse_category(&mut self) -> Result<GeneralCategory, RegexError> {
        let current = self.buffer.get(self.pos);
        let next = self.peek();

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
                return Err(RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(*b),
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
                return Err(RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(*b),
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
                return Err(RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(*b),
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
                return Err(RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(*b),
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
                return Err(RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(*b),
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
                return Err(RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(*b),
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
                return Err(RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(*b),
                    pos: self.pos + 1,
                });
            }

            (None, _) => {
                return Err(RegexError {
                    kind: RegexErrorKind::UnexpectedEof,
                    pos: self.pos,
                });
            }
            (Some(b), _) => {
                return Err(RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(*b),
                    pos: self.pos,
                });
            }
        };

        self.advance_by(n);
        Ok(category)
    }

    // In order to prevent resource exhaustion, the maximum number of nodes in the AST is 10_000.
    //
    // expr: ab
    //
    // a: Regex::Atom(0) // 0 is the index of 'a' in classes
    // b: Regex::Atom(1) // 1 is the index of 'b' in classes
    // Concat(a, b):  Regex::Concat(0, 1) // 0 refers to nodes[0](Regex::Atom(0)), 1 refers to
    // nodes[1](Regex::Atom(1))
    //
    //    ab
    //  /    \
    // a      b
    fn push_node(&mut self, node: Regex) -> Result<usize, RegexError> {
        if self.nodes.len() >= MAX_NODES as usize {
            return Err(RegexError {
                kind: RegexErrorKind::ComplexityLimitExceeded(MAX_NODES),
                pos: self.pos,
            });
        }
        self.nodes.push(node);
        Ok(self.nodes.len() - 1)
    }

    fn push_class(&mut self, class: CharClass) -> usize {
        self.classes.push(class);
        self.classes.len() - 1
    }

    fn peek(&self) -> Option<&u8> {
        self.buffer.get(self.pos + 1)
    }

    fn advance_by(&mut self, n: usize) {
        self.pos += n;
    }
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
enum RegexErrorKind {
    // invalid character set range
    InvalidCsRange,
    // parsing ints for range quantifiers
    OutOfRange,
    QuantifierLimitExceeded(u8),
    ComplexityLimitExceeded(u16),
    UnexpectedCharacter(u8),
    UnexpectedEof,
}

#[derive(Debug, PartialEq)]
struct RegexError {
    kind: RegexErrorKind,
    pos: usize,
}

impl From<OutOfRangeError> for RegexError {
    fn from(err: OutOfRangeError) -> Self {
        RegexError {
            kind: RegexErrorKind::OutOfRange,
            pos: err.pos,
        }
    }
}

#[cfg(test)]
mod test {
    use super::{GeneralCategory, Parser, Regex, RegexError, RegexErrorKind};

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

    fn invalid_categories() -> Vec<(&'static str, RegexError)> {
        vec![
            (
                r"\p{La}",
                RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(b'a'),
                    pos: 4,
                },
            ),
            (
                r"\p{Mq}",
                RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(b'q'),
                    pos: 4,
                },
            ),
            (
                r"\p{Nx}",
                RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(b'x'),
                    pos: 4,
                },
            ),
            (
                r"\p{Pz}",
                RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(b'z'),
                    pos: 4,
                },
            ),
            (
                r"\p{Zo}",
                RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(b'o'),
                    pos: 4,
                },
            ),
            (
                r"\p{Sl}",
                RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(b'l'),
                    pos: 4,
                },
            ),
            (
                r"\p{Cp}",
                RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(b'p'),
                    pos: 4,
                },
            ),
            (
                r"\p{aa}",
                RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(b'a'),
                    pos: 3,
                },
            ),
            (
                r"\p{",
                RegexError {
                    kind: RegexErrorKind::UnexpectedEof,
                    pos: 3,
                },
            ),
        ]
    }

    fn invalid_properties() -> Vec<(&'static str, RegexError)> {
        vec![
            (
                r"\p",
                RegexError {
                    kind: RegexErrorKind::UnexpectedEof,
                    pos: 1,
                },
            ),
            (
                r"\pa",
                RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(b'a'),
                    pos: 2,
                },
            ),
            (
                r"\p{N",
                RegexError {
                    kind: RegexErrorKind::UnexpectedEof,
                    pos: 3,
                },
            ),
            (
                r"\p{Soa",
                RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(b'a'),
                    pos: 5,
                },
            ),
        ]
    }

    // character sets, escapes are tested indirectly
    fn invalid_cs() -> Vec<(&'static str, RegexError)> {
        vec![
            (
                "[a-z",
                RegexError {
                    kind: RegexErrorKind::UnexpectedEof,
                    pos: 3,
                },
            ),
            // hyphen at the start is fine, then it must be escaped
            (
                "[--a]",
                RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(b'-'),
                    pos: 2,
                },
            ),
            // escaped char unescaped
            (
                "[a*]",
                RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(b'*'),
                    pos: 2,
                },
            ),
            // unknown escaped char
            (
                "[a\\b]",
                RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(b'b'),
                    pos: 3,
                },
            ),
            // start > end
            (
                "[ω-α]",
                RegexError {
                    kind: RegexErrorKind::InvalidCsRange,
                    pos: 1,
                },
            ),
            // mismatch, the lhs of the range is not a char but a property
            (
                r"[\p{L}-z]",
                RegexError {
                    kind: RegexErrorKind::InvalidCsRange,
                    pos: 1,
                },
            ),
        ]
    }

    fn valid_quantifiers() -> Vec<(&'static str, Vec<Regex>)> {
        vec![
            ("a*", vec![Regex::Atom(0), Regex::Star(0)]),
            // expands to aa*
            ("a+", vec![Regex::Atom(0), Regex::Plus(0)]),
            // expands to a|ε
            ("a?", vec![Regex::Atom(0), Regex::Question(0)]),
            (
                "a{3,5}",
                vec![
                    // creates 'a' -> returns 0
                    Regex::Atom(0),
                    // creates a? -> returns 1
                    Regex::Question(0),
                    // creates aa -> returns 3
                    Regex::Concat(0, 0),
                    // creates aaa -> returns 4
                    Regex::Concat(2, 0),
                    // creates aaaa? -> returns 5
                    Regex::Concat(3, 1),
                    // creates aaaaa? -> returns 5
                    Regex::Concat(4, 1),
                ],
            ),
            //     // a?a?a?a?
            //     (
            //         "a{0,4}",
            //         vec![
            //             // creates 'a' -> returns 0
            //             Regex::Atom(0),
            //             // creates a? -> returns 1
            //             Regex::Question(0),
            //             // creates a?a? -> returns 3
            //             Regex::Concat(1, 1),
            //             // creates a?a?a? -> returns 4
            //             Regex::Concat(2, 1),
            //             // creates a?a?a?a? -> returns 5
            //             Regex::Concat(3, 1),
            //         ],
            //     ),
            //     (
            //         "a{3}",
            //         vec![
            //             // creates 'a' -> returns 0
            //             Regex::Atom(0),
            //             // creates aa -> returns 1
            //             Regex::Concat(0, 0),
            //             // creates aaa
            //             Regex::Concat(1, 0),
            //         ],
            //     ),
            //     // expands to aaaa*
            //     (
            //         "a{3,}",
            //         vec![
            //             // creates 'a' -> returns 0
            //             Regex::Atom(0),
            //             // creates a* -> returns 1
            //             Regex::Star(0),
            //             // creates aa -> returns 2
            //             Regex::Concat(0, 0),
            //             // creates aaa -> returns 3
            //             Regex::Concat(2, 0),
            //             // creates aaaa*
            //             Regex::Concat(3, 1),
            //         ],
            //     ),
            //     // is just a*
            //     (
            //         "a{0,}",
            //         vec![
            //             // creates 'a' -> returns 0
            //             Regex::Atom(0),
            //             // creates a* -> returns 1
            //             Regex::Star(0),
            //         ],
            //     ),
        ]
    }

    fn invalid_quantifiers() -> Vec<(&'static str, RegexError)> {
        vec![
            // expected digit got char
            (
                "z{a,}",
                RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(b'a'),
                    pos: 2,
                },
            ),
            (
                "z{3",
                RegexError {
                    kind: RegexErrorKind::UnexpectedEof,
                    pos: 2,
                },
            ),
            // expected comma got char
            (
                "z{2c3}",
                RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(b'c'),
                    pos: 3,
                },
            ),
            // expected digit got char
            (
                "z{2,c}",
                RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(b'c'),
                    pos: 4,
                },
            ),
            (
                "z{2,",
                RegexError {
                    kind: RegexErrorKind::UnexpectedEof,
                    pos: 3,
                },
            ),
            // expected '}' got 'a'
            (
                "z{2,3a",
                RegexError {
                    kind: RegexErrorKind::UnexpectedCharacter(b'a'),
                    pos: 5,
                },
            ),
            (
                "z{2,3",
                RegexError {
                    kind: RegexErrorKind::UnexpectedEof,
                    pos: 4,
                },
            ),
        ]
    }

    fn full() -> Vec<(&'static str, &'static str, bool)> {
        vec![
            ("", "", true),
            ("", "(foo)*", true),
            ("", "(foo)?", true),
            ("", "(foo)*f?", true),
            ("foo", "foo", true),
            ("foo", ".*", true),
            ("foo", ".*foo", true),
            ("foo", "foo.*", true),
            ("foo", ".*foo.*", true),
            ("foo", "foo(foo)*", true),
            ("foo", "foo(foo)*f?", true),
            ("foo", "...", true),
            ("foo", ".{3}", true),
            ("foo", "f{1}o{2}", true),
            ("foo", "\\p{L}*", true),
            ("foo", "\\p{Ll}*", true),
            ("foo", "\\P{N}*", true),
            ("Foo", "[a-zA-Z][a-zA-Z][a-zA-Z]", true),
            ("foo", "[a-z\\p{N}3]{3}", true),
            ("foo.", "[a-z\\p{N}3]{3}\\.", true),
            ("foo", "(foo)+", true),
            ("foo", "[^0-9]*", true),
            ("ab", "(a|b)*", true),
            ("a", "aa*", true),
            ("aaaa", "a{3,}", true),
            ("aaaa", "a{3,4}", true),
            ("[3]", "\\[[0-9]\\]", true),
            // Punctuation-Other
            ("[@]", "\\[\\p{S}\\]", false),
            ("🙂🎉", "[🙂].", true),
            ("foo", "fo", false),
            ("foo", "", false),
            ("foo", "FOO", false),
            ("", "foo", false),
            // Dot matches anything but newline
            ("\n", ".", false),
            ("^", "[^^]", false),
        ]
    }

    // for these cases we only care about the prefix/suffix, the actual matching has already been
    // tested in full_match()
    fn partial() -> Vec<(&'static str, &'static str, bool)> {
        vec![
            ("abab", "ab", true),
            ("foobarbaz", "bar", true),
            ("barfoobaz", ".*foo", true),
            ("barfoobaz", "foo.*", true),
        ]
    }

    #[test]
    fn map_categories() {
        for (expr, category) in valid_categories() {
            let mut parser = Parser::new(expr.as_bytes());
            // skip \p{, this is done by the caller of map_category()
            parser.pos += 3;
            let result = parser.parse_category();

            assert_eq!(result, Ok(category), "mismatch for input: {}", expr);
        }
    }

    #[test]
    fn test_invalid_categories() {
        for (expr, err) in invalid_categories() {
            let mut parser = Parser::new(expr.as_bytes());
            // skip \p{, this is done by the caller of map_category()
            parser.pos += 3;
            let result = parser.parse_category();

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

    #[test]
    fn test_full_match() {
        for (input, pattern, result) in full() {
            assert_eq!(result, super::full_match(input, pattern));
        }
    }

    #[test]
    fn test_partial_match() {
        for (input, pattern, result) in partial() {
            assert_eq!(result, super::partial_match(input, pattern));
        }
    }
}
