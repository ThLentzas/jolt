use crate::parsing::value::Value;
use crate::parsing::value::path::filter::{EmbeddedQuery, LogicalExpr};
use std::borrow::Cow;
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::sync::OnceLock;

static REGISTRY: OnceLock<Registry> = OnceLock::new();

fn registry() -> &'static Registry {
    REGISTRY.get_or_init(|| {
        // This closure runs ONLY ONCE, no matter how many times you call it.
        // It acts exactly like your Java "static { ... }" block.
        let mut registry = Registry::new();

        registry.register(LengthFn {});
        registry.register(CountFn {});
        registry.register(MatchFn {});
        registry.register(SearchFn {});
        registry.register(ValueFn {});

        registry
    })
}

#[derive(Debug, PartialEq)]
pub(crate) struct FnExpr {
    pub(crate) name: String,
    pub(crate) args: Vec<FnExprArg>,
}

#[derive(Debug, PartialEq)]
pub(crate) enum FnExprArg {
    Literal(Value),
    EmbeddedQuery(EmbeddedQuery),
    FnExpr(Box<FnExpr>),
    // note: as of the time of the implementation none of the rfc defined functions accepts as
    // parameter type a logical expression
    LogicalExpr(LogicalExpr),
}

// types used in the signature of the function
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum FnType {
    ValueType,
    NodesType,
    Logical,
    Nothing,
}

// the resolved values that will be passed as parameters in a function
#[derive(Debug, PartialEq)]
enum FnArg<'a> {
    Value(Cow<'a, Value>),
    Nodelist(Vec<&'a Value>),
    Logical(bool),
    Nothing,
}

impl<'a> FnArg<'a> {
    fn as_value(&self) -> Option<&Value> {
        match self {
            // Cow<Value> -> &Value
            // v.as_ref() is the same as &**v
            // v: &Cow<'a, Value> from &args[0]
            // *v: Cow<'a, Value> deref the reference
            // **v: Value, Cow derefs to Value
            // &**v: &Value, take reference
            FnArg::Value(v) => Some(v.as_ref()),
            FnArg::Nodelist(nodes) => match nodes.len() {
                // singular query returned an empty list, it's valid(returns at most 1)
                0 => None,
                1 => Some(nodes[0]),
                // there is a bug in our type_check() method we only expect singular queries
                // can't have 2+
                _ => unreachable!(
                    "received nodelist with {} nodes, expected singular query",
                    nodes.len()
                ),
            },
            // there is a bug in our type_check() method we can only convert to ValueType either a
            // Value or a singular query, anything else is a type mismatch
            _ => unreachable!(
                "received {:?}, expected {:?}",
                self.to_fn_type(),
                FnType::ValueType
            ),
        }
    }

    // from the 3rd elision rule, the outer reference's lifetime is the same as self but if we don't
    // specify the inner we will get 'lifetime might not live long enough' when we try to use it
    fn as_nodelist(&self) -> &Vec<&'a Value> {
        match self {
            FnArg::Nodelist(nodes) => nodes,
            // there is a bug in our type_check() method
            _ => unreachable!(
                "received {:?}, expected {:?}",
                self.to_fn_type(),
                FnType::NodesType
            ),
        }
    }

    fn to_fn_type(&self) -> FnType {
        match self {
            FnArg::Value(_) => FnType::ValueType,
            FnArg::Nodelist(_) => FnType::NodesType,
            FnArg::Logical(_) => FnType::Logical,
            FnArg::Nothing => FnType::Nothing,
        }
    }
}

// length, count and value return ValueType
// search, match return Logical
#[derive(Debug, PartialEq)]
pub(super) enum FnResult<'a> {
    // If the function just returns data from the input, borrow it. If it calculates new data, own it.
    // length(), match(), search() and count() all create new data, but value() returns an existing
    // value that lives in root, so we would have to clone in this case. length() and count() return
    // ints which we will wrap to Number, match() and search() will return Logical and with Cow
    // we can return the reference to the value
    Value(Cow<'a, Value>),
    Logical(bool),
    Nothing,
}

impl FnExpr {
    // resolve_args can return references from both self (literals) and root (query results) all
    // get the same lifetime. All hold references that live in the root so they must have the same
    // lifetime
    pub(super) fn evaluate<'a>(&'a self, root: &'a Value, current: &'a Value) -> FnResult<'a> {
        // always safe to call unwrap; if we get None it means either we parsed the name incorrectly
        // or we didn't register our function correctly
        let f = registry().get(&self.name).unwrap();
        let args = self.to_fn_args(f, root, current);
        f.execute(&args)
    }

    // we never evaluate anything to determine type mismatch; for embedded queries we check if
    // singular and for fn expressions we check the return type, so we make 1 linear scan of the args
    // of the function
    pub(crate) fn type_check(&self) -> Result<(), FnExprError> {
        let func = registry().get(&self.name).unwrap();

        if self.args.len() != func.args().len() {
            return Err(FnExprError::ArityMismatch {
                expected: func.args().len(),
                got: self.args.len(),
            });
        }

        for (i, arg) in self.args.iter().enumerate() {
            let expected_type = func.args()[i];

            let valid = match (arg, expected_type) {
                (FnExprArg::Literal(_), FnType::ValueType) => true,
                (FnExprArg::Literal(_), _) => false,
                (FnExprArg::EmbeddedQuery(q), FnType::ValueType) => q.is_definite(),
                (FnExprArg::EmbeddedQuery(_), FnType::NodesType) => true,
                (FnExprArg::EmbeddedQuery(_), FnType::Logical) => true,
                (FnExprArg::FnExpr(_), _) => {
                    let return_type = arg.to_fn_type();
                    match (return_type, expected_type) {
                        (FnType::ValueType, FnType::ValueType) => true,
                        (FnType::NodesType, FnType::NodesType) => true,
                        (FnType::NodesType, FnType::Logical) => true,
                        (FnType::Logical, FnType::Logical) => true,
                        _ => false,
                    }
                }
                // rfc: LogicalTrue and LogicalFalse are unrelated to the JSON values expressed by
                // the literals true and false
                // this means that if the expected_type is Value we can not convert the LogicalTrue/False
                // to Value::Boolean
                (FnExprArg::LogicalExpr(_), FnType::Logical) => true,
                (FnExprArg::LogicalExpr(_), _) => false,
                // as of the time of this implementation no method accepts nothing as an argument type
                _ => false,
            };

            if !valid {
                return Err(FnExprError::TypeMismatch {
                    expected: expected_type,
                    found: arg.to_fn_type(),
                });
            }
        }
        Ok(())
    }

    // the approach where we used Literal(Type) for as 1 of FnExprArg variants won't work well in the
    // next step.
    //
    // the problem we face is what happens when FnArg is a literal, since FnArg owns the data, and we
    // want to pass a reference to Value as our parameter we can not get &Value from Type without cloning
    // even if we flip the types and expect a Type that is incorrect based on the RFC(we need to return
    // a special value when Value is an object) we still have to do that conversion because we now have
    // to go from &Value to Type, &Value is what a singular query returns. This will simplify the comparison
    // expressions as well when we had to compare Type and Value
    //
    // maps FnExprArg to FnArg, evaluates queries and logical expressions, so that their return values
    // to be used as FnArg
    fn to_fn_args<'a>(
        &'a self,
        // toDo: why f: &impl Function failed
        f: &dyn Function,
        root: &'a Value,
        current: &'a Value,
    ) -> Vec<FnArg<'a>> {
        let mut args = Vec::with_capacity(f.args().len());

        for arg in self.args.iter() {
            // this could also be a method to_fn_arg() or from<FnExpr> for FnArg
            match arg {
                FnExprArg::Literal(v) => args.push(FnArg::Value(Cow::Borrowed(v))),
                FnExprArg::EmbeddedQuery(q) => {
                    args.push(FnArg::Nodelist(q.evaluate(root, current)));
                }
                FnExprArg::FnExpr(expr) => match expr.evaluate(root, current) {
                    FnResult::Value(v) => args.push(FnArg::Value(v)),
                    FnResult::Logical(b) => args.push(FnArg::Logical(b)),
                    FnResult::Nothing => args.push(FnArg::Nothing),
                },
                FnExprArg::LogicalExpr(expr) => {
                    args.push(FnArg::Logical(expr.evaluate(root, current)));
                }
            }
        }
        args
    }
}

impl FnExprArg {
    fn to_fn_type(&self) -> FnType {
        match self {
            FnExprArg::Literal(_) => FnType::ValueType,
            FnExprArg::EmbeddedQuery(_) => FnType::NodesType,
            FnExprArg::FnExpr(expr) => {
                let func = registry().get(&expr.name).unwrap();
                func.return_type()
            }
            FnExprArg::LogicalExpr(_) => FnType::Logical,
        }
    }
}

struct Registry {
    // toDo: why Box<dyn Function> and not something like impl Function?
    functions: HashMap<String, Box<dyn Function>>,
}

impl Registry {
    fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    // toDo: remove static lifetime check what happens to Box::new()
    // older syntax: https://stackoverflow.com/questions/57562632/why-is-impl-needed-when-passing-traits-as-function-parameters
    // fn register<F: Function + 'static>(&mut self, f: F)
    fn register(&mut self, f: impl Function + 'static) {
        self.functions.insert(f.name().to_string(), Box::new(f));
    }

    fn get(&self, name: &str) -> Option<&dyn Function> {
        self.functions.get(name).map(|f| f.as_ref())
    }
}

// toDo: why we need Send + Sync for OnceLock
trait Function: Send + Sync {
    fn name(&self) -> &'static str;
    // we can't return [Arg] because we will get an error like:
    //  Trait `std::marker::Sized` is not implemented for `[Arg]`
    //  `[Arg]` does not have a constant size known at compile-time
    //
    // it happens because a slice does not have a known size at compile time, &[Arg]
    // a reference that points to the slice, which size is known at compile time, works Box<[Arg]> would
    // also work
    fn args(&self) -> &[FnType];
    fn return_type(&self) -> FnType;
    // initially had this: fn execute(&self, args: &[FnArg]) -> FnResult;
    // was getting this warning: hiding a lifetime that's elided elsewhere is confusing
    //
    // it is not an error; it is just that is unclear to the user that reads the code that FnResult
    // holds a reference; we fix that by returning FnResult<'_>.
    // The lifetime can be elided based on the 3rd elision rule:
    //  "if there are multiple input lifetime parameters, but one of them is &self or
    //  &mut self because this is a method, the lifetime of self is assigned to all output
    //  lifetime parameters."
    //
    // This is not what we want, FnResult holds reference to values that exists in the root, we can't
    // tie the lifetime of the values in FnResult with self, we need to tie together references
    // inside FnArg and inside FnResult. Both live inside root
    fn execute<'a>(&self, args: &[FnArg<'a>]) -> FnResult<'a>;
}

struct LengthFn {}

impl Function for LengthFn {
    fn name(&self) -> &'static str {
        "length"
    }

    // rfc uses argument for the fn expressions and parameters when resolved
    fn args(&self) -> &[FnType] {
        &[FnType::ValueType]
    }

    fn return_type(&self) -> FnType {
        FnType::ValueType
    }

    fn execute<'a>(&self, args: &[FnArg<'a>]) -> FnResult<'a> {
        if let Some(val) = args[0].as_value() {
            let len = match val {
                Value::Object(map) => map.len(),
                Value::Array(arr) => arr.len(),
                Value::String(s) => s.chars().count(),
                _ => return FnResult::Nothing,
            };
            FnResult::Value(Cow::Owned(Value::from(len as i64)))
        } else {
            // we return nothing when the singular query returned empty list(returns at most 1)
            FnResult::Nothing
        }
    }
}

struct CountFn {}

impl Function for CountFn {
    fn name(&self) -> &'static str {
        "count"
    }

    fn args(&self) -> &[FnType] {
        &[FnType::NodesType]
    }

    fn return_type(&self) -> FnType {
        FnType::ValueType
    }

    fn execute<'a>(&self, args: &[FnArg<'a>]) -> FnResult<'a> {
        FnResult::Value(Cow::Owned(Value::from(args[0].as_nodelist().len() as i64)))
    }
}

struct MatchFn {}

impl Function for MatchFn {
    fn name(&self) -> &'static str {
        "match"
    }

    fn args(&self) -> &[FnType] {
        &[FnType::ValueType, FnType::ValueType]
    }

    fn return_type(&self) -> FnType {
        FnType::Logical
    }

    fn execute<'a>(&self, args: &[FnArg<'a>]) -> FnResult<'a> {
        todo!()
    }
}

struct SearchFn {}

impl Function for SearchFn {
    fn name(&self) -> &'static str {
        "search"
    }

    fn args(&self) -> &[FnType] {
        &[FnType::ValueType, FnType::ValueType]
    }

    fn return_type(&self) -> FnType {
        FnType::Logical
    }

    fn execute<'a>(&self, args: &[FnArg<'a>]) -> FnResult<'a> {
        todo!()
    }
}

struct ValueFn {}

impl Function for ValueFn {
    fn name(&self) -> &'static str {
        "value"
    }

    fn args(&self) -> &[FnType] {
        &[FnType::NodesType]
    }

    fn return_type(&self) -> FnType {
        FnType::ValueType
    }

    // If the argument contains a single node, the result is the value of the node.
    // If the argument is the empty nodelist or contains multiple nodes, the result is Nothing
    fn execute<'a>(&self, args: &[FnArg<'a>]) -> FnResult<'a> {
        let nodes = args[0].as_nodelist();
        if nodes.len() == 1 {
            FnResult::Value(Cow::Borrowed(nodes[0]))
        } else {
            FnResult::Nothing
        }
    }
}

// toDo: regex should have their own error, dont create a constructor, have a method matches(), we don't want to scan the input twice, 1 for validation in the constructor
// and again to check if we have a match
// the states machines dont have any memory, they don't care how they got in the current state, they don't remember,
// they only know where they are now
// ε transition is a free move without reading the input
// ab is a and b but also a, empty string(ε) and b
// regular expr and finite state machines are equivalent systems and what we do is we simulate the
// regex as a state machine. If a state machine  reaches to a possible Accept state we have a match
// in order to get a match the machine has to be in an accept state at the end of the input
#[derive(Debug, PartialEq)]
pub enum FnExprError {
    // the number of arguments that the function has
    ArityMismatch { expected: usize, got: usize },
    TypeMismatch { expected: FnType, found: FnType },
}

impl Display for FnExprError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FnExprError::ArityMismatch { expected, got } => {
                write!(
                    f,
                    "expected {} parameters, but {} parameters supplied",
                    expected, got
                )
            }
            FnExprError::TypeMismatch { expected, found } => {
                // check the dif between {:?} and {}
                write!(f, "expected {:?}, found {:?}", expected, found)
            }
        }
    }
}

impl Error for FnExprError {}

// toDo: $[?match(@.timezone, 'Europe/.*') == true]	not well-typed as LogicalType may not be used in comparisons
// check that this returns false
mod regex {
    use crate::parsing::{number, utf8};
    use crate::parsing::number::Atoi;

    enum Regexp {
        Empty,
        Concat(usize, usize),
        Union(usize, usize),
        Star(usize),
        Atom(CharClass),
    }

    enum CharClass {
        Literal(char),
        Dot,
        ClassExpr(ClassExpr),
        Property(Property),
    }

    struct ClassExpr {
        negated: bool,
        items: Vec<ExprItem>,
    }

    enum ExprItem {
        Literal(char),
        Property(Property),
        Range(char, char),
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
                nodes: Vec::new(),
            }
        }

        fn parse(&mut self) -> Result<Regexp, RegexpError> {
            if self.buffer.is_empty() {
                return Ok(Regexp::Empty);
            }
            Ok(self.parse_union()?)
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

        fn parse_union(&mut self) -> Result<usize, RegexpError> {
            let lhs = self.parse_concat()?;
        }

        fn parse_concat(&mut self) -> Result<usize, RegexpError> {
            let lhs = self.parse_quantifier()?;
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
                Some(b'{') => {}
                _ => Ok(atom_idx),
            }
        }

        // we return min, Optional<max>
        fn parse_range_syntax(&mut self) -> Result<(u8, Option<u8>), RegexpError> {
            self.consume(1);
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
                            let max = Atoi::atoi(&mut self.buffer, &mut self.pos)?;
                            let max = if max == 0 { None } else { Some(max) };
                            match self.peek() {
                                Some(b'}') => Ok((min, max)),
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
                        // {2}
                        Some(b'}') => Ok((min, None)),
                        Some(b) => Err(RegexpError {
                            kind: RegexpErrorKind::UnexpectedCharacter(*b),
                            pos: self.pos,
                        }),
                    }
                }
                None => Err(RegexpError {
                    kind: RegexpErrorKind::UnexpectedEof,
                    pos: self.pos - 1,
                }),
            }
        }

        // to expand a quantifier to an expression we need to consider 3 cases
        // {2,4} at least 2, at most 4
        // {2} exactly 2
        // {2,} at least 2
        fn parse_range_quantifier(&mut self) {}

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

            // we have 3 cases to consider when we parse a hyphen
            // [-a] is treated as a literal
            // [a-] is also tread as a literal
            // [a-z] now we have a range because we encounter CCchar '-' CCchar
            while self.pos < len && self.buffer[self.pos] != b']' {
                let start = self.parse_class_expr_item()?;
                if let Some(b) = self.peek() {
                    if *b == b'-' {
                        self.consume(1);
                        match self.peek() {
                            //[a-]
                            Some(b) if *b == b']' => {
                                // we don't have to consume ']' now; it is consumed when we exit the loop
                                items.push(start);
                                items.push(ExprItem::Literal('-'));
                                break;
                            }
                            Some(_) => items.push(self.parse_range(start)?),
                            // we peeked, and we didn't get anything, we have an Eof case which will
                            // be handled in the next iteration
                            None => (),
                        }
                    } else {
                        items.push(start);
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

        fn parse_range(&mut self, start: ExprItem) -> Result<ExprItem, RegexpError> {
            let end = self.parse_class_expr_item()?;
            match (start, end) {
                (ExprItem::Literal(s), ExprItem::Literal(e)) => {
                    if s > e {
                        return Err(RegexpError {
                            kind: RegexpErrorKind::RangeOutOfOrder(s, e),
                            pos: self.pos,
                        });
                    }
                    Ok(ExprItem::Range(s, e))
                }
                // toDo: maybe we need to set pos to the start of range?
                _ => Err(RegexpError {
                    kind: RegexpErrorKind::InvalidRange,
                    pos: self.pos,
                }),
            }
        }

        fn parse_escape(&mut self) -> Result<Escape, RegexpError> {
            // consume '\'
            self.consume(1);
            match self.peek() {
                Some(b'p') | Some(b'P') => Ok(Escape::Property(self.parse_char_property()?)),
                Some(b) if !is_escape_char(*b) => Err(RegexpError {
                    kind: RegexpErrorKind::UnexpectedCharacter(*b),
                    pos: self.pos + 1,
                }),
                Some(_) => Ok(Escape::Literal(self.map_escape_character())),
                None => Err(RegexpError {
                    kind: RegexpErrorKind::UnexpectedEof,
                    pos: self.pos - 1,
                }),
            }
        }

        fn parse_class_expr_item(&mut self) -> Result<ExprItem, RegexpError> {
            match self.buffer[self.pos] {
                b'\\' => Ok(ExprItem::from(self.parse_escape()?)),
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

        fn parse_char_property(&mut self) -> Result<Property, RegexpError> {
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

        fn consume(&mut self, n: usize) {
            self.pos += n;
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

        fn map_category(&mut self) -> Result<GeneralCategory, RegexpError> {
            let current = self.peek();
            let next = self.peek_next();

            // major, minor order
            // after parsing a category we need to advance pos by the len its length, 1 for major,
            // 2 for minor
            let (category, n) = match (current, next) {
                (Some(b'L'), Some(b'}')) => (GeneralCategory::Letter, 1),
                (Some(b'L'), Some(b'l')) => (GeneralCategory::LetterLowercase, 2),
                (Some(b'L'), Some(b'm')) => (GeneralCategory::LetterModifier, 2),
                (Some(b'L'), Some(b'o')) => (GeneralCategory::LetterOther, 2),
                (Some(b'L'), Some(b't')) => (GeneralCategory::LetterTitlecase, 2),
                (Some(b'L'), Some(b'u')) => (GeneralCategory::LetterUppercase, 2),
                (Some(b'L'), None) => {
                    return Err(RegexpError {
                        kind: RegexpErrorKind::UnexpectedEof,
                        pos: self.pos + 1,
                    });
                }
                (Some(b'L'), Some(b)) => {
                    return Err(RegexpError {
                        kind: RegexpErrorKind::UnexpectedCharacter(*b),
                        pos: self.pos + 1,
                    });
                }

                (Some(b'M'), Some(b'}')) => (GeneralCategory::Mark, 1),
                (Some(b'M'), Some(b'c')) => (GeneralCategory::MarkSpacingCombining, 2),
                (Some(b'M'), Some(b'e')) => (GeneralCategory::MarkEnclosing, 2),
                (Some(b'M'), Some(b'n')) => (GeneralCategory::MarkNonSpacing, 2),
                (Some(b'M'), None) => {
                    return Err(RegexpError {
                        kind: RegexpErrorKind::UnexpectedEof,
                        pos: self.pos + 1,
                    });
                }
                (Some(b'M'), Some(b)) => {
                    return Err(RegexpError {
                        kind: RegexpErrorKind::UnexpectedCharacter(*b),
                        pos: self.pos + 1,
                    });
                }

                (Some(b'N'), Some(b'}')) => (GeneralCategory::Number, 1),
                (Some(b'N'), Some(b'd')) => (GeneralCategory::NumberDecimalDigit, 2),
                (Some(b'N'), Some(b'l')) => (GeneralCategory::NumberLetter, 2),
                (Some(b'N'), Some(b'o')) => (GeneralCategory::NumberOther, 2),
                (Some(b'N'), None) => {
                    return Err(RegexpError {
                        kind: RegexpErrorKind::UnexpectedEof,
                        pos: self.pos + 1,
                    });
                }
                (Some(b'N'), Some(b)) => {
                    return Err(RegexpError {
                        kind: RegexpErrorKind::UnexpectedCharacter(*b),
                        pos: self.pos + 1,
                    });
                }

                (Some(b'P'), Some(b'}')) => (GeneralCategory::Punctuation, 1),
                (Some(b'P'), Some(b'c')) => (GeneralCategory::PunctuationConnector, 2),
                (Some(b'P'), Some(b'd')) => (GeneralCategory::PunctuationDash, 2),
                (Some(b'P'), Some(b'e')) => (GeneralCategory::PunctuationClose, 2),
                (Some(b'P'), Some(b'f')) => (GeneralCategory::PunctuationFinalQuote, 2),
                (Some(b'P'), Some(b'i')) => (GeneralCategory::PunctuationInitialQuote, 2),
                (Some(b'P'), Some(b's')) => (GeneralCategory::PunctuationOpen, 2),
                (Some(b'P'), Some(b'o')) => (GeneralCategory::PunctuationOther, 2),
                (Some(b'P'), None) => {
                    return Err(RegexpError {
                        kind: RegexpErrorKind::UnexpectedEof,
                        pos: self.pos + 1,
                    });
                }
                (Some(b'P'), Some(b)) => {
                    return Err(RegexpError {
                        kind: RegexpErrorKind::UnexpectedCharacter(*b),
                        pos: self.pos + 1,
                    });
                }

                (Some(b'Z'), Some(b'}')) => (GeneralCategory::Separator, 1),
                (Some(b'Z'), Some(b'l')) => (GeneralCategory::SeparatorLine, 2),
                (Some(b'Z'), Some(b'p')) => (GeneralCategory::SeparatorParagraph, 2),
                (Some(b'Z'), Some(b's')) => (GeneralCategory::SeparatorSpace, 2),
                (Some(b'Z'), None) => {
                    return Err(RegexpError {
                        kind: RegexpErrorKind::UnexpectedEof,
                        pos: self.pos + 1,
                    });
                }
                (Some(b'Z'), Some(b)) => {
                    return Err(RegexpError {
                        kind: RegexpErrorKind::UnexpectedCharacter(*b),
                        pos: self.pos + 1,
                    });
                }

                (Some(b'S'), Some(b'}')) => (GeneralCategory::Symbol, 1),
                (Some(b'S'), Some(b'c')) => (GeneralCategory::SymbolCurrency, 2),
                (Some(b'S'), Some(b'k')) => (GeneralCategory::SymbolModifier, 2),
                (Some(b'S'), Some(b'm')) => (GeneralCategory::SymbolMath, 2),
                (Some(b'S'), Some(b'o')) => (GeneralCategory::SymbolOther, 2),
                (Some(b'S'), None) => {
                    return Err(RegexpError {
                        kind: RegexpErrorKind::UnexpectedEof,
                        pos: self.pos + 1,
                    });
                }
                (Some(b'S'), Some(b)) => {
                    return Err(RegexpError {
                        kind: RegexpErrorKind::UnexpectedCharacter(*b),
                        pos: self.pos + 1,
                    });
                }

                (Some(b'C'), Some(b'}')) => (GeneralCategory::Other, 1),
                (Some(b'C'), Some(b'c')) => (GeneralCategory::OtherControl, 2),
                (Some(b'C'), Some(b'f')) => (GeneralCategory::OtherFormat, 2),
                (Some(b'C'), Some(b'n')) => (GeneralCategory::OtherNotAssigned, 2),
                (Some(b'C'), Some(b'o')) => (GeneralCategory::OtherPrivateUse, 2),
                (Some(b'C'), None) => {
                    return Err(RegexpError {
                        kind: RegexpErrorKind::UnexpectedEof,
                        pos: self.pos + 1,
                    });
                }
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

    struct Property {
        category: GeneralCategory,
        negated: bool,
    }

    // major, minor
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

    struct RegexpError {
        kind: RegexpErrorKind,
        pos: usize,
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

    enum RegexpErrorKind {
        InvalidRange,
        // any character not just ascii
        RangeOutOfOrder(char, char),
        UnexpectedCharacter(u8),
        UnexpectedEof,
    }
}
