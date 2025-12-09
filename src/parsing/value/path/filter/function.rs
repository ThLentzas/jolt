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
    enum Regexp {
        Empty,
        Literal(char),
        Concat(Vec<Regexp>),
        Union(Vec<Regexp>),
        Star(Box<Regexp>),
        Dot,
        Class(CharClass),
        Property{ name: String, negated: bool },
    }

    struct CharClass {
        negated: bool,
        items: Vec<ClassItem>
    }

    enum ClassItem {
        Literal(char),
        Range(char),
        Property { name: String, negated: bool }
    }
}
