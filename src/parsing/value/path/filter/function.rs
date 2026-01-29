use crate::parsing::value::path::filter::{regex, EmbeddedQuery, LogicalExpr};
use crate::parsing::value::path::EvalContext;
use crate::parsing::value::Value;
use std::borrow::Cow;
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::sync::OnceLock;

// It can be done with LazyLock too
// https://users.rust-lang.org/t/whats-the-difference-between-std-lazylock-oncelock/116603/2
//
// we want one instance of our registry, populate it once and then reuse it; by making it static
// it will have the lifetime of our program, therefore anything we store in the registry must also
// be able to live for the lifetime of our program.
//
// Thread Safety: What if Thread A access REGISTRY, tries to initialize it but then Thread B also
// tries to access REGISTRY while still not being fully initialized? OnceLock/LazyLock guarantees
// that if thread A has the lock and the state is "Initializing" any other Thread that tries to access
// Registry has to wait. It guarantees that the value is initialized on the first access. The contents
// will never change in the future even if multiple threads have access it at the same time, because
// we return an immutable reference.
//
// Read Registry and Function definitions
static REGISTRY: OnceLock<Registry> = OnceLock::new();

fn registry() -> &'static Registry {
    REGISTRY.get_or_init(|| {
        // It acts exactly like Java's static initialization block.
        let mut registry = Registry::new();
        registry.register(LengthFn {});
        registry.register(CountFn {});
        registry.register(MatchFn {});
        registry.register(SearchFn {});
        registry.register(ValueFn {});

        registry
    })
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) enum FnExprArg {
    Literal(Value),
    EmbeddedQuery(EmbeddedQuery),
    FnExpr(Box<FnExpr>),
    // as of the time of the implementation none of the rfc defined functions accepts as parameter
    // type a logical expression
    LogicalExpr(LogicalExpr),
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct FnExpr {
    pub(crate) name: String,
    pub(crate) args: Vec<FnExprArg>,
}

impl FnExpr {
    // lifetimes: context hold references to values that live in root so it shares the same lifetime
    // as current which also lives in root; the lifetime of context, as reference is not tied to self,
    // it is independent borrow passed into the function. Similarly to evaluate() in EmbeddedQuery
    // we return a type that holds references. As described in the definition of FnResult the references
    // refer to values that live in root and this is why they get the 'r lifetime
    pub(super) fn evaluate<'r>(
        &self,
        context: &mut EvalContext<'r>,
        current: &'r Value,
    ) -> FnResult<'r> {
        // always safe to call unwrap; if we get None it means either we parsed the name incorrectly
        // or we didn't register our function
        let fun = registry().get(&self.name).unwrap();
        let args = self.to_fn_args(fun, context, current);
        fun.execute(&args)
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
                    got: arg.to_fn_type(),
                });
            }
        }
        Ok(())
    }

    // maps FnExprArg to FnArg, evaluates queries and logical expressions, so that their return values
    // will be used as FnArg
    // lifetimes: read evaluate() above
    fn to_fn_args<'r>(
        &self,
        // this is the value of our Registry map
        f: &dyn Function,
        context: &mut EvalContext<'r>,
        current: &'r Value,
    ) -> Vec<FnArg<'r>> {
        let mut args = Vec::with_capacity(f.args().len());

        // the loop runs at most twice and clone() will be called twice in the worst case where we
        // have both arguments being literals, which is quite unlikely to happen. Most of the time
        // one value is a literal(provided by the user) the other one is retrieved from root
        for arg in self.args.iter() {
            match arg {
                FnExprArg::Literal(v) => args.push(FnArg::Value(Cow::Owned(v.clone()))),
                FnExprArg::EmbeddedQuery(q) => {
                    args.push(FnArg::Nodelist(q.evaluate(context, current)));
                }
                FnExprArg::FnExpr(expr) => match expr.evaluate(context, current) {
                    FnResult::Value(v) => args.push(FnArg::Value(v)),
                    FnResult::Logical(b) => args.push(FnArg::Logical(b)),
                    FnResult::Nothing => args.push(FnArg::Nothing),
                },
                FnExprArg::LogicalExpr(expr) => {
                    args.push(FnArg::Logical(expr.evaluate(context, current)));
                }
            }
        }
        args
    }
}

// types used in the signature of the function
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum FnType {
    ValueType,
    NodesType,
    Logical,
    Nothing,
}

// length(), count() and value() return ValueType
// search, match return Logical
#[derive(Debug, PartialEq, Eq)]
pub(super) enum FnResult<'r> {
    // if the function returns data from root, borrow it. If it calculates new data, own it.
    // length(), match(), search() and count() all create new data, but value() returns an existing
    // value that lives in root, so we would have to clone in this case. length() and count() return
    // ints which we will wrap to Number, match() and search() will return Logical and with Cow
    // we can return the reference to the value; if we didn't use Cow we would have to clone the
    // reference which is not what we want
    Value(Cow<'r, Value>),
    Logical(bool),
    Nothing,
}

// the resolved values that will be passed as parameters in a function
#[derive(Debug, PartialEq, Eq)]
enum FnArg<'r> {
    Value(Cow<'r, Value>),
    Nodelist(Vec<&'r Value>),
    Logical(bool),
    Nothing,
}

impl<'r> FnArg<'r> {
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

    // lifetimes: the reference we return will have the lifetime of self but the values of vec are
    // references to values that live in root so they get the 'r lifetime
    fn as_nodelist(&self) -> &Vec<&'r Value> {
        if let FnArg::Nodelist(nodes) = self {
            return nodes;
        }
        // there is a bug in our type_check() method
        unreachable!(
            "received {:?}, expected {:?}",
            self.to_fn_type(),
            FnType::NodesType
        )
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

struct Registry {
    // dyn Trait is by default dyn Trait + static. We need the lifetime because trait objects can
    // potentially hold references inside them. When we write dyn Function Rust does not know which
    // concrete is behind it. It can be a type that holds no references or some type that holds
    // references. With the use of static we make sure that whatever concrete type is inside,
    // it must not contain any borrowed references (or only 'static ones)
    //
    // This is why register() has this signature:
    //
    // fn register<F: Function + 'static>(&mut self, f: F) {
    //         self.functions.insert(f.name().to_string(), Box::new(f));
    // }
    //
    // f will be stored inside the Box which needs to hold dyn Function + 'static so we either define
    // f as F: Function + 'static or add the static lifetime as a trait bound. If we need some other
    // lifetime we need to be explicit: fn foo<'a>(s: &'a str) -> Box<dyn Foo + 'a>
    //
    // https://users.rust-lang.org/t/why-does-t-need-static-in-box-dyn-trait/37384
    functions: HashMap<String, Box<dyn Function>>,
}

impl Registry {
    fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    // The REGISTRY variable lives forever, the HashMap inside it lives forever, the Box<dyn Function>
    // pointers inside the map must be valid forever and the data within each struct needs to live
    // forever to avoid any dangling references. Read the comment above
    fn register<F: Function>(&mut self, f: F) {
        self.functions.insert(f.name().to_string(), Box::new(f));
    }

    fn get(&self, name: &str) -> Option<&dyn Function> {
        self.functions.get(name).map(|f| f.as_ref())
    }
}

// Send + Sync: https://doc.rust-lang.org/nomicon/send-and-sync.html
// The term: A type is Sync if it is safe to share between threads (T is Sync if and only if &T is Send)
// means that multiple threads can take a &T to it
//
// Multiple threads can access REGISTRY at the same time, Rust forces us to prove that it is safe
// to do so. Send and Sync are auto traits, we don't have to implement them.
// Registry does not impl either of those traits and the reason is that Rust needs to make sure
// that the fields of Registry also implement those 2 traits and their inner fields and so on. Rust
// automatically derives Send and Sync for structs if all their fields are Send and Sync. Most
// types in Rust implement the Sync trait apart from things that work interior mutability(mutating
// while holding an immutable reference using unsafe).
//
// HashMap is Send + Sync if K and V are.
//  K (String) is standard and safe.
//  V is of type Box<dyn Function>
//
// Box<T> is Send + Sync if T is
//  T is dyn Function
//
// dyn Function
//  For trait objects the compiler cannot see the fields inside the object, it cannot check if
// those fields are thread-safe.
//
// If one of the impls of Function had a Rc we would have the following issue
//  Rc<RefCell<T>>
//  Even if T is &T a read only we can mutate with a RefCell
//    If Thread A and Thread B do this at the exact same time:
//     Thread A reads 5.
//     Thread B reads 5.
//     Thread A writes 6.
//     Thread B writes 6.
//
//  The count should be 7, but it is 6
//
// We want Registry to be Sync. (Safe to share globally). This implies &Registry must be Send.
// (Safe for threads to grab a reference to it). This implies everything inside Registry must be
// safe to access simultaneously
trait Function: Send + Sync + 'static {
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
    // lifetimes: both FnArg and FnResult hold references to values that live in root so they have
    // the same lifetimes
    fn execute<'r>(&self, args: &[FnArg<'r>]) -> FnResult<'r>;
}

struct LengthFn {}

impl Function for LengthFn {
    fn name(&self) -> &'static str {
        "length"
    }

    fn args(&self) -> &[FnType] {
        &[FnType::ValueType]
    }

    fn return_type(&self) -> FnType {
        FnType::ValueType
    }

    fn execute<'r>(&self, args: &[FnArg<'r>]) -> FnResult<'r> {
        if let Some(val) = args[0].as_value() {
            let len = match val {
                Value::Object(map) => map.len(),
                Value::Array(arr) => arr.len(),
                Value::String(s) => s.chars().count(),
                _ => return FnResult::Nothing,
            };
            // For the array case, Value::from() for usize calls Number::from() and passes usize as i64
            // for u8 that is always in range
            // for u64 in theory could overflow, but it is not possible based on the limitations that
            // we have already set. The nodelist length represents elements in a JSON array that was
            // parsed from a buffer. For the length to exceed i64::MAX, we'd need
            // 9,223,372,036,854,775,807 elements. Even at 1 byte per element ([1,1,1,...]), that's ~9
            // exabytes of JSON. The 4 MB buffer limit makes this impossible
            FnResult::Value(Cow::Owned(Value::from(len)))
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

    fn execute<'r>(&self, args: &[FnArg<'r>]) -> FnResult<'r> {
        // Value::from() for usize calls Number::from() and passes usize as i64
        // for u8 that is always in range
        // for u64 in theory could overflow, but it is not possible based on the limitations that
        // we have already set. The nodelist length represents elements in a JSON array that was
        // parsed from a buffer. For the length to exceed i64::MAX, we'd need
        // 9,223,372,036,854,775,807 elements. Even at 1 byte per element ([1,1,1,...]), that's ~9
        // exabytes of JSON. The 4 MB buffer limit makes this impossible
        FnResult::Value(Cow::Owned(Value::from(args[0].as_nodelist().len())))
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

    // for match() both arguments must be strings, in any other case we return false
    // the 2nd argument has to be valid I-Regex pattern, if not we also return false, this is handled
    // by the parser's parse(); it returns an error, and we return false
    //
    // It is always safe to access the args by index because we have already done the arity match
    fn execute<'r>(&self, args: &[FnArg<'r>]) -> FnResult<'r> {
        if let Some(val) = args[0].as_value() {
            let input = match val {
                Value::String(s) => s,
                _ => return FnResult::Logical(false),
            };
            if let Some(val) = args[1].as_value() {
                let pattern = match val {
                    Value::String(s) => s,
                    _ => return FnResult::Logical(false),
                };
                FnResult::Logical(regex::full_match(input, pattern))
            } else {
                FnResult::Logical(false)
            }
        } else {
            FnResult::Logical(false)
        }
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

    // for search() both arguments must be strings, in any other case we return false
    // the 2nd argument has to be valid I-Regex pattern, if not we also return false, this is handled
    // by the parser's parse(); it returns an error, and we return false
    //
    // It is always safe to access the args by index because we have already done the arity match
    fn execute<'r>(&self, args: &[FnArg<'r>]) -> FnResult<'r> {
        if let Some(val) = args[0].as_value() {
            let input = match val {
                Value::String(s) => s,
                _ => return FnResult::Logical(false),
            };
            if let Some(val) = args[1].as_value() {
                let pattern = match val {
                    Value::String(s) => s,
                    _ => return FnResult::Logical(false),
                };
                FnResult::Logical(regex::partial_match(input, pattern))
            } else {
                FnResult::Logical(false)
            }
        } else {
            FnResult::Logical(false)
        }
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
    // If the argument is an empty nodelist or contains multiple nodes, the result is Nothing
    fn execute<'r>(&self, args: &[FnArg<'r>]) -> FnResult<'r> {
        let nodes = args[0].as_nodelist();
        if nodes.len() == 1 {
            FnResult::Value(Cow::Borrowed(nodes[0]))
        } else {
            FnResult::Nothing
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum FnExprError {
    // the number of arguments that the function has
    ArityMismatch { expected: usize, got: usize },
    TypeMismatch { expected: FnType, got: FnType },
}

impl Error for FnExprError {}

impl fmt::Display for FnExprError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FnExprError::ArityMismatch { expected, got } => {
                write!(
                    f,
                    "expected {} parameters, but {} parameters supplied",
                    expected, got
                )
            }
            FnExprError::TypeMismatch { expected, got } => {
                write!(f, "expected {:?}, found {:?}", expected, got)
            }
        }
    }
}