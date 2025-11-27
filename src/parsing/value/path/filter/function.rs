use std::borrow::Cow;
use std::cmp::PartialEq;
use crate::parsing::value::Value;
use crate::parsing::value::error::FnExprError;
use crate::parsing::value::path::filter::{EmbeddedQuery, LogicalExpr};
use std::collections::HashMap;
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

impl FnExprArg {
    fn as_fn_type(&self) -> FnType {
        match self {
            FnExprArg::Literal(_) => FnType::ValueType,
            FnExprArg::EmbeddedQuery(_) => FnType::NodesType,
            FnExprArg::FnExpr(expr) => {
                let func = registry().get(&expr.name).unwrap();
                func.return_type()
            }
            FnExprArg::LogicalExpr(_) => FnType::Logical
        }
    }
}

impl FnExpr {
    // read the comment above execute() on the trait definition on why we need FnResult<'_>
    pub(super) fn evaluate(&self, root: &Value, current: &Value) -> FnResult<'_> {
        // always safe to call unwrap; if we get None it means either we parsed the name incorrectly
        // or we didn't register our function correctly
        let f = registry().get(&self.name).unwrap();
        let args = self.resolve_args(f, root, current);
        f.execute(&args)
    }

    pub(crate) fn type_check(&self) -> Result<(), FnExprError> {
        let func = registry().get(&self.name).unwrap();

        if self.args.len() != func.args().len() {
            return Err(FnExprError::ArityMismatch {
                expected: func.args().len(),
                got: self.args.len(),
            })
        }

        for (i, arg) in self.args.iter().enumerate() {
            let expected_type = func.args()[i];

            let valid = match (arg, expected_type) {
                (FnExprArg::Literal(_), FnType::ValueType) => true,
                (FnExprArg::Literal(_), _) => false,
                (FnExprArg::EmbeddedQuery(q), FnType::ValueType) => q.is_definite(),
                (FnExprArg::EmbeddedQuery(_), FnType::NodesType) => true,
                (FnExprArg::EmbeddedQuery(_), FnType::Logical) => true,
                // as of the time of this implementation no method accepts nothing as an argument type
                (FnExprArg::EmbeddedQuery(_), FnType::Nothing) => false,
                (FnExprArg::FnExpr(_), _) => {
                    let return_type = arg.as_fn_type();
                    match(return_type, expected_type) {
                        (FnType::ValueType, FnType::ValueType) => true,
                        (FnType::NodesType, FnType::NodesType) => true,
                        (FnType::NodesType, FnType::Logical) => true,
                        (FnType::Logical, FnType::Logical) => true,
                        _ => false,
                    }
                }
                (FnExprArg::LogicalExpr(_), FnType::Logical) => true,
                (FnExprArg::LogicalExpr(_), _) => false,
            };

            if !valid {
                return Err(FnExprError::TypeMismatch {
                    expected: expected_type,
                    got: arg.as_fn_type()
                })
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
    fn resolve_args<'a>(
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
                FnExprArg::FnExpr(expr) => {
                    match expr.evaluate(root, current) {
                        FnResult::Value(v) => args.push(FnArg::Value(v)),
                        FnResult::Logical(b) => args.push(FnArg::Logical(b)),
                        FnResult::Nothing => args.push(FnArg::Nothing),
                    }
                }
                FnExprArg::LogicalExpr(expr) => {
                    args.push(FnArg::Logical(expr.evaluate(root, current)));
                }
            }
        }
        args
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

    fn get(&self, name: &str) ->Option<& dyn Function>{
        self.functions.get(name).map(|f| f.as_ref())
    }
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
    Nothing
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
    // holds a reference; by returning FnResult<'_> we express that and when we implement the
    // method we will be explicit with lifetimes where we will tie the lifetime of the reference
    // of FnResult to the lifetime of the references of the input.
    fn execute(&self, args: &[FnArg]) -> FnResult<'_>;
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

    fn execute(&self, args: &[FnArg]) -> FnResult {
        todo!()
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

    fn execute(&self, args: &[FnArg]) -> FnResult {
        todo!()
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

    fn execute(&self, args: &[FnArg]) -> FnResult {
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

    fn execute(&self, args: &[FnArg]) -> FnResult {
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

    fn execute(&self, args: &[FnArg]) -> FnResult {
        todo!()
    }
}

mod regex {

}