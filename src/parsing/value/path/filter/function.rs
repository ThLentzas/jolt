use crate::parsing::value::Value;
use crate::parsing::value::error::{FnExprError, PathError, PathErrorKind};
use crate::parsing::value::path::filter::{EmbeddedQuery, LogicalExpr, Type};
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

impl FnExpr {
    pub(super) fn evaluate(&self, root: &Value, current: &Value) -> bool {
        true
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum FnExprArg {
    Literal(Type),
    EmbeddedQuery(EmbeddedQuery),
    FnExpr(Box<FnExpr>),
    // note: as of the time of the implementation none of the rfc defined functions accepts as
    // parameter type a logical expression
    LogicalExpr(LogicalExpr),
}

struct Registry {
    // toDo: why Box?
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
}

// the type of function's parameters
enum ParamType {
    ValueType,
    NodesType,
}

// the resolved values that will be passed as parameters in a function
enum Param<'a> {
    // The only instances that can be directly represented in JSONPath syntax are certain JSON values
    // in ValueType expressed as literals
    // resolved value (borrowed from root or literal)
    Value(&'a Type),
    Nodelist(Vec<&'a Value>),
    Nothing,
}

enum ReturnType {
    Nothing,
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
    fn params(&self) -> &[ParamType];
    fn execute(&self, params: &[Param]) -> Option<Value>;
}

struct LengthFn {}

impl Function for LengthFn {
    fn name(&self) -> &'static str {
        "length"
    }

    // rfc uses argument for the fn expressions and parameters when resolved
    fn params(&self) -> &[ParamType] {
        &[ParamType::ValueType]
    }

    fn execute(&self, params: &[Param]) -> Option<Value> {
        todo!()
    }
}

struct CountFn {}

impl Function for CountFn {
    fn name(&self) -> &'static str {
        "count"
    }

    fn params(&self) -> &[ParamType] {
        &[ParamType::NodesType]
    }

    fn execute(&self, params: &[Param]) -> Option<Value> {
        todo!()
    }
}

struct MatchFn {}

impl Function for MatchFn {
    fn name(&self) -> &'static str {
        "match"
    }

    fn params(&self) -> &[ParamType] {
        &[ParamType::ValueType, ParamType::ValueType]
    }

    fn execute(&self, params: &[Param]) -> Option<Value> {
        todo!()
    }
}

struct SearchFn {}

impl Function for SearchFn {
    fn name(&self) -> &'static str {
        "search"
    }

    fn params(&self) -> &[ParamType] {
        &[ParamType::ValueType, ParamType::ValueType]
    }

    fn execute(&self, params: &[Param]) -> Option<Value> {
        todo!()
    }
}

struct ValueFn {}

impl Function for ValueFn {
    fn name(&self) -> &'static str {
        "value"
    }

    fn params(&self) -> &[ParamType] {
        &[ParamType::NodesType]
    }

    fn execute(&self, params: &[Param]) -> Option<Value> {
        todo!()
    }
}

// the approach where we used Literal(Type) for as 1 of FnExprArg variants won't work well in the
// next step.
//
// the problem we face is what happens when FnArg is a literal, since FnArg owns the data, and we
// want to pass a reference to Value as our parameter we can not get &Value from Type without cloning
// even if we flip the types and expect a Type that is incorrect based on the RFC(we need to return
// a special value when Value is an object) we still have to do that conversion because we now have
// to go from &Value to Type, &Value is what a singular query returns
fn resolve_args<'a>(
    expr: &FnExpr,
    f: &impl Function,
    root: &'a Value,
    current: &'a Value,
) -> Result<Vec<Param<'a>>, PathError> {
    if expr.args.len() != f.params().len() {
        return Err(PathError {
            kind: PathErrorKind::FnExpr(FnExprError::ArityMismatch {
                name: f.name(),
                expected: f.params().len(),
                got: expr.args.len(),
            }),
            pos: 2,
        });
    }

    let mut params = Vec::with_capacity(f.params().len());

    for (i, arg) in expr.args.iter().enumerate() {
        let expected_type = f.params()[i];

        let param = match arg {
            FnExprArg::EmbeddedQuery(q) => {
                let nodelist = q.evaluate(root, current);
                match nodelist.len() {
                    0 => Param::Nothing,
                    1 => Param::Value(nodelist[0]),
                    _ => Param::Nothing
                }
            }
        };
    }

    Ok(())
}