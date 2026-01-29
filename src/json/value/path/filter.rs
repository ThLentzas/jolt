use crate::json::value::path::filter::function::{FnExpr, FnResult};
use crate::json::value::path::tracker::{NoOpTracker, PathNode};
use crate::json::value::path::{EvalContext, Segment};
use crate::json::value::Value;
use std::borrow::Cow;

pub(crate) mod function;
mod nfa;
mod regex;
mod table;

// https://docs.rs/recursion/latest/recursion/
//
// struct A {
//     inner : A
// }
// we get an error: - recursive without indirection
//
// Rust needs to know the size of LogicalExpression and because it is a recursive the size grows
// infinite
// AST
#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) enum LogicalExpr {
    Comparison(ComparisonExpr),
    Test(TestExpr),
    And(Box<LogicalExpr>, Box<LogicalExpr>),
    Or(Box<LogicalExpr>, Box<LogicalExpr>),
    Not(Box<LogicalExpr>),
}

impl LogicalExpr {
    // short-circuiting and well-typedness of functions
    // because the type checking happens during json, we will never encounter a case where we
    // might have an invalid function(Arity/Type mismatch) that we will miss because the lhs evaluated
    // to true/false, and we returned without evaluating the rhs
    //
    // lifetimes: context hold references to values that live in root so it shares the same lifetime
    // as current which also lives in root; the lifetime of context, as reference is not tied to self,
    // it is independent borrow passed into the function;
    pub(super) fn evaluate<'r>(&self, context: &mut EvalContext<'r>, current: &'r Value) -> bool {
        match self {
            LogicalExpr::Comparison(expr) => expr.evaluate(context, current),
            LogicalExpr::Test(expr) => expr.evaluate(context, current),
            LogicalExpr::And(lhs, rhs) => {
                // auto deref by rust to access the expr(deref coercion)
                // lhs.evaluate() is (*lhs).evaluate(root, current) internally
                lhs.evaluate(context, current) && rhs.evaluate(context, current)
            }
            LogicalExpr::Or(lhs, rhs) => {
                lhs.evaluate(context, current) || rhs.evaluate(context, current)
            }
            LogicalExpr::Not(expr) => !expr.evaluate(context, current),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct ComparisonExpr {
    pub(super) lhs: Comparable,
    pub(super) op: ComparisonOp,
    pub(super) rhs: Comparable,
}

impl ComparisonExpr {
    // lifetimes: context hold references to values that live in root so it shares the same lifetime
    // as current which also lives in root; the lifetime of context, as reference is not tied to self,
    // it is independent borrow passed into the function;
    fn evaluate<'r>(&self, context: &mut EvalContext<'r>, current: &'r Value) -> bool {
        let lhs = self.lhs.to_operand(context, current);
        let rhs = self.rhs.to_operand(context, current);

        match (lhs, rhs) {
            // both empty('absent'/'nothing' as defined in the rfc) and the operators that include
            // equals(<=, >=) return true
            (Operand::Empty, Operand::Empty) => matches!(
                self.op,
                ComparisonOp::Equal
                    | ComparisonOp::LessThanOrEqual
                    | ComparisonOp::GreaterThanOrEqual
            ),
            (Operand::Empty, _) | (_, Operand::Empty) => matches!(self.op, ComparisonOp::NotEqual),
            // non-singular queries
            (Operand::Invalid, _) | (_, Operand::Invalid) => false,
            (Operand::Value(lhs), Operand::Value(rhs)) => self.op.apply(&lhs, &rhs),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) enum TestExpr {
    EmbeddedQuery(EmbeddedQuery),
    FnExpr(FnExpr),
}

impl TestExpr {
    // lifetimes: context hold references to values that live in root so it shares the same lifetime
    // as current which also lives in root; the lifetime of context, as reference is not tied to self,
    // it is independent borrow passed into the function
    fn evaluate<'r>(&self, context: &mut EvalContext<'r>, current: &'r Value) -> bool {
        match self {
            TestExpr::EmbeddedQuery(q) => {
                let nodelist = q.evaluate(context, current);
                // yields true if the query selects at least one node
                nodelist.len() != 0
            }
            TestExpr::FnExpr(expr) => match expr.evaluate(context, current) {
                FnResult::Value(_) => false,
                FnResult::Logical(b) => b,
                FnResult::Nothing => false,
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(super) enum Comparable {
    Literal(Value),
    EmbeddedQuery(EmbeddedQuery),
    FnExpr(FnExpr),
}

// operand is what we get after evaluating a Comparable
enum Operand<'r> {
    Value(Cow<'r, Value>),
    Empty,
    Invalid,
}

impl Comparable {

    // lifetimes: we have 2 lifetimes to consider. 'r: values that are returned by evaluating
    // embedded queries, those are guaranteed to have the 'r lifetime since they are references to
    // values that in live in root. Values that are returned from evaluating a function can be either
    // owned like in the case of length() or borrowed like in the case of value(); for owned data
    // we have no lifetime conflicts and for borrowed we return a value that lives in root so it
    // gets the 'r lifetime.
    //
    // the problem is in this case: Comparable::Literal(t) => Operand::Value(Cow::Borrowed(t))
    // This is the case where either lhs or rhs was a literal. During json we wrapped the literal
    // in a Value() so we can then use the comparison operators. The Comparable::Literal(t) has the
    // lifetime of self not of 'r, it does not live in root which is shorter than 'r.
    //
    // fn to_operand(&self, context: &EvalContext<'r>, ...) -> Operand<'r>
    // we can't enforce Operand to hold references with 'r lifetime.
    // t has lifetime of &self, not 'r and &self might be shorter than 'r
    //
    // To fix it we introduce the 'op lifetime and in the contract we specify that 'r lives at least
    // as 'op. It works in both cases, for Comparable::Literal(t) => Operand::Value(Cow::Borrowed(t))
    // which basically is Cow::Borrowed(&self.literal), the reference has the self life('op) so
    // Operand<'op> is satisfied. For the 'r cases because we guarantee that 'r will live at least
    // as 'op the compiler is satisfied; t still has the same lifetime as self, but now we are sure
    // that we won't have a case where 1 lifetime is shorter than the other
    //
    // context hold references to values that live in root so it shares the same lifetime
    // as current which also lives in root; the lifetime of context, as reference is not tied to self,
    // it is independent borrow passed into the function
    fn to_operand<'op, 'r>(
        &'op self,
        context: &mut EvalContext<'r>,
        current: &'r Value,
    ) -> Operand<'op>
    where
        'r: 'op,
    {
        match self {
            Comparable::EmbeddedQuery(q) => {
                let nodelist = q.evaluate(context, current);
                match nodelist.len() {
                    0 => Operand::Empty,
                    1 => Operand::Value(Cow::Borrowed(nodelist[0])),
                    _ => Operand::Invalid,
                }
            }
            Comparable::Literal(t) => Operand::Value(Cow::Borrowed(t)),
            Comparable::FnExpr(expr) => {
                match expr.evaluate(context, current) {
                    FnResult::Value(v) => Operand::Value(v),
                    // a fn that returns logical true/false can't be used in comparison expr
                    FnResult::Logical(_) => Operand::Invalid,
                    FnResult::Nothing => Operand::Empty,
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(super) enum ComparisonOp {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

impl ComparisonOp {
    pub(super) fn len(&self) -> usize {
        match self {
            ComparisonOp::GreaterThan | ComparisonOp::LessThan => 1,
            ComparisonOp::Equal
            | ComparisonOp::NotEqual
            | ComparisonOp::LessThanOrEqual
            | ComparisonOp::GreaterThanOrEqual => 2,
        }
    }

    fn apply(&self, lhs: &Value, rhs: &Value) -> bool {
        match self {
            ComparisonOp::Equal => lhs == rhs,
            ComparisonOp::NotEqual => lhs != rhs,
            ComparisonOp::LessThan => lhs < rhs,
            ComparisonOp::LessThanOrEqual => lhs <= rhs,
            ComparisonOp::GreaterThan => lhs > rhs,
            ComparisonOp::GreaterThanOrEqual => lhs >= rhs,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(super) enum EmbeddedQueryType {
    Absolute,
    Relative,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct EmbeddedQuery {
    pub(super) id: usize,
    pub(super) query_type: EmbeddedQueryType,
    pub(super) segments: Vec<Segment>,
}

impl EmbeddedQuery {
    // as we iterate the values of map/array if the query type is relative the current value will
    // be the root of the query. For absolute paths we have a reference to the root, we will evaluate
    // once and cache it after.
    //
    // Caching:
    //
    //  We need to cache when we have an absolute path query. Absolute path queries always evaluate
    //  to the same output list. In a filter selector, we can evaluate the path once and then cache
    //  the result.
    //  In theory what we want to cache is a Vec<Segments> but that is not possible without cloning
    //  which can be slow because we have to clone a Vec<Selectors> and so on. We need to clone
    //  because the embedded query owns the Vec<Segments>. Instead, we generate a unique id for
    //  every embedded query. During evaluation if the query is an absolute path query we check the
    //  cache if an entry exists with the query's id. If it does, we return directly by cloning the
    //  value for the key(it is cheap, it is a vec of & to values), else we evaluate. We update the
    //  cache only if the query we evaluated is an absolute path query.
    //
    //  There is another case we can encounter: $[?$.config][?$.config]. Two different queries that
    //  have the same segments. The id approach will create 2 different queries so when we do the
    //  lookup the 2nd time we encounter the same segments we will have a cache miss because there
    //  will be 2 different eq ids.
    //
    // lifetimes: context hold references to values that live in root so it shares the same lifetime
    // as current which also lives in root; the lifetime of context, as reference is not tied to self,
    // it is independent borrow passed into the function. We don't return a reference but an owned
    // type that holds references; elision rules are applied the same and if we were explicit with
    // lifetimes it would tie the lifetime of references to self which is not what we want; embedded
    // queries always return references to values that live in root so they must have the 'r lifetime
    fn evaluate<'r>(&self, context: &mut EvalContext<'r>, current: &'r Value) -> Vec<&'r Value> {
        let start = if self.query_type == EmbeddedQueryType::Relative {
            current
        } else {
            context.root
        };
        // we can hard-code () as Trace because we don't care about the paths for embedded queries
        let mut reader: Vec<PathNode<'r, ()>> = vec![PathNode {
            val: start,
            trace: (),
        }];
        let mut writer: Vec<PathNode<'r, ()>> = Vec::new();

        if self.query_type == EmbeddedQueryType::Absolute {
            if let Some(values) = context.cache.get(&self.id) {
                return values.clone();
            }
        }
        let values = self.run(context, &mut reader, &mut writer);
        if self.query_type == EmbeddedQueryType::Absolute {
            context.cache.insert(self.id, values.clone());
        }
        values
    }

    // lifetimes: look at evaluate() above and segment.apply(), is a combination of both
    fn run<'r>(
        &self,
        context: &mut EvalContext<'r>,
        reader: &mut Vec<PathNode<'r, ()>>,
        writer: &mut Vec<PathNode<'r, ()>>,
    ) -> Vec<&'r Value> {
        for segment in self.segments.iter() {
            writer.clear();
            segment.apply::<NoOpTracker>(context, reader, writer);
            std::mem::swap(reader, writer);
        }
        // the final nodelist is in reader after the last swap
        reader.into_iter().map(|c| c.val).collect()
    }

    // returns at most 1 node
    fn is_definite(&self) -> bool {
        self.segments.iter().all(|seg| seg.is_singular())
    }
}

// toDo: review anchors

