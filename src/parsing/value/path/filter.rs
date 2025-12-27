use crate::parsing::value::path::filter::function::{FnExpr, FnResult};
use crate::parsing::value::path::tracker::{NoOpTracker, PathNode};
use crate::parsing::value::path::{EvalContext, Segment, SegmentKind};
use crate::parsing::value::Value;
use std::borrow::Cow;

pub(crate) mod function;
mod nfa;
mod regex;

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
    // short circuiting and well-typedness of functions
    // because the type checking happens during parsing, we will never encounter a case where we
    // might have an invalid function(Arity/Type mismatch) that we will miss because the lhs evaluated
    // to true/false and we returned without evaluating the rhs
    pub(super) fn evaluate<'v>(&self, context: &mut EvalContext<'v>, current: &'v Value) -> bool {
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
    fn evaluate<'v>(&self, context: &mut EvalContext<'v>, current: &'v Value) -> bool {
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
    fn evaluate<'v>(&self, context: &mut EvalContext<'v>, current: &'v Value) -> bool {
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
enum Operand<'a> {
    Value(Cow<'a, Value>),
    Empty,
    Invalid,
}

impl Comparable {
    fn to_operand<'op, 'v>(
        &'op self,
        context: &mut EvalContext<'v>,
        current: &'v Value,
    ) -> Operand<'op>
    where
        'v: 'op,
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
    // we want the refs to live as long as the root(they 'live in root')
    //
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
    fn evaluate<'v>(&self, context: &mut EvalContext<'v>, current: &'v Value) -> Vec<&'v Value> {
        let start = if self.query_type == EmbeddedQueryType::Relative {
            current
        } else {
            context.root
        };
        // we can hard-code () as Trace because we don't care about the paths for embedded queries
        let mut reader: Vec<PathNode<'v, ()>> = vec![PathNode {
            val: start,
            trace: (),
        }];
        let mut writer: Vec<PathNode<'v, ()>> = Vec::new();

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

    fn run<'v>(
        &self,
        context: &mut EvalContext<'v>,
        reader: &mut Vec<PathNode<'v, ()>>,
        writer: &mut Vec<PathNode<'v, ()>>,
    ) -> Vec<&'v Value> {
        for seg in self.segments.iter() {
            writer.clear();
            match seg.kind {
                SegmentKind::Child => {
                    super::apply_child_seg::<NoOpTracker>(context, reader, writer, seg);
                }
                SegmentKind::Descendant => {
                    super::apply_descendant_seg::<NoOpTracker>(context, reader, writer, seg);
                }
            }
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

// toDo: consider setting a limit on the path characters? also what happens if we recurse infinitely?
// toDo: explain the lifetimes in each case
// toDo: "'a is okay if you only have one named lifetime, but yeah if there's more than that, give them names"
// toDo: remove all lifetimes and see what happens/ add a comment
// toDo: maybe rename every lifetime that refers to a value in root as r?
// toDo: is it possible to use the arena approach for the logical expressions
// toDo: review anchors
// toDo: review the comments
