use crate::parsing::number::Number;
use crate::parsing::value::Value;
use crate::parsing::value::path::{Segment, SegmentKind};
use std::collections::VecDeque;

// https://docs.rs/recursion/latest/recursion/
//
// struct A {
//     inner : A
// }
// we get an error: - recursive without indirection
//
// Rust needs to know the size of LogicalExpression and because it is a recursive the size grows
// infinite
#[derive(Debug, PartialEq)]
pub(super) enum LogicalExpression {
    // toDo: explain how with the order of read methods in logical expression we handle precedence
    Comparison(ComparisonExpr),
    Test(TestExpr),
    And(Box<LogicalExpression>, Box<LogicalExpression>),
    Or(Box<LogicalExpression>, Box<LogicalExpression>),
    Not(Box<LogicalExpression>),
}

#[derive(Debug, PartialEq)]
pub(super) struct ComparisonExpr {
    pub(super) lhs: Comparable,
    pub(super) op: ComparisonOp,
    pub(super) rhs: Comparable,
}

#[derive(Debug, PartialEq)]
pub(super) enum TestExpr {
    EmbeddedQuery(EmbeddedQuery),
    FnExpr(FnExpr)
}

#[derive(Debug, PartialEq)]
pub(super) enum ComparisonOp {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

// maybe write a closure to do the mapping?
#[derive(Debug, PartialEq)]
pub(super) enum Comparable {
    Literal(Type),
    EmbeddedQuery(EmbeddedQuery),
    FnExpr(FnExpr),
}

// json literal types
#[derive(Debug, PartialEq)]
pub(super) enum Type {
    String(String),
    Number(Number),
    Boolean(bool),
    Null,
}

#[derive(Debug, PartialEq)]
pub(super) enum EmbeddedQueryType {
    Absolute, // toDo: consider adding a reference to root so you don't have to pass it down everytime
    Relative,
}

#[derive(Debug, PartialEq)]
pub(super) struct FnExpr {
    pub(super) name: String,
    pub(super) args: Vec<FnArg>,
}

#[derive(Debug, PartialEq)]
pub(super) enum FnArg {
    Literal(Type),
    EmbeddedQuery(EmbeddedQuery),
    FnExpr(Box<FnExpr>),
}

#[derive(Debug, PartialEq)]
pub(super) struct EmbeddedQuery {
    pub(super) query_type: EmbeddedQueryType,
    pub(super) segments: Vec<Segment>,
}

// the result of evaluating Comparable that will be used in the comparison
enum Operand<'v> {
    Literal(&'v Type),
    Nodelist(Vec<&'v Value>),
    Logical(bool),
}

enum FnResultType {

}

impl LogicalExpression {
    pub(super) fn evaluate(&self, root: &Value, current: &Value) -> bool {
        match self {
            LogicalExpression::Comparison(expr) => {

            }
        }

        1 == 1
    }
}

impl ComparisonExpr {

    fn evaluate(&self, root: &Value, current: &Value) -> bool {
        // match self.lhs - Pattern matching by value (move):
        //
        // Tries to move the value out of self.lhs The variants in the match arms (q, t) are owned values
        // We are accessing lhs, and then we try to move out q and t while we only have a read only ref
        // We borrow self, and we can not move out from lhs
        // If we pass a reference to lhs we get a reference to q,t by match ergonomics
        // we could move out q and t if we actually owned self, but we have &self.
        let lhs = match &self.lhs {
            Comparable::EmbeddedQuery(q) => Operand::Nodelist(q.evaluate(root, current)),
            Comparable::Literal(t) => Operand::Literal(t),
            _ => todo!()
        };

        let rhs = match &self.rhs {
            Comparable::EmbeddedQuery(q) => Operand::Nodelist(q.evaluate(root, current)),
            Comparable::Literal(t) => Operand::Literal(t),
            _ => todo!()
        };
    }
}

// toDo: ask Gemini about the structure of having value.read() how to handle n_paths and passing
// the root to query
impl EmbeddedQuery {
    // we want the refs to live as long as the root(they 'live in root')
    //
    // as we iterate the values of map/array if the query type is relative the current value will
    // be the root of the query. For absolute paths we have a reference to the root, we will evaluate
    // once and cache it after.
    fn evaluate<'v>(&self, root: &'v Value, current: &'v Value) -> Vec<&'v Value> {
        let mut nodelist: VecDeque<&Value> = VecDeque::new();

        match self.query_type {
            EmbeddedQueryType::Relative => nodelist.push_back(current),
            EmbeddedQueryType::Absolute => nodelist.push_back(root),
        }

        for seg in self.segments.iter() {
            match seg.kind {
                SegmentKind::Child => super::apply_child_seg(&mut nodelist, seg),
                SegmentKind::Descendant => super::apply_descendant_seg(&mut nodelist, seg),
            }
        }
        nodelist.into()
    }
}

impl ComparisonOp {
    fn apply(&self, lhs: Operand, rhs: Operand) -> bool {
        match (lhs, rhs) {
            (Operand::Nodelist(list_1), Operand::Nodelist(list_2)) => {
                // only singular queries are allowed
                if list_1.len() > 1 {
                    return false;
                }
                if list_2.len() > 1 {
                    return false;
                }

                if list_1.len() != list_2.len() {
                    return false;
                }
                // check the length first, then each value
                // because we have PartialEq for &Value and not Value we have autoderef as we have seen
                // already
                // https://doc.rust-lang.org/src/core/slice/cmp.rs.html#114-133
                list_1 == list_2
            }
            _ => false
        }
    }
}

mod regex {}

// toDo:  try to use Value::from() after parsing the lhs or the rhs operand
// consider doing both a short circuit evaluation and full evaluation
// toDo: check this for our `Regex` validation https://docs.rs/regex/latest/regex/
// toDo: also check section 4 for any Security Concerns
// toDo: consider writing a LinkedHashMap
// toDo: consider setting a limit on the path characters? also what happens if we recurse infinitely?
