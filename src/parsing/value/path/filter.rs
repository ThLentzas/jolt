use crate::parsing::value::path::{Segment, SegmentKind};
use crate::parsing::value::path::filter::function::{FnExpr, FnResult};
use crate::parsing::value::Value;
use std::borrow::Cow;

pub(crate) mod function;

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
pub(crate) enum LogicalExpr {
    // toDo: explain how with the order of read methods in logical expression we handle precedence
    Comparison(ComparisonExpr),
    Test(TestExpr),
    And(Box<LogicalExpr>, Box<LogicalExpr>),
    Or(Box<LogicalExpr>, Box<LogicalExpr>),
    Not(Box<LogicalExpr>),
}

#[derive(Debug, PartialEq)]
pub(crate) struct ComparisonExpr {
    pub(super) lhs: Comparable,
    pub(super) op: ComparisonOp,
    pub(super) rhs: Comparable,
}

#[derive(Debug, PartialEq)]
pub(crate) enum TestExpr {
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
    Literal(Value),
    EmbeddedQuery(EmbeddedQuery),
    FnExpr(FnExpr),
}

#[derive(Debug, PartialEq)]
pub(super) enum EmbeddedQueryType {
    Absolute,
    Relative,
}

// toDo: cache absolute path queries
#[derive(Debug, PartialEq)]
pub(crate) struct EmbeddedQuery {
    pub(super) query_type: EmbeddedQueryType,
    pub(super) segments: Vec<Segment>,
}

// operand is what we get after evaluating a Comparable
enum Operand<'a> {
    Value(Cow<'a, Value>),
    Empty,
    Invalid
}

impl LogicalExpr {
    // toDo: if embedded absolute path query we need to find a way to cache it
    // short circuiting and well-typedness of functions
    // because the type checking happens during parsing, we will never encounter a case where we
    // might have an invalid function(Arity/Type mismatch) that we will miss because the lhs evaluated
    // to true/false and we returned without evaluating the rhs
    pub(super) fn evaluate(&self, root: &Value, current: &Value) -> bool {
        match self {
            LogicalExpr::Comparison(expr) => expr.evaluate(root, current),
            LogicalExpr::Test(expr) => expr.evaluate(root, current),
            LogicalExpr::And(lhs, rhs) => {
                // short circuit.
                // the second operand is only evaluated if the result of the expression cannot be
                // determined from the first operand alone
                //
                // if lhs returns false rust will return immediately false without evaluating rhs
                //
                // auto deref by rust to access the expr(deref coercion)
                // lhs.evaluate() is (*lhs).evaluate(root, current) internally
                lhs.evaluate(root, current) && rhs.evaluate(root, current)
            },
            LogicalExpr::Or(lhs, rhs) => {
                lhs.evaluate(root, current) || rhs.evaluate(root, current)
            },
            LogicalExpr::Not(expr) => !expr.evaluate(root, current),
        }
    }
}

impl ComparisonExpr {
    fn evaluate (& self, root: &Value, current: &Value) -> bool {
        let lhs = self.lhs.to_operand(root, current);
        let rhs = self.rhs.to_operand(root, current);

        match(lhs, rhs) {
            // both empty('absent'/'nothing' as defined in the rfc) and the operators that include
            // equals(<=, >=) return true
            (Operand::Empty, Operand::Empty) => matches!(self.op,
                ComparisonOp::Equal
                | ComparisonOp::LessThanOrEqual
                | ComparisonOp::GreaterThanOrEqual
            ),
            (Operand::Empty, _) | (_, Operand::Empty) => matches!(self.op, ComparisonOp::NotEqual),
            // non-singular queries
            (Operand::Invalid, _) | (_, Operand::Invalid) => false,
            (Operand::Value(lhs), Operand::Value(rhs)) => self.op.apply(&lhs, &rhs)
        }
    }
}

impl TestExpr {
    fn evaluate (&self, root: &Value, current: &Value) -> bool {
        match self {
            TestExpr::EmbeddedQuery(q) => {
                let nodelist = q.evaluate(root, current);
                // yields true if the query selects at least one node and yields false if the query
                // does not select any nodes
                nodelist.len() != 0
            }
            TestExpr::FnExpr(expr) => {
                match expr.evaluate(root, current) {
                    FnResult::Value(_) => false,
                    FnResult::Logical(b) => b,
                    FnResult::Nothing => false,
                }
            }
        }
    }
}

// toDo: ask about the structure of having value.read() how to handle n_paths and passing
// the root to query
impl EmbeddedQuery {
    // we want the refs to live as long as the root(they 'live in root')
    //
    // as we iterate the values of map/array if the query type is relative the current value will
    // be the root of the query. For absolute paths we have a reference to the root, we will evaluate
    // once and cache it after.
    fn evaluate<'v>(&self, root: &'v Value, current: &'v Value) -> Vec<&'v Value> {
        // the values need to live as long as the root value
        let mut reader: Vec<&'v Value> = Vec::new();
        let mut writer: Vec<&'v Value> = Vec::new();

        match self.query_type {
            EmbeddedQueryType::Relative => reader.push(current),
            EmbeddedQueryType::Absolute => reader.push(root),
        }

        for seg in self.segments.iter() {
            writer.clear();
            match seg.kind {
                SegmentKind::Child => super::apply_child_seg(root, &reader, &mut writer, seg),
                SegmentKind::Descendant => super::apply_descendant_seg(root, &reader, &mut writer, seg),
            }
            std::mem::swap(&mut reader, &mut writer);
        }
        // the final nodelist is in reader after the last swap
        reader
    }

    fn is_definite(&self) -> bool {
        self.segments.iter().all(|seg| seg.is_singular())
    }
}

impl Comparable {
    // tried to make it a closure, couldn't get the lifetimes right
    fn to_operand<'a>(&'a self, root: &'a Value, current: &'a Value) -> Operand<'a> {
        match self {
            Comparable::EmbeddedQuery(q) => {
                // q.evaluate returns Vec<&'root Value>
                let nodelist = q.evaluate(root, current);
                match nodelist.len() {
                    0 => Operand::Empty,
                    1 => Operand::Value(Cow::Borrowed(nodelist[0])),
                    _ => Operand::Invalid,
                }
            }
            Comparable::Literal(t) => Operand::Value(Cow::Borrowed(t)),
            Comparable::FnExpr(expr) => {
               match expr.evaluate(root, current) {
                   FnResult::Value(v) => Operand::Value(v),
                   // a fn that returns logical true/false can't be used in comparison expr
                   FnResult::Logical(_) => Operand::Invalid,
                   FnResult::Nothing => Operand::Empty,
               }
            }
        }
    }
}

impl ComparisonOp {
    // fn apply<T>(&self, lhs: &T, rhs: &T) -> bool
    // where
    //     T: PartialEq + PartialOrd,
    // {
    //     match self {
    //         ComparisonOp::Equal => lhs == rhs,
    //         ComparisonOp::NotEqual => lhs != rhs,
    //         ComparisonOp::LessThan => lhs < rhs,
    //         ComparisonOp::LessThanOrEqual => lhs <= rhs,
    //         ComparisonOp::GreaterThan => lhs > rhs,
    //         ComparisonOp::GreaterThanOrEqual => lhs >= rhs,
    //     }
    // }
    //
    // this is what I had when I would do the conversion from literal to Value, and I would call
    // it either with Literal as T, no need to convert when both lhs and rhs are Literal; otherwise
    // convert to literal to Value and now T is Value
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

    pub(super) fn len(&self) -> usize {
        match self {
            ComparisonOp::GreaterThan
            | ComparisonOp::LessThan => 1,
            ComparisonOp::Equal
            | ComparisonOp::NotEqual
            | ComparisonOp::LessThanOrEqual
            | ComparisonOp::GreaterThanOrEqual => 2,
        }
    }
}

// toDo: check this for our `Regex` validation https://docs.rs/regex/latest/regex/
// toDo: check section 4 for any Security Concerns
// toDo: consider writing a LinkedHashMap
// toDo: consider setting a limit on the path characters? also what happens if we recurse infinitely?
// toDo: write about precedence in read_logical
// toDo: explain the lifetimes in each case
// toDo: "'a is okay if you only have one named lifetime, but yeah if there's more than that, give them names"
// toDo: write some complex queries including nested selectors and descendant segments(include functions that indirectly will test resolve_args() because it is really hard to set up)
// toDo: PathBuilder
// toDo: jsonPathPlus
