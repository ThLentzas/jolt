use std::cmp::Ordering;
use crate::parsing::number::Number;
use crate::parsing::value::Value;
use crate::parsing::value::path::{Segment, SegmentKind};

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
pub(super) enum LogicalExpr {
    // toDo: explain how with the order of read methods in logical expression we handle precedence
    Comparison(ComparisonExpr),
    Test(TestExpr),
    And(Box<LogicalExpr>, Box<LogicalExpr>),
    Or(Box<LogicalExpr>, Box<LogicalExpr>),
    Not(Box<LogicalExpr>),
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
pub(crate) enum Type {
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
    LogicalExpr(LogicalExpr)
}

#[derive(Debug, PartialEq)]
pub(super) struct EmbeddedQuery {
    pub(super) query_type: EmbeddedQueryType,
    pub(super) segments: Vec<Segment>,
}

enum Operand<'a> {
    Value(&'a Value),
    Literal(&'a Type),
    Empty,
    Invalid
}

enum FnResultType {
    Nothing
}

impl LogicalExpr {
    pub(super) fn evaluate(&self, root: &Value, current: &Value) -> bool {
        match self {
            LogicalExpr::Comparison(expr) => expr.evaluate(root, current),
            LogicalExpr::Test(expr) => expr.evaluate(),
            LogicalExpr::And(lhs, rhs) => {
                // short circuit
                // auto deref by rust to access the expr(deref coercion)
                if !lhs.evaluate(root, current) {
                    return false;
                }
                lhs.evaluate(root, current) && rhs.evaluate(root, current)
            },
            LogicalExpr::Or(lhs, rhs) => {
                // short circuit
                if lhs.evaluate(root, current) {
                    return true;
                }
                lhs.evaluate(root, current) || rhs.evaluate(root, current)
            },
            LogicalExpr::Not(expr) => !expr.evaluate(root, current),
        }
    }
}

impl ComparisonExpr {
    fn evaluate<'a> (&'a self, root: &Value, current: &Value) -> bool {
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
            (Operand::Literal(t1), Operand::Literal(t2)) => {
                self.op.apply(t1.partial_cmp(t2))
            },
            (Operand::Value(v1), Operand::Value(v2)) => {
                self.op.apply(v1.partial_cmp(v2))
            },
            (Operand::Literal(t), Operand::Value(v)) => {
                self.op.apply(t.cmp_with(v))
            },
            (Operand::Value(v), Operand::Literal(t)) => {
                self.op.apply(v.cmp_with(t))
            },
        }
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
                SegmentKind::Child => super::apply_child_seg(&reader, &mut writer, seg),
                SegmentKind::Descendant => super::apply_descendant_seg(&reader, &mut writer, seg),
            }
            std::mem::swap(&mut reader, &mut writer);
        }
        // the final nodelist is in reader after the last swap
        reader
    }
}

impl Comparable {
    // tried to make it closure, couldn't get the lifetimes right
    fn to_operand<'a>(&'a self, root: &'a Value, current: &'a Value) -> Operand<'a> {
        match self {
            Comparable::EmbeddedQuery(q) => {
                // q.evaluate returns Vec<&'root Value>
                let nodelist = q.evaluate(root, current);
                match nodelist.len() {
                    0 => Operand::Empty,
                    1 => Operand::Value(nodelist[0]), // borrow from root
                    _ => Operand::Invalid,
                }
            }
            Comparable::Literal(t) => Operand::Literal(t), // borrow from comparable
            Comparable::FnExpr(_expr) => todo!(),
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
    fn apply(&self, ord: Option<Ordering>) -> bool {
        match ord {
            Some(Ordering::Equal) => matches!(self,
                ComparisonOp::Equal
                | ComparisonOp::LessThanOrEqual
                | ComparisonOp::GreaterThanOrEqual
            ),
            Some(Ordering::Less) => matches!(self,
                ComparisonOp::NotEqual
                | ComparisonOp::LessThan
                | ComparisonOp::LessThanOrEqual
            ),
            Some(Ordering::Greater) => matches!(self,
                ComparisonOp::NotEqual
                | ComparisonOp::GreaterThan
                | ComparisonOp::GreaterThanOrEqual
            ),
            None => matches!(self, ComparisonOp::NotEqual), // If incomparable, only != is True
        }
    }

    pub(super) fn len(&self) -> usize {
        match self {
            ComparisonOp::Equal
            | ComparisonOp::GreaterThan
            | ComparisonOp::LessThan => 1,
            ComparisonOp::NotEqual 
            | ComparisonOp::LessThanOrEqual 
            | ComparisonOp::GreaterThanOrEqual => 2
        }
    }
}

impl Type {
    fn cmp_with(&self, other: &Value) -> Option<Ordering> {
        match (self, other) {
            (Type::String(s2), Value::String(s1)) => s1.partial_cmp(s2),
            (Type::Number(n2), Value::Number(n1), ) => n1.partial_cmp(n2),
            // we can't call b1.partial_cmp(b2) because in rust false < true results to true but in
            // the spec it should result to false
            (Type::Boolean(b1), Value::Boolean(b2)) => {
                if b1 == b2 {
                    Some(Ordering::Equal)
                } else {
                    None
                }
            },
            (Type::Null, Value::Null) => Some(Ordering::Equal),
            // mismatch
            _ => None
        }
    }
}

impl PartialOrd for Type {

    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match(self, other) {
            (Type::Number(n1), Type::Number(n2)) => n1.partial_cmp(n2),
            (Type::String(s1), Type::String(s2)) => s1.partial_cmp(s2),
            // we can't call b1.partial_cmp(b2) because in rust false < true results to true but in
            // the spec it should result to false
            (Type::Boolean(b1), Type::Boolean(b2)) => {
                if b1 == b2 {
                    Some(Ordering::Equal)
                } else {
                    None
                }
            },
            (Type::Null, Type::Null) => Some(Ordering::Equal),
            _ => None,
        }
    }
}

mod regex {}

// toDo: check this for our `Regex` validation https://docs.rs/regex/latest/regex/
// toDo: also check section 4 for any Security Concerns
// toDo: consider writing a LinkedHashMap
// toDo: consider setting a limit on the path characters? also what happens if we recurse infinitely?
// toDo: explain why we moved away from converting 1 side to Value
// toDo: write about precedence in read_logical
// toDo: why we need v to t and t to v
// toDo: explain the lifetimes in each case
