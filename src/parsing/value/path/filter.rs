use crate::parsing::number::Number;
use crate::parsing::value::path::Segment;

// toDo:  explain why we need Box for recursive data types
// https://docs.rs/recursion/latest/recursion/
//
// struct A {
//     inner : A
// }
// we get an error: - recursive without indirection
#[derive(Debug, PartialEq)]
pub(super) enum LogicalExpression {
    Comparison(ComparisonExpr),
    Test(TestExpr),
    And(Box<LogicalExpression>, Box<LogicalExpression>),
    Or(Box<LogicalExpression>, Box<LogicalExpression>),
    Not(Box<LogicalExpression>)
}

#[derive(Debug, PartialEq)]
pub(super) struct ComparisonExpr {
    pub(super) lhs: Comparable,
    pub(super) op: ComparisonOp,
    pub(super) rhs: Comparable
}

#[derive(Debug, PartialEq)]
pub(super) struct TestExpr {
    pub(super) query: EmbeddedQuery,
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

#[derive(Debug, PartialEq)]
pub(super) enum Comparable {
    Literal(Type),
    Query(EmbeddedQuery),
    FunctionExpr
}

#[derive(Debug, PartialEq)]
pub(super) enum Type {
    String(String),
    Number(Number),
    Boolean(bool),
    Null
}

#[derive(Debug, PartialEq)]
pub(super) enum QueryType {
    Absolute,
    Relative,
}

#[derive(Debug, PartialEq)]
pub(super) struct EmbeddedQuery {
    pub(super) query_type: QueryType,
    pub(super) segments: Vec<Segment>
}

// toDo:  try to use Value::from() after parsing the lhs or the rhs operand