use crate::{parsing, ParserError, Value};
use indexmap::IndexMap;

enum OpKind {
    Add,
    Remove,
    Replace,
    Move,
    Copy,
    Test,
}

enum Operation {
    Add {
        kind: OpKind,
        path: String,
        value: Value,
    },
    Remove {
        kind: OpKind,
        path: String,
    },
    Replace {
        kind: OpKind,
        path: String,
        value: Value,
    },
    Move {
        kind: OpKind,
        from: String,
        path: String,
    },
    Copy {
        kind: OpKind,
        from: String,
        path: String,
    },
    Test {
        kind: OpKind,
        path: String,
        value: Value,
    },
}


fn into_op(map: IndexMap<String, Value>) -> Operation {

}


fn parse(buffer: &[u8]) -> Result<(), ParserError>{
    if let Value::Array(ops) = parsing::parse(buffer)? {
        for (i, elem) in ops.into_iter().enumerate() {
            if let Value::Object(map) = elem {
                let op = into_op(map);
                op.apply();
            }
        }
    }
    Ok(())
}


fn to_kind(val: &str) -> Option<OpKind> {
    match val {
        "add" => Some(OpKind::Add),
        "remove" => Some(OpKind::Remove),
        "replace" => Some(OpKind::Replace),
        "move" => Some(OpKind::Move),
        "copy" => Some(OpKind::Copy),
        "test" => Some(OpKind::Test),
        _ => None,
    }
}

