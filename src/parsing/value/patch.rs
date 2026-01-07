use crate::parsing::value::error::{OpError, PatchError, PatchErrorKind, PointerError};
use crate::parsing::value::pointer;
use crate::parsing::value::pointer::{Pointer, RefToken};
use crate::{Value, parsing};
use indexmap::IndexMap;

enum OpKind {
    Add,
    Remove,
    Replace,
    Move,
    Copy,
    Test,
}

pub(super) enum Operation {
    Add { path: String, value: Value },
    Remove { path: String },
    Replace { path: String, value: Value },
    Move { from: String, path: String },
    Copy { from: String, path: String },
    Test { path: String, value: Value },
}

impl Operation {
    pub(super) fn apply(self, root: &mut Value) -> Result<(), PatchError> {
        match self {
            Operation::Add { path, value } => match descend(path, root)? {
                Location::Root => *root = value,
                Location::Child {
                    parent,
                    token,
                    depth,
                } => match parent {
                    Value::Object(map) => {
                        map.insert(token.val, value);
                    }
                    Value::Array(arr) => {
                        let index = pointer::check_array_index_strict(&token)?;
                        if index >= arr.len() {
                            return Err(PatchError {
                                kind: PatchErrorKind::IndexOutOfBounds,
                                pos: Some(token.pos),
                            });
                        }
                        arr.insert(index, value);
                    }
                    _ => {
                        return Err(PatchError {
                            kind: PatchErrorKind::PathNotFound { depth },
                            pos: None,
                        });
                    }
                },
            },
            Operation::Remove { path } => match descend(path, root)? {
                // toDo: add an error variant, can't remove, move, copy the root
                Location::Root => {}
                Location::Child {
                    parent,
                    token,
                    depth,
                } => match parent {
                    Value::Object(map) => {
                        map.shift_remove(&token.val).ok_or(PatchError {
                            kind: PatchErrorKind::PathNotFound { depth },
                            pos: None,
                        })?;
                    }
                    Value::Array(arr) => {
                        let index = pointer::check_array_index_strict(&token)?;
                        if index >= arr.len() {
                            return Err(PatchError {
                                kind: PatchErrorKind::IndexOutOfBounds,
                                pos: Some(token.pos),
                            });
                        }
                        arr.remove(index);
                    }
                    _ => {
                        return Err(PatchError {
                            kind: PatchErrorKind::PathNotFound { depth },
                            pos: None,
                        });
                    }
                },
            },
            Operation::Replace { path, value } => match descend(path, root)? {
                Location::Root => *root = value,
                Location::Child {
                    parent,
                    token,
                    depth,
                } => match parent {
                    Value::Object(map) => {
                        if map.contains_key(&token.val) {
                            map.insert(token.val, value);
                        } else {
                            return Err(PatchError {
                                kind: PatchErrorKind::PathNotFound { depth },
                                pos: None,
                            });
                        }
                    }
                    Value::Array(arr) => {
                        let index = pointer::check_array_index_strict(&token)?;
                        if index >= arr.len() {
                            return Err(PatchError {
                                kind: PatchErrorKind::IndexOutOfBounds,
                                pos: Some(token.pos),
                            });
                        }
                        arr[index] = value;
                    }
                    _ => {
                        return Err(PatchError {
                            kind: PatchErrorKind::PathNotFound { depth },
                            pos: None,
                        });
                    }
                },
            },
            // from: full path must exist (including the final value)
            // path: parent must exist, final key can be missing (like add)
            Operation::Move { from, path } => {
                // toDo: compiler complains that it is left uninitialized
                let mut val = Value::Null;
                match descend(from, root)? {
                    // toDo: add an error variant, can't remove, move, copy the root
                    Location::Root => {}
                    Location::Child {
                        parent,
                        token,
                        depth,
                    } => match parent {
                        Value::Object(map) => {
                            val =map.shift_remove(&token.val).ok_or(PatchError {
                                kind: PatchErrorKind::PathNotFound { depth },
                                pos: None,
                            })?;
                        }
                        Value::Array(arr) => {
                            let index = pointer::check_array_index_strict(&token)?;
                            if index >= arr.len() {
                                return Err(PatchError {
                                    kind: PatchErrorKind::IndexOutOfBounds,
                                    pos: Some(token.pos),
                                });
                            }
                            val = arr.remove(index);
                        }
                        _ => {
                            return Err(PatchError {
                                kind: PatchErrorKind::PathNotFound { depth },
                                pos: None,
                            });
                        }
                    },
                }
                match descend(path, root)? {
                    Location::Root => *root = val,
                    Location::Child {
                        parent,
                        token,
                        depth,
                    } => match parent {
                        Value::Object(map) => {
                            map.insert(token.val, val);
                        }
                        Value::Array(arr) => {
                            let index = pointer::check_array_index_strict(&token)?;
                            if index >= arr.len() {
                                return Err(PatchError {
                                    kind: PatchErrorKind::IndexOutOfBounds,
                                    pos: Some(token.pos),
                                });
                            }
                            arr.insert(index, val);
                        }
                        _ => {
                            return Err(PatchError {
                                kind: PatchErrorKind::PathNotFound { depth },
                                pos: None,
                            });
                        }
                    },
                }
            },
            Operation::Copy { from, path } => {
                // toDo: compiler complains that it is left uninitialized
                let mut val = Value::Null;
                match descend(from, root)? {
                    // toDo: add an error variant, can't remove, move, copy the root
                    Location::Root => {}
                    Location::Child {
                        parent,
                        token,
                        depth,
                    } => match parent {
                        Value::Object(map) => {
                            val = map.get(&token.val).cloned().ok_or(PatchError {
                                kind: PatchErrorKind::PathNotFound { depth },
                                pos: None,
                            })?;
                        }
                        Value::Array(arr) => {
                            let index = pointer::check_array_index_strict(&token)?;
                            if index >= arr.len() {
                                return Err(PatchError {
                                    kind: PatchErrorKind::IndexOutOfBounds,
                                    pos: Some(token.pos),
                                });
                            }
                            val = arr[index].clone();
                        }
                        _ => {
                            return Err(PatchError {
                                kind: PatchErrorKind::PathNotFound { depth },
                                pos: None,
                            });
                        }
                    },
                }
                match descend(path, root)? {
                    Location::Root => *root = val,
                    Location::Child {
                        parent,
                        token,
                        depth,
                    } => match parent {
                        Value::Object(map) => {
                            map.insert(token.val, val);
                        }
                        Value::Array(arr) => {
                            let index = pointer::check_array_index_strict(&token)?;
                            if index >= arr.len() {
                                return Err(PatchError {
                                    kind: PatchErrorKind::IndexOutOfBounds,
                                    pos: Some(token.pos),
                                });
                            }
                            arr.insert(index, val);
                        }
                        _ => {
                            return Err(PatchError {
                                kind: PatchErrorKind::PathNotFound { depth },
                                pos: None,
                            });
                        }
                    },
                }
            },
            Operation::Test { path, value } => {
                // toDo: check if we can drop this initialization when we address the root
                let val;
                match descend(path, root)? {
                    // toDo: add an error variant, can't remove, move, copy the root
                    Location::Root => val = root,
                    Location::Child {
                        parent,
                        token,
                        depth,
                    } => match parent {
                        Value::Object(map) => {
                            val = map.get_mut(&token.val).ok_or(PatchError {
                                kind: PatchErrorKind::PathNotFound { depth },
                                pos: None,
                            })?;
                        }
                        Value::Array(arr) => {
                            let index = pointer::check_array_index_strict(&token)?;
                            if index >= arr.len() {
                                return Err(PatchError {
                                    kind: PatchErrorKind::IndexOutOfBounds,
                                    pos: Some(token.pos),
                                });
                            }
                            val = &mut arr[index];
                        }
                        _ => {
                            return Err(PatchError {
                                kind: PatchErrorKind::PathNotFound { depth },
                                pos: None,
                            });
                        }
                    },
                }
                if *val != value {
                    return Err(PatchError { kind: PatchErrorKind::OpError(OpError::NotEqual), pos: None })
                }
            }
        }
        Ok(())
    }
}

enum Location<'a> {
    Root,
    Child {
        parent: &'a mut Value,
        token: RefToken,
        depth: usize,
    },
}

fn descend(path: String, root: &mut Value) -> Result<Location<'_>, PatchError> {
    if path.is_empty() {
        return Ok(Location::Root);
    }

    let mut tokens = Vec::new();
    let mut ptr = Pointer::new(path.as_bytes());
    while let Some(token) = ptr.next().map_err(|err| PointerError::from(err))? {
        tokens.push(token);
    }

    let last = tokens.pop().unwrap();
    let mut current = root;
    let mut depth = 0;
    for token in tokens {
        match current {
            Value::Object(map) => {
                current = map.get_mut(&token.val).ok_or(PatchError {
                    kind: PatchErrorKind::PathNotFound { depth },
                    pos: None,
                })?;
            }
            Value::Array(arr) => {
                let index = pointer::check_array_index_strict(&token)?;
                current = arr.get_mut(index).ok_or(PatchError {
                    kind: PatchErrorKind::IndexOutOfBounds,
                    pos: Some(token.pos),
                })?;
            }
            _ => {
                return Err(PatchError {
                    kind: PatchErrorKind::PathNotFound { depth },
                    pos: None,
                });
            }
        }
        depth += 1;
    }

    Ok(Location::Child {
        parent: current,
        token: last,
        depth,
    })
}

// swap_remove() is fine here because we consume the map so disturbing the order doesn't matter
fn into_op(mut map: IndexMap<String, Value>) -> Result<Operation, OpError> {
    let op = map.swap_remove("op").ok_or(OpError::MissingMember("op"))?;
    let Value::String(op) = op else {
        return Err(OpError::UnexpectedValue { expected: "string" });
    };
    let kind = to_kind(&op).ok_or(OpError::UnexpectedValue {
        expected: "valid op",
    })?;

    let path = map
        .swap_remove("path")
        .ok_or(OpError::MissingMember("path"))?;
    let Value::String(path) = path else {
        return Err(OpError::UnexpectedValue { expected: "string" });
    };

    match kind {
        OpKind::Add => {
            let value = require_value(&mut map)?;
            Ok(Operation::Add { path, value })
        }
        OpKind::Remove => Ok(Operation::Remove { path }),
        OpKind::Replace => {
            let value = require_value(&mut map)?;
            Ok(Operation::Replace { path, value })
        }
        OpKind::Move => {
            let from = require_from(&mut map)?;
            Ok(Operation::Move { path, from })
        }
        OpKind::Copy => {
            let from = require_from(&mut map)?;
            Ok(Operation::Copy { path, from })
        }
        OpKind::Test => {
            let value = require_value(&mut map)?;
            Ok(Operation::Test { path, value })
        }
    }
}

fn require_value(map: &mut IndexMap<String, Value>) -> Result<Value, OpError> {
    map.swap_remove("value")
        .ok_or(OpError::MissingMember("value"))
}

fn require_from(map: &mut IndexMap<String, Value>) -> Result<String, OpError> {
    let from = map
        .swap_remove("from")
        .ok_or(OpError::MissingMember("from"))?;
    let Value::String(from) = from else {
        return Err(OpError::UnexpectedValue { expected: "string" });
    };
    Ok(from)
}

pub(super) fn parse(buffer: &[u8]) -> Result<Vec<Operation>, PatchError> {
    let Value::Array(arr) = parsing::parse(buffer)? else {
        return Err(PatchError {
            kind: PatchErrorKind::UnexpectedValue { expected: "Array" },
            pos: 0,
        });
    };

    let mut ops = Vec::new();
    for (i, elem) in arr.into_iter().enumerate() {
        let Value::Object(map) = elem else {
            return Err(PatchError {
                kind: PatchErrorKind::UnexpectedValue { expected: "Object" },
                pos: i,
            });
        };
        let op = into_op(map).map_err(|err| PatchError {
            kind: PatchErrorKind::OpError(err),
            pos: i,
        })?;
        ops.push(op);
    }

    Ok(ops)
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
