use crate::parsing::value::error::{OpError, PatchError, PointerError};
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
    pub(super) fn apply(self, root: &mut Value) -> Result<(), OpError> {
        match self {
            Operation::Add { path, value } => match descend(path, root)? {
                Location::Root => *root = value,
                Location::Child {
                    parent,
                    path,
                    token,
                    depth,
                } => {
                    insert_at(parent, token, value, path, depth)?;
                }
            },

            Operation::Remove { path } => match descend(path, root)? {
                Location::Root => return Err(OpError::InvalidRootOp { op: "remove" }),
                Location::Child {
                    parent,
                    path,
                    token,
                    depth,
                } => {
                    remove_at(parent, token, path, depth)?;
                }
            },

            Operation::Replace { path, value } => match descend(path, root)? {
                Location::Root => *root = value,
                Location::Child {
                    parent,
                    path,
                    token,
                    depth,
                } => {
                    replace_at(parent, &token, value, path, depth)?;
                }
            },

            Operation::Move { from, path } => {
                let val = match descend(from, root)? {
                    Location::Root => return Err(OpError::InvalidRootOp { op: "move" }),
                    Location::Child {
                        parent,
                        path,
                        token,
                        depth,
                    } => remove_at(parent, token, path, depth)?,
                };
                match descend(path, root)? {
                    Location::Root => *root = val,
                    Location::Child {
                        parent,
                        path,
                        token,
                        depth,
                    } => {
                        insert_at(parent, token, val, path, depth)?;
                    }
                }
            }

            Operation::Copy { from, path } => {
                let val = match descend(from, root)? {
                    Location::Root => root.clone(),
                    Location::Child {
                        parent,
                        path,
                        token,
                        depth,
                    } => get_at(parent, token, path, depth)?.clone(),
                };
                match descend(path, root)? {
                    Location::Root => *root = val,
                    Location::Child {
                        parent,
                        path,
                        token,
                        depth,
                    } => {
                        insert_at(parent, token, val, path, depth)?;
                    }
                }
            }

            Operation::Test { path, value } => {
                let val = match descend(path, root)? {
                    Location::Root => root,
                    Location::Child {
                        parent,
                        path,
                        token,
                        depth,
                    } => get_at(parent, token, path, depth)?,
                };
                if *val != value {
                    return Err(OpError::NotEqual);
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
        path: String,
        token: RefToken,
        depth: usize,
    },
}

fn descend(path: String, root: &mut Value) -> Result<Location<'_>, OpError> {
    if path.is_empty() {
        return Ok(Location::Root);
    }

    let mut tokens = Vec::new();
    let mut ptr = Pointer::new(path.as_bytes());
    while let Some(token) = ptr
        .next()
        .map_err(|err| OpError::PointerError(PointerError::from(err)))?
    {
        tokens.push(token);
    }

    let last = tokens.pop().unwrap();
    let mut current = root;
    let mut depth = 0;
    for token in tokens {
        match current {
            Value::Object(map) => {
                // current = map.get_mut(&token.val).ok_or(OpError::PathNotFound { path, depth })?;
                // won't work, the compiler sees that path is moved inside the loop, and it will
                // complain despite using '?'
                let Some(next) = map.get_mut(&token.val) else {
                    return Err(OpError::PathNotFound { path, depth });
                };
                current = next;
            }
            Value::Array(arr) => {
                let index = pointer::check_array_index_strict(&token)?;
                let len = arr.len();
                // we can't call arr.len() inside the else {}. We borrow it as mutable when we do
                // arr.get_mut() and the borrow is still active in the else {}, we can't borrow it
                // again as immutable to call len()
                let Some(next) = arr.get_mut(index) else {
                    return Err(OpError::IndexOutOfBounds {
                        path,
                        depth,
                        index,
                        len,
                    });
                };
                current = next;
            }
            _ => return Err(OpError::PathNotFound { path, depth }),
        }
        depth += 1;
    }

    Ok(Location::Child {
        parent: current,
        path,
        token: last,
        depth,
    })
}

// swap_remove() is fine here because we consume the map so disturbing the order doesn't matter
fn into_op(mut map: IndexMap<String, Value>) -> Result<Operation, OpError> {
    let op = map
        .swap_remove("op")
        .ok_or(OpError::MissingMember { member: "op" })?;
    let Value::String(op) = op else {
        return Err(OpError::UnexpectedValue { expected: "string" });
    };
    let kind = to_kind(&op).ok_or(OpError::UnexpectedValue {
        expected: "valid op",
    })?;

    let path = map
        .swap_remove("path")
        .ok_or(OpError::MissingMember { member: "path" })?;
    let Value::String(path) = path else {
        return Err(OpError::UnexpectedValue { expected: "string" });
    };

    match kind {
        OpKind::Add => {
            let value = value(&mut map)?;
            Ok(Operation::Add { path, value })
        }
        OpKind::Remove => Ok(Operation::Remove { path }),
        OpKind::Replace => {
            let value = value(&mut map)?;
            Ok(Operation::Replace { path, value })
        }
        OpKind::Move => {
            let from = from(&mut map)?;
            Ok(Operation::Move { path, from })
        }
        OpKind::Copy => {
            let from = from(&mut map)?;
            Ok(Operation::Copy { path, from })
        }
        OpKind::Test => {
            let value = value(&mut map)?;
            Ok(Operation::Test { path, value })
        }
    }
}

fn value(map: &mut IndexMap<String, Value>) -> Result<Value, OpError> {
    map.swap_remove("value")
        .ok_or(OpError::MissingMember { member: "value" })
}

fn from(map: &mut IndexMap<String, Value>) -> Result<String, OpError> {
    let from = map
        .swap_remove("from")
        .ok_or(OpError::MissingMember { member: "from" })?;
    let Value::String(from) = from else {
        return Err(OpError::UnexpectedValue { expected: "string" });
    };
    Ok(from)
}

pub(super) fn parse(buffer: &[u8]) -> Result<Vec<Operation>, PatchError> {
    let Value::Array(arr) = parsing::parse(buffer)? else {
        return Err(PatchError::UnexpectedValue { expected: "Array" });
    };

    let mut ops = Vec::new();
    for (i, elem) in arr.into_iter().enumerate() {
        let Value::Object(map) = elem else {
            return Err(PatchError::UnexpectedValue {
                expected: "Object ",
            });
        };
        let op = into_op(map).map_err(|err| PatchError::OpError(err, i))?;
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

fn remove_at(
    parent: &mut Value,
    token: RefToken,
    path: String,
    depth: usize,
) -> Result<Value, OpError> {
    match parent {
        Value::Object(map) => map.shift_remove(&token.val).ok_or(OpError::PathNotFound {
            path,
            depth: depth + 1,
        }),
        Value::Array(arr) => {
            let index = pointer::check_array_index_strict(&token)?;
            let len = arr.len();
            if index >= len {
                return Err(OpError::IndexOutOfBounds {
                    path,
                    depth: depth + 1,
                    index,
                    len,
                });
            }
            Ok(arr.remove(index))
        }
        _ => Err(OpError::PathNotFound {
            path,
            depth: depth + 1,
        }),
    }
}

fn get_at(parent: &Value, token: RefToken, path: String, depth: usize) -> Result<&Value, OpError> {
    match parent {
        Value::Object(map) => map.get(&token.val).ok_or(OpError::PathNotFound {
            path,
            depth: depth + 1,
        }),
        Value::Array(arr) => {
            let index = pointer::check_array_index_strict(&token)?;
            let len = arr.len();
            if index >= len {
                return Err(OpError::IndexOutOfBounds {
                    path,
                    depth: depth + 1,
                    index,
                    len,
                });
            }
            Ok(&arr[index])
        }
        _ => Err(OpError::PathNotFound {
            path,
            depth: depth + 1,
        }),
    }
}

fn insert_at(
    parent: &mut Value,
    token: RefToken,
    value: Value,
    path: String,
    depth: usize,
) -> Result<(), OpError> {
    match parent {
        Value::Object(map) => {
            map.insert(token.val, value);
        }
        Value::Array(arr) => {
            let index = pointer::check_array_index_strict(&token)?;
            let len = arr.len();
            // if index == len we insert at the end
            if index > len {
                return Err(OpError::IndexOutOfBounds {
                    path,
                    depth: depth + 1,
                    index,
                    len,
                });
            }
            arr.insert(index, value);
        }
        _ => {
            return Err(OpError::PathNotFound {
                path,
                depth: depth + 1,
            });
        }
    }
    Ok(())
}

fn replace_at(
    parent: &mut Value,
    token: &RefToken,
    value: Value,
    path: String,
    depth: usize,
) -> Result<(), OpError> {
    match parent {
        Value::Object(map) => {
            if map.contains_key(&token.val) {
                map.insert(token.val.clone(), value);
                Ok(())
            } else {
                Err(OpError::PathNotFound {
                    path,
                    depth: depth + 1,
                })
            }
        }
        Value::Array(arr) => {
            let index = pointer::check_array_index_strict(token)?;
            let len = arr.len();
            if index >= len {
                return Err(OpError::IndexOutOfBounds {
                    path,
                    depth: depth + 1,
                    index,
                    len,
                });
            }
            arr[index] = value;
            Ok(())
        }
        _ => Err(OpError::PathNotFound {
            path,
            depth: depth + 1,
        }),
    }
}
