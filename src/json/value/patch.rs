use crate::json::value::error::{OpError, PatchError, PatchErrorKind, PointerError};
use crate::json::value::pointer;
use crate::json::value::pointer::{Pointer, RefToken};
use crate::{Value, json};
use indexmap::IndexMap;

enum OpKind {
    Add,
    Remove,
    Replace,
    Move,
    Copy,
    Test,
}

#[derive(Debug, PartialEq, Eq)]
pub(super) enum Operation {
    Add { path: String, value: Value },
    Remove { path: String },
    Replace { path: String, value: Value },
    Move { from: String, path: String },
    Copy { from: String, path: String },
    Test { path: String, value: Value },
}

impl Operation {
    // target location is the term used to refer to the result of path/from
    pub(super) fn apply(self, root: &mut Value) -> Result<(), OpError> {
        match self {
            Operation::Add { path, value } => match descend(path, root)? {
                Location::Root => *root = value,
                // add the key/value pair if key does not exist
                // update the value if key does
                // append the value if index == len, otherwise shift the elements and insert
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
                // can't call remove on root
                Location::Root => return Err(OpError::InvalidRootOp { op: "remove" }),
                Location::Child {
                    parent,
                    path,
                    token,
                    depth,
                } => {
                    // target location must exist for the value to be removed
                    remove_at(parent, token, path, depth)?;
                }
            },
            // the difference between add and replace is that for replace the key MUST exist in order
            // to update the value, add will insert the key/value if key is absent
            Operation::Replace { path, value } => match descend(path, root)? {
                Location::Root => *root = value,
                Location::Child {
                    parent,
                    path,
                    token,
                    depth,
                } => {
                    replace_at(parent, token, value, path, depth)?;
                }
            },
            // combination of remove and add
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
            // copy is nothing but add where copy's path is add's path and copy's from is add's value
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

pub(super) fn parse(buffer: &[u8]) -> Result<Vec<Operation>, PatchError> {
    let val = json::parse(buffer).map_err(|err| PatchError {
        kind: PatchErrorKind::ParseError(err),
        index: None,
    })?;

    let Value::Array(arr) = val else {
        return Err(PatchError {
            kind: PatchErrorKind::UnexpectedValue { expected: "Array" },
            index: None, // No index because this is the root
        });
    };

    let mut ops = Vec::new();
    for (i, elem) in arr.into_iter().enumerate() {
        let Value::Object(map) = elem else {
            return Err(PatchError {
                kind: PatchErrorKind::UnexpectedValue { expected: "Object" },
                index: Some(i),
            });
        };
        let op = into_op(map).map_err(|err| PatchError {
            kind: PatchErrorKind::InvalidOp(err),
            index: Some(i),
        })?;
        ops.push(op);
    }
    Ok(ops)
}

// the target location as referred to by the rfc
#[derive(Debug, PartialEq, Eq)]
enum Location<'a> {
    Root,
    // parent is the Value that we should apply the op
    // path is returned because descend() took ownership(for error handling) but no error occurred
    // helps to avoid allocation
    // token is the last token of the path, the one that we should apply to parent
    // depth is used for error handling
    Child {
        parent: &'a mut Value,
        path: String,
        token: RefToken,
        depth: usize,
    },
}

// walks down the path until the last ref token where it returns to control to apply(). The last
// ref token is where the doc changes so apply() will handle it based on the type of operation.
// Walking the json is done based on the pointer rules. It is the same logic as json.pointer() but
// if the path diverges we return an Error, not an Option::None
// The function returns a Location, the target location where the operations should be applied.
fn descend(path: String, root: &mut Value) -> Result<Location<'_>, OpError> {
    if path.is_empty() {
        return Ok(Location::Root);
    }

    let mut tokens = Vec::new();
    let mut ptr = Pointer::new(path.as_bytes());
    ptr.expect_root()?;
    while let Some(token) = ptr
        .next()
        .map_err(|err| OpError::Pointer(PointerError::from(err)))?
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
            // Case: "-"
            // According to the Pointer spec, if '-' is used on an array it refers to a nonexisting
            // element, the one after the last array element. When we call descend() if we encounter
            // '-' as a token value in an array we return an error because the "target location"
            // does not exist, but if the target location is an array we treat '-' as append.
            if token.val == "-" {
                arr.push(value);
                return Ok(());
            }
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
    token: RefToken,
    value: Value,
    path: String,
    depth: usize,
) -> Result<(), OpError> {
    match parent {
        Value::Object(map) => {
            if map.contains_key(&token.val) {
                map.insert(token.val, value);
                Ok(())
            } else {
                Err(OpError::PathNotFound {
                    path,
                    depth: depth + 1,
                })
            }
        }
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
            arr[index] = value;
            Ok(())
        }
        _ => Err(OpError::PathNotFound {
            path,
            depth: depth + 1,
        }),
    }
}

// swap_remove() is fine here because we consume the map so disturbing the order doesn't matter
fn into_op(mut map: IndexMap<String, Value>) -> Result<Operation, OpError> {
    let op = map
        .swap_remove("op")
        .ok_or(OpError::MissingMember { member: "op" })?;
    let Value::String(op) = op else {
        return Err(OpError::UnexpectedField {
            field: "op",
            expected: "string",
        });
    };
    let kind = to_kind(&op).ok_or(OpError::UnexpectedField {
        field: "op",
        expected: "valid op",
    })?;

    let path = map
        .swap_remove("path")
        .ok_or(OpError::MissingMember { member: "path" })?;
    let Value::String(path) = path else {
        return Err(OpError::UnexpectedField {
            field: "path",
            expected: "string",
        });
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
        return Err(OpError::UnexpectedField {
            field: "from",
            expected: "string",
        });
    };
    Ok(from)
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

#[cfg(test)]
mod tests {
    use crate::Value;
    use crate::json;
    use crate::json::value::error::{OpError, PatchError, PatchErrorKind};
    use crate::json::value::patch::Operation;

    fn missing_members() -> Vec<(&'static str, PatchError)> {
        vec![
            (
                r#"[{}]"#,
                PatchError {
                    kind: PatchErrorKind::InvalidOp(OpError::MissingMember { member: "op" }),
                    index: Some(0),
                },
            ),
            (
                r#"[{"op": "foo"}]"#,
                PatchError {
                    kind: PatchErrorKind::InvalidOp(OpError::UnexpectedField {
                        field: "op",
                        expected: "valid op",
                    }),
                    index: Some(0),
                },
            ),
            (
                r#"[{"op": "add"}]"#,
                PatchError {
                    kind: PatchErrorKind::InvalidOp(OpError::MissingMember { member: "path" }),
                    index: Some(0),
                },
            ),
            (
                r#"[{"op": "add", "path": 3}]"#,
                PatchError {
                    kind: PatchErrorKind::InvalidOp(OpError::UnexpectedField {
                        field: "path",
                        expected: "string",
                    }),
                    index: Some(0),
                },
            ),
            (
                r#"[{"op": "add", "path": "/foo/bar"}]"#,
                PatchError {
                    kind: PatchErrorKind::InvalidOp(OpError::MissingMember { member: "value" }),
                    index: Some(0),
                },
            ),
            (
                r#"[{"op": "move", "path": "/foo/bar"}]"#,
                PatchError {
                    kind: PatchErrorKind::InvalidOp(OpError::MissingMember { member: "from" }),
                    index: Some(0),
                },
            ),
        ]
    }

    // Path traversal can fail for 3 reasons:
    // 1. Object missing key, path references a key that doesn't exist
    // 2. Array index out of bounds, index exceeds array length
    // 3. Primitive node, path tries to descend into a non-container
    fn path_traversal_errors() -> Vec<(String, Value, OpError)> {
        vec![
            (
                String::from("/foo/bar/buzz"),
                json!(
                    {
                        "foo": {
                            "buzz": 3
                        }
                    }
                ),
                OpError::PathNotFound {
                    path: String::from("/foo/bar/buzz"),
                    depth: 1,
                },
            ),
            (
                String::from("/foo/10/buzz"),
                json!(
                    {
                        "foo": [1, 2, 3, {}]
                    }
                ),
                OpError::IndexOutOfBounds {
                    path: String::from("/foo/10/buzz"),
                    depth: 1,
                    index: 10,
                    len: 4,
                },
            ),
            (
                String::from("/foo/bar/buzz"),
                json!(5),
                OpError::PathNotFound {
                    path: String::from("/foo/bar/buzz"),
                    depth: 0,
                },
            ),
        ]
    }

    fn invalid_op() -> Vec<(Operation, Value, OpError)> {
        // removing a key that does not exist
        vec![
            (
                Operation::Remove {
                    path: String::from("/foo/bar/buzz"),
                },
                json!(
                    {
                        "foo": {
                            "bar": {}
                        }
                    }
                ),
                OpError::PathNotFound {
                    path: String::from("/foo/bar/buzz"),
                    depth: 3,
                },
            ),
            // can't call remove on root
            (
                Operation::Remove {
                    path: String::from(""),
                },
                json!(
                    {
                        "foo": {
                            "bar": 3
                        }
                    }
                ),
                OpError::InvalidRootOp { op: "remove" },
            ),
            // can't replace a value in array where index >= len
            (
                Operation::Replace {
                    path: String::from("/foo/5"),
                    value: json!(5),
                },
                json!(
                    {
                        "foo": [1, 2, 3, {}]
                    }
                ),
                OpError::IndexOutOfBounds {
                    path: String::from("/foo/5"),
                    depth: 2,
                    index: 5,
                    len: 4,
                },
            ),
            // can't replace a key that does not exist
            (
                Operation::Replace {
                    path: String::from("/foo/buzz"),
                    value: json!(5),
                },
                json!(
                    {
                        "foo": {
                            "bar": 3
                        }
                    }
                ),
                OpError::PathNotFound {
                    path: String::from("/foo/buzz"),
                    depth: 2,
                },
            ),
            // can't move root
            (
                Operation::Move {
                    from: String::from(""),
                    path: String::from("/foo"),
                },
                json!(
                    {
                        "foo": {
                            "bar": 3
                        }
                    }
                ),
                OpError::InvalidRootOp { op: "move" },
            ),
            // it's 3 not 5
            (
                Operation::Test {
                    path: String::from("/foo/2"),
                    value: json!(5),
                },
                json!(
                    {
                        "foo": [1, 2, 3, {}]
                    }
                ),
                OpError::NotEqual,
            ),
        ]
    }

    fn valid_op() -> Vec<(Operation, Value, Value)> {
        vec![
            // key exists, value is updated
            (
                Operation::Add {
                    path: String::from("/foo/bar"),
                    value: json!([1, 2, 3]),
                },
                json!(
                    {
                        "foo": {
                            "bar": 5
                        }
                    }
                ),
                json!(
                    {
                        "foo": {
                            "bar": [1, 2, 3]
                        }
                    }
                ),
            ),
            // key does not exist, we insert key, value
            (
                Operation::Add {
                    path: String::from("/foo/bar"),
                    value: json!([1, 2, 3]),
                },
                json!(
                    {
                        "foo": {}
                    }
                ),
                json!(
                    {
                        "foo": {
                            "bar": [1, 2, 3]
                        }
                    }
                ),
            ),
            // add called for root, because the target location always exists we update the root
            (
                Operation::Add {
                    path: String::from(""),
                    value: json!([1, 2, 3]),
                },
                json!(
                    {
                        "foo": {}
                    }
                ),
                json!([1, 2, 3]),
            ),
            // insert at the end of the array
            (
                Operation::Add {
                    path: String::from("/foo/bar/3"),
                    value: json!(4),
                },
                json!(
                    {
                        "foo": {
                            "bar": [1, 2, 3]
                        }
                    }
                ),
                json!(
                    {
                        "foo": {
                            "bar": [1, 2, 3, 4]
                        }
                    }
                ),
            ),
            // insert at the end of the array where last token.val is '-'
            (
                Operation::Add {
                    path: String::from("/foo/bar/-"),
                    value: json!(4),
                },
                json!(
                    {
                        "foo": {
                            "bar": [1, 2, 3]
                        }
                    }
                ),
                json!(
                    {
                        "foo": {
                            "bar": [1, 2, 3, 4]
                        }
                    }
                ),
            ),
            // remove key
            (
                Operation::Remove {
                    path: String::from("/foo/bar"),
                },
                json!(
                    {
                        "foo": {
                            "bar": 5
                        }
                    }
                ),
                json!(
                    {
                        "foo": {}
                    }
                ),
            ),
            // remove at index
            (
                Operation::Remove {
                    path: String::from("/foo/bar/0"),
                },
                json!(
                    {
                        "foo": {
                            "bar": [1, 2, 3]
                        }
                    }
                ),
                json!(
                    {
                        "foo": {
                            "bar": [2, 3]
                        }
                    }
                ),
            ),
            // similar to the 1st case of add, target location must exist in order for the value
            // to be updated
            (
                Operation::Replace {
                    path: String::from("/foo/bar"),
                    value: json!([1, 2, 3]),
                },
                json!(
                    {
                        "foo": {
                            "bar": 5
                        }
                    }
                ),
                json!(
                    {
                        "foo": {
                            "bar": [1, 2, 3]
                        }
                    }
                ),
            ),
            // replace at index
            (
                Operation::Replace {
                    path: String::from("/foo/bar/0"),
                    value: json!(4),
                },
                json!(
                    {
                        "foo": {
                            "bar": [1, 2, 3]
                        }
                    }
                ),
                json!(
                    {
                        "foo": {
                            "bar": [4, 2, 3]
                        }
                    }
                ),
            ),
            (
                Operation::Copy {
                    from: String::from("/foo/1"),
                    path: String::from("/bar/0"),
                },
                json!(
                    {
                        "foo": [1, 2, 3],
                        "bar": [1, 2, 5]

                    }
                ),
                json!(
                    {
                        "foo": [1, 2, 3],
                        "bar": [2, 1, 2, 5]

                    }
                ),
            ),
            // does not change the root, it is more of not an error case
            (
                Operation::Test {
                    path: String::from("/bar/0"),
                    value: json!(1),
                },
                json!(
                    {
                        "foo": [1, 2, 3],
                        "bar": [1, 2, 5]

                    }
                ),
                json!(
                    {
                        "foo": [1, 2, 3],
                        "bar": [1, 2, 5]

                    }
                ),
            ),
        ]
    }

    #[test]
    fn invalid_root() {
        let res = super::parse("5".as_bytes());
        let err = PatchError {
            kind: PatchErrorKind::UnexpectedValue { expected: "Array" },
            index: None,
        };

        assert_eq!(res, Err(err));
    }

    #[test]
    fn non_object_in_array() {
        let res = super::parse("[null]".as_bytes());
        let err = PatchError {
            kind: PatchErrorKind::UnexpectedValue { expected: "Object" },
            index: Some(0),
        };

        assert_eq!(res, Err(err));
    }

    #[test]
    fn test_missing_members() {
        for (input, err) in missing_members() {
            let res = super::parse(input.as_bytes());
            assert_eq!(res, Err(err));
        }
    }

    #[test]
    fn test_path_traversal_errors() {
        for (path, mut root, err) in path_traversal_errors() {
            let res = super::descend(path, &mut root);
            assert_eq!(res, Err(err));
        }
    }

    #[test]
    fn test_apply_op_errors() {
        for (op, mut root, err) in invalid_op() {
            let res = op.apply(&mut root);
            assert_eq!(res, Err(err));
        }
    }

    // we don't have to test Op::Move because it is a combination of Op::Remove and Op::Add that
    // we already tested
    #[test]
    fn test_apply_op() {
        for (op, mut root, actual) in valid_op() {
            let _res = op.apply(&mut root);
            assert_eq!(root, actual);
        }
    }
}
