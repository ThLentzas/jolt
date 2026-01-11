use crate::parsing::number::Number;
use crate::parsing::value::error::{PatchError, PointerError};
use crate::parsing::value::path::Parser;
use crate::parsing::value::path::tracker::{NoOpTracker, PathTracker};
use crate::parsing::value::pointer::Pointer;
use indexmap::IndexMap;
use std::cmp::{Ordering, PartialEq};
use std::hash::{Hash, Hasher};

mod error;
mod from;
mod patch;
mod path;
pub mod pointer;

pub use crate::parsing::value::error::PathError;
pub use crate::parsing::value::path::tracker::Node;

// Clone is needed for Cow
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value {
    // Value is recursive type like LogicalExpression, but we don't need Box because both map and
    // vec store their data in the heap
    Object(IndexMap<String, Value>),
    Array(Vec<Value>),
    Number(Number),
    String(String),
    Boolean(bool),
    Null,
}
// https://docs.rs/ryu/latest/ryu/
// https://docs.rs/itoa/latest/itoa/

impl Value {
    pub fn is_object(&self) -> bool {
        matches!(self, Value::Object(_))
    }

    pub fn is_array(&self) -> bool {
        matches!(self, Value::Array(_))
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Boolean(_))
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }

    pub fn as_object(&self) -> Option<&IndexMap<String, Value>> {
        match self {
            Value::Object(map) => Some(map),
            _ => None,
        }
    }

    pub fn as_object_mut(&mut self) -> Option<&mut IndexMap<String, Value>> {
        match self {
            Value::Object(map) => Some(map),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<&Vec<Value>> {
        match self {
            Value::Array(vec) => Some(vec),
            _ => None,
        }
    }

    pub fn as_array_mut(&mut self) -> Option<&mut Vec<Value>> {
        match self {
            Value::Array(vec) => Some(vec),
            _ => None,
        }
    }

    pub fn as_number(&self) -> Option<&Number> {
        match self {
            Value::Number(num) => Some(num),
            _ => None,
        }
    }

    pub fn as_number_mut(&mut self) -> Option<&mut Number> {
        match self {
            Value::Number(num) => Some(num),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<&String> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_string_mut(&mut self) -> Option<&mut String> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_boolean(&self) -> Option<&bool> {
        match self {
            Value::Boolean(b) => Some(b),
            _ => None,
        }
    }

    pub fn as_boolean_mut(&mut self) -> Option<&mut bool> {
        match self {
            Value::Boolean(b) => Some(b),
            _ => None,
        }
    }

    pub fn as_null(&self) -> Option<()> {
        match self {
            Value::Null => Some(()),
            _ => None,
        }
    }

    pub fn get<I: Index>(&self, index: I) -> Option<&Value> {
        index.index_into(self)
    }
    pub fn get_mut<I: IndexMut>(&mut self, index: I) -> Option<&mut Value> {
        index.index_into(self)
    }

    // in a path /foo/bar/1 we don't know if 1 is an index or a key. If the current value is an object
    // it is treated as a key, if an array as an index
    //
    // starting from the root value, for every token created by gen_ref_token() we check with the
    // current val
    // -if val is an object then the token's value must be a key
    // -if val is an array then the token's value must be an unsigned base-10 integer value
    // -any other type we return None
    //
    // we update val and move on to the next token
    //
    // path: "/users/1/name", val = "{"users": [{"name": "Alice"}, {"name": "Bob"}]}
    //
    // 3 tokens are generated: "users", 1, "name"
    // "users" exists as key in val, update val with the value of the users val,
    //      val = [{"name": "Alice"}, {"name": "Bob"}]
    // 1 is an index and val is an array, update val with the value at the given index,
    //      val = {"name": "Bob"}
    // "name" exists as key in val, update val with the value of the key
    //      val = "Bob"
    pub fn pointer(&self, pointer: &str) -> Result<Option<&Value>, PointerError> {
        if !self.is_object() && !self.is_array() {
            return Ok(None);
        }

        // empty path at root level returns the entire object/array
        if pointer.is_empty() {
            return Ok(Some(self));
        }

        let buffer = pointer.as_bytes();
        let mut ptr = Pointer::new(buffer);
        ptr.check_start()?;
        // in the previous approach, we generated all the ref tokens and then iterated over the vector
        // calling match on current and returning a value if present. It worked, but we iterated twice,
        // once the input buffer to generate all the tokens and once the vector of those tokens. We
        // can do better by generating a token at a time, check against the current value and only
        // if we find a value, we keep going.
        let mut current = self;
        while let Some(token) = ptr.next()? {
            // We never check if number of tokens exceed the NestingDepthLimit because even if they did
            // we would get no match at NestingDepthLimit + 1 and we would return None
            match current {
                Value::Object(map) => match map.get(&token.val) {
                    Some(val) => current = val,
                    None => return Ok(None),
                },
                // we can not check the value earlier to see if it is valid numeric index because
                // we don't know if it will be called in an Object or an Array
                Value::Array(values) => match pointer::check_array_index(&token)? {
                    Some(index) if index >= values.len() => return Ok(None),
                    Some(index) => current = &values[index],
                    _ => return Ok(None),
                },
                _ => return Ok(None),
            }
        }
        Ok(Some(current))
    }

    // look at pointer() above for any comments
    pub fn pointer_mut(&mut self, pointer: &str) -> Result<Option<&mut Value>, PointerError> {
        if !self.is_object() && !self.is_array() {
            return Ok(None);
        }

        if pointer.is_empty() {
            return Ok(Some(self));
        }

        let buffer = pointer.as_bytes();
        let mut ptr = Pointer::new(buffer);
        ptr.check_start()?;

        let mut current = self;
        while let Some(token) = ptr.next()? {
            // We never check if number of tokens exceed the NestingDepthLimit because even if they did
            // we would get no match at NestingDepthLimit + 1 and we would return None
            match current {
                Value::Object(map) => {
                    let Some(val) = map.get_mut(&token.val) else {
                        return Ok(None);
                    };
                    current = val;
                }
                // we can not check the value earlier to see if it is valid numeric index because
                // we don't know if it will be called in an Object or an Array
                Value::Array(arr) => match pointer::check_array_index(&token)? {
                    Some(index) if index >= arr.len() => return Ok(None),
                    Some(index) => current = &mut arr[index],
                    _ => return Ok(None),
                },
                _ => return Ok(None),
            }
        }
        Ok(Some(current))
    }

    // we need to specify a lifetime for Node, otherwise it would be unclear to the user that reads
    // the code that Node holds a reference; we fix that by returning Node<'_>.
    // The lifetime can be elided based on the 3rd elision rule
    //
    // in all select() methods below we call into_iter() because we don't care about PathNode at all
    // we just want to consume it and do the mapping
    pub fn select(&self, path_expr: &str) -> Result<Vec<Node<'_>>, PathError> {
        let mut query = Parser::new(path_expr.as_bytes(), self);
        let nodes = query
            .parse::<PathTracker>()?
            .into_iter()
            .map(Node::from)
            .collect();
        Ok(nodes)
    }

    pub fn select_as_npaths(&self, path_expr: &str) -> Result<Vec<String>, PathError> {
        let mut query = Parser::new(path_expr.as_bytes(), self);
        let paths = query
            .parse::<PathTracker>()?
            .into_iter()
            // it is safe to call unwrap because every node has a trace apart from root
            // but when we reach the root we are done iterating so we never call unwarp on None
            .map(|pn| pn.trace.unwrap().to_npath())
            .collect();
        Ok(paths)
    }

    pub fn select_as_values(&self, path_expr: &str) -> Result<Vec<&Value>, PathError> {
        let mut query = Parser::new(path_expr.as_bytes(), self);
        let values = query
            .parse::<NoOpTracker>()?
            .into_iter()
            .map(|pn| pn.val)
            .collect();
        Ok(values)
    }

    // if an error occurs changes are maintained
    pub fn modify(&mut self, input: &str) -> Result<(), PatchError> {
        let ops = patch::parse(input.as_bytes())?;
        for (i, op) in ops.into_iter().enumerate() {
            op.apply(self).map_err(|err| PatchError::InvalidOp(err, i))?;
        }
        Ok(())
    }

    // this method rolls back the changes if one operation resulted in an error
    pub fn try_modify(&mut self, input: &str) -> Result<(), PatchError> {
        let copy = self.clone();
        let ops = patch::parse(input.as_bytes())?;
        for (i, op) in ops.into_iter().enumerate() {
            if let Err(err) = op.apply(self) {
                *self = copy;
                return Err(PatchError::InvalidOp(err, i));
            }
        }
        Ok(())
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            // https://github.com/indexmap-rs/indexmap/issues/288
            Value::Object(map) => map.as_slice().hash(state),
            Value::Array(arr) => arr.hash(state),
            Value::Number(n) => n.hash(state),
            Value::String(s) => s.hash(state),
            Value::Boolean(b) => b.hash(state),
            Value::Null => {}
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::String(s1), Value::String(s2)) => s1.partial_cmp(s2),
            (Value::Number(n1), Value::Number(n2)) => n1.partial_cmp(n2),
            // we can't call b1.partial_cmp(b2) because in rust false < true results to true but in
            // the spec it should result to false
            (Value::Boolean(b1), Value::Boolean(b2)) => {
                if b1 == b2 {
                    Some(Ordering::Equal)
                } else {
                    None
                }
            }
            (Value::Null, Value::Null) => Some(Ordering::Equal),
            // Objects and arrays do not offer < comparison
            // note that <= or >= will still evaluate to true according to the rfc
            (Value::Object(map1), Value::Object(map2)) => {
                if map1 == map2 {
                    Some(Ordering::Equal)
                } else {
                    None
                }
            }
            (Value::Array(vec1), Value::Array(vec2)) => {
                if vec1 == vec2 {
                    Some(Ordering::Equal)
                } else {
                    None
                }
            }
            // mismatch
            _ => None,
        }
    }
}

// implementing the std::ops::Index won't work as expected because index() always returns &Value,
// map["key"] on a missing entry should return None, arr[index] for out bounds should also return
// None, but we always need to return &Value. We could return Null but then the user won't know if
// Value::Null was the actual value that was found or a None case
pub trait Index {
    fn index_into<'r>(&self, value: &'r Value) -> Option<&'r Value>;
}

impl Index for usize {
    fn index_into<'r>(&self, value: &'r Value) -> Option<&'r Value> {
        match value {
            Value::Array(arr) => arr.get(*self),
            _ => None,
        }
    }
}

impl Index for &str {
    fn index_into<'r>(&self, value: &'r Value) -> Option<&'r Value> {
        match value {
            Value::Object(map) => map.get(*self),
            _ => None,
        }
    }
}

pub trait IndexMut {
    fn index_into<'r>(&self, value: &'r mut Value) -> Option<&'r mut Value>;
}

impl IndexMut for usize {
    fn index_into<'r>(&self, value: &'r mut Value) -> Option<&'r mut Value> {
        match value {
            Value::Array(arr) => arr.get_mut(*self),
            _ => None,
        }
    }
}

impl IndexMut for &str {
    fn index_into<'r>(&self, value: &'r mut Value) -> Option<&'r mut Value> {
        match value {
            Value::Object(map) => map.get_mut(*self),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::macros::json;

    // path, source, result
    fn valid_pointer_paths() -> Vec<(&'static str, Value, Option<Value>)> {
        vec![
            // (
            //     "",
            //     json!(
            //         {
            //             "foo": "bar"
            //         }
            //     ),
            //     Some(json!(
            //         {
            //             "foo": "bar"
            //         }
            //     )),
            // ),
            (
                "/foo/1",
                json!(
                    {
                        "foo": [false, null]
                    }
                ),
                Some(json!(null)),
            ),
            // (
            //     "//1",
            //     json!(
            //         {
            //             "": [false, null]
            //         }
            //     ),
            //     Some(json!(null)),
            // ),
            // (
            //     "/foo//0",
            //     json!(
            //     {
            //         "foo": {
            //             "": [true]
            //         }
            //     }),
            //     Some(json!(true)),
            // ),
            // // Rust defaults integer literals to i32
            // (
            //     "/foo~1bar/baz",
            //     json!(
            //     {
            //         "foo/bar": {
            //             "baz":  50000
            //         }
            //     }),
            //     Some(json!(50000)),
            // ),
            // (
            //     "/foo~0bar/baz",
            //     json!(
            //     {
            //         "foo~bar": {
            //             "baz": ":)"
            //         }
            //     }),
            //     Some(json!(":)")),
            // ),
            // (
            //     "/foo~\\u0030bar/baz",
            //     json!(
            //     {
            //         "foo~bar": {
            //             "baz": ":)"
            //         }
            //     }),
            //     Some(json!(":)")),
            // ),
            // (
            //     "/foo\\u007e0bar/baz",
            //     json!(
            //     {
            //         "foo~bar": {
            //             "baz": ":)"
            //         }
            //     }),
            //     Some(json!(":)")),
            // ),
            // (
            //     "/foo\\u007e\\u0030bar/baz",
            //     json!(
            //     {
            //         "foo~bar": {
            //             "baz": ":)"
            //         }
            //     }),
            //     Some(json!(":)")),
            // ),
            // // index out of bounds
            // ("/2", json!([2]), None),
            // // unparsable index starting with a digit leads to None
            // ("/2e", json!([2]), None),
            // // '-' as array index always returns none according to spec
            // ("/-", json!([2, 3]), None),
            // (
            //     "/é",
            //     json!(
            //     {
            //         "é": false
            //     }),
            //     Some(json!(false)),
            // ),
            // (
            //     "/1",
            //     json!(
            //     {
            //         "foo": "bar"
            //     }),
            //     None,
            // ),
        ]
    }

    // false < true evaluates to false by returning none, otherwise rust would result that to true
    // Objects and arrays do not offer < comparison, so in the case of [1, 2, 3] and [1, 2, 4] we
    // don't want any order by checking each value 1 by 1
    fn comparisons() -> Vec<(Value, Value, Option<Ordering>)> {
        vec![
            (Value::Boolean(false), Value::Boolean(true), None),
            (
                Value::Boolean(false),
                Value::Boolean(false),
                Some(Ordering::Equal),
            ),
            (json!({}), json!({}), Some(Ordering::Equal)),
            (
                json!({ "foo": "bar" }),
                json!({ "foo": "bar" }),
                Some(Ordering::Equal),
            ),
            (json!({ "foo": "bar" }), json!({ "buzz": "bar" }), None),
            (json!([]), json!([]), Some(Ordering::Equal)),
            (json!([1, 2, 3]), json!([1, 2, 3]), Some(Ordering::Equal)),
            (json!([1, 2, 3]), json!([1, 2, 4]), None),
            (json!([1, 2, 3]), json!(4), None), // mismatch
        ]
    }

    // path_expr, source, result
    // in the cases below selectors are applied to objects and return values
    // cases like applying selectors to a non-object value or not finding keys with the given name
    // are part of this test
    //
    // we don't have to repeat all the cases for both single and double-quoted names since we use
    // the same function
    fn valid_names() -> Vec<(&'static str, Value, Vec<Value>)> {
        vec![
            (
                "$.foo \n\r\t",
                json!(
                {
                    "foo": "bar"
                }),
                vec![json!("bar")],
            ),
            // é: 2-byte sequence
            (
                "$.namé",
                json!(
                {
                    "namé": "Joe"
                }),
                vec![json!("Joe")],
            ),
            (
                "$._j0lt",
                json!(
                {
                    "_j0lt": ":)"
                }),
                vec![json!(":)")],
            ),
            (
                "$[\"foo\"]",
                json!(
                {
                    "foo": "bar"
                }),
                vec![json!("bar")],
            ),
            // in double-quoted name ' can appear as char literal
            (
                "$[\"hello ' world\"]",
                json!(
                {
                    "hello ' world": "://"
                }),
                vec![json!("://")],
            ),
            // in double-quoted name " must be escaped at the parser level like we did with json strings
            (
                "$[\"hello \\\" world\"]",
                json!(
                {
                    "hello \" world": "://"
                }),
                vec![json!("://")],
            ),
            // in single-quoted name " must be escaped, similar to how we handle json strings(merge / and ' into ')
            // at runtime the value is $['hello \' world'] and we map to $['hello ' world']
            (
                "$['hello \\' world']",
                json!(
                {
                    "hello ' world": "://"
                }),
                vec![json!("://")],
            ),
            // in single-quoted name " can appear unescaped at the parser level
            // at runtime the value is $['hello " world'] and " is just a char literal
            (
                "$['hello \" world']",
                json!(
                {
                    "hello \" world": "://"
                }),
                vec![json!("://")],
            ),
            (
                "$[\"\"]",
                json!(
                {
                    "": "empty key"
                }),
                vec![json!("empty key")],
            ),
            (
                "$['foo']",
                json!(
                {
                    "foo": "bar"
                }),
                vec![json!("bar")],
            ),
            (
                "$['\\u263A']",
                json!(
                {
                    "☺": "smiley_face"
                }),
                vec![json!("smiley_face")],
            ),
            // surrogate pair
            (
                "$['\\uD83D\\uDE80']",
                json!(
                {
                    "🚀": "rocket"
                }),
                vec![json!("rocket")],
            ),
            // called in an Object that does not have 'foo' as key
            (
                "$['foo']",
                json!(
                {
                    "key": "value"
                }),
                vec![],
            ),
            // called in a non-Object value
            ("$['foo']", json!([1, 2, 3]), vec![]),
        ]
    }

    fn valid_indices() -> Vec<(&'static str, Value, Vec<Value>)> {
        vec![
            // n_idx = 3 + (-3) = 0
            ("$[-3]", json!([2, 5, 1]), vec![json!(2)]),
            ("$[2]", json!([2, 5, 1]), vec![json!(1)]),
            // n_idx = 3 + (-5) = -2
            ("$[-5]", json!([2, 5, 1]), vec![]),
            // out of bounds
            ("$[5]", json!([2, 5, 1]), vec![]),
        ]
    }

    fn valid_slices() -> Vec<(&'static str, Value, Vec<Value>)> {
        vec![
            (
                "$[2:4]",
                json!([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
                vec![json!(2), json!(3)],
            ),
            (
                "$[1:8:2]",
                json!([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
                vec![json!(1), json!(3), json!(5), json!(7)],
            ),
            // [0, 10)
            (
                "$[:]",
                json!([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
                vec![
                    json!(0),
                    json!(1),
                    json!(2),
                    json!(3),
                    json!(4),
                    json!(5),
                    json!(6),
                    json!(7),
                    json!(8),
                    json!(9),
                ],
            ),
            // [0, 10)
            (
                "$[::]",
                json!([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
                vec![
                    json!(0),
                    json!(1),
                    json!(2),
                    json!(3),
                    json!(4),
                    json!(5),
                    json!(6),
                    json!(7),
                    json!(8),
                    json!(9),
                ],
            ),
            // [0, 3)
            (
                "$[:3]",
                json!([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
                vec![json!(0), json!(1), json!(2)],
            ),
            // [0, 10) step by 2
            (
                "$[::2]",
                json!([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
                vec![json!(0), json!(2), json!(4), json!(6), json!(8)],
            ),
            // [0, 10) in reverse order
            (
                "$[::-1]",
                json!([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
                vec![
                    json!(9),
                    json!(8),
                    json!(7),
                    json!(6),
                    json!(5),
                    json!(4),
                    json!(3),
                    json!(2),
                    json!(1),
                    json!(0),
                ],
            ),
            // (0, 10) in reverse order
            (
                "$[:0:-1]",
                json!([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
                vec![
                    json!(9),
                    json!(8),
                    json!(7),
                    json!(6),
                    json!(5),
                    json!(4),
                    json!(3),
                    json!(2),
                    json!(1),
                ],
            ),
            // (-1, 4] in reverse order
            (
                "$[4::-1]",
                json!([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
                vec![json!(4), json!(3), json!(2), json!(1), json!(0)],
            ),
            // [1, 1)
            ("$[1:1]", json!([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]), vec![]),
            // read new() from Range for the cases below
            // [9, 4)
            ("$[-1:-6:]", json!([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]), vec![]),
            // [0, 10)
            (
                "$[-11:12:]",
                json!([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
                vec![
                    json!(0),
                    json!(1),
                    json!(2),
                    json!(3),
                    json!(4),
                    json!(5),
                    json!(6),
                    json!(7),
                    json!(8),
                    json!(9),
                ],
            ),
            // (2, 9] in reverse order and step by 2
            (
                "$[-1:-8:-2]",
                json!([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
                vec![json!(9), json!(7), json!(5), json!(3)],
            ),
            // [2, 8)
            (
                "$[2:-2]",
                json!([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
                vec![json!(2), json!(3), json!(4), json!(5), json!(6), json!(7)],
            ),
            // [8, 9)
            (
                "$[-2:9]",
                json!([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
                vec![json!(8)],
            ),
            // [6, 10)
            (
                "$[-4:]",
                json!([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
                vec![json!(6), json!(7), json!(8), json!(9)],
            ),
            // [0, 7)
            (
                "$[:-3]",
                json!([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
                vec![
                    json!(0),
                    json!(1),
                    json!(2),
                    json!(3),
                    json!(4),
                    json!(5),
                    json!(6),
                ],
            ),
            // start *S ":" *S end *S ":" *S step
            (
                "$[ 1 : \n\t 5 : \r]",
                json!([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
                vec![json!(1), json!(2), json!(3), json!(4)],
            ),
        ]
    }

    // we don't need to add any more comparison cases because we already tested in partial_cmp()
    fn valid_filter() -> Vec<(&'static str, Value, Vec<Value>)> {
        vec![
            // test expression with an embedded relative query with 0 segments
            //
            // try to apply the filter selector on the root; we can since it is an object
            // @ adds the current as root; in this case both current and root are the same
            // tries to apply the segments we don't have any and our list returns 1 entry
            // because the test expression returned true for the current we add current to the list
            // for maps we add their values, for arrays their elements
            (
                "$[?@]",
                json!(
                {
                    "foo": "bar",
                    "buzz": 0
                }),
                vec![json!("bar"), json!(0)],
            ),
            // test expression with an embedded relative query with 1 segment, 1 selector(name shorthand)
            //
            // iterate through the array and for each element try to apply
            // if the query returns any result, we add the current element into our return list
            //
            // 1st element -> add the root(very important), in this case we add the object and
            // then try to apply .foo in the input list(reader) we add "bar" because it has a key
            // named foo, no more segment to process, subquery returns a non-empty list it evaluates
            // to true, and we add the 1st element of the array into our list
            //
            // 2nd element -> add the root, the subquery returns empty list, it evaluates to false
            // move to the next element
            //
            // 3rd element -> can't apply filter selector to numbers we return the list containing
            // the 1st element.
            (
                "$[?@.foo]",
                // this is an array of objects, initially I would wrap every entry into json!()
                // but I was wrong because this is what the macro does
                // ([ $($elem:tt),+ $(,)? ]) => { $crate::Value::Array(vec![$(json!($elem)),+]) };
                // it takes elem and wraps it in json!()
                json!([
                    {
                        "foo": "bar",
                        "buzz": 0
                    },
                    {
                        "bar": "foo"
                    },
                    3
                ]),
                vec![json!(
                    {
                        "foo": "bar",
                        "buzz": 0
                    }

                )],
            ),
            // test expression with an embedded absolute query with 0 segments
            // select all children of root where the test expression $ is true
            //
            // try to apply the filter selector on the root; we can since it is an array
            //
            // for each entry in the array try to apply the test expr
            //
            // 1st element -> $ adds the root; try to apply the segments we don't have any and our
            // list returns 1 entry, because the test expression returned true for the current element
            // of the array we add current to the list
            //
            // 2nd element/3rd element -> exactly the same as 1st. For the 3rd element which is just
            // a number we don't apply the filter selector we just check if the test expression returns
            // true for this node
            //
            // This is a special case where the input will always match the output because:
            // $ (root with zero segments) always returns a nodelist containing exactly one node:
            // the root itself. So the existence test is always true, regardless of what the
            // current element is. $[?$] = $[*]
            //
            // Note that we return a vector of json values, not a json array containing json values
            (
                "$[?$]",
                json!([
                    {
                        "foo": "bar",
                        "buzz": 0
                    },
                    {
                        "bar": "foo"
                    },
                    3
                ]),
                vec![
                    json!(
                        {
                            "foo": "bar",
                            "buzz": 0
                        }
                    ),
                    json!(
                        {
                            "bar": "foo"
                        }
                    ),
                    json!(3),
                ],
            ),
            // for every element in the array we try check if the test expression returns true
            // test expression has an absolute query
            // .* adds all the elements of the root, so our input list is of size and then we apply
            // the next segment which is [0]
            // for the first two elements [0] can't be applied but for the last one it can and now
            // our list consists of just 3, no more segments to apply the test expression returns
            // true for all 3 elements of the array(applied 3 times) and we add all to the return list
            //
            // if it was [1] -> it would have been out of bounds, the subquery will not return anything
            // and the test expression would evaluate to false; that would also be the case if the
            // last element of the root was not an array; if it was 3 instead of [3], [0] can't be
            // applied
            (
                "$[?$.*[0]]",
                json!([
                    {
                        "foo": "bar",
                        "buzz": 0
                    },
                    {
                        "bar": "foo"
                    },
                    [3]
                ]),
                vec![
                    json!(
                    {
                        "foo": "bar",
                        "buzz": 0
                    }),
                    json!(
                    {
                        "bar": "foo"
                    }),
                    json!([3]),
                ],
            ),
            // single quotes are enough
            (
                "$[?@.foo == 'bar']",
                json!([
                    {
                        "foo": "bar",
                        "buzz": 0
                    },
                    {
                        "bar": "foo"
                    },
                    [3]
                ]),
                vec![json!(
                {
                    "foo": "bar",
                    "buzz": 0
                })],
            ),
            (
                "$[?@.foo >= 20]",
                json!([
                    {
                        "foo": 22,
                        "buzz": 0
                    },
                    {
                        "bar": "foo"
                    },
                    [3]
                ]),
                vec![json!(
                {
                    "foo": 22,
                    "buzz": 0
                })],
            ),
            (
                "$[?@.foo >= 20]",
                json!([
                    {
                        "foo": 19,
                        "buzz": 0
                    },
                    {
                        "bar": "foo"
                    },
                    [3]
                ]),
                vec![],
            ),
            // read the comment of the 1st entry: "$[?@]"
            (
                "$[?@ == @]",
                json!([
                    {
                        "foo": "bar",
                        "buzz": 0
                    },
                    {
                        "bar": "foo"
                    },
                    [3]
                ]),
                vec![
                    json!(
                    {
                        "foo": "bar",
                        "buzz": 0
                    }),
                    json!(
                    {
                        "bar": "foo"
                    }),
                    json!([3]),
                ],
            ),
            // same logic as "$[?$]"
            (
                "$[? $ == $]",
                json!([
                    {
                        "foo": "bar",
                        "buzz": 0
                    },
                    {
                        "bar": "foo"
                    },
                    [3]
                ]),
                vec![
                    json!(
                    {
                        "foo": "bar",
                        "buzz": 0
                    }),
                    json!(
                    {
                        "bar": "foo"
                    }),
                    json!([3]),
                ],
            ),
            // both embedded queries return an empty list, so the expression evaluates to true
            // for all entries which means we include them all in the output
            (
                "$[?$.foo == @[3]]",
                json!([
                    {
                        "foo": "bar",
                        "buzz": 0
                    },
                    {
                        "bar": "foo"
                    },
                    [3]
                ]),
                vec![
                    json!(
                    {
                        "foo": "bar",
                        "buzz": 0
                    }),
                    json!(
                    {
                        "bar": "foo"
                    }),
                    json!([3]),
                ],
            ),
            // $[0].foo returns 3 because this is the value of "foo" in the 1st element of the root
            // @[0] returns nothing for the 2 elements, so comparing EMPTY vs NON_EMPTY results to
            // false, we test that case indirectly here
            // @[0] returns 3 for [3]
            // note we don't compare the size of the returned lists, for each list we retrieve the
            // value and compare them and since both are 3, we add to the output list the element of
            // the array that the comparison returned true, so [3] not 3
            (
                "$[?$[0].foo == @[0]]",
                json!([
                    {
                        "foo": 3,
                        "buzz": 0
                    },
                    {
                        "bar": "foo"
                    },
                    [3]
                ]),
                vec![json!([3])],
            ),
            // multi-selector in a subquery(duplicate nodes)
            // the 3 times we get to evaluate the comparison we have:
            //      INVALID - EMPTY
            //      INVALID - EMPTY
            //      INVALID - VALUE
            // all 3 return false and the output is an empty vector and not an error
            (
                "$[?$[:] == @[0]]",
                json!([
                    {
                        "bar": 3,
                        "buzz": 0
                    },
                    {
                        "bar": "foo"
                    },
                    [3]
                ]),
                vec![],
            ),
            // $[2][0]] returns 3 for every element in root
            // @.bar evaluates to:
            //      3
            //      "foo"
            //      empty
            //
            // after comparing:
            //      3 != 3 false, not included
            //      "foo" != 3 true, included
            //      empty != 3 true, included
            (
                "$[?@.bar != $[2][0]]",
                json!([
                    {
                        "bar": 3,
                        "buzz": 0
                    },
                    {
                        "bar": "foo"
                    },
                    [3]
                ]),
                vec![
                    json!({
                        "bar": "foo"
                    }),
                    json!([3]),
                ],
            ),
            // @.bar returns a value only for the 2nd element and $[2][0]] returns 3 for every element
            // since it is an absolute path. The only time that the expression evaluates to true
            // is for the 2nd element
            (
                "$[?@.bar && $[2][0]]",
                json!([
                    {
                        "foo": 3,
                        "buzz": 0
                    },
                    {
                        "bar": "foo"
                    },
                    [3]
                ]),
                vec![json!({
                    "bar": "foo"
                })],
            ),
            // @.foo returns 3 for the 1st element so this expression evaluates to true fast, because
            // of short-circuiting, rhs never gets evaluated
            // $[1][0] returns 3 for both elements, but it will only be evaluated for the 2nd element
            //
            // for the output, lhs returned true for the 1st element, while the rhs returned true
            // for the 2nd element we include both in the output
            (
                "$[?@.foo || $[1][0]]",
                json!([
                    {
                        "foo": 3,
                        "buzz": 0
                    },
                    [3]
                ]),
                vec![
                    json!({
                        "foo": 3,
                        "buzz": 0
                    }),
                    json!([3]),
                ],
            ),
            // $[0].foo > 2 returns 3, as an existence test returns true for both elements of the
            // root
            // @[0] <= 4 returns a value only for the 2nd element and this is where both conditions
            // evaluate to true and include only [3] in our output list
            (
                "$[?$[0].foo > 2 && @[0] <= 4]",
                json!([
                    {
                        "foo": 3,
                        "buzz": 0
                    },
                    [3]
                ]),
                vec![json!([3])],
            ),
            // logical not operator:
            // @.foo evaluates to true for the 1st element; in our logic when we evaluate the test
            // expression it returns true but because we have '!' we flip the result so it is false
            // and we don't include it
            // for the 2nd element though .foo returns false but it is flipped due to '!' and it
            // evaluates to true and we include current to the output to the list
            (
                "$[?!@.foo]",
                json!([
                    {
                        "foo": 3
                    },
                    [3]
                ]),
                vec![json!([3])],
            ),
            // same as above parenthesized
            (
                "$[?!(@.foo)]",
                json!([
                    {
                        "foo": 3
                    },
                    [3]
                ]),
                vec![json!([3])],
            ),
            // @.* is an existence test and we are allowed to use multi-selectors like '*'. The result
            // of @.* returns a lists of all the values of the map and because it returns a non-empty
            // list it evaluates to true so we include the 1st element.
            //
            // for the 2nd element we can't apply '*' in a non-container node so it returns an empty
            // list and we don't include it to our output list
            (
                "$[?@.*]",
                json!([
                    {
                        "foo": null,
                        "bar": true
                    },
                    null
                ]),
                vec![json!({
                    "foo": null,
                    "bar": true
                })],
            ),
            // a test expression with a child segment that has 3 selectors
            // the output vector of evaluating @[*, 0, :] for the 1st element is
            // [1, 2, 3, 4, 5, 1, 1, 2, 3, 4, 5]. All 3 selectors are applied to every node in the list,
            // and we process the 1st element of root which is the [1, 2, 3, 4, 5] array
            // * -> selects them all (1, 2, 3, 4, 5)
            // 0 -> selects 1st if array (1, 2, 3, 4, 5, 1)
            // : -> from 0 - len with step 1 (1, 2, 3, 4, 5, 1, 1, 2, 3, 4, 5)
            // @[*, 0, :] returns an not empty list, we include the element in the output list
            //
            // for the 2nd element we can't apply the subquery
            (
                "$[?@[*, 0, :]]",
                json!([[1, 2, 3, 4, 5], null]),
                vec![json!([1, 2, 3, 4, 5])],
            ),
            // nested filter selector
            //
            // for every element in the input list try to apply the filter selector
            // the 1st element is an array of arrays of objects. Iterate through all the elements
            // of the 1st element, all the nested arrays and try to apply 4 <= @[0].x
            // @[0].x returns 6, 1, 3; the output list contains [{"x": 6}], because the expression
            // returned true for the 1st element of the root, we include that into the output
            // output: [[{"x": 6}], [{"x": 1}], [{"x": 3}]] // 1st element
            //
            // for the rest of the elements we can't apply a filter selector
            (
                "$[?@[?(4 <= @[0].x)]]",
                json!([
                    [
                        [
                            {
                                "x": 6
                            }
                        ],
                        [
                            {
                                "x": 1
                            }
                        ],
                        [
                            {
                                "x": 3
                            }
                        ]
                    ],
                    null,
                    true,
                    []
                ]),
                vec![json!([[{"x": 6}], [{"x": 1}], [{"x": 3}]])],
            ),
            // @.x returns true for the 1st element, include it in the output list, no need to evaluate
            // rhs due to short-circuiting for OR
            //
            // @.x returns false for the 2nd element, check rhs, @.y returns true must evaluate @.z
            // both return true, overall true OR false, true we include the 2nd element
            //
            // @x returns false for the 3rd element, @.y returns false, we don't evaluate rhs because
            // of short-circuiting, false OR false, false
            //
            // if our precedence logic was wrong then for the 1st element we have (true or false) && false
            // which would evaluate to false, and we wouldn't include that in our list
            (
                "$[?(@.x || @.y && @.z)]",
                json!([
                    {
                        "x": 1,
                        "y": 2
                    },
                    {
                        "y": 2,
                        "z": 3
                    },
                    {
                        "z": 3
                    }
                ]),
                vec![
                    json!(
                        {
                            "x": 1,
                            "y": 2
                        }
                    ),
                    json!(
                        {
                            "y": 2,
                            "z": 3
                        }
                    ),
                ],
            ),
            // () has the highest precedence
            // @.y and @.z evaluates to true for the 2nd element
            (
                "$[?(@.x || @.y) && @.z]",
                json!([
                    {
                        "x": 1,
                        "y": 2
                    },
                    {
                        "y": 2,
                        "z": 3
                    },
                    {
                        "z": 3
                    }
                ]),
                vec![json!(
                    {
                        "y": 2,
                        "z": 3
                    }
                )],
            ),
            // the same expression with groups: (!@.x) || ((@.y < 10) && (@.z == 3))
            // the NOT operator binds tighter than OR which means we don't evaluate lhs OR rhs and
            // then negate the result
            // relations also bind tighter than OR/AND, the result of @.y < 10 is the lhs of AND
            // and the result of @.z == 3 is the rhs. As mentioned before AND has higher precedence
            // than OR so the result of @.y < 10 is grouped with AND not with OR
            // ((!@.x) || (@.y < 10)) && (@.z == 3) -> this is wrong
            (
                "$[?!@.x || @.y < 10 && @.z == 3]",
                json!([
                    {
                        "x": 1,
                        "y": 2
                    },
                    {
                        "y": 2,
                        "z": 3
                    },
                    {
                        "x": 1,
                        "y": 11,
                        "z": 3
                    }
                ]),
                vec![json!(
                    {
                        "y": 2,
                        "z": 3
                    }
                )],
            ),
            // same precedence, we look at associativity.
            //
            // associativity is the tie-breaker rule used by the parser when it encounters a sequence
            // of operators that have the same precedence. Precedence decides between different
            // operators. Associativity decides between the same operator
            //
            // AND is left to right
            // so the expression is evaluated as (@.x && @.y) && @.z
            // in boolean algebra it does not matter @x && (@.y && @.z) is the same but we need
            // this left to right approach for our parser to be consistent
            //
            // OR is exactly the same
            //
            // NOT is right associative; is a prefix operator (it comes before the value), it binds
            // to whatever is immediately to its right. Similar to ^ in math where 2 ^ 3 ^ 4 evaluates
            // to 2 ^ (3 ^ 4)
            //
            // Relations are not associative
            (
                "$[?@.x && @.y && @.z]",
                json!([
                    {
                        "x": 1,
                        "y": 2
                    },
                    {
                        "y": 2,
                        "z": 3
                    },
                    {
                        "x": 1,
                        "y": 2,
                        "z": 3
                    }
                ]),
                vec![json!(
                    {   "x": 1,
                        "y": 2,
                        "z": 3
                    }
                )],
            ),
            // same as above with OR
            (
                "$[?@.x || @.y || @.z]",
                json!([
                    {
                        "x": 1,
                    },
                    {
                        "y": 2,
                    },
                    {
                        "z": 3
                    }
                ]),
                vec![
                    json!(
                        {
                            "x": 1,
                        }
                    ),
                    json!(
                        {
                            "y": 2,
                        }
                    ),
                    json!(
                        {
                            "z": 3
                        }
                    ),
                ],
            ),
            // Nothing == Nothing evaluates to true, and we return 3
            // @.x and @.y return an empty list
            (
                "$[?length(@.x) == length(@.y)]",
                json!(
                    {
                        "z": 3
                    }
                ),
                vec![json!(3)],
            ),
            // Nothing == Empty
            // length(@.z) returns Nothing because despite @.z returning a value(3) it is not
            // object/array/string and @.y returns Empty.
            (
                "$[?length(@.z) == @.y]",
                json!(
                    {
                        "z": 3
                    }
                ),
                vec![json!(3)],
            ),
            // this was the trickiest one, initially i had "$[?value(@.x) == 2]" and value(@.x)
            // returned an empty list. This is because the filter selector for root iterates through
            // all the members and try to evaluate the expression on the VALUE so i was calling
            // .x for 2
            (
                "$[?value(@) == 2]",
                json!(
                    {
                        "x": 2
                    }
                ),
                vec![json!(2)],
            ),
            // non singular query -> nothing
            (
                "$[?value(@.*) == 2]",
                json!(
                    {
                        "x": 2
                    }
                ),
                vec![],
            ),
            // look at the comment in path::apply_selector() for the filter selector if confused
            (
                "$[?match(@.timezone, 'Europe/.*')]",
                json!([
                    {
                        "timezone": "Europe/Finland",
                    },
                    {
                        "timezone": "Europe/Iceland",
                    },
                    {
                        "timezone": "America/New_York",
                    }
                ]),
                vec![
                    json!({"timezone": "Europe/Finland"}),
                    json!({"timezone": "Europe/Iceland"}),
                ],
            ),
            // LogicalTrue can't be compared with boolean true
            (
                "$[?match(@.timezone, 'Europe/.*') == true]",
                json!([
                    {
                        "timezone": "Europe/Finland",
                    },
                    {
                        "timezone": "Europe/Iceland",
                    },
                    {
                        "timezone": "America/New_York",
                    }
                ]),
                vec![],
            ),
            (
                "$[?search(@.timezone, 'Europe')]",
                json!([
                    {
                        "timezone": "Europe/Finland",
                    },
                    {
                        "timezone": "Europe/Iceland",
                    },
                    {
                        "timezone": "America/New_York",
                    }
                ]),
                vec![
                    json!({"timezone": "Europe/Finland"}),
                    json!({"timezone": "Europe/Iceland"}),
                ],
            ),
            // LogicalTrue can't be compared with boolean true
            (
                "$[?search(@.timezone, 'Europe/.*') == true]",
                json!([
                    {
                        "timezone": "Europe/Finland",
                    },
                    {
                        "timezone": "Europe/Iceland",
                    },
                    {
                        "timezone": "America/New_York",
                    }
                ]),
                vec![],
            ),
        ]
    }

    #[test]
    fn test_comparisons() {
        for (lhs, rhs, order) in comparisons() {
            assert_eq!(
                lhs.partial_cmp(&rhs),
                order,
                "comparing {:?} with {:?}",
                lhs,
                rhs
            );
        }
    }

    #[test]
    fn test_valid_pointer_paths() {
        for (path, source, result) in valid_pointer_paths() {
            assert_eq!(source.pointer(path), Ok(result.as_ref()));
        }
    }

    // jpath
    #[test]
    fn test_root() {
        let root = json!({
            "x": 1,
            "y": 2
        });

        let res = vec![Node {
            path: String::from("$"),
            val: &root,
        }];

        assert_eq!(res, root.select("$").unwrap());
    }

    #[test]
    fn test_valid_names() {
        for (path_expr, root, nodelist) in valid_names() {
            let res = root.select_as_values(path_expr).unwrap();

            assert_eq!(res.len(), nodelist.len());
            for (i, val) in res.into_iter().enumerate() {
                assert_eq!(*val, nodelist[i], "invalid path: {path_expr}");
            }
        }
    }

    #[test]
    fn test_valid_indices() {
        for (path_expr, root, nodelist) in valid_indices() {
            let res = root.select_as_values(path_expr).unwrap();

            assert_eq!(res.len(), nodelist.len());
            for (i, val) in res.into_iter().enumerate() {
                assert_eq!(*val, nodelist[i], "invalid path: {path_expr}");
            }
        }
    }

    #[test]
    fn test_valid_slices() {
        for (path_expr, root, nodelist) in valid_slices() {
            let res = root.select_as_values(path_expr).unwrap();

            assert_eq!(res.len(), nodelist.len());
            for (i, val) in res.into_iter().enumerate() {
                assert_eq!(*val, nodelist[i], "invalid path: {path_expr}");
            }
        }
    }

    #[test]
    fn wildcard_on_object() {
        let val = json!({
            "key": "value",
            "key1": "value1"
        });
        let res = val.select_as_values("$.*").unwrap();

        if let Value::Object(ref map) = val {
            let expected: Vec<&Value> = map.values().collect();
            assert_eq!(res, expected);
        }
        // maybe panic! here with an else because the test will pass if val is not an object
    }

    #[test]
    fn wildcard_on_array() {
        let val = json!([1, 5, 3, 9]);
        let res = val.select_as_values("$[*]").unwrap();

        if let Value::Array(ref vec) = val {
            let expected: Vec<&Value> = vec.iter().collect();
            assert_eq!(res, expected);
        }
    }

    #[test]
    fn test_valid_filters() {
        for (path_expr, root, nodelist) in valid_filter() {
            let res = root.select_as_values(path_expr).unwrap();

            assert_eq!(res.len(), nodelist.len());
            for (i, val) in res.into_iter().enumerate() {
                assert_eq!(*val, nodelist[i], "invalid path: {path_expr}");
            }
        }
    }

    // ..[?length(@.name) > 2 && count(@.tags[*]) >= 1]
    // try to apply [?length(@.name) > 2 && count(@.tags[*]) >= 1] to the current node and all its
    // descendants before applying to its siblings
    //
    // input list holds the root
    // try to apply every selector of the segment
    // filter selector, current is object -> iterate through all the keys and evaluate the logical
    // expression: length(@.name) > 2 && count(@.tags[*]) >= 1
    // root has 2 keys: "users" and "other" so @.name returns an empty list because no match was
    // found: length(@.name) > 2 -> 0 > 2 returns false, we stop due to short-circuiting.
    // selector returns false for "users", but before we move to "other" we visit all the descendants
    // of "users" before.
    //
    // "users" is an array, and we can apply the filter selector -> iterate through every element
    // evaluate the expression
    //
    // users[0] -> length(@.name) > 2 && count(@.tags[*]) >= 1, length(@.name) > 2 returns false(2 > 2)
    // Again apply the same logic, before moving to users[1] descend can't apply to "id" and "name"
    // but we can for "tags". Applying the selector returns false, and we return back to users[1]
    //
    // users[1] -> length(@.name) > 2 && count(@.tags[*]) >= 1, length(@.name) > 2 returns true(5 > 2)
    // count(@.tags[*]) >= 1 for ["user", "active"] returns true(2 >= 1), overall true && true ->
    // add the current value to the writer buffer; writer now holds 1 Value, the object at users[1]
    // trying to descend results in the same behavior as users[0]
    //
    // users[2] -> length(@.name) > 2 && count(@.tags[*]) >= 1, length(@.name) > 2 returns true(3 > 2)
    // count(@.tags[*]) >= 1 for [] returns false(0 >= 1). Descending into users[2] has the same
    // behavior as the 2 previous cases
    //
    // users[3] -> evaluating length(@.name) > 2 && count(@.tags[*]) >= 1 results to true, and  we
    // add the object to our writer; writer so far holds users[1], users[3]
    //
    // at this point we are done with the descendants of "users" we move to its siblings. Applying
    // the same logic to "other" we get 1 more entry, mods[1]. No more keys for root, we are done
    // applying the 1st segment
    //
    // writer: { users[1], users[2], mods[1] }, we use that as the input to apply the next segment
    // child segment with multiple selector -> apply every selector to every node of the input list
    //
    // users[1] for 'id' and 'name' returns 2, Alice
    // users[2] for 'id' and 'name' returns 4, Charlie
    // mods[1] for 'id' and 'name' returns 11, Dave
    //
    // All those are json values, done processing segments
    #[test]
    fn test_desc_seg_and_multi_selector() {
        let path_expr = "$..[?length(@.name) > 2 && count(@.tags[*]) >= 1]['id', 'name']";
        let root = json!(
            {
                "users": [
                    {
                        "id": 1,
                        "name": "Jo",
                        "tags": ["admin"]
                    },
                    {
                        "id": 2,
                        "name": "Alice",
                        "tags": ["user", "active"]
                    },
                    {
                        "id": 3,
                        "name": "Bob",
                        "tags": []
                    },
                    {
                        "id": 4,
                        "name": "Charlie",
                        "tags": ["admin"]
                    }
                ],
                "other": {
                    "mods": [
                        {
                            "id": 10,
                            "name": "XY",
                            "tags": ["mod"]
                        },
                        {
                            "id": 11,
                            "name": "Dave",
                            "tags": ["mod", "senior"]
                        }
                    ]
                }
            }
        );
        let nodelist = root.select(path_expr).unwrap();
        let vals = vec![
            json!(2),
            json!("Alice"),
            json!(4),
            json!("Charlie"),
            json!(11),
            json!("Dave"),
        ];
        let expected = vec![
            Node {
                path: String::from("$['users'][1]['id']"),
                val: &vals[0],
            },
            Node {
                path: String::from("$['users'][1]['name']"),
                val: &vals[1],
            },
            Node {
                path: String::from("$['users'][3]['id']"),
                val: &vals[2],
            },
            Node {
                path: String::from("$['users'][3]['name']"),
                val: &vals[3],
            },
            Node {
                path: String::from("$['other']['mods'][1]['id']"),
                val: &vals[4],
            },
            Node {
                path: String::from("$['other']['mods'][1]['name']"),
                val: &vals[5],
            },
        ];

        assert_eq!(expected.len(), nodelist.len());
        for (i, val) in expected.into_iter().enumerate() {
            assert_eq!(val, nodelist[i], "invalid path: {path_expr}");
        }
    }

    #[test]
    fn test_npaths() {
        let path_expr = "$.*";
        let root = json!(
            {
                "\\": 0,
                "\n": 1,
                "\u{001F}": 2,
                "": 3
            }
        );
        let nodelist = root.select_as_npaths(path_expr).unwrap();
        let expected_paths = vec!["$['\\\\']", "$['\\n']", "$['\\u001f']", "$['']"];

        assert_eq!(expected_paths.len(), nodelist.len());
        for (i, path) in expected_paths.into_iter().enumerate() {
            assert_eq!(path, nodelist[i], "invalid path at index {i}");
        }
    }

    // patch
    #[test]
    fn test_modify() {
        let mut root = json!({"foo": 1, "bar": 2, "buzz": 3});
        let actual = json!({"foo": 100, "bar": 4, "buzz": 3});

        // the first two operations modify the root, but the last one fails. Changes are maintained
        let input = r#"[
            {"op": "replace", "path": "/foo", "value": 100},
            {"op": "add", "path": "/bar", "value": 4},
            {"op": "remove", "path": "/:)"}
        ]"#;
        let _res = root.modify(input);
        assert_eq!(root, actual);
    }

    #[test]
    fn test_try_modify() {
        let mut root = json!({"foo": 1, "bar": 2, "buzz": 3});
        let actual = root.clone();

        // the first two operations modify the root, but the last one fails. Changes are rolled back
        let input = r#"[
            {"op": "replace", "path": "/foo", "value": 100},
            {"op": "add", "path": "/bar", "value": 4},
            {"op": "remove", "path": "/:)"}
        ]"#;
        let _res = root.try_modify(input);
        assert_eq!(root, actual);
    }
}
