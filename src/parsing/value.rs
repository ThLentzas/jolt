use crate::parsing::number::Number;
use crate::parsing::value::error::{PathError, PointerError};
use crate::parsing::value::path::Query;
use crate::parsing::value::pointer::Pointer;
use indexmap::IndexMap; // toDo: consider moving to a linked hash map because deletions are slow
use std::cmp::{Ordering, PartialEq};

mod error;
mod path;
mod pointer;

// Clone is needed for Cow
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    // why this recursive type does not need a box? containers handle it for us?
    Object(IndexMap<String, Value>),
    Array(Vec<Value>),
    Number(Number),
    String(String),
    Boolean(bool),
    Null,
} // toDo: pretty print https://crates.io/crates/pprint

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

    // in a path /foo/bar/1 we don't know if 1 is an index or a key. If the current value is an object
    // it is treated as a key, if an array as an index
    //
    // starting from the root value, for every token created by split() we check with the current val
    //
    // -if val is an object then the token's value must be a key of that object
    // -if val is an array then the token's value must be an unsigned base-10 integer value
    // -any other type we return None
    //
    // we update val and move on to the next token
    //
    // path: "/users/1/name", val = "{"users": [{"name": "Alice"}, {"name": "Bob"}]}
    //
    // 3 tokens are generated: "tokens", 1, "name"
    // "users" exists as key in val, update val with the value of the users key, val = [{"name": "Alice"}, {"name": "Bob"}]
    // 1 is an index and val is an array, update val with the value of at the given index, val = {"name": "Bob"}
    // "name" exists as key in val, update val with the value of the name key, val = "Bob"
    //
    // toDo: read Appendix C, pointer always returns 1 value, path expressions can return more than 1 and are more flexible
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

        // In the previous approach, we generated all the ref tokens and then iterated over the vector
        // calling match on current and returning a value if present. It worked, but we iterated twice
        // once the input buffer to generate all the tokens and once the vector of those tokens. We
        // can do better by generating a token at a time, check against the current value and only
        // if we find a value, we keep going.
        let mut current = self;
        loop {
            // We never check if number of tokens exceed the NestingDepthLimit because even if they did
            // we would get no match at NestingDepthLimit + 1 and we would return None
            match ptr.gen_ref_token()? {
                Some(token) => match current {
                    Value::Object(map) => match map.get(&token.val) {
                        Some(val) => current = val,
                        None => return Ok(None),
                    },
                    Value::Array(values) => match pointer::check_array_index(&token)? {
                        Some(index) if index >= values.len() => return Ok(None),
                        Some(index) => current = &values[index],
                        _ => return Ok(None),
                    },
                    _ => return Ok(None),
                },
                None => break,
            }
        }
        Ok(Some(current))
    }

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
        loop {
            // We never check if number of tokens exceed the NestingDepthLimit because even if they did
            // we would get no match at NestingDepthLimit + 1 and we would return None
            match ptr.gen_ref_token()? {
                Some(token) => match current {
                    Value::Object(map) => match map.get_mut(&token.val) {
                        Some(val) => current = val,
                        None => return Ok(None),
                    },
                    Value::Array(values) => match pointer::check_array_index(&token)? {
                        Some(index) if index >= values.len() => return Ok(None),
                        Some(index) => current = &mut values[index],
                        _ => return Ok(None),
                    },
                    _ => return Ok(None),
                },
                None => break,
            }
        }
        Ok(Some(current))
    }

    pub fn read(&self, path_expr: &str) -> Result<Vec<&Value>, PathError> {
        let mut query = Query::new(path_expr.as_bytes(), self);
        Ok(query.parse()?)
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
            (Value::Object(map_1), Value::Object(map_2)) => {
                if map_1 == map_2 {
                    Some(Ordering::Equal)
                } else {
                    None
                }
            }
            (Value::Array(vec_1), Value::Array(vec_2)) => {
                if vec_1 == vec_2 {
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

// In Rust, we are allowed to:
//  -implement our trait for any type
//  -implement any trait for our type
//
// We are not allowed to implement a trait for a type if neither the trait nor the type is defined
// in the current crate(our project). We can't not implement Display for u32.
// It is called the Orphan Rule: https://ianbull.com/notes/rusts-orphan-rule/
//
// used by the json!()
// toDo: if this part of the public api can someone call .from() and pass an invalid json string?
impl From<&str> for Value {
    fn from(val: &str) -> Self {
        Value::String(val.to_string())
    }
}

impl From<u8> for Value {
    fn from(val: u8) -> Self {
        Value::Number(Number::from(val as u64))
    }
}

impl From<i8> for Value {
    fn from(val: i8) -> Self {
        Value::Number(Number::from(val as i64))
    }
}

impl From<u16> for Value {
    fn from(val: u16) -> Self {
        Value::Number(Number::from(val as u64))
    }
}

impl From<i16> for Value {
    fn from(val: i16) -> Self {
        Value::Number(Number::from(val as i64))
    }
}

impl From<u32> for Value {
    fn from(val: u32) -> Self {
        Value::Number(Number::from(val as u64))
    }
}

impl From<i32> for Value {
    fn from(val: i32) -> Self {
        Value::Number(Number::from(val as i64))
    }
}

impl From<u64> for Value {
    fn from(val: u64) -> Self {
        Value::Number(Number::from(val))
    }
}

impl From<i64> for Value {
    fn from(val: i64) -> Self {
        Value::Number(Number::from(val))
    }
}

impl From<f32> for Value {
    fn from(val: f32) -> Self {
        Value::Number(Number::from(val as f64))
    }
}

impl From<f64> for Value {
    fn from(val: f64) -> Self {
        Value::Number(Number::from(val))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::macros::json;
    use crate::parsing::error::{StringError, StringErrorKind};
    use crate::parsing::value::error::PointerErrorKind;

    // We could also move those tests to pointer and break it down to the 3 methods called by pointer()
    // check_start(), gen_ref_token(), check_array_index()
    fn invalid_pointer_paths() -> Vec<(&'static str, PointerError)> {
        vec![
            // does not start with '/'
            (
                "foo/bar",
                PointerError {
                    kind: PointerErrorKind::InvalidPointerSyntax,
                    pos: 0,
                },
            ),
            // does not start with the Unicode sequence of  '/'
            (
                "\\u005E",
                PointerError {
                    kind: PointerErrorKind::InvalidPointerSyntax,
                    pos: 0,
                },
            ),
            // unpaired pointer escape
            (
                "/foo/bar~",
                PointerError::from(StringError {
                    kind: StringErrorKind::UnexpectedEndOf,
                    pos: 8,
                }),
            ),
            // unknown pointer escape
            (
                "/foo/bar~3",
                PointerError::from(StringError {
                    kind: StringErrorKind::UnknownEscapedCharacter { byte: b'3' },
                    pos: 9,
                }),
            ),
            // ~e
            (
                "/foo/bar~\\u0065",
                PointerError::from(StringError {
                    kind: StringErrorKind::UnknownEscapedCharacter { byte: b'\\' },
                    pos: 9,
                }),
            ),
            // passing it as "/\u{007e}" is wrong because this is not how we use Unicode sequence in json strings
            // the parser has to map the sequence to the character
            // unpaired pointer escape where '~' is represented as Unicode sequence
            (
                "/\\u007e",
                PointerError::from(StringError {
                    kind: StringErrorKind::UnexpectedEndOf,
                    pos: 6,
                }),
            ),
            // unknown pointer escape where '~' is represented as Unicode
            (
                "/\\u007e4",
                PointerError::from(StringError {
                    kind: StringErrorKind::UnknownEscapedCharacter { byte: b'4' },
                    pos: 7,
                }),
            ),
            // // unknown pointer escape where '~' and the next character are represented as Unicode sequences
            (
                "/\\u007e\\u0065",
                PointerError::from(StringError {
                    kind: StringErrorKind::UnknownEscapedCharacter { byte: b'\\' },
                    pos: 7,
                }),
            ),
            (
                "/foo/+1",
                PointerError {
                    kind: PointerErrorKind::InvalidIndex {
                        message: "index can not be prefixed with a sign",
                    },
                    pos: 5,
                },
            ),
            (
                "/foo/01",
                PointerError {
                    kind: PointerErrorKind::InvalidIndex {
                        message: "leading zeros are not allowed",
                    },
                    pos: 5,
                },
            ),
            (
                "/foo/+",
                PointerError {
                    kind: PointerErrorKind::InvalidIndex {
                        message: "invalid array index",
                    },
                    pos: 5,
                },
            ),
        ]
    }

    // path, source, result
    fn valid_pointer_paths() -> Vec<(&'static str, Value, Option<Value>)> {
        vec![
            (
                "",
                json!(
                    {
                        "foo": "bar"
                    }
                ),
                Some(json!(
                    {
                        "foo": "bar"
                    }
                )),
            ),
            (
                "/foo/1",
                json!(
                    {
                        "foo": [false, null]
                    }
                ),
                Some(json!(null)),
            ),
            (
                "//1",
                json!(
                    {
                        "": [false, null]
                    }
                ),
                Some(json!(null)),
            ),
            (
                "/foo//0",
                json!(
                {
                    "foo": {
                        "": [true]
                    }
                }),
                Some(json!(true)),
            ),
            // Rust defaults integer literals to i32
            (
                "/foo~1bar/baz",
                json!(
                {
                    "foo/bar": {
                        "baz":  9223372036854775808u64
                    }
                }),
                Some(json!(9223372036854775808u64)),
            ),
            (
                "/foo~0bar/baz",
                json!(
                {
                    "foo~bar": {
                        "baz": ":)"
                    }
                }),
                Some(json!(":)")),
            ),
            (
                "/foo~\\u0030bar/baz",
                json!(
                {
                    "foo~bar": {
                        "baz": ":)"
                    }
                }),
                Some(json!(":)")),
            ),
            (
                "/foo\\u007e0bar/baz",
                json!(
                {
                    "foo~bar": {
                        "baz": ":)"
                    }
                }),
                Some(json!(":)")),
            ),
            (
                "/foo\\u007e\\u0030bar/baz",
                json!(
                {
                    "foo~bar": {
                        "baz": ":)"
                    }
                }),
                Some(json!(":)")),
            ),
            // index out of bounds
            ("/2", json!([2]), None),
            // unparsable index starting with a digit leads to None
            ("/2e", json!([2]), None),
            // '-' as array index always returns none according to spec
            ("/-", json!([2, 3]), None),
            (
                "/é",
                json!(
                {
                    "é": false
                }),
                Some(json!(false)),
            ),
            (
                "/1",
                json!(
                {
                    "foo": "bar"
                }),
                None,
            ),
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

    fn valid_filter() -> Vec<(&'static str, Value, Vec<Value>)> {
        vec![
            // // test expression with an embedded relative query with 0 segments
            // //
            // // try to apply the filter selector on the root; we can since it is an object
            // // @ adds the current as root; in this case both current and root are the same
            // // tries to apply the segments we don't have any and our list returns 1 entry
            // // because the test expression returned true for the current we add current to the list
            // // for maps we add their values, for arrays their elements
            // (
            //     "$[?@]",
            //     json!(
            //     {
            //         "foo": "bar",
            //         "buzz": 0
            //     }),
            //     vec![json!("bar"), json!(0)],
            // ),
            // // test expression with an embedded relative query with 1 segment, 1 selector(name shorthand)
            // //
            // // iterate through the array and for each element try to apply
            // // if the query returns any result, we add the current element into our return list
            // //
            // // 1st element -> add the root(very important), in this case we add the object and
            // // then try to apply .foo in the input list(reader) we add "bar" because it has a key
            // // named foo, no more segment to process, subquery returns a non-empty list it evaluates
            // // to true, and we add the 1st element of the array into our list
            // //
            // // 2nd element -> add the root, the subquery returns empty list, it evaluates to false
            // // move to the next element
            // //
            // // 3rd element -> can't apply filter selector to numbers we return the list containing
            // // the 1st element.
            // (
            //     "$[?@.foo]",
            //     // this is an array of objects, initially I would wrap every entry into json!()
            //     // but I was wrong because this is what the macro does
            //     // ([ $($elem:tt),+ $(,)? ]) => { $crate::Value::Array(vec![$(json!($elem)),+]) };
            //     // it takes elem and wraps it in json!()
            //     json!([
            //         {
            //             "foo": "bar",
            //             "buzz": 0
            //         },
            //         {
            //             "bar": "foo"
            //         },
            //         3
            //     ]),
            //     vec![json!(
            //         {
            //             "foo": "bar",
            //             "buzz": 0
            //         }
            //
            //     )],
            // ),
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
            // (
            //     "$[?$]",
            //     json!([
            //         {
            //             "foo": "bar",
            //             "buzz": 0
            //         },
            //         {
            //             "bar": "foo"
            //         },
            //         3
            //     ]),
            //     vec![
            //         json!(
            //             {
            //                 "foo": "bar",
            //                 "buzz": 0
            //             }
            //         ),
            //         json!(
            //             {
            //                 "bar": "foo"
            //             }
            //         ),
            //         json!(3)
            //     ],
            // ),
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
            // (
            //     "$[?$.*[0]]",
            //     json!([
            //         {
            //             "foo": "bar",
            //             "buzz": 0
            //         },
            //         {
            //             "bar": "foo"
            //         },
            //         [3]
            //     ]),
            //     vec![
            //         json!(
            //         {
            //             "foo": "bar",
            //             "buzz": 0
            //         }),
            //         json!(
            //         {
            //             "bar": "foo"
            //         }),
            //         json!([3]),
            //     ],
            // ),
        ]
    }

    // toDo: in multiple selectors include cases where we get duplicate nodes

    // maybe instead of expected/actual change it to left/right?
    #[test]
    fn test_valid_pointer_paths() {
        for (path, source, expected) in valid_pointer_paths() {
            let actual = source.pointer(path);
            assert_eq!(actual, Ok(expected.as_ref()));
        }
    }

    #[test]
    fn test_invalid_pointer_paths() {
        let val = json!({ "foo": [1] });

        for (path, err) in invalid_pointer_paths() {
            // problem: if the call to pointer() returned Ok() the test passes but that is not what we want
            // if let Err(e) = val.pointer(path) {
            //     assert_eq!(e, err);
            // }
            let result = val.pointer(path);
            assert_eq!(result, Err(err), "invalid path: {path}");
        }
    }

    // jpath
    #[test]
    fn test_valid_names() {
        for (path_expr, source, nodelist) in valid_names() {
            let res = source.read(path_expr).unwrap();

            assert_eq!(res.len(), nodelist.len());
            for (i, val) in res.into_iter().enumerate() {
                assert_eq!(*val, nodelist[i], "invalid path: {path_expr}");
            }
        }
    }

    #[test]
    fn test_valid_indices() {
        for (path_expr, source, nodelist) in valid_indices() {
            let res = source.read(path_expr).unwrap();

            assert_eq!(res.len(), nodelist.len());
            for (i, val) in res.into_iter().enumerate() {
                assert_eq!(*val, nodelist[i], "invalid path: {path_expr}");
            }
        }
    }

    #[test]
    fn test_valid_slices() {
        for (path_expr, source, nodelist) in valid_slices() {
            let res = source.read(path_expr).unwrap();

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
            "key_1": "value_1"
        });
        let res = val.read("$.*").unwrap();

        if let Value::Object(ref map) = val {
            let expected: Vec<&Value> = map.values().collect();
            assert_eq!(res, expected);
        }
        // maybe panic! here with an else because the test will pass if val is not an object
    }

    #[test]
    fn wildcard_on_array() {
        let val = json!([1, 5, 3, 9]);
        let res = val.read("$[*]").unwrap();

        if let Value::Array(ref vec) = val {
            let expected: Vec<&Value> = vec.iter().collect();
            assert_eq!(res, expected);
        }
    }

    #[test]
    fn test_valid_filters() {
        for (path_expr, source, nodelist) in valid_filter() {
            let res = source.read(path_expr).unwrap();

            assert_eq!(res.len(), nodelist.len());
            for (i, val) in res.into_iter().enumerate() {
                assert_eq!(*val, nodelist[i], "invalid path: {path_expr}");
            }
        }
    }
}
