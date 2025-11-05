use std::cmp::PartialEq;
use std::collections::VecDeque;
use indexmap::IndexMap;
use crate::parsing::number::Number;
use crate::parsing::value::error::{PathError, PointerError};
use crate::parsing::value::path::{Query, Segment, SegmentKind, Selector};
use crate::parsing::value::pointer::Pointer;

mod path;
mod pointer;
mod error;

#[derive(Debug, PartialEq)]
pub enum Value {
    // better reads from a LinkedHashMap, worse in removals
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

    pub fn as_boolean(&self) -> Option<&bool>{
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
                Some(token) => {
                    match current {
                        Value::Object(map) => {
                            match map.get(&token.val) {
                                Some(val) => current = val,
                                None => return Ok(None)
                            }
                        }
                        Value::Array(values) => {
                            match pointer::check_array_index(&token)? {
                                Some(index) if index >= values.len() => return Ok(None),
                                Some(index) => current = &values[index],
                                _ => return Ok(None),
                            }
                        }
                        _ => return  Ok(None)
                    }
                }
                None => break
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
                Some(token) => {
                    match current {
                        Value::Object(map) => {
                            match map.get_mut(&token.val) {
                                Some(val) => current = val,
                                None => return Ok(None)
                            }
                        }
                        Value::Array(values) => {
                            match pointer::check_array_index(&token)? {
                                Some(index) if index >= values.len() => return Ok(None),
                                Some(index) => current = &mut values[index],
                                _ => return Ok(None),
                            }
                        }
                        _ => return  Ok(None)
                    }
                }
                None => break
            }
        }
        Ok(Some(current))
    }

    // toDo: A JSONPath implementation MUST raise an error for any query that is not well-formed and
    // valid. The well-formedness and the validity of JSONPath queries are independent of the JSON
    // value the query is applied to. No further errors relating to the well-formedness and the
    // validity of a JSONPath query can be raised during application of the query to a value.
    pub fn read(&self, path_expr: &str) -> Result<Vec<&Value>, PathError> {
        let mut query = Query::new(path_expr.as_bytes());
        let mut nodelist = VecDeque::new();
        nodelist.push_back(self);

        // read_seg() returns the segment, so the variable we define takes ownership, we don't need a ref
        // to the segment, the returned segment is not part of the Query
        // note that this is the only place that we get an error, when we process the expression,
        // any other case, like calling a selector in a non-container node returns an empty vec
        while let Some(segment) = query.read_seg()? {
            match segment.kind {
                SegmentKind::Child => {
                    apply_child_segment(&mut nodelist, &segment);
                }
                SegmentKind::Descendant => {
                    apply_descendant_segment(&mut nodelist, &segment);
                }
            }
        }
        Ok(nodelist.into())
    }
}

// child segments
//
//{
//   "users": [
//     {
//       "name": "Alice",
//       "age": 30,
//       "city": "NYC"
//     },
//     {
//       "name": "Bob",
//       "age": 25,
//       "city": "LA"
//     },
//     {
//       "name": "Charlie",
//       "age": 35,
//       "city": "Chicago"
//     }
//   ]
// }
//
// $.users[*]['name', 'city']
//
// 1. .users returns the array
// 2. [*] is called on the array, and it returns all the elements
// 3. for every element in the nodelist the input list we apply each selector, in this case we have
// 2 name selectors.
// output: ["]Alice", "NYC", "Bob", "LA", "Charlie", "Chicago"]
// It is important to note that from the output we see that we applied all the selector for the 1st
// element we got back "Alice", "NYC" and so on
//
// applying each segment essentially increases the depth by 1 level
fn apply_child_segment(nodelist: &mut VecDeque<&Value>, segment: &Segment) {
    if nodelist.is_empty() {
        return;
    }

    let initial_count = nodelist.len();
    for i in 0..initial_count  {
        for selector in &segment.selectors {
            apply_selector(nodelist, selector, nodelist[i]);
        }
    }
    // this is similar to iterating from 0 to initial_count and calling pop_front()
    nodelist.drain(..initial_count);
}

fn apply_descendant_segment(nodelist: &mut VecDeque<&Value>, segment: &Segment) {
    if nodelist.is_empty() {
        return;
    }

    let initial_count = nodelist.len();
    for i in 0..initial_count {
        for selector in &segment.selectors {
            recursive_descendant(nodelist, nodelist[i], selector);
        }
    }
    // this is similar to iterating from 0 to initial_count and calling pop_front()
    nodelist.drain(..initial_count);
}

fn recursive_descendant<'a>(nodelist: &mut VecDeque<&'a Value>, root: &'a Value, selector: &Selector) {
    if !root.is_object() && !root.is_array() {
        return;
    }

    apply_selector(nodelist, selector, root);
    match root {
        Value::Object(map) => {
            for entry in map.values() {
                recursive_descendant(nodelist, entry, selector);
            }
        }
        Value::Array(arr) => {
            for elem in arr {
                recursive_descendant(nodelist, elem, selector);
            }
        }
        _ => unreachable!()
    }
}

// If we don't specify the lifetime, Rust due to lifetime elision will assign lifetimes to the references,
// but we will get an error when we try to do nodelist.extend(map.values()); with an error 'lifetime may not live long enough'
// Without explicitly specifying the lifetime, our function signature does not require the passed in
// value to live as long as the values referenced by nodelist
//
// note that not all references get that lifetime, just the contents of VecDeque and value, because
// those are the two we want to tie together
//
// a simplified version

// let mut vec: Vec<&str> = vec![];
//
//     {
//         let s = String::from("hello");
//         let s_ref = s.as_str();
//         vec.push(s_ref);  // s doesn't live long enough
//     }  // s drops here
//
//     // vec would contain dangling reference
fn apply_selector<'a>(nodelist: &mut VecDeque<&'a Value>, selector: &Selector, value: &'a Value) {
    match (value, selector) {
        (Value::Object(map), Selector::Name(name)) => {
            if let Some(val) = map.get(name) {
                nodelist.push_back(val);
            }
        }
        // map.values() returns an iterator over &Value, extend() adds all items from the iterator to values
        (Value::Object(map), Selector::WildCard) => {
            nodelist.extend(map.values());
        }
        // arr.iter() returns an iterator over &Value, extend() adds all items to values
        (Value::Array(arr), Selector::WildCard) => {
            nodelist.extend(arr.iter());
        }
        (Value::Array(arr), Selector::Index(index)) => {
            let n_idx = path::normalize_index(*index, arr.len());
            if let Some(val) = arr.get(n_idx) {
                nodelist.push_back(val);
            }
        }
        (Value::Array(arr), Selector::Slice { start, end, step}) => {
            let (lower, upper, step) = path::slice_bounds(*start, *end, *step, arr.len());
        }
        // queries can only be applied to containers(object, array)
        _ => (),
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
    use crate::macros::json;
    use crate::parsing::error::{StringError, StringErrorKind};
    use crate::parsing::value::error::PointerErrorKind;
    use super::*;

    // We could also move those tests to pointer and break it down to the 3 methods called by pointer()
    // check_start(), gen_ref_token(), check_array_index()
    fn invalid_paths() -> Vec<(&'static str, PointerError)> {
        vec![
            // does not start with '/'
            ("foo/bar", PointerError::new(PointerErrorKind::InvalidPathSyntax, 0)),
            // does not start with the Unicode sequence of  '/'
            ("\\u005E",PointerError::new(PointerErrorKind::InvalidPathSyntax, 0)),
            // unpaired pointer escape
            ("/foo/bar~", PointerError::from(StringError { kind: StringErrorKind::UnexpectedEndOf, pos: 8 })),
            // unknown pointer escape
            ("/foo/bar~3", PointerError::from(StringError { kind: StringErrorKind::UnknownEscapedCharacter { byte: b'3' }, pos: 9 })),
            // ~e
            ("/foo/bar~\\u0065", PointerError::from(StringError { kind: StringErrorKind::UnknownEscapedCharacter { byte: b'\\' }, pos: 9 })),
            // passing it as "/\u{007e}" is wrong because this is not how we use Unicode sequence in json strings
            // the parser has to map the sequence to the character
            // unpaired pointer escape where '~' is represented as Unicode sequence
            ("/\\u007e", PointerError::from(StringError { kind: StringErrorKind::UnexpectedEndOf, pos: 6 })),
            // unknown pointer escape where '~' is represented as Unicode
            ("/\\u007e4", PointerError::from(StringError { kind: StringErrorKind::UnknownEscapedCharacter { byte: b'4' }, pos: 7 })),
            // // unknown pointer escape where '~' and the next character are represented as Unicode sequences
            ("/\\u007e\\u0065", PointerError::from(StringError { kind: StringErrorKind::UnknownEscapedCharacter { byte: b'\\' }, pos: 7 })),
            ("/foo/+1",PointerError::new(PointerErrorKind::InvalidIndex { message: format!("index can not be prefixed with a sign, syntax : {}", pointer::ARRAY_INDEX_SYNTAX) }, 5)),
            ("/foo/01", PointerError::new(PointerErrorKind::InvalidIndex { message: format!("leading zeros are not allowed, syntax: {}", pointer::ARRAY_INDEX_SYNTAX) }, 5)),
            ("/foo/+", PointerError::new(PointerErrorKind::InvalidIndex { message: format!("invalid array index, syntax: {}", pointer::ARRAY_INDEX_SYNTAX) }, 5))
        ]
    }

    // this could be a struct?
    fn valid_paths() -> Vec<(&'static str, Value, Option<Value>)> {
        vec![
            ("", json!({ "foo": "bar" }), Some(json!({ "foo": "bar" }))),
            ("/foo/1", json!({ "foo": [false, null] }), Some(json!(null))),
            ("//1", json!({ "": [false, null] }), Some(json!(null))),
            ("/foo//0", json!({ "foo": { "": [true] }}), Some(json!(true))),
            // Rust defaults integer literals to i32
            ("/foo~1bar/buzz", json!({ "foo/bar": { "buzz":  9223372036854775808u64 } }), Some(json!(9223372036854775808u64))),
            ("/foo~0bar/buzz", json!({ "foo~bar": { "buzz": ":)" } }), Some(json!(":)"))),
            ("/foo~\\u0030bar/buzz", json!({ "foo~bar": { "buzz": ":)" } }), Some(json!(":)"))),
            ("/foo\\u007e0bar/buzz", json!({ "foo~bar": { "buzz": ":)" } }), Some(json!(":)"))),
            ("/foo\\u007e\\u0030bar/buzz", json!({ "foo~bar": { "buzz": ":)" } }), Some(json!(":)"))),
            // index out of bounds
            ("/2", json!([2]), None),
            // unparsable index starting with a digit leads to None
            ("/2e", json!([2]), None),
            // '-' as array index always returns none according to spec
            ("/-", json!([2, 3]), None),
            ("/é", json!({ "é": false }), Some(json!(false))),
            ("/1", json!({ "foo": "bar" }), None),
        ]
    }

    // maybe instead of expected/actual change it to left/right?
    #[test]
    fn test_valid_paths() {
        for (path, val, expected) in valid_paths() {
            let actual = val.pointer(path);
            assert_eq!(actual, Ok(expected.as_ref()), "invalid path: {path}");
        }
    }

    #[test]
    fn test_invalid_paths() {
        let val = json!({ "foo": [1] });

        for(path, err) in invalid_paths() {
            // problem: if the call to pointer() returned Ok() the test passes but that is not what we want
            // if let Err(e) = val.pointer(path) {
            //     assert_eq!(e, err);
            // }
            let result = val.pointer(path);
            assert_eq!(result, Err(err), "invalid path: {path}");
        }
    }
}