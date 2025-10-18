use std::cmp::PartialEq;
use indexmap::IndexMap;
use crate::parsing::error::{PointerError, PointerErrorKind};
use crate::parsing::utf8;
use crate::parsing::escapes::{self, EscapeError};
use crate::parsing::number::Number;

// This can't be String; to_string(), String::from()
// Constants must be known at compile time, and String operations like String::from() or
// string literal .to_string() require heap allocation, which can't happen at compile time
const ARRAY_INDEX_SYNTAX: &str = "%x30 / ( %x31-39 * (%x30-39) )";

// when we split the input pointer path to create tokens, apart from the value we also need to know
// where the token started based on the underline buffer for better error messaging
//
// we are building the token value based on the pointer path, but we will need to do replacements
// (replacing ~0 and ~1), RefTokens must own their data rather than referencing slices of the
// original pointer path.
struct RefToken {
    val: String,
    pos: usize
}

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

        let tokens: Vec<RefToken> = split(pointer)?;
        let mut current = self;
        for token in &tokens {
            match current {
                Value::Object(map) => {
                    if map.contains_key(&token.val) {
                        current = map.get(&token.val).unwrap();
                    } else {
                        return Ok(None);
                    }
                }
                Value::Array(values) => {
                    match check_array_index(token)? {
                        Some(index) if index >= values.len() => return Ok(None),
                        Some(index) => current = &values[index],
                        _ => return Ok(None),
                    }
                }
                _ => return  Ok(None)
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

        let tokens: Vec<RefToken> = split(pointer)?;
        let mut val: &mut Value = self;
        for token in &tokens {
            match val {
                Value::Object(map) => {
                    if map.contains_key(&token.val) {
                        val = map.get_mut(&token.val).unwrap();
                    } else {
                        return Ok(None);
                    }
                }
                Value::Array(values) => {
                    match check_array_index(token)? {
                        Some(index) if index >= values.len() => return Ok(None),
                        Some(index) => val = &mut values[index],
                        _ => return Ok(None),
                    }
                }
                _ => return  Ok(None)
            }
        }
        Ok(Some(val))
    }
}

// In Rust, we are allowed to:
//  -implement our trait for any type
//  -implement any trait for our type
//
// We are not allowed to implement a trait for a type if neither the trait nor the type is defined
// in the current crate(our project). We can't not implement Display for u32.
// It is called the Orphan Rule: https://ianbull.com/notes/rusts-orphan-rule/
// toDo: fall-through
impl From<&str> for Value {
    fn from(val: &str) -> Self {
        Value::String(val.to_string())
    }
}

impl From<u8> for Value {
    fn from(val: u8) -> Self {
        Value::Number(Number::from_u64(val as u64))
    }
}

impl From<i8> for Value {
    fn from(val: i8) -> Self {
        Value::Number(Number::from_i64(val as i64))
    }
}

impl From<u16> for Value {
    fn from(val: u16) -> Self {
        Value::Number(Number::from_u64(val as u64))
    }
}

impl From<i16> for Value {
    fn from(val: i16) -> Self {
        Value::Number(Number::from_i64(val as i64))
    }
}

impl From<u32> for Value {
    fn from(val: u32) -> Self {
        Value::Number(Number::from_u64(val as u64))
    }
}

impl From<i32> for Value {
    fn from(val: i32) -> Self {
        Value::Number(Number::from_i64(val as i64))
    }
}

impl From<u64> for Value {
    fn from(val: u64) -> Self {
        Value::Number(Number::from_u64(val))
    }
}

impl From<i64> for Value {
    fn from(val: i64) -> Self {
        Value::Number(Number::from_i64(val))
    }
}

impl From<f32> for Value {
    fn from(val: f32) -> Self {
        Value::Number(Number::from_f64(val as f64))
    }
}

impl From<f64> for Value {
    fn from(val: f64) -> Self {
        Value::Number(Number::from_f64(val))
    }
}


//  when we encounter '/' we treat it as a delimiter, and we create a token(apart from the 1st one)
//
//  if we have '/' as part of a key name we need to escape it, ~1 -> /
//  /foo/bar evaluates to: token_1: foo, token_2: bar but /foo~1bar evaluates to token: foo/bar
//  the order matters; if we map first and then try to split we will end up with the wrong tokens
//  /foo~1bar -> /foo/bar -> token_1: foo, token_2: bar which is incorrect
//  The same logic applies if we want to escape `~` we write it as `~0`
//
//  To handle this correctly, we must process the input character-by-character in a single pass:
//
//  For input "/foo~1bar", we:
//  1. Skip the initial '/' delimiter
//  2. Build the current token character by character
//  3. When we hit '~1', we immediately convert it to '/' and add to token: "foo/"
//  4. Continue building the same token: "foo/b", "foo/ba", "foo/bar"
//  5. When we hit the end of input (or another unescaped '/'), we finalize the token
//
//  This way the '/' from ~1 becomes part of the token's content rather than being mistaken for a
//  delimiter. We never rescan or reprocess characters, each character in the input is examined
//  exactly once. We handle cases like "~01" where "~0" becomes '~' and then "~1" becomes "/", "~01"
//  is just "~1"
fn split(pointer: &str) -> Result<Vec<RefToken>, PointerError> {
    let buffer = pointer.as_bytes();
    // neither '/', nor a Unicode sequence that could map to '/'
    // a pointer path always starts with '/', unless it is empty which we already checked by the time
    // this method gets called
    if buffer[0] != b'/' && buffer[0] != b'\\' {
        return Err(PointerError::new(PointerErrorKind::InvalidPathSyntax, 0));
    }

    let mut i = 0;
    // was an escape sequence but not '/'
    if buffer[0] == b'\\' {
        escapes::check_escape_character(buffer, i)?;
        if escapes::map_escape_character(buffer, i) != '/' {
            return Err(PointerError::new(PointerErrorKind::InvalidPathSyntax, 0));
        }
    }

    i = 1; // skip the opening '/'
    let mut start = 1;
    let mut token = String::new();
    let mut tokens = Vec::new();
    // similar validation to Json String, the only difference is that because pointer is &str,
    // it is guaranteed that any utf8 byte sequence we encounter is always valid so we can skip
    // that step https://doc.rust-lang.org/std/primitive.str.html
    while i < buffer.len() {
        let current = buffer[i];
        match current {
            c if c.is_ascii_control() => return Err(PointerError::new(PointerErrorKind::InvalidControlCharacter { byte: current }, i)),
            b'/' => {
                // empty strings are allowed as keys in objects("//foo/bar"); for an
                // empty string there is no starting index so we set pos to usize:MAX
                // if start and i are the at the same index and the current char is '/' it means we
                // encountered an empty string as key
                let pos = if start == i { usize::MAX } else { start };
                tokens.push(RefToken { val: token, pos });
                token = String::new();
                start = i + 1;
            }
            b'\\' => {
                escapes::check_escape_character(buffer, i)?;
                let ch = escapes::map_escape_character(buffer,  i);
                i += escapes::len(buffer, i) - 1;
                if ch == '~' {
                    // check the next character in the buffer after the Unicode sequence mapped to `~`
                    token.push(check_pointer_escape(buffer, &mut i)?);
                }
            }
            b'~' => token.push(check_pointer_escape(buffer, &mut i)?),
            c if c.is_ascii() => token.push(c as char),
            _ => {
                let width = utf8::utf8_char_width(current);
                token.push_str(str::from_utf8(&buffer[i..i + width]).unwrap());
                // without - 1 we would move i to the next character after the sequence then i += 1;
                // gets executed, and we skip a character
                i += width - 1;
            }
        }
        i += 1;
    }
    // last token
    tokens.push(RefToken { val: token, pos: start });

    Ok(tokens)
}

// When this method gets called buffer[pos] is at '~', we only need to check the next character
// This method is only used by our pointer, and we can mutate the pos index. In escapes and utf8,
// we don't mutate the index we just advance after the method returns accordingly
fn check_pointer_escape(buffer: &[u8], pos: &mut usize) -> Result<char, EscapeError> {
    let next = buffer.get(*pos + 1);

    // advance the pointer of the buffer to the next character
    match next {
        Some(b'0') => {
            *pos += 1;
            Ok('~')
        }
        Some(b'1') => {
            *pos += 1;
            Ok('/')
        }
        Some(b'\\') => {
            // an escape sequence that could map to '0' or '1'
            escapes::check_escape_character(buffer, *pos)?;
            let ch = escapes::map_escape_character(buffer, *pos + 1);
            match ch {
                '0' => {
                    *pos += 6;
                    Ok('~')
                },
                '1' => {
                    *pos += 6;
                    Ok('/')
                }
                _ => Err(EscapeError::UnknownEscapedCharacter { byte: b'\\', pos: *pos + 1 })
            }
        }
        // ~8
        Some(b) => Err(EscapeError::UnknownEscapedCharacter { byte: *b, pos: *pos + 1 }),
        // ~ unpaired
        None => Err(EscapeError::UnexpectedEof { pos: *pos } )
    }
}

fn check_array_index(token: &RefToken) -> Result<Option<usize>, PointerError> {
    let first = token.val.chars().nth(0);
    let second = token.val.chars().nth(1);

    match (first, second) {
        (Some('+') | Some('-'), Some('0'..='9')) => return Err(PointerError::new(
            PointerErrorKind::InvalidIndex { message: format!("index can not be prefixed with a sign, syntax : {}", ARRAY_INDEX_SYNTAX) },
            token.pos)),
        (Some('0'), Some('0'..='9')) => return Err(PointerError::new(
            PointerErrorKind::InvalidIndex { message: format!("leading zeros are not allowed, syntax: {}", ARRAY_INDEX_SYNTAX) },
            token.pos)),
        (Some('-'), None) => return Ok(None),
        // if the token val does not start with a digit, it is invalid
        (Some(d), _) if !d.is_ascii_digit() => return Err(PointerError::new(
            PointerErrorKind::InvalidIndex { message: format!("invalid array index, syntax: {}", ARRAY_INDEX_SYNTAX) },
            token.pos)),
        _ => ()
    }

    // token had a digit as the first character, but it was not parsable to usize, we treat that
    // as not found case, not an error
    match token.val.parse::<usize>() {
        Ok(index) => Ok(Some(index)),
        Err(_) => Ok(None),
    }
}

#[cfg(test)]
mod tests {
    use crate::macros::json;
    use super::*;

    fn invalid_paths() -> Vec<(&'static str, PointerError)> {
        vec![
            // does not start with '/'
            ("foo/bar", PointerError::new(PointerErrorKind::InvalidPathSyntax, 0)),
            // does not start with the Unicode sequence of  '/'
            ("\\u005E",PointerError::new(PointerErrorKind::InvalidPathSyntax, 0)),
            // unpaired pointer escape
            ("/foo/bar~", PointerError::new(PointerErrorKind::UnexpectedEof, 8)),
            // unknown pointer escape
            ("/foo/bar~3", PointerError::new(PointerErrorKind::UnknownEscapedCharacter { byte: b'3' }, 9)),
            // ~e
            ("/foo/bar~\\u0065", PointerError::new(PointerErrorKind::UnknownEscapedCharacter { byte: b'\\' }, 9)),
            // passing it as "/\u{007e}" is wrong because this is not how we use Unicode sequence in json strings
            // the parser has to map the sequence to the character
            // unpaired pointer escape where '~' is represented as Unicode sequence
            ("/\\u007e", PointerError::new(PointerErrorKind::UnexpectedEof, 6)),
            // unknown pointer escape where '~' is represented as Unicode
            ("/\\u007e4", PointerError::new(PointerErrorKind::UnknownEscapedCharacter { byte: b'4' }, 7)),
            // // unknown pointer escape where '~' and the next character are represented as Unicode sequences
            ("/\\u007e\\u0065", PointerError::new(PointerErrorKind::UnknownEscapedCharacter { byte: b'\\' }, 7)),
            ("/foo/+1",PointerError::new(PointerErrorKind::InvalidIndex { message: format!("index can not be prefixed with a sign, syntax : {}", ARRAY_INDEX_SYNTAX) }, 5)),
            ("/foo/01", PointerError::new(PointerErrorKind::InvalidIndex { message: format!("leading zeros are not allowed, syntax: {}", ARRAY_INDEX_SYNTAX) }, 5)),
            ("/foo/+", PointerError::new(PointerErrorKind::InvalidIndex { message: format!("invalid array index, syntax: {}", ARRAY_INDEX_SYNTAX) }, 5))
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
            ("/-", json!([2,3]), None),
            ("/é", json!({ "é": false }), Some(json!(false))),
            ("/1", json!({ "foo": "bar" }), None),
        ]
    }

    #[test]
    fn test_valid_paths() {
        for (path, val, res) in valid_paths() {
            let result = val.pointer(path);
            assert_eq!(result, Ok(res.as_ref()), "invalid path: {path}");
        }
    }

    #[test]
    fn test_invalid_paths() {
        let val = json!({ "foo": [1] });

        for(path, error) in invalid_paths() {
            // problem: if the call to pointer() returned Ok() the test passes but that is not what we want
            // if let Err(e) = val.pointer(path) {
            //     assert_eq!(e, err);
            // }
            let result = val.pointer(path);
            assert_eq!(result, Err(error), "invalid path: {path}");
        }

    }
}