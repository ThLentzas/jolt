use std::cmp::PartialEq;
use linked_hash_map::LinkedHashMap;
use crate::parsing::error::PointerError;
use crate::parsing::{escapes, utf8};
use crate::parsing::number::Number;

// This can't be String; to_string(), String::from() won't work
// Constants (const) must be known at compile time, and String operations like String::from() or
// string literal .to_string() require heap allocation, which can't happen at compile time
const ARRAY_INDEX_SYNTAX: &str = "%x30 / ( %x31-39 * (%x30-39) )";

struct RefToken {
    val: String,
    pos: usize
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Object(LinkedHashMap<String, Value>),
    Array(Vec<Value>),
    Number(Number),
    String(String),
    Boolean(bool),
    Null,
}

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

    pub fn as_object(&self) -> Option<&LinkedHashMap<String, Value>> {
        match self {
            Value::Object(map) => Some(map),
            _ => None,
        }
    }

    pub fn as_object_mut(&mut self) -> Option<&mut LinkedHashMap<String, Value>> {
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

    pub fn pointer(&self, pointer: &str) -> Result<Option<&Value>, PointerError> {
        if !self.is_object() && !self.is_array() {
            return Ok(None);
        }

        if pointer.is_empty() {
            return Ok(Some(self))
        }

        let tokens: Vec<RefToken> = check_pointer_path(pointer)?;
        let mut val: &Value = self;
        for token in &tokens {
            match val {
                Value::Object(map) => {
                    if map.contains_key(&token.val) {
                        val = map.get(&token.val).unwrap();
                    } else {
                        return Ok(None);
                    }
                }
                Value::Array(values) => {
                    match check_array_index(token)? {
                        Some(index) if index >= values.len() => return Ok(None),
                        Some(index) => val = &values[index],
                        _ => return Ok(None),
                    }
                }
                _ => return  Ok(None)
            }
        }
        Ok(Some(val))
    }

    pub fn pointer_mut(&mut self, pointer: &str) -> Result<Option<&mut Value>, PointerError> {
        if !self.is_object() && !self.is_array() {
            return Ok(None);
        }
        // ""
        if pointer.is_empty() {
            return Ok(Some(self))
        }

        let tokens: Vec<RefToken> = check_pointer_path(pointer)?;
        let mut val: &mut Value = self;
        for token in &tokens {
            // in a path /foo/bar/1 we don't know if 1 is an index or a key. If the current value
            // is an object it is treated as a key, if an array as an index
            match val {
                Value::Object(map) => {
                    // val = map.get(token).ok_or(None)?;
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

fn check_pointer_path(pointer: &str) -> Result<Vec<RefToken>, PointerError> {
    let buffer = pointer.as_bytes();
    // neither '/', nor a Unicode sequence that could map to '/'
    // a pointer path always starts with '/', unless it is empty which we already checked
    if buffer[0] != b'/' && buffer[0] != b'\\' {
        return Err(PointerError::InvalidPathSyntax { pos: 0 } );
    }

    let mut i = 0;
    // was an escape sequence but not '/'
    if buffer[0] == b'\\' {
        escapes::check_escape_character(buffer, i)?;
        if escapes::map_escape_character(buffer, i) != '/' {
            return Err(PointerError::InvalidPathSyntax { pos: 0 } );
        }
    }

    i = 1; // skip the opening '/'
    let mut start = 1;
    let mut token = String::new();
    let mut tokens = Vec::new();
    // similar validation to Json String, the only difference is that because pointer is &str
    // it is guaranteed that any utf8 byte sequence we encounter is always valid so we can skip
    // that step https://doc.rust-lang.org/std/primitive.str.html
    while i < buffer.len() {
        let current = buffer[i];
        match current {
            c if c.is_ascii_control() => return Err(PointerError::InvalidControlCharacter { byte: current, pos: i }),
            b'/' => {
                // empty strings are allowed as keys in objects("//foo/bar"); for an
                // empty string there is no starting index so we set pos to usize:MAX
                let pos = if start + 1 == i { usize::MAX } else { start };
                tokens.push(RefToken { val: token, pos });
                token = String::new();
                start = i + 1;
            }
            b'\\' => {
                escapes::check_escape_character(buffer, i)?;
                let ch = escapes::map_escape_character(buffer,  i);
                i += escapes::len(buffer, i) - 1;
                if ch == '~' {
                    // we need to check the next character in the buffer after the Unicode sequence
                    // mapped to `~`
                    token.push(check_pointer_escape(buffer, i)?);
                }
            }
            b'~' => token.push(check_pointer_escape(buffer, i)?),
            c if c.is_ascii() => {
                token.push(c as char);
            }
            _ => {
                let width = utf8::utf8_char_width(current);
                token.push_str(str::from_utf8(&buffer[i..i + width]).unwrap());
                i += width;
                continue;
            }
        }
        i += 1;
    }

    // add the last token
    tokens.push(RefToken { val: token, pos: start });

    Ok(tokens)
}

// When this method gets called buffer[pos] is at '~', we only need to check the next character
fn check_pointer_escape(buffer: &[u8], pos: usize) -> Result<char, PointerError> {
    let next = buffer.get(pos + 1);

    match next {
        Some(b'0') => Ok('~'),
        Some(b'1') => Ok('/'),
        Some(b'\\') => {
            escapes::check_escape_character(buffer, pos)?;
            match escapes::map_escape_character(buffer, pos) {
                '0' => Ok('~'),
                '1' => Ok('/'),
                _ => Err(PointerError::InvalidEscapeSequence { pos } )
            }
        }
        // ~8 or ~ invalid
        Some(_) => Err(PointerError::InvalidEscapeSequence { pos: pos + 1 } ),
        None => Err(PointerError::InvalidEscapeSequence { pos} )
    }
}

fn check_array_index(token: &RefToken) -> Result<Option<usize>, PointerError> {
    let first = token.val.chars().nth(0);
    let second = token.val.chars().nth(1);

    match (first, second) {
        (Some('+') | Some('-'), Some('0'..='9')) => return Err(PointerError::InvalidIndex {
            message: format!("index can not be prefixed with a sign, syntax : {}", ARRAY_INDEX_SYNTAX),
            pos: token.pos
        }),
        (Some('0'), Some('0'..='9')) => return Err(PointerError::InvalidIndex {
            message: format!("leading zeros are not allowed, syntax: {}", ARRAY_INDEX_SYNTAX),
            pos: token.pos
        }),
        (Some('-'), None) => return Ok(None),
        // if the token val does not start with a digit, it is invalid
        (Some(d), _) if !d.is_ascii_digit() => return Err(PointerError::InvalidIndex {
            message: format!("invalid array index, syntax: {}", ARRAY_INDEX_SYNTAX),
            pos: token.pos
        }),
        _ => ()
    }

    // token had a digit as the first character, but it was not parsable to usize -> we treat that
    // as not found case, not an error
    match token.val.parse::<usize>() {
        Ok(index) => Ok(Some(index)),
        Err(_) => Ok(None),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn invalid_paths() -> Vec<(&'static str, PointerError)> {
        vec![
            // does not start with '/'
            ("foo/bar", PointerError::InvalidPathSyntax { pos: 0 }),
            // does not start with the Unicode sequence of  '/'
            ("\\u005E", PointerError::InvalidPathSyntax { pos: 0 }),
            // unpaired pointer escape
            ("/foo/bar~", PointerError::InvalidEscapeSequence { pos: 8 }),
            // unknown pointer escape
            ("/foo/bar~3", PointerError::InvalidEscapeSequence { pos: 9 }),
            // unpaired pointer escape represented as Unicode sequence
            ("/\\u007e", PointerError::InvalidEscapeSequence { pos: 6 }),
            ("/\\u007e4", PointerError::InvalidEscapeSequence { pos: 7 }),
            ("/foo/+1", PointerError::InvalidIndex { message: format!("index can not be prefixed with a sign, syntax : {}", ARRAY_INDEX_SYNTAX), pos: 5 }),
            ("/foo/+", PointerError::InvalidIndex { message: format!("invalid array index, syntax: {}", ARRAY_INDEX_SYNTAX), pos: 5 }),
            ("/foo/01", PointerError::InvalidIndex { message: format!("leading zeros are not allowed, syntax: {}", ARRAY_INDEX_SYNTAX), pos: 5 })
        ]
    }

    // setup with a macro! it is much easier to create a value from a macro
    fn valid_paths() -> Vec<(&'static str, Value)> {
        vec![]
    }

    #[test]
    fn test_invalid_paths() {
        let mut map = LinkedHashMap::new();
        map.insert("foo".to_string(), Value::Array(vec![Value::Number(Number::from_u64(1))]));
        let val = Value::Object(map);

        for(path, err) in invalid_paths() {
            if let Err(e) = val.pointer(path) { // toDo: if the call to pointer() returned Ok() the test passes but that is not what we want
                assert_eq!(e, err);
            }
        }

    }
}