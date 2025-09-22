use std::cmp::PartialEq;
use linked_hash_map::LinkedHashMap;
use crate::parsing::error::MalformedStringError;
use crate::parsing::number::Number;
use crate::parsing::escapes;

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

    pub fn pointer(&self, pointer: &str) -> Option<&Value> {
        // match self {
        //     Value::Object(_) | Value::Array(_) => {
        //
        //     }
        //     _ => return None
        // }
        if !self.is_object() && !self.is_array() {
            return None;
        }

        let buffer = pointer.as_bytes();
        let mut index = 0;
        if buffer.is_empty() {
            return Some(self)
        }
        if buffer[0] != b'/' {
            return None;
        }

        let len = buffer.len();
        while index < len {
            let current = buffer[index];
            if current.is_ascii_control() {
                return Err(MalformedStringError::InvalidControlCharacter { byte: current, pos: self.pos });
            }
            if current == b'\\' {
                escapes::check_escape_character(buffer, &mut index)?;
            }
            index += 1;
        }


        None
    }
}