use crate::parsing::error::StringError;
use crate::parsing::number::Number;
use crate::{parsing, Value};
use indexmap::IndexMap;

impl From<&str> for Value {
    // if the input value is an invalid json string we return Value::Null
    // we can't just use the input, we have to handle invalid strings like the cases where we have
    // raw control characters
    fn from(val: &str) -> Self {
        parsing::to_jstr(&val)
            .map(Value::String)
            .unwrap_or(Value::Null)
    }
}

impl From<usize> for Value {
    fn from(val: usize) -> Self {
        Value::Number(Number::from(val as i64))
    }
}

impl From<IndexMap<String, Value>> for Value {
    fn from(val: IndexMap<String, Value>) -> Self {
        val.into_iter()
            .map(|(key, value)| parsing::to_jstr(&key).map(|k| (k, value)))
            // .collect::<Result<IndexMap<_, _>, _>>() this would also work
            .collect::<Result<IndexMap<String, Value>, StringError>>()
            .map(Value::Object)
            .unwrap_or(Value::Null)
    }
}

impl From<u8> for Value {
    fn from(val: u8) -> Self {
        Value::Number(Number::from(val as i64))
    }
}

impl From<i8> for Value {
    fn from(val: i8) -> Self {
        Value::Number(Number::from(val as i64))
    }
}

impl From<u16> for Value {
    fn from(val: u16) -> Self {
        Value::Number(Number::from(val as i64))
    }
}

impl From<i16> for Value {
    fn from(val: i16) -> Self {
        Value::Number(Number::from(val as i64))
    }
}

impl From<u32> for Value {
    fn from(val: u32) -> Self {
        Value::Number(Number::from(val as i64))
    }
}

impl From<i32> for Value {
    fn from(val: i32) -> Self {
        Value::Number(Number::from(val as i64))
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
