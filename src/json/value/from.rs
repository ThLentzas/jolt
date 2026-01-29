use crate::json::number::Number;
use crate::Value;
use indexmap::IndexMap;

impl From<&str> for Value {
    // the difference between this method and jolt::from_str() is that Value::from() always creates
    // Value::String() variant while from_str() can create any Value
    fn from(val: &str) -> Self {
        Value::String(val.to_owned())
    }
}

impl From<usize> for Value {
    fn from(val: usize) -> Self {
        Value::Number(Number::from(val as i64))
    }
}

impl From<IndexMap<String, Value>> for Value {
    fn from(map: IndexMap<String, Value>) -> Self {
        Value::Object(map)
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
