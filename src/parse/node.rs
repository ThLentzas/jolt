use linked_hash_map::LinkedHashMap;
use crate::utils::numeric_utils::Number;

#[derive(Debug, PartialEq)]
pub enum JsonValue {
    Null,
    Boolean(bool),
    Number(Number),
    String(String),
    Array(Vec<JsonValue>),
    Object(LinkedHashMap<String, JsonValue>),
}

#[derive(Debug, PartialEq)]
pub struct JsonNode {
    // [](empty input buffer) is not invalid, the value of the root of AST is None
    // same logic applies for [\n, \t, '\r', ' ']
    val: Option<JsonValue>
}

impl JsonNode {
    pub fn new(val: Option<JsonValue>) -> Self {
        Self {
            val
        }
    }

    pub fn value(&self) -> Option<&JsonValue> {
        self.val.as_ref()
    }
}