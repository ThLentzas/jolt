# jolt

A library that provides JSON support

## Overview

- **Parsing**: Parse JSON data using `from_slice()`, `from_str()` and `from_file()`.
- **Reading**: Extract values using [JSON Pointer](https://www.rfc-editor.org/rfc/rfc6901) or query with [JSONPath ](https://www.rfc-editor.org/rfc/rfc9535).
- **Writing**: Modify with [JSON Patch](https://www.rfc-editor.org/rfc/rfc6902).

```toml
[dependencies]
jolt = "0.1.0"
```

## Parsing

`from_slice()` and `from_file()` perform strict utf-8 validation according to the spec. Any invalid utf-8 sequence will
result in a `ParserError` and it will not be replaced by the replacement character `�`.

If the parsing process is successful the method should return any of the following enum variants:

```rust
enum Value {
    Object(IndexMap<String, Value>),
    Array(Vec<Value>),
    Number(Number),
    String(String),
    Boolean(bool),
    Null,
}
```

## Reading

There are 3 ways you can extract a value from a JSON document:

1. `JSON pointer`: Defines a string syntax for navigating to a specific location in a JSON document. Always returns a single value if found.
    
    ```rust
    use jolt::parsing::Value;
    
    fn example() {
        let json = r#"{
            "store": {
                "books": ["A", "B", "C"]
            }
        }"#;
    
        let val = jolt::from_str(json).unwrap();
        let book = val.pointer("/store/books/0");
        assert_eq!(*book, Value::from("A"));
    }
    ```

2. `JSON path`: A JSON path expression is a string that, when applied to a JSON value (the query argument), selects zero or more nodes of the argument and outputs these nodes as a nodelist. It is not intended as a replacement but as a more powerful companion to JSON pointer.

    **Retrieving results:**
    The nodelist, the result of the query, can be represented as:
    - an array of values: `select_as_values()`
    - an array of normalized paths, where a normalized path is a unique representation of the location of a node in a value that uniquely identifies the node in the value: `select_as_npaths()`
    - both: `select()`
    ```rust
    use jolt::parsing::Value;
    
    fn example() {
        let root = jolt::from_str(r#"
            {
                "store": {
                    "books": [
                        { "title": "Rust in Action", "price": 35 },
                        { "title": "The Rust Book", "price": 0 },
                        { "title": "Programming Rust", "price": 45 }
                    ]
                }
            }
        "#)?;
    
        // Select all book titles
        let titles = data.select_as_values("$.store.books[*].title")?;
        assert_eq!(titles.len(), 3);
    
        // Get normalized paths instead of values
        // ["$['store']['books'][0]['title']", "$['store']['books'][1]['title']", ...]
        let paths = data.select_as_npaths("$.store.books[*].title")?;
        assert_eq!(paths.len(), 3);
    
        // Filter: books with price > 30
        let expensive = data.select("$.store.books[?@.price > 30]")?;
        assert_eq!(expensive.len(), 2);
    }
    ```
    **Functions and Regular Expressions:**  
    `Functions`: All functions that are defined in the [rfc](https://www.rfc-editor.org/rfc/rfc9535.html#name-function-extensions) are supported. There are plans to add new ones such as `min()`, `max()` and `avg()` in future updates.  
    `Regex`: Jolt includes a built-in [I-Regexp](https://www.rfc-editor.org/rfc/rfc9485) engine for pattern matching in filter expressions. The engine uses Thompson's NFA construction, guaranteeing `O(m * n)` time complexity where `m` is the pattern length and `n` is the input length. Unicode character properties (e.g., `\p{L}` for letters) are supported via a two-stage lookup table for `O(1)` category retrieval.  
    This approach generates a binary of approximately `170 KB`, which is:
    - `~40%` smaller than a binary-search-based approach `~280 KB`
    - significantly smaller than a naive vector-based approach `~3.04 MB` for 1,114,112 entries 
  
3. `get()`: A method on `Value` for direct access to container elements. Works with both objects and arrays through an interface, pass a string key for objects or an integer index for arrays. Returns `Option<&Value>`, allowing safe access without panicking.
    ```rust
    use jolt::parsing::Value;
    
    fn example() {
        let data = jolt::from_str(r#"
            {
                "name": "Alice",
                "scores": [95, 87, 92] 
            }
        "#)?;
    
        let name = data.get("name").unwrap();
        assert_eq!(*name, Value::from("Alice"));
        let score = data.get("scores")
            .and_then(|s| s.get(1))
            .unwrap();
        assert_eq!(*score, Value::from(87));
    }
    ```
## Implementation Limits

Jolt enforces limits to prevent resource exhaustion:

- **Byte Buffer**: `4 MB` maximum. Larger buffers are rejected before parsing.
- **String length**: `8,192` characters per string value.
- **Nesting depth**: `128` levels of nested objects/arrays.
- **Integers**: `[−(2⁵³)+1, (2⁵³)−1]` the [I-JSON](https://www.rfc-editor.org/rfc/rfc7493#section-2.2) interoperable range.
- **Floats**: IEEE 754 double-precision range `(±1.8 * 10³⁰⁸)`.

By default, integers are stored as `i64` and floats as `f64`. If you need to work with numbers beyond these ranges, large integers, high-precision decimals, or exact decimal arithmetic enable the `arbitrary_precision` feature.
### License

This project is licensed under the MIT License.
