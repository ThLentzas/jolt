# jolt

A library that provides JSON support

## Overview

- **Parsing**: Parse JSON data using `from_slice()` and `from_str()`.
- **Reading**: Extract values using [JSON Pointer](https://www.rfc-editor.org/rfc/rfc6901) or query
  with [JSONPath ](https://www.rfc-editor.org/rfc/rfc9535).
- **Writing**: Modify with [JSON Patch](https://www.rfc-editor.org/rfc/rfc6902).

```toml
[dependencies]
jolt = "0.1.0"
```

## Parse

`from_slice()` performs strict utf-8 validation according to the spec. Any invalid utf-8 sequence will
result in a `ParserError` and it will not be replaced by the replacement character `�`.

If the parsing process is successful the result is the following enum:

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

## Read

There are 3 ways you can extract a value from a JSON document:

1. `JSON pointer`: Defines a string syntax for navigating to a specific location in a JSON document. Always returns a
   single value if found.

    ```rust
    use jolt::Value;
    
    fn example() {
       let text = json!({
           "store": {
               "books": ["A", "B", "C"]
           }
       });
    
       let val = jolt::from_str(text)?;
       let book = val.pointer("/store/books/0")?;
       assert_eq!(book.unwrap(), &json!("A"));
    }
    ```

2. `JSON path`: A JSON path expression is a string that, when applied to a JSON value (the query argument), selects zero
   or more nodes of the argument and outputs these nodes as a nodelist. It is not intended as a replacement but as a
   more powerful companion to JSON pointer.

   **Retrieving results:**
   The nodelist, the result of the query, can be represented as:
    - an array of values: `select_as_values()`
    - an array of normalized paths, where a normalized path is a unique representation of the location of a node in a
      value that uniquely identifies the node in the value: `select_as_npaths()`
    - both: `select()`  
      <br>

    ```rust
    use jolt::Value;
    
    fn example() {
        let val = jolt::from_str(r#"
            {
                "store": {
                    "books": [
                        { "title": "Rust in Action", "capacity": 35 },
                        { "title": "The Rust Book", "capacity": 0 },
                        { "title": "Programming Rust", "capacity": 45 }
                    ]
                }
            }
        "#)?;
    
        // Select all book titles
        let titles = val.select_as_values("$.store.books[*].title")?;
        assert_eq!(titles.len(), 3);
    
        // Get normalized paths instead of values
        // ["$['store']['books'][0]['title']", "$['store']['books'][1]['title']", ...]
        let paths = val.select_as_npaths("$.store.books[*].title")?;
        assert_eq!(paths.len(), 3);
    
        // Filter: books with capacity > 30
        let books = val.select("$.store.books[?@.capacity > 30]")?;
        assert_eq!(books.len(), 2);
    }
    ```
   **Functions and Regular Expressions:**
    - `Functions`: All functions that are defined in
      the [rfc](https://www.rfc-editor.org/rfc/rfc9535.html#name-function-extensions) are supported. There are plans to
      add new ones such as `min()`, `max()` and `avg()` in future updates.
    - `Regex`: Jolt includes a built-in [I-Regexp](https://www.rfc-editor.org/rfc/rfc9485) engine for pattern matching
      in filter expressions. The engine uses Thompson's NFA construction, guaranteeing `O(m * n)` time complexity where
      `m` is the pattern length and `n` is the input length. Unicode character properties (e.g., `\p{L}` for letters)
      are supported via a two-stage lookup table for `O(1)` category retrieval.This approach generates a binary of approximately `170 KB`, which is `~40%` smaller than a binary-search-based
      approach `~280 KB` and significantly smaller than a naive vector-based approach `~3.04 MB` for 1,114,112 entries

3. `get()`: A method on `Value` for direct access to container elements. Works with both objects and arrays through an
   interface, pass a string key for objects or an integer index for arrays. Returns `Option<&Value>`, allowing safe
   access without panicking.
    ```rust
    use jolt::Value;
    
    fn example() {
        let val = jolt::from_str(r#"
            {
                "name": "Alice",
                "scores": [95, 87, 92] 
            }
        "#)?;
    
        let name = val.get("name").unwrap();
        assert_eq!(*name, Value::from("Alice"));
   
        let score = val.get("scores")
            .and_then(|s| s.get(1))
            .unwrap();
        assert_eq!(*score, Value::from(87));
    }
    ```

## Write

To modify a JSON document, use either `modify()` or `try_modify()`. Both methods
accept a [JSON Patch](https://datatracker.ietf.org/doc/html/rfc6902) compliant string.

- `modify()`: If an operation fails, previous changes are retained.
- `try_modify()`: If an operation fails, the changes are rolled back.

```rust
use jolt::Value;

fn example() {
   let mut val = jolt::from_str(r#"{"name": "Alice", "age": 30}"#)?;

   val.modify(r#"[
        {"op": "replace", "path": "/age", "value": 31}
    ]"#)?;
   assert_eq!(val, jolt::from_str(r#"{"name": "Alice", "age": 31}"#)?);
}
```

## Implementation Limits

Jolt enforces limits to prevent resource exhaustion:

- **Byte Buffer**: `4 MB` maximum. Larger buffers are rejected before parsing.
- **String length**: `8,192` characters per string value.
- **Nesting depth**: `128` levels of nested objects/arrays.
- **Integers**: `[−(2^53)+1, (2^53)−1]` the [I-JSON](https://www.rfc-editor.org/rfc/rfc7493#section-2.2) interoperable
  range.
- **Floats**: IEEE 754 double-precision range.
- **Regex**: The max value of a quantifier is `100`, `a{100}`, and the total number of nodes in the AST to `10000`.  
  For example, `ab` produces 3 nodes:
```
   Concat(Atom(a), Atom(b))
        /         \
     Atom(a)     Atom(b)
```   

### Arbitrary precision

By default, integers are stored as `i64` and floats as `f64`. If you need to work with numbers beyond these ranges, large integers, high-precision decimals, or exact decimal arithmetic enable the `arbitrary_precision` feature:
```toml
[dependencies]
jolt = { version = "0.1", features = ["arbitrary_precision"] }
```

### json! macro
Constructs a `Value` from a literal.

```rust
let val = json!({
   "Image": {
      "Width": 800,
      "Height": 600,
      "Title": "View from 15th Floor"
     } 
 });
 ```
This macro is intended for quickly constructing test data. It does **not** perform any escape sequence handling. For example, `"ab\u0063"` will not be converted to `"abc"` as per the JSON grammar, and
invalid escapes like `"ab\p"` will not produce an error.Invalid input creates a `Value` in an
undefined state, which may cause unexpected behavior. For untrusted input, use `from_slice()`
or `from_str()` instead. Use with your own discretion.

#### License

This project is licensed under the MIT License.
