//! # Jolt
//!
//! A library that provides JSON support.
//!
//! # Parsing
//!
//! You can parse JSON data using either [`from_slice()`](from_slice) or [`from_str()`](from_str). If the parsing process is successful the
//! result is the following enum.
//!
//! ```
//! # use jolt::{Value, Number};
//! #
//! enum Value {
//!     Object(IndexMap<String, Value>),
//!     Array(Vec<Value>),
//!     Number(Number),
//!     String(String),
//!     Boolean(bool),
//!     Null,
//! }
//! ```
//!
//! # Reading
//!
//! There are 3 ways you can extract a value from a JSON document:
//!
//! **Pointer**: Defines a string syntax for navigating to a specific location in a JSON document. Always returns
//! a single value if found. You can use either [`pointer()`](Value::pointer) that returns a reference to a matched
//! value or [`pointer_mut()`](Value::pointer_mut) that returns a mutable reference instead.
//! ```
//! use jolt::{Value, json};
//!
//! fn example() {
//!     let val = json!({
//!         "store": {
//!             "books": ["A", "B", "C"]
//!         }
//!     });
//!
//!     let book = val.pointer("/store/books/0")?;
//!
//!     assert_eq!(book.unwrap(), &json!("A"));
//! }
//! ```
//!
//! **Path**: A JSON path expression is a string that, when applied to a JSON value (the query argument), selects zero
//!    or more nodes of the argument and outputs these nodes as a nodelist. It is not intended as a replacement but as a
//!    more powerful companion to JSON pointer.
//!
//! **Retrieving results:**
//!
//! The nodelist, the result of the query, can be represented as:
//!
//! - An array of values: [`select_as_values()`](Value::select_as_values)
//! - An array of normalized paths, where a normalized path is a unique representation
//!   of the location of a node that uniquely identifies it in the value: [`select_as_npaths()`](Value::select_as_npaths)
//! - Both: [`select()`](Value::select)
//!
//! ```
//! use jolt::{Value, json};
//!
//!  fn example() {
//!     let val = json!( {
//!         "store": {
//!             "books": [
//!                 { "title": "Rust in Action", "capacity": 35 },
//!                 { "title": "The Rust Book", "capacity": 0 },
//!                 { "title": "Programming Rust", "capacity": 45 }
//!             ]
//!          }
//!     });
//!
//!     // Select all book titles
//!     let titles = val.select_as_values("$.store.books[*].title")?;
//!     assert_eq!(titles.len(), 3);
//!
//!     // Get normalized paths instead of values
//!     // ["$['store']['books'][0]['title']", "$['store']['books'][1]['title']", ...]
//!     let paths = val.select_as_npaths("$.store.books[*].title")?;
//!     assert_eq!(paths.len(), 3);
//!
//!     // Filter: books with capacity > 30
//!     let books = val.select("$.store.books[?@.capacity > 30]")?;
//!     assert_eq!(books.len(), 2);
//! }
//!```
//! **Functions and Regular Expressions:**
//!
//! - `Functions`: All functions that are defined in
//!   the [rfc](https://www.rfc-editor.org/rfc/rfc9535.html#name-function-extensions) are supported. There are plans to
//!   add new ones such as `min()`, `max()` and `avg()` in future updates.
//! - `Regex`: Jolt includes a built-in [I-Regexp](https://www.rfc-editor.org/rfc/rfc9485) engine for pattern matching
//!   in filter expressions. The engine uses Thompson's NFA construction, guaranteeing `O(m * n)` time complexity where
//!   `m` is the pattern length and `n` is the input length. Unicode character properties (e.g., `\p{L}` for letters)
//!   are supported via a two-stage lookup table for `O(1)` category retrieval. This approach generates a binary of
//!   approximately `170 KB`, which is `~40%` smaller than a binary-search-based approach `~280 KB` and significantly
//!   smaller than a naive vector-based approach `~3.04 MB` for 1,114,112 entries.

//! **get()**: A method on [Value] for direct access to container elements. Works with both objects and arrays
//! through an interface, pass a string key for objects or an integer index for arrays. Returns
//! `Option<&Value>`, allowing safe access without panicking.
//! ```
//! use jolt::{Value, json};
//!
//! fn example() {
//!     let val = json!({
//!         "name": "Alice",
//!         "foo": ["bar", "baz"]
//!     })?;
//!
//!     let name = val.get("name").unwrap();
//!     assert_eq!(name, &json!("Alice"));
//!
//!     let elem = val.get("foo")
//!         .and_then(|s| s.get(1))
//!         .unwrap();
//!     assert_eq!(elem, &json!("baz"));
//! }
//!```
//!
//! # Writing
//! To modify a JSON document, use either [modify()](Value::modify) or [try_modify()](Value::try_modify). Both methods
//! accept a [JSON Patch](https://datatracker.ietf.org/doc/html/rfc6902) compliant string.
//!
//! - `modify()`: If an operation fails, previous changes are retained.
//! - `try_modify()`: If an operation fails, the changes are rolled back.
//!
//! ```rust
//! use jolt::{Value, json};
//!
//! fn example() {
//!     let mut val = json!({
//!         "name": "Alice",
//!         "age": 30
//!     });
//!
//!     val.modify(r#"[
//!         {"op": "replace", "path": "/age", "value": 31}
//!     ]"#).unwrap();
//!
//!     assert_eq!(val.pointer("/age").unwrap(), Some(&json!(31)));
//! }
//! ```
//! # Implementation Limits
//!
//! Jolt enforces limits to prevent resource exhaustion:
//!
//! - **Byte Buffer**: `4 MB` maximum. Larger buffers are rejected before parsing.
//! - **String length**: `8,192` characters per string value.
//! - **Nesting depth**: `128` levels of nested objects/arrays.
//! - **Integers**: `[−(2^53)+1, (2^53)−1]` the [I-JSON](https://www.rfc-editor.org/rfc/rfc7493#section-2.2) interoperable range.
//! - **Floats**: IEEE 754 double-precision range.
//! - **Regex**: The max value of a quantifier is `100` (`a{100}`), and the total number
//!   of nodes in the AST is limited to `10,000`.
//!
//! For example, `ab` produces 3 nodes:
//!
//!
//!   ```text
//!      Concat(Atom(a), Atom(b))
//!           /         \
//!        Atom(a)     Atom(b)
//!   ```
//! # Arbitrary Precision
//!
//! By default, integers are stored as `i64` and floats as `f64`. If you need to work with numbers
//! beyond these ranges, large integers, high-precision decimals, or exact decimal arithmetic enable
//! the `arbitrary_precision` feature:
//!
//! ```toml
//! [dependencies]
//! jolt = { version = "0.1", features = ["arbitrary_precision"] }
//! ```

mod macros;
mod parsing;

// Any type that appears in a public function signature must be accessible to users
pub use parsing::error::ParseError;
pub use parsing::number::Number;
pub use parsing::value::Value;
pub use parsing::value::pointer::to_ptr_path;

/// Parses a byte slice into a `Value`.
/// # Example
///
/// ```
/// # use jolt;
/// #
/// let value = jolt::from_slice(b r#"{"foo": "bar"}"#).unwrap();
/// ```
/// # Errors
/// Performs strict UTF-8 validation where any invalid sequence results in an error. Non-strict parsers typically
/// substitute invalid sequences with the replacement character (�), but this parser rejects them.
/// The process can also fail if the slice violates any of the rules defined in the [JSON](https://www.rfc-editor.org/rfc/rfc8259) grammar.
pub fn from_slice(buffer: &[u8]) -> Result<Value, ParseError> {
    parsing::parse(buffer)
}

/// Parses a string slice into a `Value`.
///
/// # Example
///
/// ```
/// let value = jolt::from_str(r#"{"foo": "bar"}"#).unwrap();
/// ```
///
/// # Errors
///
/// The process can fail if the slice violates any of the rules defined in the [JSON](https://www.rfc-editor.org/rfc/rfc8259) grammar.
pub fn from_str(text: &str) -> Result<Value, ParseError> {
    parsing::parse(text.as_bytes())
}
