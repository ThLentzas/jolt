// For the input json!([null, true, false]) the code will give us as output [Value(false), Value(false), Value(false)]
// which is not what we expect. When we use $elem:expr, the macro captures null, true, and false as
// expressions, not as raw tokens. Once captured as an expression, they can't be re-matched
// against literal patterns. When we call json!(null), null is treated as an expression it never
// matches the 1st rule, but it does the last one. The reason why we don't get an error like the below
// case is that we never refer to $elem in the expansion, and Rust never checks that null is not a valid
// expression
//
// macro_rules! json {
//     (null) => { $crate::Value::Null };
//     ([ $($elem:expr),+ $(,)? ]) => { $crate::Value::Array(vec![$(json!($elem)),+]) };
//     ($elem:expr) => { $crate::Value::Boolean(false) }
// }
//
// The logic is exactly the same as the comment above. The only difference is that we actually use
// $elem in println!() which means now Rust checks if $elem is actually a valid expression, before
// it was just part of the special macro syntax, and it fails because null is not a valid expression
//
// macro_rules! json {
//     (null) => { $crate::Value::Null };
//     ([ $($elem:expr),+ $(,)? ]) => { $crate::Value::Array(vec![$(json!($elem)),+]) };
//     ($elem:expr) => {{
//         println!("{}", $elem);
//         $crate::Value::Boolean(false)
//     }}
// }
//
// The code below for input: json!([null, true, false]) gives us as output [Value(false), Value(false), Value(false)]
// When we use $elem:expr, the macro captures null, true, and false as  expressions, not as raw tokens.
// Once captured as an expression, they can't be re-matched  against literal patterns. When we call
// json!(null), null is treated as an expression it never matches the 1st rule, but it does the last one.
//#[macro_export]
// macro_rules! json {
//     ....
//     ([ $($elem:expr),+ $(,)? ]) => { $crate::Value::Array(vec![$(json!($elem)),+]) };
//     ($elem:expr) => { $crate::Value::Boolean(false) }
// }
//
// The below specifiers parse the input:
//  $x:expr    // Parses as expression
//  $t:ty      // Parses as type
//  $p:pat     // Parses as pattern
//  $i:ident   // Parses as identifier
//
// tt is special because it's the only fragment specifier that doesn't parse. Think about it like
// using recursion in a smaller and smaller range of the initial input, the input remains a sequence
// of token trees. It forwards the input AS-IS
//
//      json!([[null], true, false])
//
// when we break the input into tokens the call is with json!([null]) which matches again ([ $($elem:tt),+ $(,)? ])
// and then we call again but now is json!(null); this matches the literal null, and we return Value::Null
// Recursion returns back to json!([null]) which returns Value::Array(Value::Null) and so on. We apply
// the same logic for the Object case. This is the core idea behind a TT muncher:
// https://lukaswirth.dev/tlborm/decl-macros/patterns/tt-muncher.html
//
// macro_rules! show_expr {
//     ($e:expr) => {
//         stringify!($e)
//     };
// }
//
// macro_rules! show_tt {
//     ($t:tt) => {
//         stringify!($t)
//     };
// }
//
// Both return "null" as a string, but internally:
// - expr version has PARSED null into an expression node
// - tt version has the RAW TOKEN null
//
// also when we refer to our structs we need to provide the full path to it
macro_rules! json {
    ([]) => { $crate::parsing::Value::Array(Vec::new()) };
    ({}) => { $crate::parsing::Value::Object(IndexMap::new()) };
    (null) => { $crate::parsing::Value::Null };
    (true) => { $crate::parsing::Value::Boolean(true) };
    (false) => { $crate::parsing::Value::Boolean(false) };
    ([ $($elem:tt),+ $(,)? ]) => { $crate::parsing::Value::Array(vec![$(json!($elem)),+]) };
    ({ $($key:tt: $val:tt),+ $(,)? }) => {{
        let mut map = IndexMap::new();
        $(map.insert($key.to_string(), json!($val));)+
        $crate::parsing::Value::Object(map)
    }};
     // can only be &str or some numeric type
    ($other:expr) => { $crate::parsing::Value::from($other) };
}

// https://www.youtube.com/watch?v=q6paRBbLgNw around 47:00
//
// we could also implement atoi for every numeric type but this way it is more efficient
//
// why we need signed/unsigned?
// at some point in our code we do Ok(sign * num) which is invalid for unsigned numbers
macro_rules! impl_atoi {
    ($t: ty, signed) => {
        impl crate::parsing::number::Atoi for $t {
            fn atoi(
                buffer: &[u8],
                pos: &mut usize,
            ) -> Result<Self, crate::parsing::number::OutOfRangeError> {
                let mut num: $t = 0;
                let sign = if buffer[*pos] == b'-' {
                    *pos += 1;
                    -1
                } else {
                    1
                };

                let start = *pos;
                let mut current = buffer[*pos];
                while *pos < buffer.len() && current.is_ascii_digit() {
                    if num > <$t>::MAX / 10
                        || (num == <$t>::MAX / 10 && current > (<$t>::MAX % 10) as u8)
                    {
                        return Err(crate::parsing::number::OutOfRangeError { pos: start });
                    }
                    num = num * 10 + (current - 0x30) as $t;
                    *pos += 1;
                    if *pos < buffer.len() {
                        current = buffer[*pos];
                    }
                }
                Ok(sign * num)
            }
        }
    };
    ($t: ty, unsigned) => {
        impl crate::parsing::number::Atoi for $t {
            fn atoi(
                buffer: &[u8],
                pos: &mut usize,
            ) -> Result<Self, crate::parsing::number::OutOfRangeError> {
                let mut num: $t = 0;

                let start = *pos;
                let mut current = buffer[*pos];
                while *pos < buffer.len() && current.is_ascii_digit() {
                    if num > <$t>::MAX / 10
                        || (num == <$t>::MAX / 10 && current > (<$t>::MAX % 10) as u8)
                    {
                        return Err(crate::parsing::number::OutOfRangeError { pos: start });
                    }
                    num = num * 10 + (current - 0x30) as $t;
                    *pos += 1;
                    if *pos < buffer.len() {
                        current = buffer[*pos];
                    }
                }
                Ok(num)
            }
        }
    };
}

impl_atoi!(i64, signed);
impl_atoi!(u8, unsigned);

// not part of the public api, only for internal usage, no #[macro_export]
pub(crate) use impl_atoi;
pub(crate) use json;
