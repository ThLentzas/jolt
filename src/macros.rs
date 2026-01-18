/// Constructs a [Value](crate::Value) from a literal.
///
/// # Example
///
/// ```
/// # use jolt::json;
/// #
/// let val = json!({
///     "Image": {
///         "Width": 800,
///         "Height": 600,
///         "Title": "View from 15th Floor"
///     }
/// });
/// ```
///
/// This macro is intended for quickly constructing test data. It does **not** perform any escape
/// sequence handling. For example, `"ab\u0063"` will not be converted to `"abc"`, and
/// invalid escapes like `"ab\p"` will not produce an error.Invalid input creates a `Value` in an
/// undefined state, which may cause unexpected behavior. For untrusted input, use [`from_str`](crate::from_str)
/// or [`from_slice`](crate::from_slice) instead. Use with your own discretion.
#[macro_export]
macro_rules! json {
    ([]) => { $crate::Value::Array(Vec::new()) };
    ({}) => { $crate::Value::Object(IndexMap::new()) };
    (null) => { $crate::Value::Null };
    (true) => { $crate::Value::Bool(true) };
    (false) => { $crate::Value::Bool(false) };
    ([ $($elem:tt),+ $(,)? ]) => { $crate::Value::Array(vec![$(json!($elem)),+]) };
    ({ $($key:tt: $val:tt),+ $(,)? }) => {{
        use indexmap::IndexMap;
        
        let mut map = IndexMap::new();
        $(map.insert($key.to_string(), json!($val));)+
        $crate::Value::Object(map)
    }};
     // can only be &str or some numeric type
    ($other:expr) => { $crate::Value::from($other) };
}

// https://www.youtube.com/watch?v=q6paRBbLgNw around 47:00
//
// we could also implement atoi for every numeric type but this way is more efficient
macro_rules! impl_atoi {
    ($t: ty) => {
        impl crate::parsing::number::Atoi for $t {
            fn atoi(
                buffer: &[u8],
                pos: &mut usize,
            ) -> Result<Self, crate::parsing::number::OutOfRangeError> {
                use crate::parsing::number::OutOfRangeError;
                use crate::parsing::number::NumericBounds;
                
                let mut num: $t = 0;
                let start = *pos;
                let neg = if buffer[*pos] == b'-' {
                    *pos += 1;
                    true
                } else {
                    false
                };

                while *pos < buffer.len() {
                    let current = buffer[*pos];
                    if !current.is_ascii_digit() {
                        break;
                    }
                    let digit = (current - 0x30) as $t;
                    if neg {
                        num = num
                            .checked_mul(10)
                            .and_then(|n| n.checked_sub(digit))
                            .filter(|&n| n >= <$t as NumericBounds>::MIN)
                            .ok_or_else(|| OutOfRangeError {
                                bound: <$t as NumericBounds>::MIN.to_string(),
                                pos: start
                            })?;
                    } else {
                        num = num
                            .checked_mul(10)
                            .and_then(|n| n.checked_add(digit))
                            .filter(|&n| n <= <$t as NumericBounds>::MAX)
                            .ok_or_else(|| OutOfRangeError {
                                bound: <$t as NumericBounds>::MAX.to_string(),
                                pos: start
                            })?;
                    }
                    *pos += 1;
                }
                Ok(num)
            }
        }
    };
}

impl_atoi!(i64);
impl_atoi!(u8);
