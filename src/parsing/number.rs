#[cfg(feature = "arbitrary_precision")]
use bigdecimal::num_bigint::BigInt;
#[cfg(feature = "arbitrary_precision")]
use bigdecimal::BigDecimal;
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::{error, fmt};

// https://www.rfc-editor.org/rfc/rfc7493#section-2.2
// [-(2^53)+1, (2^53)-1]
// [-9007199254740991, 9007199254740991]
// both bounds have same absolute value
const INT_LIMIT: &[u8] = b"9007199254740991";

// https://github.com/Alexhuszagh/rust-lexical interesting num parsing library
// much better approach than passing boolean flags around, foo(true, true, false) is hard to understand
struct NumberState {
    decimal_point: bool,
    scientific_notation: bool,
    negative: bool,
}

// The way BigDecimal works is by storing every digit of the number as BigInt, (unscaled value,
// an arbitrary-precision integer) and also the number of decimal places as int(scale)
//
//  The number 123.45 is stored as:
//      Unscaled value: 12345
//      Scale: 2
//      Precision: 5 (number of digits)
//
// The actual value is calculated as: unscaled value * 10^(-scale)
//
// To store the unscaled value as an arbitrary precision integer BigInt uses a Vec<u32/u64> under the hood

// In base 10: 1234 = 1 * 10^3 + 2 * 10^2 + 3 * 10^1 + 4 * 10^0
// In base 2^32: each u32 is a coefficient for powers of 2^32

// 2^32 = 4,294,967,296
// 10,000,000,000,000 / 4,294,967,296 = 2328 remainder 1,316,134,912

// So it's stored as:
// vec![1_316_134_912, 2328]
// Meaning: 1_316_134_912 * (2^32)^0 + 2328 * (2^32)^1
//
// The least significant "digit" (the remainder) goes in index 0, and more significant digits go in
// higher indices. This is called "little-endian" order.
//
// 50,000,000,000,000,000,000 (50 quintillion)
//
// Step 1: Divide by 2^32
//  50,000,000,000,000,000,000 ÷ 4,294,967,296 = 11,641,532,182
//  Remainder: 1,695,547,392
//
// Step 2: That quotient is still > 2^32, so divide again
//  11,641,532,182 / 4,294,967,296 = 2
//  Remainder: 3,051,597,590
//
//  Step 3: Final quotient is 2 (fits in u32)
//
//  So our Vec<u32> is: vec![1_695_547_392, 3_051_597_590, 2]
//  1,695,547,392 * (2^32)^0 + 3,051,597,590 * (2^32)^1 + 2 * (2^32)^2 = 50,000,000,000,000,000,000
//
// In parser.rs for the test valid_array() the number 340282366920938463463374607431768211456
// is vec![0, 0, 1] for u64 and vec![0, 0, 0, 0, 1]
//
// 10^40
//
// With u32:
// vec![1661992960, 1808227885, 3721402093, 4028081056, 542101086]
//
//With u64:
// vec![7766279631452241920, 542101086035307936, 2]
#[derive(Debug, PartialEq, Clone)]
enum NumberKind {
    I64(i64),
    F64(f64),
    #[cfg(feature = "arbitrary_precision")]
    BigInt(BigInt),
    #[cfg(feature = "arbitrary_precision")]
    BigDecimal(BigDecimal),
}

impl Eq for NumberKind {}

impl Hash for NumberKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);

        match self {
            NumberKind::I64(i) => i.hash(state),
            NumberKind::F64(f) => {
                let bits = if *f == 0.0 { 0 } else { f.to_bits() };
                bits.hash(state);
            }
            #[cfg(feature = "arbitrary_precision")]
            NumberKind::BigInt(bi) => bi.hash(state),
            #[cfg(feature = "arbitrary_precision")]
            NumberKind::BigDecimal(bd) => bd.hash(state),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Number {
    kind: NumberKind,
}

impl Number {
    // for i64, f64 self.0kind will copy v because all those types implement Copy; BigDecimal/Int does
    // not, so we need to borrow it and clone it, similar to &self.kind in error.rs
    pub fn as_i64(&self) -> Option<i64> {
        match self.kind {
            NumberKind::I64(v) => Some(v),
            _ => None,
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        match self.kind {
            NumberKind::F64(v) => Some(v),
            _ => None,
        }
    }

    #[cfg(feature = "arbitrary_precision")]
    pub fn as_big_decimal(&self) -> Option<BigDecimal> {
        match &self.kind {
            NumberKind::BigDecimal(v) => Some(v.clone()),
            _ => None,
        }
    }

    #[cfg(feature = "arbitrary_precision")]
    pub fn as_big_int(&self) -> Option<BigInt> {
        match &self.kind {
            NumberKind::BigInt(v) => Some(v.clone()),
            _ => None,
        }
    }
}

// why is it always safe to cast an i64 to f64 lossless?
//
// from the IEEE754 standard we know mantissa has 52 bits of precision and for normal numbers we
// get an implicit 1 which gives us 53 bits that are enough to represent any number in the
// [-(2^53)+1, (2^53)-1]. We cast i64 in the comparison as f64 without any precision loss
//
// for float comparisons we don't try to be clever, we let Rust apply whatever rules they have
// we could try to follow the logic of the articles below and implement a relative epsilon comparison
// or ULP but there is no silver bullet
//
// https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/
// https://embeddeduse.com/2019/08/26/qt-compare-two-floats/
// https://floating-point-gui.de/errors/comparison/
//
// when arbitrary precision is enabled we never have to consider f64 and BigInt/Decimal because
// we never parse numbers as f64. This is true because our program runs with the flag either set or
// not so we will never run in a case where we parsed as f64 and then somehow encounter a BigInt/Decimal
// on the other side of the comparison
impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // match takes ownership and NumberKind does not implement Copy, so we pass a ref
        // we have &Self, and we can not move it out of a borrowed context
        match (&self.kind, &other.kind) {
            (NumberKind::I64(lhs), NumberKind::I64(rhs)) => lhs.partial_cmp(rhs),
            (NumberKind::F64(lhs), NumberKind::F64(rhs)) => lhs.partial_cmp(rhs),
            (NumberKind::I64(lhs), NumberKind::F64(rhs)) => (*lhs as f64).partial_cmp(rhs),
            (NumberKind::F64(lhs), NumberKind::I64(rhs)) => lhs.partial_cmp(&(*rhs as f64)),
            #[cfg(feature = "arbitrary_precision")]
            (NumberKind::BigDecimal(lhs), NumberKind::I64(rhs)) => {
                lhs.partial_cmp(&BigDecimal::from(rhs))
            }
            #[cfg(feature = "arbitrary_precision")]
            (NumberKind::I64(lhs), NumberKind::BigDecimal(rhs)) => {
                BigDecimal::from(lhs).partial_cmp(rhs)
            }
            #[cfg(feature = "arbitrary_precision")]
            (NumberKind::BigInt(lhs), NumberKind::BigDecimal(rhs)) => {
                BigDecimal::from(lhs.clone()).partial_cmp(rhs)
            }
            #[cfg(feature = "arbitrary_precision")]
            (NumberKind::BigDecimal(lhs), NumberKind::BigInt(rhs)) => {
                lhs.partial_cmp(&BigDecimal::from(rhs.clone()))
            }
            #[cfg(feature = "arbitrary_precision")]
            (NumberKind::BigInt(lhs), NumberKind::BigInt(rhs)) => lhs.partial_cmp(rhs),
            #[cfg(feature = "arbitrary_precision")]
            (NumberKind::BigDecimal(lhs), NumberKind::BigDecimal(rhs)) => lhs.partial_cmp(rhs),
            #[cfg(feature = "arbitrary_precision")]
            (NumberKind::BigInt(lhs), NumberKind::I64(rhs)) => lhs.partial_cmp(&BigInt::from(*rhs)),
            #[cfg(feature = "arbitrary_precision")]
            (NumberKind::I64(lhs), NumberKind::BigInt(rhs)) => BigInt::from(*lhs).partial_cmp(rhs),
            #[cfg(feature = "arbitrary_precision")]
            _ => None,
        }
    }
}

impl From<i64> for Number {
    fn from(val: i64) -> Self {
        Number {
            kind: NumberKind::I64(val),
        }
    }
}

impl From<f64> for Number {
    fn from(val: f64) -> Self {
        Number {
            kind: NumberKind::F64(val),
        }
    }
}

#[cfg(feature = "arbitrary_precision")]
impl From<BigDecimal> for Number {
    fn from(val: BigDecimal) -> Self {
        Number {
            kind: NumberKind::BigDecimal(val),
        }
    }
}

#[cfg(feature = "arbitrary_precision")]
impl From<BigInt> for Number {
    fn from(val: BigInt) -> Self {
        Number {
            kind: NumberKind::BigInt(val),
        }
    }
}

// We return u16 because the valid range for the Unicode sequences are 0x0000-0xFFFF(0-65535)
// 0-65535 is the range for u16
pub(super) fn hex_to_u16(buffer: &[u8]) -> Result<u16, HexError> {
    let mut val: u16 = 0;

    // Logic similar to base 10 seen on Leetcode problems
    // digit - b'a' returns a value in the [0,25] range but in base 16 a/A is 10 not 0, b/B is 11
    // + 10 brings into that range
    for (index, &byte) in buffer.iter().enumerate() {
        let hex_val = match byte {
            b'0'..=b'9' => byte - b'0',
            b'a'..=b'f' => byte - b'a' + 10,
            b'A'..=b'F' => byte - b'A' + 10,
            _ => {
                return Err(HexError {
                    digit: byte,
                    pos: index,
                });
            }
        };
        val = val * 16 + hex_val as u16;
    }
    Ok(val)
}

// we are parsing from a buffer that represent the number's value as a utf8 string; it is a two-step
// process: 1) convert the byte buffer to string 2) parse the string
// always safe to call unwrap because previously we have called read()
pub(super) fn parse(buffer: &[u8]) -> Number {
    let float = buffer.iter().any(|&b| matches!(b, b'.' | b'e' | b'E'));
    let s = std::str::from_utf8(buffer).unwrap();

    #[cfg(feature = "arbitrary_precision")]
    type N = BigDecimal;
    #[cfg(not(feature = "arbitrary_precision"))]
    type N = f64;
    if float {
        return Number::from(s.parse::<N>().unwrap());
    }

    // Try to optimize even if big_decimal is true; for any integer we can still store it as i64
    // as long as it is not out of range. In read() we don't check if the number is out of range
    // when big_decimal is enabled
    #[cfg(feature = "arbitrary_precision")]
    {
        let digits = if buffer[0] == b'-' {
            &buffer[1..]
        } else {
            buffer
        };
        if is_out_of_range_i64(digits) {
            return Number::from(s.parse::<BigInt>().unwrap());
        }
    }
    Number::from(s.parse::<i64>().unwrap())
}

pub(super) fn read(buffer: &[u8], pos: &mut usize) -> Result<(), NumericError> {
    let len = buffer.len();
    let start = *pos;
    let mut current = buffer[*pos];
    let next = buffer.get(*pos + 1);

    match (current, next) {
        // +9
        (b'+', Some(n)) if n.is_ascii_digit() => {
            return Err(NumericError {
                kind: NumericErrorKind::InvalidSign {
                    message: "json specification prohibits numbers from being prefixed with a plus sign",
                },
                pos: *pos,
            });
        }
        // -0 is valid
        (b'-', Some(n)) if !n.is_ascii_digit() => {
            return Err(NumericError {
                kind: NumericErrorKind::InvalidSign {
                    message: "a valid numeric value requires a digit (0-9) after the minus sign",
                },
                pos: *pos,
            });
        }
        // 05 not allowed
        (b'0', Some(n)) if n.is_ascii_digit() => {
            return Err(NumericError {
                kind: NumericErrorKind::LeadingZeros,
                pos: *pos,
            });
        }
        _ => (),
    }

    // this is the case for single digit numbers '3' where next is none, self.pos is at the digit
    // itself, not in the 1st character after reading the number, we don't need to reset the position,
    // later advance() is called self.pos moves correctly to the next character without skipping one
    if next.is_none() {
        return Ok(());
    }

    *pos += 1;
    current = *next.unwrap();
    let mut state = NumberState {
        decimal_point: false,
        scientific_notation: false,
        negative: buffer[start] == b'-',
    };
    while *pos < len {
        match current {
            b'0'..=b'9' => (),
            b'.' => check_decimal_point(buffer, *pos, &mut state)?,
            b'e' | b'E' | b'-' | b'+' => check_scientific_notation(buffer, *pos, &mut state)?,
            _ => break,
        }
        *pos += 1;
        // update current only if we did not reach the end of the buffer
        if *pos < len {
            current = buffer[*pos];
        }
    }

    let slice = &buffer[start..*pos];
    if !cfg!(feature = "arbitrary_precision") && is_out_of_range(slice, state) {
        return Err(NumericError {
            kind: NumericErrorKind::OutOfRange(OutOfRangeError { pos: start }),
            pos: start,
        });
    };
    Ok(())
}

// the code below would work, but we needed to parse different numeric types(u8, i64 etc.)
// when we want to parse integers for index selectors we need to use i64::MAX for our bound but
// when we parse range quantifiers for regex we need min/max to be at most 3 digits to prevent
// resource exhaustion we can't keep scanning for more than that and have a bound like i64::MAX
// we need u8 or even i8.
pub(crate) trait Atoi: Sized {
    fn atoi(buffer: &[u8], pos: &mut usize) -> Result<Self, OutOfRangeError>;
}

// Leetcode atoi baby let's go!!!!!!!!!!!
//
// this is different from reading a json number when calling next(); in that case, we didn't
// care about the value of the number, we just needed the range of the number in the initial
// buffer, also we didn't know what type of number we had(i64, f64 etc)
// we could also try to create a string by keeping track of the starting position and then
// call s.parse() but that way we would traverse the same buffer range twice, once to create the
// string and once to parse the number
//
// this way we handle overflow cases and having the number value with a single pass
// pub(super) fn atoi(buffer: &[u8], pos: &mut usize) -> Result<i64, OutOfRangeError> {
//     let mut num: i64 = 0;
//     let sign = if buffer[*pos] == b'-' {
//         *pos += 1;
//         -1
//     } else {
//         1
//     };
//
//     let start = *pos;
//     let mut current = buffer[*pos];
//     while *pos < buffer.len() && current.is_ascii_digit() {
//         if num > i64::MAX / 10 || (num == i64::MAX / 10 && current > (i64::MAX % 10) as u8) {
//             return Err(OutOfRangeError { pos: start });
//         }
//         num = num * 10 + (current - 0x30) as i64;
//         *pos += 1;
//         if *pos < buffer.len() {
//             current = buffer[*pos];
//         }
//     }
//     Ok(sign * num)
// }

// this is probably poor design
// why not have an enum NumericError with the variants below?
//
// a method like atoi() can return NumericError if nobody depended on it, like the case with JsonError
// we never do From<JsonError> for Foo but for NumericError we do;  we do: impl From<NumericError>
// for EscapeError then we call match on the NumericError variants, and we see that other than the
// InvalidHexDigit there is not a variant on EscapeError that maps to OutOfRange so we end up doing
// _ => unreachable() which is not recommended, from() should not panic!
//
// the same logic applies for impl From<NumericError> for PathError where we have the OutOfRange case
#[derive(Debug, PartialEq)]
pub(super) struct HexError {
    pub(super) digit: u8,
    pub(super) pos: usize,
}

// toDo: add the range as field
#[derive(Debug, PartialEq)]
pub(crate) struct OutOfRangeError {
    pub(crate) pos: usize,
}

impl fmt::Display for OutOfRangeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

#[derive(Debug, PartialEq)]
pub struct NumericError {
    pub(super) kind: NumericErrorKind,
    pub(super) pos: usize,
}

#[derive(Debug, PartialEq)]
pub(super) enum NumericErrorKind {
    LeadingZeros,
    InvalidSign { message: &'static str },
    InvalidScientific { message: &'static str },
    InvalidDecimal { message: &'static str },
    OutOfRange(OutOfRangeError),
}

impl fmt::Display for NumericError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // for Leading zeros add: "json specification prohibits numbers from being prefixed with a plus sign"
        match &self.kind {
            NumericErrorKind::LeadingZeros => {
                write!(
                    f,
                    "json specification prohibits numbers from being prefixed with a plus sign at index {} ",
                    self.pos
                )
            }
            NumericErrorKind::InvalidSign { message } => {
                write!(f, "{} at index {}", message, self.pos)
            }
            NumericErrorKind::InvalidScientific { message } => {
                write!(f, "{} at index {}", message, self.pos)
            }
            NumericErrorKind::InvalidDecimal { message } => {
                write!(f, "{} at index {}", message, self.pos)
            }
            NumericErrorKind::OutOfRange(err) => {
                write!(f, "{} at index {}", err, self.pos)
            }
        }
    }
}

impl error::Error for OutOfRangeError {}
impl error::Error for NumericError {}

fn is_out_of_range(buffer: &[u8], state: NumberState) -> bool {
    if state.decimal_point || state.scientific_notation {
        is_out_of_range_f64(buffer)
    } else {
        let digits = if state.negative { &buffer[1..] } else { buffer };
        is_out_of_range_i64(digits)
    }
}

// how we handle negatives?
//
// because the range is symmetric, if the number is greater than the limit in absolute value it means
// that if positive, num > LIMIT and if num is negative it is smaller than -LIMIT.
// this approach wouldn't work if LIMIT was i64::MAX because MIN/MAX are not symmetric
fn is_out_of_range_i64(buffer: &[u8]) -> bool {
    match buffer.len().cmp(&INT_LIMIT.len()) {
        // fewer digits, in range
        Ordering::Less => false,
        // same length, compares the buffers lexicographically
        Ordering::Equal => buffer > INT_LIMIT,
        // more digits, definitely out of range
        Ordering::Greater => true,
    }
}

// Overflow: Parsing a number that exceeds f64::MAX/MIN will cause parsing::<f64>() to return inf/-inf
// Based on that handling normal numbers is straightforward. If the result of parsing is inf/-inf since
// infinity and nan are not supported in the json rfc we can safely say that the number is out of range
//
// If we actually had to parsing parts of the number and check for over/under flow look at dec2flt() at src/num/dec2flt/mod.rs
fn is_out_of_range_f64(buffer: &[u8]) -> bool {
    let s = str::from_utf8(buffer).unwrap();
    let val = s.parse::<f64>().unwrap();

    val.is_infinite()
}

fn check_decimal_point(
    buffer: &[u8],
    pos: usize,
    state: &mut NumberState,
) -> Result<(), NumericError> {
    // 1.2.3
    if state.decimal_point {
        return Err(NumericError {
            kind: NumericErrorKind::InvalidDecimal {
                message: "double decimal point found",
            },
            pos,
        });
    }
    // 1. or 2.g
    if pos + 1 >= buffer.len() || !buffer[pos + 1].is_ascii_digit() {
        return Err(NumericError {
            kind: NumericErrorKind::InvalidDecimal {
                message: "decimal point must be followed by a digit",
            },
            pos,
        });
    }
    // 1e4.5
    if state.scientific_notation {
        return Err(NumericError {
            kind: NumericErrorKind::InvalidDecimal {
                message: "decimal point is not allowed after exponential notation",
            },
            pos,
        });
    }
    state.decimal_point = true;
    Ok(())
}

fn check_scientific_notation(
    buffer: &[u8],
    pos: usize,
    state: &mut NumberState,
) -> Result<(), NumericError> {
    let current = buffer[pos];

    match current {
        b'e' | b'E' => {
            // 1e2E3
            if state.scientific_notation {
                return Err(NumericError {
                    kind: NumericErrorKind::InvalidScientific {
                        message: "double exponential notation('e' or 'E') found",
                    },
                    pos,
                });
            }
            // 1e or 1eg
            if pos + 1 >= buffer.len() || !matches!(buffer[pos + 1], b'-' | b'+' | b'0'..=b'9') {
                return Err(NumericError {
                    kind: NumericErrorKind::InvalidScientific {
                        message: "exponential notation must be followed by a digit or a sign",
                    },
                    pos,
                });
            }
            // Leading zeros are allowed on the exponent 1e005 evaluates to 100000
            state.scientific_notation = true;
        }
        b'+' | b'-' => {
            // 1+2
            if !matches!(buffer[pos - 1], b'e' | b'E') {
                return Err(NumericError {
                    kind: NumericErrorKind::InvalidScientific {
                        message: "sign ('+' or '-') is only allowed as part of exponential notation",
                    },
                    pos,
                });
            }
            // 1E+g
            if pos + 1 >= buffer.len() || !buffer[pos + 1].is_ascii_digit() {
                return Err(NumericError {
                    kind: NumericErrorKind::InvalidScientific {
                        message: "exponential notation must be followed by a digit",
                    },
                    pos,
                });
            }
        }
        _ => unreachable!("Called with {} instead of 'e', 'E', '+', or '-'", current),
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[cfg(feature = "arbitrary_precision")]
    use std::str::FromStr;

    fn invalid_numbers() -> Vec<(&'static [u8], NumericError)> {
        vec![
            (
                b"+9",
                NumericError {
                    kind: NumericErrorKind::InvalidSign {
                        message: "json specification prohibits numbers from being prefixed with a plus sign",
                    },
                    pos: 0,
                },
            ),
            (
                b"-a",
                NumericError {
                    kind: NumericErrorKind::InvalidSign {
                        message: "a valid numeric value requires a digit (0-9) after the minus sign",
                    },
                    pos: 0,
                },
            ),
            (
                b"06",
                NumericError {
                    kind: NumericErrorKind::LeadingZeros,
                    pos: 0,
                },
            ),
            (
                b"1.2.3",
                NumericError {
                    kind: NumericErrorKind::InvalidDecimal {
                        message: "double decimal point found",
                    },
                    pos: 3,
                },
            ),
            (
                b"1.",
                NumericError {
                    kind: NumericErrorKind::InvalidDecimal {
                        message: "decimal point must be followed by a digit",
                    },
                    pos: 1,
                },
            ),
            (
                b"1.a",
                NumericError {
                    kind: NumericErrorKind::InvalidDecimal {
                        message: "decimal point must be followed by a digit",
                    },
                    pos: 1,
                },
            ),
            (
                b"4e5.1",
                NumericError {
                    kind: NumericErrorKind::InvalidDecimal {
                        message: "decimal point is not allowed after exponential notation",
                    },
                    pos: 3,
                },
            ),
            (
                b"1e2e5",
                NumericError {
                    kind: NumericErrorKind::InvalidScientific {
                        message: "double exponential notation('e' or 'E') found",
                    },
                    pos: 3,
                },
            ),
            (
                b"1e",
                NumericError {
                    kind: NumericErrorKind::InvalidScientific {
                        message: "exponential notation must be followed by a digit or a sign",
                    },
                    pos: 1,
                },
            ),
            (
                b"246Ef",
                NumericError {
                    kind: NumericErrorKind::InvalidScientific {
                        message: "exponential notation must be followed by a digit or a sign",
                    },
                    pos: 3,
                },
            ),
            (
                b"83+1",
                NumericError {
                    kind: NumericErrorKind::InvalidScientific {
                        message: "sign ('+' or '-') is only allowed as part of exponential notation",
                    },
                    pos: 2,
                },
            ),
            (
                b"1e+",
                NumericError {
                    kind: NumericErrorKind::InvalidScientific {
                        message: "exponential notation must be followed by a digit",
                    },
                    pos: 2,
                },
            ),
            (
                b"1e+f",
                NumericError {
                    kind: NumericErrorKind::InvalidScientific {
                        message: "exponential notation must be followed by a digit",
                    },
                    pos: 2,
                },
            ),
        ]
    }

    fn out_of_range() -> Vec<(&'static [u8], NumberState)> {
        vec![
            (
                b"1e309",
                NumberState {
                    decimal_point: false,
                    scientific_notation: true,
                    negative: false,
                },
            ),
            (
                b"-1e309",
                NumberState {
                    decimal_point: false,
                    scientific_notation: true,
                    negative: true,
                },
            ),
            // INT_LIMIT + 1
            (
                b"9007199254740992",
                NumberState {
                    decimal_point: false,
                    scientific_notation: false,
                    negative: false,
                },
            ),
            // INT_LIMIT - 1
            (
                b"-9007199254740992",
                NumberState {
                    decimal_point: false,
                    scientific_notation: false,
                    negative: true,
                },
            ),
        ]
    }

    // we don't need to test i64/i64 or f64/f64 cases, partial_cmp() for those cases is tested via
    // Rust. We test cases where cast either lhs or rhs for i64/f64, no precision loss for numbers
    // up to the upper/lower bound. For BigInt/Decimal cases we check that we correctly pass values
    // to from().
    fn comparisons() -> Vec<(Number, Number, Option<Ordering>)> {
        let mut entries = vec![
            (Number::from(5), Number::from(5.0), Some(Ordering::Equal)),
            (Number::from(3), Number::from(3.5), Some(Ordering::Less)),
            (Number::from(10), Number::from(9.9), Some(Ordering::Greater)),
            (Number::from(0), Number::from(-0.0), Some(Ordering::Equal)),
            (
                Number::from(9007199254740991),
                Number::from(4),
                Some(Ordering::Greater),
            ),
            (
                Number::from(4),
                Number::from(-9007199254740991),
                Some(Ordering::Greater),
            ),
            (Number::from(5.0), Number::from(5), Some(Ordering::Equal)),
        ];

        #[cfg(feature = "arbitrary_precision")]
        {
            // i64/BigInt
            entries.push((
                Number::from(100),
                Number::from(BigInt::from(100)),
                Some(Ordering::Equal),
            ));
            // BigInt/i64
            entries.push((
                Number::from(BigInt::from(100)),
                Number::from(100),
                Some(Ordering::Equal),
            ));
            // i64/BigDecimal
            entries.push((
                Number::from(BigInt::from(100)),
                Number::from(BigDecimal::from_str("101.2").unwrap()),
                Some(Ordering::Less),
            ));
            // BigDecimal/i64
            entries.push((
                Number::from(BigDecimal::from_str("101.2").unwrap()),
                Number::from(100),
                Some(Ordering::Greater),
            ));
            // BigDecimal/BigInt
            entries.push((
                Number::from(BigDecimal::from_str("101.2").unwrap()),
                Number::from(BigInt::from(101)),
                Some(Ordering::Greater),
            ));
            // BigInt/BigDecimal
            entries.push((
                Number::from(BigInt::from(101)),
                Number::from(BigDecimal::from_str("101.2").unwrap()),
                Some(Ordering::Less),
            ));
        }
        entries
    }

    #[test]
    fn test_comparisons() {
        for (lhs, rhs, order) in comparisons() {
            assert_eq!(
                lhs.partial_cmp(&rhs),
                order,
                "comparing {:?} with {:?}",
                lhs,
                rhs
            );
        }
    }

    #[test]
    fn test_out_of_range() {
        for (buffer, state) in out_of_range() {
            assert!(is_out_of_range(buffer, state));
        }
    }

    #[test]
    fn convert_to_u16() {
        // "12aB"
        // unwrap() uses the Debug trait, not Display
        assert_eq!(hex_to_u16(&[0x31, 0x32, 0x61, 0x42]).unwrap(), 0x12ab);
    }

    #[test]
    fn invalid_hex_digit() {
        // "12gB"
        let res = hex_to_u16(&[0x31, 0x32, 0x67, 0x42]);
        if let Err(e) = res {
            assert_eq!(
                e,
                HexError {
                    digit: 0x67,
                    pos: 2
                }
            );
        }
    }

    #[test]
    fn test_invalid_numbers() {
        for (buffer, error) in invalid_numbers() {
            let mut pos = 0;
            let result = read(buffer, &mut pos);

            assert_eq!(result, Err(error), "failed to read: {buffer:?}");
        }
    }
}
