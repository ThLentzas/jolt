use core::str;
use crate::parsing::error::{KeywordError, KeywordErrorKind};
use crate::parsing::{error::ParseError, parse::Parser, value::Value};
use memchr;

pub(super) mod error;
mod escapes;
mod lex;
mod map;
pub(super) mod number;
mod parse;
mod utf8;
pub(super) mod value;

//implementation limits: https://www.ibm.com/docs/en/datapower-gateway/10.6.0?topic=20-json-parser-limits
const INPUT_BUFFER_LIMIT: usize = 5_242_880;
// the number of characters in the String after parsing
// in the worst case, where we have only Unicode sequences, the length of the input buffer is roughly
// 49_000 bytes
const STRING_LENGTH_LIMIT: usize = 8192;
const NESTING_DEPTH_LIMIT: u16 = 128;

pub(super) fn parse(buffer: &[u8]) -> Result<Value, ParseError> {
    Parser::new(buffer).parse()
}

// used to read true, false, null during parsing and functions name of filter selectors
fn read_keyword(buffer: &[u8], pos: &mut usize, keyword: &[u8]) -> Result<(), KeywordError> {
    let remaining = &buffer[*pos..];

    if keyword.len() > remaining.len() {
        return Err(KeywordError {
            kind: KeywordErrorKind::UnexpectedEof,
            pos: buffer.len() - 1,
        });
    }

    for c in keyword.iter() {
        if buffer[*pos] != *c {
            return Err(KeywordError {
                kind: KeywordErrorKind::UnexpectedCharacter { byte: buffer[*pos] },
                pos: *pos,
            });
        }
        *pos += 1;
    }
    Ok(())
}

fn is_rfc_whitespace(b: u8) -> bool {
    matches!(b, b'\t' | b'\n' | b'\r' | b' ')
}

fn skip_whitespaces(buffer: &[u8], pos: &mut usize) {
    while *pos < buffer.len() {
        if is_rfc_whitespace(buffer[*pos]) {
            *pos += 1;
        } else {
            break;
        }
    }
}

// finds the needle while considering escape sequences
//
// let end = self.buffer[self.pos..]
//     .iter()
//     // returns leftmost first
//     .position(|&b| b == b'/')
//     .map(|i| self.pos + i)
//     .unwrap_or(len);
//
// the above code won't work for cases like "hello\"world" and /foo\/bar/baz where '"' and '/' are
// escaped
//
// why the predicate?
// 
// if we are looking for 'a' and encounter it as '\a' the previous logic of randomly skipping everything
// after \ would incorrectly miss it, so we would either return None or not the leftmost occurrence.
// We need to know which characters to consider after '\' and they are not always the same; we have
// an edge case in path when we have to scan for closing single quote '\''. If we used a function we
// would miss it in a case where it is escaped, it won't match in matches and return its index in
// the escape sequence. 'foo\'bar', find() should return 10 not 5
fn find<P>(haystack: &[u8], needle: u8, predicate: P) -> Option<usize>
where
    P: Fn(u8) -> bool,
{
    let mut pos = 0;
    let len = haystack.len();
    while pos < len {
        // be aware that memchr2 returns the position relative to the slice not the entire buffer
        // so we need to adjust and get the absolute position when we index into the buffer
        //
        // if memchr2() returns the index of '\', we peek to determine what decision to make.
        //
        // Case 1: out of bounds -> incomplete sequence -> None
        // Case 2: not a valid escape, just skip '\', if the needle is found later, our logic will
        // return an UnknownCharacter for the invalid sequence rather than Eof
        // Case 3: valid escape, skip it
        match memchr::memchr2(b'\\', needle, &haystack[pos..]) {
            Some(p) if (haystack[pos + p]) == b'\\' => {
                // incomplete escape
                if pos + p + 1 >= len {
                    return None;
                }
                // invalid escape
                if !predicate(haystack[pos + p + 1]) {
                    pos = p + 1;
                } else {
                    // valid escape, skip it
                    pos = pos + p + 2;
                }
            }
            Some(p) => return Some(pos + p),
            None => break,
        }
    }
    None
}

// returns the text representation of a json string; it is used in normalized path and in fmt_pretty
// this is why we pass quote as an argument, because in npaths we need single and for pretty we want
// double
//
// For Vale::String("foo\nbar") the output is "\"foo\\nbar\""
fn to_jstr(s: &str, quote: char) -> String {
    let buffer = s.as_bytes();
    let len = buffer.len();
    let mut pos = 0;
    let mut val = String::with_capacity(s.len() + 2);
    val.push(quote);

    while pos < len {
        let j = pos;
        while pos < len {
            if buffer[pos]  < 0x20 || buffer[pos] == b'\"' || buffer[pos] == b'\\' {
                break;
            }
            pos += utf8::char_width(buffer[pos]);
        }
        if j < pos {
            val.push_str(unsafe { str::from_utf8_unchecked(&buffer[j..pos]) });
        }
        if pos >= len {
            break;
        }

        // we reverse the logic we applied during parsing
        // we mapped '\' and '\n' to '\n'
        // now we split '\n' to \' and 'n'
        // what we want to achieve is represent the character with some text
        // it is not possible for all characters, this is why for the 00 - 1F range we use the Unicode
        // sequence
        match buffer[pos] {
            b'\'' => val.push_str("\\'"),
            b'\\' => val.push_str("\\\\"),
            0x08 => val.push_str("\\b"),
            0x09 => val.push_str("\\t"),
            0x0A => val.push_str("\\n"),
            0x0C => val.push_str("\\f"),
            0x0D => val.push_str("\\r"),
            b => val.push_str(&format!("\\u{:04x}", b)),
        }
        // advance for escape
        pos += 1;
    }
    val.push(quote);

    val
}
