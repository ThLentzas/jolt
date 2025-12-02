use crate::parsing::value::Value;
use std::rc::Rc;

pub(crate) trait Tracker<'a>: Clone {
    type Trace: Clone;

    fn root() -> Self::Trace;
    // keys live as long as the root, since they exist in root
    fn descend(parent: &Self::Trace, step: Step<'a>) -> Self::Trace;
}

#[derive(Copy, Clone)]
pub(crate) struct NoOpTracker;

impl Tracker<'_> for NoOpTracker {
    type Trace = ();

    fn root() -> Self::Trace {}
    fn descend(_: &(), _: Step) -> () {} // this works as well {()} because it also returns ()
}

#[derive(Copy, Clone)]
pub(crate) struct PathTracker;

impl<'a> Tracker<'a> for PathTracker {
    // Trace needs to have at least the same visibility as PathTrace
    type Trace = Option<Rc<PathTrace<'a>>>;

    fn root() -> Self::Trace {
        Some(Rc::new(PathTrace {
            parent: None,
            step: Step::Root,
        }))
    }

    fn descend(parent: &Self::Trace, step: Step<'a>) -> Self::Trace {
        if let Some(p) = parent {
            Some(Rc::new(PathTrace {
                // idiomatic way according to the docs, don't use p.clone()
                parent: Some(Rc::clone(p)),
                step,
            }))
        } else {
            None
        }
    }
}

// It describes only the specific move we just made, where we stepped into.
pub(crate) enum Step<'a> {
    Root,
    Key(&'a str),
    Index(usize),
}

// it describes how we got where we are now, parent is the path up to now and step is where we just
// stepped(the latest move), so we can trace the path back to root following the parent pointer each
// time; current path = parent path + step
pub(crate) struct PathTrace<'a> {
    // pointer to the parent(the root to parent path)
    parent: Option<Rc<PathTrace<'a>>>,
    step: Step<'a>,
}

impl<'a> PathTrace<'a> {
    pub(crate) fn to_npath(&self) -> String{
        let mut steps = Vec::new();
        let mut current = self;

        loop {
            match &current.step {
                Step::Root => break,
                step => {
                    steps.push(step);
                }
            }
            // we can't call current = current.parent.unwrap().as_ref()
            // current.parent returns own data, current is a reference, and we are accessing via the
            // ref the parent field of PathTrace, then we call unwrap().
            // unwrap takes ownership of Option and returns T, but we are borrowing from self, and
            // we can't move out data that we are borrowing
            // current.parent.as_ref().unwrap().as_ref() gives us a reference to parent, we move up
            // by 1 level
            current = current.parent.as_ref().unwrap().as_ref();
        }
        let mut path = String::new();
        path.push('$');

        for step in steps.into_iter().rev() {
            path.push('[');
            match step {
                Step::Root => (),
                Step::Key(key) => path.push_str(format_name(key).as_str()),
                // char in Rust is a Unicode Scalar value so it is safe cast
                Step::Index(i) => path.push(char::from_u32(*i as u32).unwrap()),
            }
            path.push(']');
        }
        path
    }
}

// formats a name selector to be compliant with the npath requirements
fn format_name(name: &str) -> String {
    let mut val = String::new();
    val.push('\'');

    // we reverse the logic we applied during parsing
    // we mapped '\' and '\n' to '\n'
    // now we split '\n' to \' and '\n'
    // what we want to achieve is represent the character with some text presentation
    // it is not possible for all characters, this is why for the 00 - 1F range we use the Unicode
    // sequence
    for c in name.chars() {
        match c {
            '\'' => val.push_str("\\'"),
            '\\' => val.push_str("\\\\"),
            '\u{0008}' => val.push_str("\\b"),
            '\u{000C}' => val.push_str("\\f"),
            '\n' => val.push_str("\\n"),
            '\r' => val.push_str("\\r"),
            '\t' => val.push_str("\\t"),
            c if c < '\u{0020}' => {
                val.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => val.push(c),
        }
    }
    val
}

// value and the root to value path
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Cursor<'a, T> {
    pub(crate) val: &'a Value,
    pub(crate) trace: T,
}

// Consumes Cursor returns Node
impl<'a> From<Cursor<'a, Option<Rc<PathTrace<'a>>>>> for Node<'a> {
    fn from(cursor: Cursor<'a, Option<Rc<PathTrace<'a>>>>) -> Self {
        let path = cursor.trace.unwrap().to_npath();
        Node {
            value: cursor.val,
            path,
        }
    }
}

// what we return to the user
#[derive(Debug, Clone)]
pub struct Node<'a> {
    pub value: &'a Value,
    pub path: String,
}
