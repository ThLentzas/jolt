use crate::parsing::value::Value;
use std::rc::Rc;

// It describes only the specific move we just made, where we stepped into.
// lifetimes: Step holds a reference to a key of an object that lives in root
pub(crate) enum Step<'r> {
    // Root as a step variant is just the 1st step we make when we start processing
    Root,
    Key(&'r str),
    Index(usize),
}

// lifetimes: read Step
pub(crate) trait Tracker<'r> {
    type Trace;

    fn root() -> Self::Trace;
    fn descend(parent: &Self::Trace, step: Step<'r>) -> Self::Trace;
}

pub(crate) struct NoOpTracker;

impl Tracker<'_> for NoOpTracker {
    type Trace = ();

    fn root() -> Self::Trace {}
    fn descend(_: &(), _: Step) -> () {} // this works as well {()} because it also returns ()
}

pub(crate) struct PathTracker;

impl<'r> Tracker<'r> for PathTracker {
    // We use Option because root has no parent path
    type Trace = Option<Rc<PathTrace<'r>>>;

    fn root() -> Self::Trace {
        Some(Rc::new(PathTrace {
            parent: None,
            step: Step::Root,
        }))
    }

    // This is how we keep track of the paths as we walk the AST
    // The naive approach would be to hold a Vec<String>. Every current path is a full string. To
    // extend the path, we must copy the old string and append the new segment
    //
    // {
    //  "store":
    //      {
    //          "books": [ "A", "B" ]
    //      }
    // }
    //
    // Start with root: '$'
    // Append 'store' -> allocate a new string and append
    // Now for every element in books we need to append the index to the parent path
    // For index 0 -> take the parent path, path: $['store']['books'] and append the index. We need
    // to allocate a new String and then append [0], path for 'A': $['store']['books'][0]
    // For index 1 -> take the parent path, path: $['store']['books'] and append the index. We need
    // to allocate a new String and then append [1], path for 'B': $['store']['books'][1]
    // If nesting increases, and we have more items we will die by a thousand allocations
    //
    // The problem with cloning and appending for each node in the list occurs because the path
    // history forms a Tree, not a single straight line. If we had only singular selectors(name/index)
    // and 1 selector per segment, $.store.info.name, we would never need Rc, we could just mutate
    // a single buffer. We would have 1 parent -> 1 child
    // Thins break when we longer have a one-to-one relationship but one to many where multiple children
    // share the same parent. Like in the case above, calling a multi-selector like *, or slice on
    // books would return all its children and those share the same parent.
    //
    // To solve the branching problem we are going to use the Breadcrumb pattern/ Structural sharing
    // which allows us to share an unchanged data(parent) without copying them.
    // As we walk the AST we create PathNodes, these what the reader/writer holds. A PathNode holds
    // a PathTrace which is the path on how we got here, a pointer to the parent, the step we
    // just made, and the value of the given path.
    //
    // When we apply a selector like '*' on the current node, we call descend by passing the trace
    // of current, which is the path to the parent node, and the step we just made; descend() will
    // create a trace for each child node by cloning the parent, for Rc this is just a pointer increment
    // and appending the step. Now all the children share the same parent without copying any data.
    //
    // on how we create the npath for each PathNode read to_npath() below
    fn descend(parent: &Self::Trace, step: Step<'r>) -> Self::Trace {
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

// it describes how we got where we are now, parent is the path up to now and step is where we just
// stepped(the latest move), so we can trace the path back to root following the parent pointer each
// time; current path = parent path + step
//
// It is a recursive type, and it compiles because similarly to LogicalExpr we wrap it in a smart pointer
// Rc in this case(very similar to ConsList described in the docs)
//
// This is struct that we use as Trace in the PathTracker impl of the Tracker trait.
//
// lifetimes: read Step
pub(crate) struct PathTrace<'r> {
    // pointer to the parent(the root to parent path)
    parent: Option<Rc<PathTrace<'r>>>,
    step: Step<'r>,
}

impl<'r> PathTrace<'r> {
    // At this point we are done processing the query, and we have the list of PathNodes that we
    // will return to the user. These nodes are the leaves of the subtree we walked. Now we have to
    // generate the npaths. Previously we walked the tree in a top-down approach now we are going
    // to do it, bottom-up, leaf to root. We have seen this before in LCA on Leetcode, we set current
    // to current's parent, and we keep track of the step we just made. When we reach the root
    // the vec of steps holds the leaf to root path; we want root to leaf so we iterate steps in
    // reverse order and build the npath
    pub(crate) fn to_npath(&self) -> String {
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
                // we can't call char::from_u32(*i as u32).unwrap()
                // 'i' in this case is the codepoint, so if index is 1 we get back '\u{1}' which
                // is a control character
                Step::Index(i) => path.push_str(&*i.to_string()),
            }
            path.push(']');
        }
        path
    }
}

// value and the root to value path
#[derive(Debug, PartialEq, Clone)]
pub(crate) struct PathNode<'r, T> {
    pub(crate) trace: T,
    pub(crate) val: &'r Value,
}

// Consumes PathNode returns Node
impl<'r> From<PathNode<'r, Option<Rc<PathTrace<'r>>>>> for Node<'r> {
    fn from(cursor: PathNode<'r, Option<Rc<PathTrace<'r>>>>) -> Self {
        let path = cursor.trace.unwrap().to_npath();
        Node {
            val: cursor.val,
            path,
        }
    }
}

// what we return to the user
// lifetimes: val references a Value that lives in root
#[derive(Debug, PartialEq, Clone)]
pub struct Node<'r> {
    pub val: &'r Value,
    pub path: String,
}

// formats a name selector to be compliant with the npath requirements
fn format_name(name: &str) -> String {
    let mut val = String::new();
    val.push('\'');

    // we reverse the logic we applied during parsing
    // we mapped '\' and '\n' to '\n'
    // now we split '\n' to \' and 'n'
    // what we want to achieve is represent the character with some text
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
            // [00,1F]
            c if c < '\u{0020}' => {
                // always lowercase
                val.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => val.push(c),
        }
    }
    val.push('\'');
    val
}
