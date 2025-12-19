use crate::parsing::value::path::filter::regexp::{CharClass, Regexp};

enum State {
    // from the paper we can see that single characters have 1 danging arrow in their fragment
    // and Split has 2
    Atom(usize, Option<usize>),
    // dangling arrows don't point anywhere(represented as None)
    // if we don't use Option we would need some other value as a placeholder
    Split(Option<usize>, Option<usize>),
    Accept,
}

// the fragments that we build are partial NFAs for each subexpression, with a different
// construction for each operator. The partial NFAs have no matching states: instead they have
// one or more dangling arrows, pointing to nothing. The construction process will finish by
// connecting these arrows to a matching state.
pub(super) struct Fragment {
    start: usize,
    // dangling arrows, the values of outs are pointers to states that are not yet connected to anything
    // dangling arrows are represented as None
    outs: Vec<usize>,
}

pub(super) struct NfaBuilder {
    states: Vec<State>,
    nodes: Vec<Regexp>,
    classes: Vec<CharClass>,
}

impl NfaBuilder {
    pub(super) fn build(mut self) -> Nfa {
        // the root of the ast in postorder is the last node that we processed
        let frag = self.walk(self.nodes.len() - 1);
        let i = self.append(State::Accept);
        // the last fragment's dangling arrows lead to an ACCEPT state
        self.patch(&frag.outs, i);

        Nfa {
            states: self.states,
            classes: self.classes,
            // the index of the first state of our automaton
            // it is not always 0
            // in ab, it is 0, the first fragment we create is the fragment for a and concatenation
            // uses f1.start as its starting state, the Regexp::Concat(lhs, rhs) case in walk()
            // but what about a | b?
            // the starting state is the index of that new state we added the Split state despite
            // processing a first and then b
            // for each case we set the expected starting state to our fragment
            // the final fragment the fragment that is essentially our automaton holds the start
            // state and the ACCEPT state; for the ACCEPT state we know exactly where it is, it is
            // the last state added so we don't need to keep track of it
            start: frag.start,
        }
    }

    pub(super) fn new(nodes: Vec<Regexp>, classes: Vec<CharClass>) -> Self {
        Self {
            states: Vec::new(),
            nodes,
            classes,
        }
    }

    fn append(&mut self, state: State) -> usize {
        self.states.push(state);
        self.states.len() - 1
    }

    // this method is used to connect the dangling arrows of some state to the next one
    fn patch(&mut self, outs: &Vec<usize>, target: usize) {
        for &i in outs {
            match &mut self.states[i] {
                State::Atom(_, out) => *out = Some(target),
                State::Split(out1, out2) => {
                    if out1.is_none() {
                        *out1 = Some(target);
                    }
                    if out2.is_none() {
                        *out2 = Some(target);
                    }
                }
                State::Accept => {}
            }
        }
    }

    // we walk the ast recursively(post order/ bottom up)
    //
    // what we define as start and outs are shown in the paper at the Converting Regular Expressions to NFAs
    // chapter
    pub(super) fn walk(&mut self, i: usize) -> Fragment {
        match self.nodes[i] {
            // empty input string
            // we create a single split state where we have 2 dangling arrows that will be connected
            // to the ACCEPT state
            Regexp::Empty => {
                let s = self.append(State::Split(None, None));
                Fragment {
                    start: s,
                    outs: vec![s],
                }
            }
            // closure under concatenation(check the notes)
            // we know that we use as start the lhs start, f1.start, and outs the rhs outs but with
            // patch we make sure we connect the lhs.outs to the rhs. start
            Regexp::Concat(lhs, rhs) => {
                // from the paper we can see that concat does not create a new state
                let f1 = self.walk(lhs);
                let f2 = self.walk(rhs);
                self.patch(&f1.outs, f2.start);
                // in this case the dangling arrows exist in f2, because we patched f1.outs to f2.start
                Fragment {
                    start: f1.start,
                    outs: f2.outs,
                }
            }
            // a | b
            // we add the new split state before 'a' and 'b' with 2 empty transitions that are
            // attached to the start of each automaton. This is the state of our automaton after:
            // let start = self.append(State::Split(Some(f1.start), Some(f2.start)));
            //
            //               ┌───► [Match 'a'] ───► ???  (dangling, in f1.outs)
            // [Split] ──────┤
            //               └───► [Match 'b'] ───► ???  (dangling, in f2.outs)
            //
            // we connect split's outs to f1/f2 start
            //
            //               ┌───► [Match 'a'] ───┐
            // [Split] ──────┤                    ├───► [Accept]
            //               └───► [Match 'b'] ───┘
            //
            // what we want after is no matter what choice we made a or b to move to the next state, this
            // happens by doing f1.expand(f2.outs), we merge the dangling arrows; important to note there
            // is no patch in this case
            Regexp::Union(lhs, rhs) => {
                let mut f1 = self.walk(lhs);
                let f2 = self.walk(rhs);
                let start = self.append(State::Split(Some(f1.start), Some(f2.start)));
                // from the paper we see that both e1 | e2 have dangling pointers
                f1.outs.extend(f2.outs);
                Fragment {
                    start,
                    outs: f1.outs,
                }
            }
            // a*
            // walk() creates the fragment for 'a' which has 1 dangling arrow as the paper shows
            // we create a new split state where out1 goes to 'a', and out2 is None(empty transition)
            // will be patched later
            // next take f.outs the dangling arrow that we have from creating fragment 'a' and make
            // it point to split to handle the multiple a's case // self.patch(&f.outs, split)
            // look at the image in the paper, easier when visualized
            Regexp::Star(idx) => {
                let f = self.walk(idx);
                let split = self.append(State::Split(Some(f.start), None));
                self.patch(&f.outs, split);
                // split is the state that has a dangling arrow, an arrow that is not yet patched
                Fragment {
                    start: split,
                    outs: vec![split],
                }
            }
            // 'a+'
            // from the paper we see that the starting state is the new state(split) and the dangling
            // arrows are from that new state; we also patch the dangling arrows of 'a' to loop back to
            // the new split state
            // the starting state for this new fragment is f.start because we expect at least one
            // 'a+' needs at least one 'a' so the fragment starts from f.start(a)
            // 'a?' and 'a*' start from the new split state because it can be 'a' or empty
            Regexp::Plus(idx) => {
                let f = self.walk(idx);
                let split = self.append(State::Split(Some(f.start), None));
                self.patch(&f.outs, split);
                Fragment {
                    start: f.start,
                    outs: vec![split],
                }
            }
            // we can see that in the above 2 cases where we have a loop we always call patch() to
            // make a's dangling pointer(f.outs) point back to split; for a? this is not the case
            //
            // question is just union in disguise: a? is a | ε
            // for this reason we call f.outs.push(split) similar to how we called f1.expand(f2.outs)
            //
            //               ┌───► [Match 'a'] ───► ???  (dangling, from f.outs)
            // [Split] ──────┤
            //               └───► ???                   (dangling, the skip path)
            //
            //
            //               ┌───► [Match 'a'] ───┐
            // [Split] ──────┤                    ├───► [Accept]
            //               └────────────────────┘
            //
            // in either case both need to be patched later to point to Accept
            Regexp::Question(idx) => {
                let mut f = self.walk(idx);
                let split = self.append(State::Split(Some(f.start), None));
                f.outs.push(split);
                Fragment {
                    start: split,
                    outs: f.outs,
                }
            }
            Regexp::Atom(idx) => {
                // this none is the dangling arrow that the paper shows for matching a single character
                // for example if all we have is 'a' after walking the ast we will connect(patch) this
                // dangling arrow to the ACCEPT state
                // eventually those pending arrows will be connected by calling patch()
                let start = self.append(State::Atom(idx, None));
                Fragment {
                    start,
                    // start is the state that has a dangling arrow, an arrow that is not yet patched
                    outs: vec![start],
                }
            }
        }
    }
}

pub(super) struct Nfa {
    states: Vec<State>,
    // read the comment on parser struct in regexp.rs
    classes: Vec<CharClass>,
    start: usize,
}

impl Nfa {
    // for detailed examples look at the notes: Tracing
    //
    // in the full match case we return true when we are done processing the input and we 1 Accept
    // state
    pub(super) fn full_match(&mut self, input: &str) -> bool {
        let mut states = self.advance_epsilon(vec![self.start]);

        for c in input.chars() {
            states = self.step(states, c);
            states = self.advance_epsilon(states);
        }
        states
            .into_iter()
            .any(|s| matches!(self.states[s], State::Accept))
    }

    pub(super) fn partial_match(&mut self, input: &str) -> bool {
        let mut states = self.advance_epsilon(vec![self.start]);

        for c in input.chars() {
            states = self.step(states, c);
            states.push(self.start);
            states = self.advance_epsilon(states);

            if states
                .iter()
                .any(|s| matches!(self.states[*s], State::Accept)) {
                return true;
            }
        }
        false
    }

    // if the current state is match we advance otherwise the path dies
    // we return the next set of states
    fn step(&self, states: Vec<usize>, c: char) -> Vec<usize> {
        let mut next = Vec::new();

        for i in states {
            if let State::Atom(class_idx, out) = self.states[i] {
                // toDo: we need to optimize the matches in CharClass
                if self.classes[class_idx].matches(c) {
                    next.push(out.unwrap());
                }
            }
        }
        next
    }

    // looks at the current set of states and performs "perfect guessing"; if the current state is
    // a Split we select both paths
    //
    //               ┌───► [Match 'a'] ───┐
    // [Split] ──────┤                    ├───► next state
    //               └───► [Match 'b'] ───┘
    //
    //
    // we are at split ,and we move towards both directions at the same time; we advance to the
    // next state without consuming any input character, we essentially eliminate those empty
    // transitions. Any other state is either Accept or a CharClass state where we need a char to
    // advance
    fn advance_epsilon(&self, states: Vec<usize>) -> Vec<usize> {
        let mut next = Vec::new();
        let mut stack = states;
        let mut seen: u64 = 0;

        // This is part is very similar to how we walk graphs. We need to avoid cycles, the cases
        // where we have loops
        // In graph traversals we use a preallocated vector initialized to false and mark as we walk.
        // State machines have no memory, they don't know the path to the current state the only know
        // where they are now. We don't need global state to which states we have seen across every step
        // that we made so far, just for the current step. Allocating every time we make step is
        // inefficient. We can either use a bitset as we do bellow or if the number of states
        // increases we can go with u128 or Sparse set.
        //
        // We have seen this before with 1 byte and the days of the week and also on Leetcode.
        // Each state is 1 bit, if that bit is set we visited that state at the current step, and we
        // can skip it, otherwise set the bit
        while let Some(i) = stack.pop() {
            if seen & (1 << i) != 0 {
                continue;
            }
            seen |= 1 << i;
            next.push(i);
            if let State::Split(out1, out2) = self.states[i] {
                stack.push(out1.unwrap());
                stack.push(out2.unwrap());
            }
        }
        next
    }
}
