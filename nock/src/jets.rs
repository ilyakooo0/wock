use std::{collections::BTreeMap, rc::Rc};

use crate::noun::{Atom, Noun};

pub type Jets = BTreeMap<u64, Jet>;

pub fn generate_jets() -> Jets {
    BTreeMap::from([(2989787771712986378u64, ADD)])
}

pub type Jet = fn(Rc<Noun>) -> Rc<Noun>;

static BINARY_ATOM: fn(Rc<Noun>, fn(Atom, Atom) -> Atom) -> Rc<Noun> =
    |n: Rc<Noun>, f: fn(Atom, Atom) -> Atom| {
        let Noun::Cell { p, q, .. } = (*n).clone() else {
            panic!()
        };
        let Noun::Atom(p) = (*p).clone() else {
            panic!()
        };
        let Noun::Atom(q) = (*q).clone() else {
            panic!()
        };
        Rc::new(Noun::Atom(f(p, q)))
    };

static ADD: Jet = |n| BINARY_ATOM(n, |a, b| a + b);
