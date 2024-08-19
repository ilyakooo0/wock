use num_integer::Integer;
use std::{collections::BTreeMap, rc::Rc};

use crate::noun::{self, cell, Atom, Noun};

pub type Jets = BTreeMap<noun::Hash, Jet>;

pub fn generate_jets() -> Jets {
    BTreeMap::from([
        (15030307987607209070707551851657016156, ADD),
        (63755436425185291665709128251960810903, DEC),
        (103625312163793685360054974742842323626, DVR),
    ])
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

static BINARY_ATOM_PAIR: fn(Rc<Noun>, fn(Atom, Atom) -> (Atom, Atom)) -> Rc<Noun> =
    |n: Rc<Noun>, f: fn(Atom, Atom) -> (Atom, Atom)| {
        let Noun::Cell { p, q, .. } = (*n).clone() else {
            panic!()
        };
        let Noun::Atom(p) = (*p).clone() else {
            panic!()
        };
        let Noun::Atom(q) = (*q).clone() else {
            panic!()
        };
        let (a, b) = f(p, q);
        cell(Rc::new(Noun::Atom(a)), Rc::new(Noun::Atom(b)))
    };

static ADD: Jet = |n| BINARY_ATOM(n, |a, b| a + b);

static DEC: Jet = |n| Rc::new(Noun::Atom(n.as_atom().unwrap() - 1u32));

static DVR: Jet = |n| BINARY_ATOM_PAIR(n, |a, b| a.div_rem(&b));
