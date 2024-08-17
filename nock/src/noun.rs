use std::rc::Rc;

use num_bigint::BigUint;
use std::fmt;

pub type Atom = BigUint;

#[derive(PartialEq, Clone, Debug)]
pub enum Noun {
    Atom(Atom),
    Cell(Rc<Noun>, Rc<Noun>)
}

impl Noun {
    pub const SIG: Noun = Noun::Atom(BigUint::ZERO);
    pub fn atom(self: &Self) -> Option<&Atom> {
        match self {
            Noun::Atom(a) => Option::Some(a),
            _ => Option::None
        }
    }
}

impl fmt::Display for Noun {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Noun::Cell(p, q) => write!(f, "[{} {}]", p, q),
            Noun::Atom(a) => write!(f, "{}", a)
        }
    }
}

pub fn cell(p: Rc<Noun>, q: Rc<Noun>) -> Rc<Noun> {
    Rc::new(Noun::Cell(p, q))
}
        
