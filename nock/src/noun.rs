use std::rc::Rc;

use num_bigint::BigUint;
use std::fmt;

pub type Atom = BigUint;

#[derive(PartialEq, Clone, Debug)]
pub enum Noun {
    Atom(Atom),
    Cell(Rc<Noun>, Rc<Noun>),
}

impl Noun {
    pub const SIG: Noun = Noun::Atom(BigUint::ZERO);
    pub fn as_atom(self: &Self) -> Option<&Atom> {
        match self {
            Noun::Atom(a) => Option::Some(a),
            _ => Option::None,
        }
    }
}

impl fmt::Display for Noun {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Noun::Cell(p, q) => write!(f, "[{} {}]", p, q),
            Noun::Atom(a) => {
                let mut result = String::new();
                let mut counter = 0;
                for char in a.to_string().chars().rev() {
                    if counter == 3 {
                        result.push('.');
                        counter = 0;
                    }
                    counter += 1;
                    result.push(char);
                }
                let foo: String = result.chars().rev().collect();
                f.write_str(&foo)
            }
        }
    }
}

pub fn cell(p: Rc<Noun>, q: Rc<Noun>) -> Rc<Noun> {
    Rc::new(Noun::Cell(p, q))
}
