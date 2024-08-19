use std::{rc::Rc, str::FromStr};

use num_bigint::BigUint;
use std::fmt;
use xxhash_rust::xxh3::xxh3_64;

pub type Atom = BigUint;

pub type Hash = u64;

#[derive(PartialEq, Clone, Debug)]
pub enum Noun {
    Atom(Atom),
    Cell {
        p: Rc<Noun>,
        q: Rc<Noun>,
        hash: Hash,
    },
}

impl Noun {
    pub const SIG: Noun = Noun::Atom(BigUint::ZERO);
    pub fn as_atom(self: &Self) -> Option<&Atom> {
        match self {
            Noun::Atom(a) => Option::Some(a),
            _ => Option::None,
        }
    }

    pub fn hash(self: &Self) -> Hash {
        match self {
            Noun::Cell { hash, .. } => *hash,
            Noun::Atom(a) => xxh3_64(&*a.to_bytes_le()),
        }
    }

    /// returns the hash and the current sample
    pub fn hash_gate(self: &Self) -> (Hash, Rc<Noun>) {
        let Noun::Cell {
            p: battery,
            q: payload,
            ..
        } = self
        else {
            panic!()
        };
        let Noun::Cell {
            p: sample,
            q: context,
            ..
        } = (**payload).clone()
        else {
            panic!()
        };

        (hash_pair((*battery).clone(), context), sample)
    }
}

impl fmt::Display for Noun {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(print_noun(Rc::new(self.clone()), false).as_str())
    }
}

fn print_noun(noun: Rc<Noun>, is_rhs: bool) -> String {
    match (*noun).clone() {
        Noun::Cell { p, q, .. } if is_rhs => {
            format!("{} {}", print_noun(p, false), print_noun(q, true))
        }
        Noun::Cell { p, q, .. } => format!("[{} {}]", print_noun(p, false), print_noun(q, true)),
        Noun::Atom(a) => {
            let atom_bytes = a.to_bytes_le();
            if atom_bytes.len() > 1 && atom_bytes.into_iter().all(|c| (c > 33) && c < 126) {
                format!("%{}", unsafe {
                    String::from_utf8_unchecked(a.to_bytes_le())
                })
            } else if a == BigUint::ZERO {
                String::from_str("~").unwrap()
            } else {
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
                foo
            }
        }
    }
}

pub fn cell(p: Rc<Noun>, q: Rc<Noun>) -> Rc<Noun> {
    Rc::new(Noun::Cell {
        p: p.clone(),
        q: q.clone(),
        hash: hash_pair(p, q),
    })
}

fn hash_pair(p: Rc<Noun>, q: Rc<Noun>) -> Hash {
    xxh3_64(&[p.hash().to_le_bytes(), q.hash().to_le_bytes()].as_flattened())
}
