use std::{
    cmp::{self, Ordering},
    rc::Rc,
    str::FromStr,
};

use num_bigint::BigUint;
use std::fmt;
use xxhash_rust::xxh3::xxh3_128;

pub type Atom = BigUint;

pub type Hash = u128;

#[derive(Clone, Debug, Eq)]
pub enum Noun {
    Atom(Atom),
    Cell {
        p: Rc<Noun>,
        q: Rc<Noun>,
        hash: Hash,
    },
}

impl Ord for Noun {
    fn cmp(&self, other: &Self) -> Ordering {
        compare_nouns(self, other)
    }
}

impl PartialOrd for Noun {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(compare_nouns(self, other))
    }
}

fn compare_nouns(p: &Noun, q: &Noun) -> Ordering {
    match (p, q) {
        (Noun::Atom(_), Noun::Cell { .. }) => Ordering::Less,
        (Noun::Cell { .. }, Noun::Atom(_)) => Ordering::Greater,
        (Noun::Atom(x), Noun::Atom(y)) => x.cmp(y),
        (Noun::Cell { hash: x, .. }, Noun::Cell { hash: y, .. }) => x.cmp(y),
    }
}

impl PartialEq for Noun {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Noun::Atom(lhs), Noun::Atom(rhs)) => lhs == rhs,

            (
                Noun::Cell {
                    p: lhs_p,
                    q: lhs_q,
                    hash: lhs_hash,
                },
                Noun::Cell {
                    p: rhs_p,
                    q: rhs_q,
                    hash: rhs_hash,
                },
            ) => lhs_hash == rhs_hash && lhs_p == rhs_p && lhs_q == rhs_q,
            _ => false,
        }
    }
}

impl Noun {
    pub const SIG: Noun = Noun::Atom(BigUint::ZERO);

    pub fn from_u32(a: u32) -> Noun {
        Noun::Atom(Atom::new(vec![a]))
    }

    pub fn list<I: Iterator<Item = Noun> + DoubleEndedIterator>(l: I) -> Rc<Noun> {
        l.rfold(Rc::new(Noun::SIG), |tail, head| cell(Rc::new(head), tail))
    }

    pub fn list_refs<I: Iterator<Item = Rc<Noun>> + DoubleEndedIterator>(l: I) -> Rc<Noun> {
        l.rfold(Rc::new(Noun::SIG), |tail, head| cell(head, tail))
    }

    pub fn unit(self: Rc<Self>) -> Rc<Noun> {
        cell(Rc::new(Noun::SIG), self)
    }

    pub fn as_atom(self: &Self) -> Option<&Atom> {
        match self {
            Noun::Atom(a) => Option::Some(a),
            _ => Option::None,
        }
    }

    pub fn as_u32(self: &Self) -> Option<u32> {
        let mut iter = self.as_atom()?.iter_u32_digits();

        match iter.len() {
            0 => Some(0),
            1 => Some(iter.next().unwrap()),
            _ => Option::None,
        }
    }

    pub fn gate_sample(self: Rc<Self>) -> Option<Rc<Self>> {
        let (_battery, payload) = self.as_cell()?;
        let (sample, _context) = payload.as_cell()?;
        Option::Some(sample)
    }

    pub fn as_cell(self: &Self) -> Option<(Rc<Noun>, Rc<Noun>)> {
        match self {
            Noun::Cell { p, q, .. } => Option::Some((p.clone(), q.clone())),
            _ => Option::None,
        }
    }

    pub fn is_sig(self: &Self) -> bool {
        match self {
            Noun::Atom(n) if *n == Atom::ZERO => true,
            _ => false,
        }
    }

    pub fn is_y(self: &Self) -> bool {
        self.is_sig()
    }

    pub fn is_n(self: &Self) -> bool {
        !self.is_sig()
    }

    pub fn list_iter(self: Rc<Self>) -> NounListIterator {
        NounListIterator { noun: self.clone() }
    }

    /// panics if malformed
    pub fn as_unit(self: &Self) -> Option<Rc<Self>> {
        if self.is_sig() {
            Option::None
        } else {
            Option::Some(self.as_cell().unwrap().1)
        }
    }

    pub fn hash(self: &Self) -> Hash {
        match self {
            Noun::Cell { hash, .. } => *hash,
            Noun::Atom(a) => xxh3_128(&*a.to_bytes_le()),
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

    pub fn as_bite(self: &Self) -> Bite {
        match self {
            Noun::Atom(_) => Bite {
                bloq: self.as_u32().unwrap(),
                step: 1,
            },

            Noun::Cell {
                p: bloq, q: step, ..
            } => Bite {
                bloq: bloq.as_u32().unwrap(),
                step: step.as_u32().unwrap(),
            },
        }
    }

    pub fn from_bytes(bytes: &[u8]) -> Self {
        Noun::Atom(Atom::from_bytes_le(bytes))
    }

    pub fn as_bytes(self: &Self) -> Option<Vec<u8>> {
        let a = self.as_atom()?;
        Some(a.to_bytes_le())
    }
}

#[derive(Clone)]
pub struct Bite {
    pub bloq: u32,
    pub step: u32,
}

impl Bite {
    pub fn bits(self: &Self) -> u32 {
        2u32.pow(self.bloq) * self.step
    }
}

#[derive(Clone)]
pub struct NounListIterator {
    noun: Rc<Noun>,
}

impl Iterator for NounListIterator {
    type Item = Rc<Noun>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.noun.is_sig() {
            Option::None
        } else {
            let (p, q) = self.noun.as_cell().unwrap();
            self.noun = q.clone();
            Option::Some(p)
        }
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
    xxh3_128(&[p.hash().to_le_bytes(), q.hash().to_le_bytes()].as_flattened())
}
