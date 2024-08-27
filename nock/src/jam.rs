use std::{collections::BTreeMap, rc::Rc};

use bitvec::vec::BitVec;
use num_traits::FromPrimitive;

use crate::noun::{Atom, Noun};

pub fn jam_to_bytes(noun: Rc<Noun>) -> Vec<u8> {
    let mut vec: BitVec<u8> = BitVec::new();
    jam(noun, &mut vec, &mut BTreeMap::new());
    vec.into_vec()
}

fn jam(noun: Rc<Noun>, vec: &mut BitVec<u8>, refs: &mut BTreeMap<Noun, usize>) {
    let offset = vec.len();

    match refs.get(&noun).cloned() {
        Some(id) => {
            let id = Atom::from_usize(id).unwrap();
            if let Noun::Atom(a) = (*noun).clone()
                && a.bits() <= id.bits()
            {
                vec.push(false);
                mat(&a, vec);
            } else {
                vec.push(true);
                vec.push(true);
                mat(&id, vec);
            }
        }
        None => {
            refs.insert((*noun).clone(), offset);

            match (*noun).clone() {
                Noun::Atom(a) => {
                    vec.push(false);
                    mat(&a, vec);
                }
                Noun::Cell { p, q, .. } => {
                    vec.push(true);
                    vec.push(false);
                    jam(p, vec, refs);
                    jam(q, vec, refs);
                }
            }
        }
    }
}

fn mat(atom: &Atom, vec: &mut BitVec<u8>) {
    if *atom == Atom::ZERO {
        vec.push(true);
    } else {
        vec.push(false);

        encode_length(atom.bits() as usize, vec);

        for b in 0..atom.bits() {
            vec.push(atom.bit(b));
        }
    }
}

fn encode_length(length: usize, vec: &mut BitVec<u8>) {
    for _ in 0..(number_of_bits(length) - 1) {
        vec.push(false);
    }
    vec.push(true);

    let mut length = length;

    while length > 1 {
        vec.push((length & 1) == 1);
        length = length >> 1;
    }
}

fn number_of_bits(mut a: usize) -> usize {
    let mut target = 0;

    while a > 0 {
        a = a >> 1;
        target += 1;
    }

    target
}
