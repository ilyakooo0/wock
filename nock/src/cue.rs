use std::{collections::BTreeMap, rc::Rc};

use bitter::{BitReader, LittleEndianReader};
use num_bigint::BigUint;

use crate::noun::{cell, Atom, Noun};

pub fn cue_bytes(bytes: &[u8]) -> Rc<Noun> {
    cue_reader(
        &mut LittleEndianReader::new(bytes),
        &mut 0,
        &mut BTreeMap::new(),
    )
}

fn cue_reader(
    reader: &mut LittleEndianReader,
    offset: &mut u32,
    refs: &mut BTreeMap<u32, Rc<Noun>>,
) -> Rc<Noun> {
    let original_offset: u32 = offset.clone();

    let is_atom = !reader.read_bit().unwrap();
    *offset += 1;

    if is_atom {
        let atom = Rc::new(rub_reader(reader, offset));
        refs.insert(original_offset, atom.clone());
        atom
    } else {
        let is_ref = reader.read_bit().unwrap();
        *offset += 1;

        if is_ref {
            // This can be optimized.
            let ref_id = {
                let id = rub_reader(reader, offset);
                let a = id.as_atom().unwrap();
                let mut a_iter = a.iter_u32_digits();
                match a_iter.len() {
                    0 => 0,
                    1 => a_iter.next().unwrap(),
                    _ => panic!(),
                }
            };
            refs.get(&ref_id).unwrap().clone()
        } else {
            let p = cue_reader(reader, offset, refs);
            let q = cue_reader(reader, offset, refs);
            let x = cell(p, q);
            refs.insert(original_offset, x.clone());
            x
        }
    }
}

fn rub_reader(reader: &mut LittleEndianReader, offset: &mut u32) -> Noun {
    let is_zero = reader.read_bit().unwrap();
    *offset += 1;
    if is_zero {
        Noun::SIG
    } else {
        let length_of_length = count_zeros(reader);
        *offset += length_of_length + 1;
        let length = reader.read_bits(length_of_length).unwrap() | (1u64 << length_of_length);
        *offset += length_of_length;
        let atom = read_bits(reader, length);
        *offset += length as u32;
        Noun::Atom(atom)
    }
}

fn count_zeros(reader: &mut LittleEndianReader) -> u32 {
    let mut count = 0u32;
    while !reader.read_bit().unwrap() {
        count = count + 1;
    }
    count
}

fn read_bits(reader: &mut LittleEndianReader, bits: u64) -> Atom {
    let u32s = bits / 32;
    let bits = bits % 32;

    let mut v = Vec::new();

    for _ in 0..u32s {
        v.push(reader.read_u32().unwrap());
    }

    {
        let mut digit = 0u32;
        for shift in 0..bits {
            digit = digit
                | (if reader.read_bit().unwrap() {
                    1 << shift
                } else {
                    0
                });
        }
        v.push(digit);
    }

    BigUint::new(v)
}
