#![feature(iter_intersperse)]
#![feature(let_chains)]

use core::str;
use std::{borrow::Borrow, rc::Rc};

use interpreter::{eval_pulled_gate, slam_pulled_gate, tar, InterpreterContext, Tanks};
use noun::{cell, Noun};
pub mod cue;
pub mod interpreter;
pub mod jam;
mod jets;
pub mod noun;

pub type Vase = (Rc<Noun>, Rc<Noun>);

pub fn wash(ctx: &mut InterpreterContext, urbit: &Rc<Noun>, tanks: Tanks) -> String {
    let mut target = String::new();
    for (subj, q) in tanks.iter() {
        match tar(ctx, subj.clone(), q).unwrap().borrow() {
            Noun::Atom(a) => {
                target.push_str(str::from_utf8_mut(&mut *a.to_bytes_le()).unwrap());
            }
            gate @ Noun::Cell { .. } => {
                let tank = eval_pulled_gate(ctx, Rc::new(gate.clone())).unwrap();
                let mut str = slam_pulled_gate(
                    ctx,
                    &urbit,
                    &cell(
                        &Rc::new(Noun::from_bytes(b"wash")),
                        &cell(
                            &cell(&Rc::new(Noun::from_u32(0)), &Rc::new(Noun::from_u32(80))),
                            &tank,
                        ),
                    ),
                )
                .unwrap()
                .as_atom()
                .unwrap()
                .to_bytes_le();

                target.push_str(str::from_utf8_mut(&mut *str).unwrap())
            }
        }
    }

    target
}
