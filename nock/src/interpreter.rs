use core::panic;
use std::rc::Rc;

use num_bigint::BigUint;

use crate::noun::*;

fn one_big_uint() -> BigUint {
    BigUint::new(vec![1])
}

fn one() -> Noun {
    Noun::Atom(one_big_uint())
}

fn atom(num: u32) -> Noun {
    Noun::Atom(BigUint::new(vec![num]))
}
fn atom_ref(num: u32) -> Rc<Noun> {
    Rc::new(atom(num))
}

fn wut(noun: Rc<Noun>) -> Noun {
    match *noun {
        Noun::Cell(_, _) => Noun::SIG,
        Noun::Atom(_) => atom(1),
    }
}

fn lus(noun: Rc<Noun>) -> Noun {
    match (*noun).clone() {
        Noun::Cell(_, _) => panic!(),
        Noun::Atom(atm) => Noun::Atom(atm + 1u32),
    }
}

fn tis(lhs: Rc<Noun>, rhs: Rc<Noun>) -> Noun {
    if lhs == rhs {
        Noun::SIG
    } else {
        atom(1)
    }
    // match ((*lhs).clone(), (*rhs).clone()) {
    //     (Noun::Atom(lhs), Noun::Atom(rhs)) => if lhs == rhs {Noun::SIG} else {atom(1)}
    //     (Noun::Cell(lhs_lhs, lhs_rhs), Noun::Cell(rhs_lhs, rhs_rhs)) =>
    //       if *lhs_lhs == *rhs_lhs && *lhs_rhs == *rhs_rhs {Noun::SIG} else {atom(1)} ,
    //     _ => panic!()
    // }
}

fn fas(addr: Atom, noun: Rc<Noun>) -> Rc<Noun> {
    let mut addr_iter = addr.iter_u32_digits();

    match addr_iter.len() {
        0 => panic!(),
        1 => fas_u32(addr_iter.next().expect("invariant"), noun),
        _ => {
            let rest = fas(&addr >> 1, noun);
            fas_u32(
                if (addr & one_big_uint()) == one_big_uint() {
                    3
                } else {
                    2
                },
                rest,
            )
        }
    }
}

fn fas_u32(addr: u32, noun: Rc<Noun>) -> Rc<Noun> {
    match addr {
        0 => panic!(),
        1 => noun,
        n => match (*noun).clone() {
            Noun::Atom(_) => panic!(),
            Noun::Cell(p, q) => match n {
                2 => p,
                3 => q,
                _ => {
                    let rest = fas_u32(n >> 1, cell(p, q));
                    fas_u32(if (n & 1) == 1 { 3 } else { 2 }, rest)
                }
            },
        },
    }
}

fn hax_u32(addr: u32, new_value: Rc<Noun>, target: Rc<Noun>) -> Rc<Noun> {
    match addr {
        0 => panic!(),
        1 => new_value,
        _ => match (*target).clone() {
            Noun::Atom(_) => panic!(),
            Noun::Cell(p, q) => {
                if (addr & 1) == 1 {
                    cell(hax_u32(addr, new_value, p), q)
                } else {
                    cell(p, hax_u32(addr, new_value, q))
                }
            }
        },
    }
}

fn hax(addr: Atom, new_value: Rc<Noun>, target: Rc<Noun>) -> Rc<Noun> {
    let mut addr_iter = addr.iter_u32_digits();

    match addr_iter.len() {
        0 => panic!(),
        1 => hax_u32(addr_iter.next().expect("invariant"), new_value, target),
        _ => {
            let a = &addr >> 1;
            if (&addr & one_big_uint()) == one_big_uint() {
                hax(
                    a,
                    cell(fas(addr - 1u32, target.clone()), target.clone()),
                    target.clone(),
                )
            } else {
                hax(
                    a,
                    Rc::new(Noun::Cell(new_value, fas(addr + 1u32, target.clone()))),
                    target,
                )
            }
        }
    }
}

fn tar_u32(subj: Rc<Noun>, op: u32, formula: Rc<Noun>) -> Rc<Noun> {
    match op {
        0 => {
            let Noun::Atom(b) = (*formula).clone() else {
                panic!()
            };
            fas(b, subj)
        }
        1 => formula,
        2 => {
            let Noun::Cell(b, c) = (*formula).clone() else {
                panic!()
            };
            tar(tar(subj.clone(), b), tar(subj, c))
        }
        3 => Rc::new(wut(tar(subj, formula))),
        4 => Rc::new(lus(tar(subj, formula))),
        5 => {
            let Noun::Cell(b, c) = (*formula).clone() else {
                panic!()
            };
            Rc::new(tis(tar(subj.clone(), b), tar(subj, c)))
        }
        6 => {
            let Noun::Cell(b, c) = (*formula).clone() else {
                panic!()
            };
            let Noun::Cell(c, d) = (*c).clone() else {
                panic!()
            };

            tar(
                subj.clone(),
                tar_u32(
                    cell(c, d),
                    0,
                    tar_u32(
                        cell(atom_ref(2), atom_ref(3)),
                        0,
                        tar_u32(subj, 4, cell(atom_ref(4), b)),
                    ),
                ),
            )
        }
        7 => {
            let Noun::Cell(b, c) = (*formula).clone() else {
                panic!()
            };
            tar(tar(subj, b), c)
        }
        8 => {
            let Noun::Cell(b, c) = (*formula).clone() else {
                panic!()
            };
            tar(cell(tar(subj.clone(), b), subj), c)
        }
        9 => {
            let Noun::Cell(b, c) = (*formula).clone() else {
                panic!()
            };
            tar_u32(
                tar(subj, c),
                2,
                cell(
                    cell(Rc::new(Noun::SIG), Rc::new(one())),
                    cell(Rc::new(Noun::SIG), b),
                ),
            )
        }
        10 => {
            let Noun::Cell(b, d) = (*formula).clone() else {
                panic!()
            };
            let Noun::Cell(b, c) = (*b).clone() else {
                panic!()
            };
            let Noun::Atom(b) = (*b).clone() else {
                panic!()
            };

            hax(b, tar(subj.clone(), c), tar(subj, d))
        }
        11 => {
            let Noun::Cell(b, d) = (*formula).clone() else {
                panic!()
            };
            match (*b).clone() {
                Noun::Atom(_) => tar(subj, d),
                Noun::Cell(_, c) => {
                    tar_u32(cell(tar(subj.clone(), c), tar(subj, d)), 0, atom_ref(3))
                }
            }
        }
        _ => panic!(),
    }
}

pub fn tar(subj: Rc<Noun>, formula: Rc<Noun>) -> Rc<Noun> {
    let Noun::Cell(op, formula) = (*formula).clone() else {
        panic!()
    };
    match (*op).clone() {
        Noun::Cell(_, _) => cell(tar(subj.clone(), op), tar(subj, formula)),
        Noun::Atom(op) => {
            let mut op_iter = op.iter_u32_digits();
            let op = match op_iter.len() {
                0 => 0,
                1 => op_iter.next().expect("invariant"),
                _ => panic!(),
            };
            tar_u32(subj, op, formula)
        }
    }
}

pub fn eval_gate(gate: Rc<Noun>) -> Rc<Noun> {
    tar(
        gate,
        cell(
            atom_ref(9),
            cell(atom_ref(2), cell(Rc::new(Noun::SIG), atom_ref(1))),
        ),
    )
}
