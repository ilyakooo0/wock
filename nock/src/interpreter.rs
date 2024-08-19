use core::panic;
use std::rc::Rc;

use num_bigint::BigUint;

use crate::jets::*;
use crate::noun::*;

struct InterpreterContext {
    jets: Jets,
}

fn generate_interpreter_context() -> InterpreterContext {
    let jets = generate_jets();
    InterpreterContext { jets }
}

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
        Noun::Cell { .. } => Noun::SIG,
        Noun::Atom(_) => atom(1),
    }
}

fn lus(noun: Rc<Noun>) -> Noun {
    match (*noun).clone() {
        Noun::Cell { .. } => panic!(),
        Noun::Atom(atm) => Noun::Atom(atm + 1u32),
    }
}

fn tis(lhs: Rc<Noun>, rhs: Rc<Noun>) -> Noun {
    if lhs == rhs {
        Noun::SIG
    } else {
        atom(1)
    }
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
            Noun::Cell { p, q, .. } => match n {
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
        _ => hax_u32(
            addr >> 1,
            if (addr & 1) == 1 {
                cell(fas_u32(addr - 1, target.clone()), new_value)
            } else {
                cell(new_value, fas_u32(addr + 1, target.clone()))
            },
            target,
        ),
    }
}

fn hax(addr: Atom, new_value: Rc<Noun>, target: Rc<Noun>) -> Rc<Noun> {
    let mut addr_iter = addr.iter_u32_digits();

    match addr_iter.len() {
        0 => panic!(),
        1 => hax_u32(addr_iter.next().expect("invariant"), new_value, target),
        _ => hax(
            &addr >> 1u32,
            if (&addr & one_big_uint()) == one_big_uint() {
                cell(fas(addr - one_big_uint(), target.clone()), new_value)
            } else {
                cell(new_value, fas(addr + one_big_uint(), target.clone()))
            },
            target,
        ),
    }
}

fn tar_u32(ctx: &InterpreterContext, subj: Rc<Noun>, op: u32, formula: Rc<Noun>) -> Rc<Noun> {
    match op {
        0 => {
            let Noun::Atom(b) = (*formula).clone() else {
                panic!()
            };
            fas(b, subj)
        }
        1 => formula,
        2 => {
            let Noun::Cell { p: b, q: c, .. } = (*formula).clone() else {
                panic!()
            };

            match (*c).clone() {
                Noun::Cell { p, q, .. }
                    if *p == Noun::SIG
                        && q == atom_ref(2)
                        && b == cell(Rc::new(Noun::SIG), atom_ref(1)) =>
                {
                    // This means we are about the evaluate a gate at the head of the current subject.
                    let (hash, sample) = subj.hash_gate();

                    match ctx.jets.get(&hash) {
                        Some(f) => {
                            println!("Evaluating jet {}", hash);
                            f(sample)
                        }
                        None => tar(tar(subj.clone(), b), tar(subj, c)),
                    }
                }
                _ => tar(tar(subj.clone(), b), tar(subj, c)),
            }

            // let formula_hash = formula.hash();
            // let sample = subj.right().left();
        }
        3 => Rc::new(wut(tar(subj, formula))),
        4 => Rc::new(lus(tar(subj, formula))),
        5 => {
            let Noun::Cell { p: b, q: c, .. } = (*formula).clone() else {
                panic!()
            };
            Rc::new(tis(tar(subj.clone(), b), tar(subj, c)))
        }
        6 => {
            let Noun::Cell { p: b, q: c, .. } = (*formula).clone() else {
                panic!()
            };
            let Noun::Cell { p: c, q: d, .. } = (*c).clone() else {
                panic!()
            };

            tar(
                subj.clone(),
                tar_u32(
                    ctx,
                    cell(c, d),
                    0,
                    tar_u32(
                        ctx,
                        cell(atom_ref(2), atom_ref(3)),
                        0,
                        tar_u32(ctx, subj, 4, cell(atom_ref(4), b)),
                    ),
                ),
            )
        }
        7 => {
            let Noun::Cell { p: b, q: c, .. } = (*formula).clone() else {
                panic!()
            };
            tar(tar(subj, b), c)
        }
        8 => {
            let Noun::Cell { p: b, q: c, .. } = (*formula).clone() else {
                panic!()
            };
            tar(cell(tar(subj.clone(), b), subj), c)
        }
        9 => {
            let Noun::Cell { p: b, q: c, .. } = (*formula).clone() else {
                panic!()
            };
            tar_u32(
                ctx,
                tar(subj, c),
                2,
                cell(
                    cell(Rc::new(Noun::SIG), Rc::new(one())),
                    cell(Rc::new(Noun::SIG), b),
                ),
            )
        }
        10 => {
            let Noun::Cell { p: b, q: d, .. } = (*formula).clone() else {
                panic!()
            };
            let Noun::Cell { p: b, q: c, .. } = (*b).clone() else {
                panic!()
            };
            let Noun::Atom(b) = (*b).clone() else {
                panic!()
            };

            hax(b, tar(subj.clone(), c), tar(subj, d))
        }
        11 => {
            let Noun::Cell { p: b, q: d, .. } = (*formula).clone() else {
                panic!()
            };
            match (*b).clone() {
                Noun::Atom(_) => tar(subj, d),
                Noun::Cell { q: c, .. } => tar_u32(
                    ctx,
                    cell(tar(subj.clone(), c), tar(subj, d)),
                    0,
                    atom_ref(3),
                ),
            }
        }
        _ => panic!(),
    }
}

pub fn tar(subj: Rc<Noun>, formula: Rc<Noun>) -> Rc<Noun> {
    let ctx = &generate_interpreter_context();

    let Noun::Cell {
        p: op, q: formula, ..
    } = (*formula).clone()
    else {
        panic!()
    };
    match (*op).clone() {
        Noun::Cell { .. } => cell(tar(subj.clone(), op), tar(subj, formula)),
        Noun::Atom(op) => {
            let mut op_iter = op.iter_u32_digits();
            let op = match op_iter.len() {
                0 => 0,
                1 => op_iter.next().expect("invariant"),
                _ => panic!(),
            };
            tar_u32(ctx, subj, op, formula)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_wut() {
        assert_eq!(wut(cell(atom_ref(1), atom_ref(2))), atom(0));
        assert_eq!(wut(atom_ref(5)), atom(1));
    }

    #[test]
    fn test_tis() {
        assert_eq!(tis(atom_ref(1), atom_ref(1)), atom(0));
        assert_eq!(tis(atom_ref(1), atom_ref(2)), atom(1));
    }

    #[test]
    fn test_lus() {
        assert_eq!(lus(atom_ref(5)), atom(6));
    }

    #[test]
    fn test_fas() {
        assert_eq!(
            fas(
                BigUint::new(vec![1]),
                cell(atom_ref(531), cell(atom_ref(25), atom_ref(99)))
            ),
            cell(atom_ref(531), cell(atom_ref(25), atom_ref(99)))
        );
        assert_eq!(
            fas(
                BigUint::new(vec![6]),
                cell(atom_ref(531), cell(atom_ref(25), atom_ref(99)))
            ),
            atom_ref(25)
        );
        assert_eq!(
            fas(
                BigUint::new(vec![3]),
                cell(atom_ref(531), cell(atom_ref(25), atom_ref(99)))
            ),
            cell(atom_ref(25), atom_ref(99))
        );
    }

    #[test]
    fn test_hax() {
        assert_eq!(
            hax(
                BigUint::new(vec![2]),
                atom_ref(11),
                cell(atom_ref(22), atom_ref(33))
            ),
            cell(atom_ref(11), atom_ref(33))
        );
        assert_eq!(
            hax(
                BigUint::new(vec![3]),
                atom_ref(11),
                cell(atom_ref(22), atom_ref(33))
            ),
            cell(atom_ref(22), atom_ref(11))
        );
        assert_eq!(
            hax(
                BigUint::new(vec![5]),
                atom_ref(11),
                cell(cell(atom_ref(22), atom_ref(33)), atom_ref(44))
            ),
            cell(cell(atom_ref(22), atom_ref(11)), atom_ref(44))
        )
    }
}
