use core::panic;
use std::rc::Rc;

use num_bigint::BigUint;

use crate::jets::*;
use crate::noun::*;

pub struct Nouns {
    pub y: Rc<Noun>,
    pub n: Rc<Noun>,
    pub sig: Rc<Noun>,
    pub one: Rc<Noun>,
    pub two: Rc<Noun>,
    pub three: Rc<Noun>,
    pub four: Rc<Noun>,
    pub sig_one: Rc<Noun>,
    pub two_three: Rc<Noun>,
}

pub struct BigUints {
    pub zero: BigUint,
    pub one: BigUint,
    pub two: BigUint,
    pub three: BigUint,
    pub four: BigUint,
}

pub struct InterpreterContext {
    pub jets: Jets,
    pub nouns: Nouns,
    pub big_uints: BigUints,
}

pub fn generate_interpreter_context() -> InterpreterContext {
    let jets = generate_jets();
    let big_uints = BigUints {
        zero: BigUint::ZERO,
        one: BigUint::new(vec![1]),
        two: BigUint::new(vec![2]),
        three: BigUint::new(vec![3]),
        four: BigUint::new(vec![4]),
    };
    let sig = Rc::new(Noun::SIG);
    let one = Rc::new(Noun::Atom(big_uints.one.clone()));
    let two = Rc::new(Noun::Atom(big_uints.two.clone()));
    let three = Rc::new(Noun::Atom(big_uints.three.clone()));
    let four = Rc::new(Noun::Atom(big_uints.four.clone()));
    let sig_one = cell(sig.clone(), one.clone());
    let two_three = cell(two.clone(), three.clone());
    InterpreterContext {
        jets,
        nouns: Nouns {
            n: one.clone(),
            y: sig.clone(),
            sig,
            one,
            two,
            three,
            four,
            sig_one,
            two_three,
        },
        big_uints,
    }
}

fn wut(ctx: &InterpreterContext, noun: Rc<Noun>) -> Rc<Noun> {
    match *noun {
        Noun::Cell { .. } => ctx.nouns.sig.clone(),
        Noun::Atom(_) => ctx.nouns.one.clone(),
    }
}

fn lus(noun: Rc<Noun>) -> Noun {
    match (*noun).clone() {
        Noun::Cell { .. } => panic!(),
        Noun::Atom(atm) => Noun::Atom(atm + 1u32),
    }
}

fn tis(ctx: &InterpreterContext, lhs: Rc<Noun>, rhs: Rc<Noun>) -> Rc<Noun> {
    if lhs == rhs {
        ctx.nouns.sig.clone()
    } else {
        ctx.nouns.one.clone()
    }
}

fn fas(ctx: &InterpreterContext, addr: Atom, noun: Rc<Noun>) -> Rc<Noun> {
    let mut addr_iter = addr.iter_u32_digits();

    match addr_iter.len() {
        0 => panic!(),
        1 => fas_u32(addr_iter.next().unwrap(), noun),
        _ => {
            let rest = fas(ctx, &addr >> 1, noun);
            fas_u32(
                if (addr & &ctx.big_uints.one) == ctx.big_uints.one {
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

fn hax(ctx: &InterpreterContext, addr: Atom, new_value: Rc<Noun>, target: Rc<Noun>) -> Rc<Noun> {
    let mut addr_iter = addr.iter_u32_digits();

    match addr_iter.len() {
        0 => panic!(),
        1 => hax_u32(addr_iter.next().expect("invariant"), new_value, target),
        _ => hax(
            ctx,
            &addr >> 1u32,
            if (&addr & &ctx.big_uints.one) == ctx.big_uints.one {
                cell(
                    fas(ctx, addr - &ctx.big_uints.one, target.clone()),
                    new_value,
                )
            } else {
                cell(
                    new_value,
                    fas(ctx, addr + &ctx.big_uints.one, target.clone()),
                )
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
            fas(ctx, b, subj)
        }
        1 => formula,
        2 => {
            let Noun::Cell { p: b, q: c, .. } = (*formula).clone() else {
                panic!()
            };

            match (*c).clone() {
                Noun::Cell { p, q, .. }
                    if *p == Noun::SIG
                        && q == ctx.nouns.two
                        && b == cell(Rc::new(Noun::SIG), ctx.nouns.one.clone()) =>
                {
                    // This means we are about the evaluate a gate at the head of the current subject.
                    let (hash, sample) = subj.hash_gate();

                    match ctx.jets.get(&hash) {
                        Some(f) => {
                            println!("Evaluating jet {}", hash);
                            f(ctx, sample)
                        }
                        None => tar(ctx, tar(ctx, subj.clone(), b), tar(ctx, subj, c)),
                    }
                }
                _ => tar(ctx, tar(ctx, subj.clone(), b), tar(ctx, subj, c)),
            }
        }
        3 => wut(ctx, tar(ctx, subj, formula)),
        4 => Rc::new(lus(tar(ctx, subj, formula))),
        5 => {
            let Noun::Cell { p: b, q: c, .. } = (*formula).clone() else {
                panic!()
            };
            tis(ctx, tar(ctx, subj.clone(), b), tar(ctx, subj, c))
        }
        6 => {
            let Noun::Cell { p: b, q: c, .. } = (*formula).clone() else {
                panic!()
            };
            let Noun::Cell { p: c, q: d, .. } = (*c).clone() else {
                panic!()
            };

            tar(
                ctx,
                subj.clone(),
                tar_u32(
                    ctx,
                    cell(c, d),
                    0,
                    tar_u32(
                        ctx,
                        ctx.nouns.two_three.clone(),
                        0,
                        tar_u32(ctx, subj, 4, cell(ctx.nouns.four.clone(), b)),
                    ),
                ),
            )
        }
        7 => {
            let Noun::Cell { p: b, q: c, .. } = (*formula).clone() else {
                panic!()
            };
            tar(ctx, tar(ctx, subj, b), c)
        }
        8 => {
            let Noun::Cell { p: b, q: c, .. } = (*formula).clone() else {
                panic!()
            };
            tar(ctx, cell(tar(ctx, subj.clone(), b), subj), c)
        }
        9 => {
            let Noun::Cell { p: b, q: c, .. } = (*formula).clone() else {
                panic!()
            };
            tar_u32(
                ctx,
                tar(ctx, subj, c),
                2,
                cell(ctx.nouns.sig_one.clone(), cell(ctx.nouns.sig.clone(), b)),
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

            hax(ctx, b, tar(ctx, subj.clone(), c), tar(ctx, subj, d))
        }
        11 => {
            let Noun::Cell { p: b, q: d, .. } = (*formula).clone() else {
                panic!()
            };
            match (*b).clone() {
                Noun::Atom(_) => tar(ctx, subj, d),
                Noun::Cell { q: c, .. } => tar_u32(
                    ctx,
                    cell(tar(ctx, subj.clone(), c), tar(ctx, subj, d)),
                    0,
                    ctx.nouns.three.clone(),
                ),
            }
        }
        _ => panic!(),
    }
}

pub fn tar(ctx: &InterpreterContext, subj: Rc<Noun>, formula: Rc<Noun>) -> Rc<Noun> {
    let Noun::Cell {
        p: op, q: formula, ..
    } = (*formula).clone()
    else {
        panic!()
    };
    match (*op).clone() {
        Noun::Cell { .. } => cell(tar(ctx, subj.clone(), op), tar(ctx, subj, formula)),
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

pub fn eval_gate(ctx: &InterpreterContext, gate: Rc<Noun>) -> Rc<Noun> {
    tar(
        ctx,
        gate,
        cell(
            Rc::new(Noun::Atom(BigUint::new(vec![9]))),
            cell(
                ctx.nouns.two.clone(),
                cell(ctx.nouns.sig.clone(), ctx.nouns.one.clone()),
            ),
        ),
    )
}
