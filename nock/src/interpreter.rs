use std::borrow::Borrow;
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
    pub double_jets: DoubleJets,
    pub nouns: Nouns,
    pub big_uints: BigUints,
}

pub fn generate_interpreter_context() -> InterpreterContext {
    let jets = generate_jets();
    let double_jets = generate_double_jets();
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
    let sig_one = cell(&sig, &one);
    let two_three = cell(&two, &three);
    InterpreterContext {
        jets,
        double_jets,
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

fn wut<'a>(ctx: &'a InterpreterContext, noun: &Noun) -> &'a Rc<Noun> {
    match noun {
        Noun::Cell { .. } => &ctx.nouns.sig,
        Noun::Atom(_) => &ctx.nouns.one,
    }
}

fn lus(noun: &Noun) -> Option<Noun> {
    match noun {
        Noun::Cell { .. } => None,
        Noun::Atom(atm) => Some(Noun::Atom(atm + 1u32)),
    }
}

fn tis<'a>(ctx: &'a InterpreterContext, lhs: &Noun, rhs: &Noun) -> &'a Rc<Noun> {
    if lhs == rhs {
        &ctx.nouns.sig
    } else {
        &ctx.nouns.one
    }
}

fn fas(ctx: &InterpreterContext, addr: &Atom, noun: &Noun) -> Option<Rc<Noun>> {
    let mut addr_iter = addr.iter_u32_digits();

    match (addr_iter.next(), addr_iter.next()) {
        (None, _) => None,
        (Some(x), None) => fas_u32(x, noun),
        _ => {
            let rest = fas(ctx, &(addr >> 1u8), noun)?;
            Some(fas_u32(
                if (addr & &ctx.big_uints.one) == ctx.big_uints.one {
                    3
                } else {
                    2
                },
                &rest,
            )?)
        }
    }
}

fn fas_u32(addr: u32, noun: &Noun) -> Option<Rc<Noun>> {
    match addr {
        0 => None,
        1 => Some(Rc::new(noun.clone())),
        n => match noun.borrow() {
            Noun::Atom(_) => None,
            Noun::Cell { p, q, .. } => match n {
                2 => Some(p.clone()),
                3 => Some(q.clone()),
                _ => {
                    let rest = fas_u32(n >> 1, &cell(p, q))?;
                    fas_u32(if (n & 1) == 1 { 3 } else { 2 }, &rest)
                }
            },
        },
    }
}

fn hax_u32(addr: u32, new_value: &Rc<Noun>, target: &Rc<Noun>) -> Option<Rc<Noun>> {
    match addr {
        0 => None,
        1 => Some(new_value.clone()),
        _ => {
            let new_value = if (addr & 1) == 1 {
                let foo = fas_u32(addr - 1, &target)?;
                &cell(&foo, &new_value)
            } else {
                let foo = fas_u32(addr + 1, &target)?;
                &cell(&new_value, &foo)
            };
            hax_u32(addr >> 1, new_value, target)
        }
    }
}

fn hax(
    ctx: &InterpreterContext,
    addr: &Atom,
    new_value: &Rc<Noun>,
    target: &Rc<Noun>,
) -> Option<Rc<Noun>> {
    let mut addr_iter = addr.iter_u32_digits();

    match (addr_iter.next(), addr_iter.next()) {
        (None, _) => None,
        (Some(x), None) => hax_u32(x, new_value, target),
        _ => hax(
            ctx,
            &(addr >> 1u32),
            &(if (addr & &ctx.big_uints.one) == ctx.big_uints.one {
                let foo = fas(ctx, &(addr - &ctx.big_uints.one), &target)?;
                cell(&foo, new_value)
            } else {
                let foo = fas(ctx, &(addr + &ctx.big_uints.one), &target)?;
                cell(&new_value, &foo)
            }),
            target,
        ),
    }
}

fn tar_u32<'a>(
    ctx: &InterpreterContext,
    subj: &'a Noun,
    op: u32,
    formula: &'a Rc<Noun>,
) -> Option<Rc<Noun>> {
    match op {
        0 => {
            let b = formula.as_atom()?;
            fas(ctx, b, subj)
        }
        1 => Some(formula.clone()),
        2 => {
            let (b, c) = formula.as_cell()?;

            match c.borrow() {
                Noun::Cell { p, q, .. }
                    if **p == Noun::SIG && *q == ctx.nouns.two && *b == ctx.nouns.sig_one =>
                {
                    // This means we are about the evaluate a gate at the head of the current subject.
                    let (hash, sample) = subj.hash_gate();

                    match ctx.jets.get(&hash) {
                        Some(f) => f(ctx, sample),
                        None => {
                            let (hash, sample_1, sample_2) = subj.hash_double_gate();
                            match ctx.double_jets.get(&hash) {
                                Some(f) => f(ctx, sample_1, sample_2),
                                None => {
                                    let foo = tar(ctx, subj, &b)?;
                                    let bar = tar(ctx, subj, &c)?;
                                    tar(ctx, &foo, &bar)
                                }
                            }
                        }
                    }
                }
                _ => {
                    let foo = tar(ctx, &subj, &b)?;
                    let bar = tar(ctx, &subj, &c)?;
                    tar(ctx, &foo, &bar)
                }
            }
        }
        3 => {
            let foo = tar(ctx, subj, &formula)?;
            Some(wut(ctx, &foo).clone())
        }
        4 => {
            let foo = tar(ctx, subj, &formula)?;
            Some(Rc::new(lus(&foo)?))
        }
        5 => {
            let (b, c) = formula.as_cell()?;
            let foo = tar(ctx, &subj, &b)?;
            let bar = tar(ctx, subj, &c)?;
            Some(tis(ctx, &foo, &bar).clone())
        }
        6 => {
            let (b, c) = formula.as_cell()?;
            let (c, d) = c.as_cell()?;

            let foo = tar_u32(
                ctx,
                &cell(&c, &d),
                0,
                &tar_u32(
                    ctx,
                    &ctx.nouns.two_three,
                    0,
                    &tar_u32(ctx, subj, 4, &cell(&ctx.nouns.four, &b))?,
                )?,
            )?;

            tar(ctx, &subj, &foo)
        }
        7 => {
            let (b, c) = formula.as_cell()?;
            let foo = tar(ctx, subj, &b)?;
            tar(ctx, &foo, &c)
        }
        8 => {
            let (b, c) = formula.as_cell()?;
            let foo = tar(ctx, &subj, &b)?;
            tar(ctx, &cell(&foo, &Rc::new(subj.clone())), &c)
        }
        9 => {
            let (b, c) = formula.as_cell()?;
            let foo = tar(ctx, &subj, &c)?;
            tar_u32(
                ctx,
                &foo,
                2,
                &cell(&ctx.nouns.sig_one, &cell(&ctx.nouns.sig, &b)),
            )
        }
        10 => {
            let (b, d) = formula.as_cell()?;
            let (b, c) = b.as_cell()?;
            let b = b.as_atom()?;

            hax(ctx, b, &tar(ctx, subj, &c)?, &tar(ctx, subj, &d)?)
        }
        11 => {
            let (b, d) = formula.as_cell()?;
            // println!("{b}");
            match b.borrow() {
                Noun::Atom(_) => tar(ctx, subj, &d),
                Noun::Cell { q: c, .. } => {
                    let foo = tar(ctx, subj, &c)?;
                    let bar = tar(ctx, subj, &d)?;
                    tar_u32(ctx, &cell(&foo, &bar), 0, &ctx.nouns.three)
                }
            }
        }
        _ => None,
    }
}

pub fn tar<'a>(ctx: &InterpreterContext, subj: &'a Noun, formula: &'a Noun) -> Option<Rc<Noun>> {
    let (op, formula) = formula.as_cell()?;
    match op.borrow() {
        Noun::Cell { .. } => {
            let foo = tar(ctx, subj, &op)?;
            let bar = tar(ctx, subj, &formula)?;
            Some(cell(&foo, &bar))
        }
        Noun::Atom(ref op) => {
            let mut op_iter = op.iter_u32_digits();
            let op = match op_iter.len() {
                0 => Some(0),
                1 => Some(op_iter.next().unwrap()),
                _ => None,
            };
            let op = op?;
            stacker::maybe_grow(32 * 1024, 1024 * 1024, || tar_u32(ctx, subj, op, formula))
        }
    }
}

pub fn eval_gate(ctx: &InterpreterContext, gate: &Rc<Noun>) -> Option<Rc<Noun>> {
    tar(
        ctx,
        &gate,
        &cell(
            &Rc::new(Noun::Atom(BigUint::new(vec![9]))),
            &cell(&ctx.nouns.two, &cell(&ctx.nouns.sig, &ctx.nouns.one)),
        ),
    )
}

pub fn slam(ctx: &InterpreterContext, gate: &Rc<Noun>, sample: &Rc<Noun>) -> Option<Rc<Noun>> {
    let (battery, payload) = gate.as_cell()?;
    let (_sample, context) = payload.as_cell()?;

    eval_gate(ctx, &cell(battery, &cell(sample, context)))
}
