use std::borrow::Borrow;
use std::collections::BTreeMap;
use std::rc::Rc;

use fplist::cons;
use fplist::PersistentList;
use num_bigint::BigUint;

use crate::jets::*;
use crate::noun::*;

#[derive(Clone)]
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
    pub memo: Rc<Noun>,
    pub mean: Rc<Noun>,
}

#[derive(Clone)]
pub struct BigUints {
    pub zero: BigUint,
    pub one: BigUint,
    pub two: BigUint,
    pub three: BigUint,
    pub four: BigUint,
}

#[derive(Clone)]
pub struct InterpreterContext {
    pub jets: Jets,
    pub double_jets: DoubleJets,
    pub nouns: Nouns,
    pub big_uints: BigUints,
    pub memo: BTreeMap<Hash, Rc<Noun>>,
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
    let memo = Rc::new(Noun::from_bytes(b"memo"));
    let mean = Rc::new(Noun::from_bytes(b"mean"));
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
            memo,
            mean,
        },
        big_uints,
        memo: BTreeMap::new(),
    }
}

fn wut<'a>(ctx: &'a InterpreterContext, noun: &Noun) -> &'a Rc<Noun> {
    match noun {
        Noun::Cell { .. } => &ctx.nouns.sig,
        Noun::Atom(_) => &ctx.nouns.one,
    }
}

fn lus(noun: &Noun) -> Result<Noun, Tanks> {
    match noun {
        Noun::Cell { .. } => Err(PersistentList::new()),
        Noun::Atom(atm) => Ok(Noun::Atom(atm + 1u32)),
    }
}

fn tis<'a>(ctx: &'a InterpreterContext, lhs: &Noun, rhs: &Noun) -> &'a Rc<Noun> {
    if lhs == rhs {
        &ctx.nouns.sig
    } else {
        &ctx.nouns.one
    }
}

fn fas(ctx: &InterpreterContext, addr: &Atom, noun: &Noun) -> Result<Rc<Noun>, Tanks> {
    let mut addr_iter = addr.iter_u32_digits();

    match (addr_iter.next(), addr_iter.next()) {
        (None, _) => Err(PersistentList::new()),
        (Some(x), None) => fas_u32(x, noun),
        _ => {
            let rest = fas(ctx, &(addr >> 1u8), noun)?;
            Ok(fas_u32(
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

fn fas_u32(addr: u32, noun: &Noun) -> Result<Rc<Noun>, Tanks> {
    match addr {
        0 => Err(PersistentList::new()),
        1 => Ok(Rc::new(noun.clone())),
        n => match noun {
            Noun::Atom(_) => Err(PersistentList::new()),
            Noun::Cell { p, q, .. } => match n {
                2 => Ok(p.clone()),
                3 => Ok(q.clone()),
                _ => {
                    let rest = fas_u32(n >> 1, &cell(p, q))?;
                    fas_u32(if (n & 1) == 1 { 3 } else { 2 }, &rest)
                }
            },
        },
    }
}

fn hax_u32(addr: u32, new_value: &Rc<Noun>, target: &Rc<Noun>) -> Result<Rc<Noun>, Tanks> {
    match addr {
        0 => Err(PersistentList::new()),
        1 => Ok(new_value.clone()),
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
) -> Result<Rc<Noun>, Tanks> {
    let mut addr_iter = addr.iter_u32_digits();

    match (addr_iter.next(), addr_iter.next()) {
        (None, _) => Err(Tanks::new()),
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
    ctx: &mut InterpreterContext,
    subj: Rc<Noun>,
    op: u32,
    formula: &'a Rc<Noun>,
) -> Result<Rc<Noun>, Tanks> {
    match op {
        0 => {
            let b = formula.as_atom().ok_or(Tanks::new())?;
            fas(ctx, b, &*subj)
        }
        1 => Ok(formula.clone()),
        2 => {
            let (b, c) = formula.as_cell().ok_or(Tanks::new())?;

            match c.borrow() {
                Noun::Cell { p, q, .. }
                    if **p == Noun::SIG && *q == ctx.nouns.two && *b == ctx.nouns.sig_one =>
                {
                    // This means we are about the evaluate a gate at the head of the current subject.
                    let (hash, sample) = subj.hash_gate();

                    match ctx.jets.get(&hash) {
                        Some(f) => f(ctx, sample),
                        None => {
                            let foo = tar(ctx, subj.clone(), &b)?;
                            let bar = tar(ctx, subj, &c)?;
                            tar(ctx, foo, &bar)
                        }
                    }
                }
                _ => {
                    let foo = tar(ctx, subj.clone(), &b)?;
                    let bar = tar(ctx, subj, &c)?;
                    tar(ctx, foo, &bar)
                }
            }
        }
        3 => {
            let foo = tar(ctx, subj, &formula)?;
            Ok(wut(ctx, &foo).clone())
        }
        4 => Ok(Rc::new(lus(&*tar(ctx, subj, &formula)?)?)),
        5 => {
            let (b, c) = formula.as_cell().ok_or(Tanks::new())?;
            let foo = tar(ctx, subj.clone(), &b)?;
            let bar = tar(ctx, subj, &c)?;
            Ok(tis(ctx, &foo, &bar).clone())
        }
        6 => {
            let (b, c) = formula.as_cell().ok_or(Tanks::new())?;
            let (c, d) = c.as_cell().ok_or(Tanks::new())?;

            let bar = tar_u32(ctx, subj.clone(), 4, &cell(&ctx.nouns.four.clone(), &b))?;

            let foo = tar_u32(ctx, ctx.nouns.two_three.clone(), 0, &bar)?;
            let foo = tar_u32(ctx, cell(&c, &d), 0, &foo)?;

            tar(ctx, subj, &foo)
        }
        7 => {
            let (b, c) = formula.as_cell().ok_or(Tanks::new())?;
            let foo = tar(ctx, subj, &b)?;
            tar(ctx, foo, &c)
        }
        8 => {
            let (b, c) = formula.as_cell().ok_or(Tanks::new())?;
            let foo = tar(ctx, subj.clone(), &b)?;
            tar(ctx, cell(&foo, &subj), &c)
        }
        9 => {
            let (b, c) = formula.as_cell().ok_or(Tanks::new())?;
            let foo = tar(ctx, subj, &c)?;
            tar_u32(
                ctx,
                foo,
                2,
                &cell(&ctx.nouns.sig_one, &cell(&ctx.nouns.sig, &b)),
            )
        }
        10 => {
            let (b, d) = formula.as_cell().ok_or(Tanks::new())?;
            let (b, c) = b.as_cell().ok_or(Tanks::new())?;
            let b = b.as_atom().ok_or(Tanks::new())?;

            let foo = tar(ctx, subj.clone(), &c)?;
            let bar = tar(ctx, subj, &d)?;
            hax(ctx, b, &foo, &bar)
        }
        11 => {
            let (b, d) = formula.as_cell().ok_or(Tanks::new())?;
            match b.borrow() {
                Noun::Atom(_) => tar(ctx, subj, &d),
                Noun::Cell { p, q, .. } => {
                    let eval = |ctx: &mut InterpreterContext| tar(ctx, subj.clone(), &d);
                    if *p == ctx.nouns.memo {
                        let hash = cell(&subj, &d).hash();
                        match ctx.memo.get(&hash) {
                            Some(target) => Ok(target.clone()),
                            None => {
                                let target = eval(ctx)?;
                                ctx.memo.insert(hash, target.clone());
                                Ok(target)
                            }
                        }
                    } else if *p == ctx.nouns.mean {
                        with_trace((subj.clone(), q.clone()), eval(ctx))
                    } else {
                        eval(ctx)
                    }
                }
            }
        }
        _ => Err(Tanks::new()),
    }
}

fn with_trace<T>(tank: (Rc<Noun>, Rc<Noun>), src: Result<T, Tanks>) -> Result<T, Tanks> {
    match src {
        Ok(a) => Ok(a),
        Err(tanks) => Err(cons(tank, tanks)),
    }
}

pub type Tank = Rc<Noun>;
pub type Tanks = PersistentList<(Rc<Noun>, Rc<Noun>)>;

pub const EMPTY_TANKS: Tanks = Tanks::new();

pub fn tar<'a>(
    ctx: &mut InterpreterContext,
    subj: Rc<Noun>,
    formula: &'a Noun,
) -> Result<Rc<Noun>, Tanks> {
    let (op, formula) = formula.as_cell().ok_or(Tanks::new())?;
    match op.borrow() {
        Noun::Cell { .. } => {
            let foo = tar(ctx, subj.clone(), &op)?;
            let bar = tar(ctx, subj, &formula)?;
            Ok(cell(&foo, &bar))
        }
        Noun::Atom(ref op) => {
            let mut op_iter = op.iter_u32_digits();
            let op = match op_iter.len() {
                0 => Some(0),
                1 => Some(op_iter.next().unwrap()),
                _ => None,
            };
            let op = op.ok_or(Tanks::new())?;
            stacker::maybe_grow(32 * 1024, 1024 * 1024, || tar_u32(ctx, subj, op, formula))
        }
    }
}

pub fn eval_gate(ctx: &mut InterpreterContext, gate_factory: &Rc<Noun>) -> Result<Rc<Noun>, Tanks> {
    let pulled_gate = tar(ctx, ctx.nouns.sig.clone(), &gate_factory)?;
    eval_pulled_gate(ctx, pulled_gate)
}

pub fn slam(
    ctx: &mut InterpreterContext,
    gate_factory: &Rc<Noun>,
    sample: &Rc<Noun>,
) -> Result<Rc<Noun>, Tanks> {
    let gate = tar(ctx, ctx.nouns.sig.clone(), &gate_factory)?;

    eval_pulled_gate(ctx, replace_sample(&gate, sample)?)
}

/// This is useful for evaluaing gates produced by running `.foo/nock gate-name` in the dojo.
/// Also for calling gates from within jets.
pub fn eval_pulled_gate(ctx: &mut InterpreterContext, gate: Rc<Noun>) -> Result<Rc<Noun>, Tanks> {
    tar(
        ctx,
        gate,
        &cell(
            &Rc::new(Noun::Atom(BigUint::new(vec![9]))),
            &cell(&ctx.nouns.two, &cell(&ctx.nouns.sig, &ctx.nouns.one)),
        ),
    )
}

/// This is useful for evaluaing gates produced by running `.foo/nock gate-name` in the dojo.
/// Also for calling gates from within jets.
pub fn slam_pulled_gate(
    ctx: &mut InterpreterContext,
    gate: &Rc<Noun>,
    sample: &Rc<Noun>,
) -> Result<Rc<Noun>, Tanks> {
    eval_pulled_gate(ctx, replace_sample(gate, sample)?)
}

pub fn replace_sample(gate: &Rc<Noun>, sample: &Rc<Noun>) -> Result<Rc<Noun>, Tanks> {
    let (battery, payload) = gate.as_cell().ok_or(Tanks::new())?;
    let (_sample, context) = payload.as_cell().ok_or(Tanks::new())?;

    Ok(cell(battery, &cell(sample, context)))
}
