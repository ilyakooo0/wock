use core::str;
use std::borrow::Borrow;
use std::borrow::BorrowMut;
use std::collections::BTreeMap;
use std::rc::Rc;
use std::str::Utf8Error;

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
    pub slog: Rc<Noun>,
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
    let slog = Rc::new(Noun::from_bytes(b"slog"));
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
            slog,
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

fn lus(noun: &Noun) -> Result<Noun, TTanks> {
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

fn fas(ctx: &InterpreterContext, addr: &Atom, noun: &Noun) -> Result<Rc<Noun>, TTanks> {
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

fn fas_u32(addr: u32, noun: &Noun) -> Result<Rc<Noun>, TTanks> {
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

fn hax_u32(addr: u32, new_value: &Rc<Noun>, target: &Rc<Noun>) -> Result<Rc<Noun>, TTanks> {
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
) -> Result<Rc<Noun>, TTanks> {
    let mut addr_iter = addr.iter_u32_digits();

    match (addr_iter.next(), addr_iter.next()) {
        (None, _) => Err(TTanks::new()),
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
) -> Result<Rc<Noun>, TTanks> {
    match op {
        0 => {
            let b = formula.as_atom().ok_or(TTanks::new())?;
            fas(ctx, b, &*subj)
        }
        1 => Ok(formula.clone()),
        2 => {
            let (b, c) = formula.as_cell().ok_or(TTanks::new())?;

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
            let (b, c) = formula.as_cell().ok_or(TTanks::new())?;
            let foo = tar(ctx, subj.clone(), &b)?;
            let bar = tar(ctx, subj, &c)?;
            Ok(tis(ctx, &foo, &bar).clone())
        }
        6 => {
            let (b, c) = formula.as_cell().ok_or(TTanks::new())?;
            let (c, d) = c.as_cell().ok_or(TTanks::new())?;

            let bar = tar_u32(ctx, subj.clone(), 4, &cell(&ctx.nouns.four.clone(), &b))?;

            let foo = tar_u32(ctx, ctx.nouns.two_three.clone(), 0, &bar)?;
            let foo = tar_u32(ctx, cell(&c, &d), 0, &foo)?;

            tar(ctx, subj, &foo)
        }
        7 => {
            let (b, c) = formula.as_cell().ok_or(TTanks::new())?;
            let foo = tar(ctx, subj, &b)?;
            tar(ctx, foo, &c)
        }
        8 => {
            let (b, c) = formula.as_cell().ok_or(TTanks::new())?;
            let foo = tar(ctx, subj.clone(), &b)?;
            tar(ctx, cell(&foo, &subj), &c)
        }
        9 => {
            let (b, c) = formula.as_cell().ok_or(TTanks::new())?;
            let foo = tar(ctx, subj, &c)?;
            tar_u32(
                ctx,
                foo,
                2,
                &cell(&ctx.nouns.sig_one, &cell(&ctx.nouns.sig, &b)),
            )
        }
        10 => {
            let (b, d) = formula.as_cell().ok_or(TTanks::new())?;
            let (b, c) = b.as_cell().ok_or(TTanks::new())?;
            let b = b.as_atom().ok_or(TTanks::new())?;

            let foo = tar(ctx, subj.clone(), &c)?;
            let bar = tar(ctx, subj, &d)?;
            hax(ctx, b, &foo, &bar)
        }
        11 => {
            let (b, d) = formula.as_cell().ok_or(TTanks::new())?;
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
                    } else if *p == ctx.nouns.slog {
                        let noun = tar(ctx, subj.clone(), &q)?;
                        let (_prio, tank) = noun.as_cell().ok_or(TTanks::new())?;

                        eprintln!("{}", ram(ctx, tank.clone()).unwrap_or(String::new()));
                        eval(ctx)
                    } else {
                        eval(ctx)
                    }
                }
            }
        }
        _ => Err(TTanks::new()),
    }
}

fn ram(ctx: &mut InterpreterContext, tank: Rc<Noun>) -> Option<String> {
    match tank.borrow() {
        Noun::Atom(str) => {
            let mut bytes = str.to_bytes_le();
            Some(String::from(str::from_utf8_mut(&mut bytes).ok()?))
        }
        Noun::Cell { p, q, .. } => {
            let tag = p.as_atom()?.to_bytes_le();
            if tag == b"leaf" {
                let mut bytes = crip(ctx, q.clone()).ok()?.as_atom()?.to_bytes_le();
                Some(String::from(str::from_utf8_mut(&mut bytes).ok()?))
            } else if tag == b"palm" {
                let (mid, foo) = q.as_cell()?;
                let (open, foo) = foo.as_cell()?;
                let (flat_open, foo) = foo.as_cell()?;
                let (close, tanks) = foo.as_cell()?;

                let mut mid = mid.as_bytes()?;
                let mid = String::from(str::from_utf8_mut(&mut mid).ok()?);

                let mut open = open.as_bytes()?;
                let open = String::from(str::from_utf8_mut(&mut open).ok()?);

                let mut flat_open = flat_open.as_bytes()?;
                let flat_open = String::from(str::from_utf8_mut(&mut flat_open).ok()?);

                let mut close = close.as_bytes()?;
                let close = String::from(str::from_utf8_mut(&mut close).ok()?);

                let mut target = String::new();

                target.push_str(&*open);
                target.push_str(&*flat_open);

                let mut is_first = true;
                for tank in tanks.list_iter() {
                    if !is_first {
                        target.push_str(&*mid);
                    }

                    target.push_str(&*(ram(ctx, tank.clone())?));

                    is_first = false;
                }

                target.push_str(&*close);

                Some(target)
            } else if tag == b"rose" {
                let (mid, foo) = q.as_cell()?;
                let (open, foo) = foo.as_cell()?;
                let (close, tanks) = foo.as_cell()?;

                let mut mid = mid.as_bytes()?;
                let mid = String::from(str::from_utf8_mut(&mut mid).ok()?);

                let mut open = open.as_bytes()?;
                let open = String::from(str::from_utf8_mut(&mut open).ok()?);

                let mut close = close.as_bytes()?;
                let close = String::from(str::from_utf8_mut(&mut close).ok()?);

                let mut target = String::new();

                target.push_str(&*open);

                let mut is_first = true;
                for tank in tanks.list_iter() {
                    if !is_first {
                        target.push_str(&*mid);
                    }

                    target.push_str(&*(ram(ctx, tank.clone())?));

                    is_first = false;
                }

                target.push_str(&*close);

                Some(target)
            } else {
                None
            }
        }
    }
}

pub fn ram_ttanks(ctx: &mut InterpreterContext, tanks: TTanks) -> String {
    let mut target = String::new();

    for tank in tanks.iter() {
        target.push_str(&*ram_ttank(ctx, tank.clone()));
    }

    target
}

pub fn ram_ttank(ctx: &mut InterpreterContext, ttank: TTank) -> String {
    let (subj, q) = ttank;

    let tank = match tar(ctx, subj.clone(), &q) {
        Ok(gate) => match eval_pulled_gate(ctx, gate) {
            Ok(tank) => tank,
            Err(_) => return String::new(),
        },
        Err(_) => return String::new(),
    };
    ram(ctx, tank).unwrap_or(String::new())
}

fn crip(ctx: &mut InterpreterContext, tape: Rc<Noun>) -> Result<Rc<Noun>, TTanks> {
    RAP(ctx, &cell(&Rc::new(Noun::from_u32(3)), &tape))
}

fn with_trace<T>(tank: (Rc<Noun>, Rc<Noun>), src: Result<T, TTanks>) -> Result<T, TTanks> {
    match src {
        Ok(a) => Ok(a),
        Err(tanks) => Err(cons(tank, tanks)),
    }
}

/// Not actually a tank, but rather a thunk (subject, formula) which calculates the tank.
pub type TTank = (Rc<Noun>, Rc<Noun>);
pub type TTanks = PersistentList<TTank>;

pub const EMPTY_TANKS: TTanks = TTanks::new();

pub fn tar<'a>(
    ctx: &mut InterpreterContext,
    subj: Rc<Noun>,
    formula: &'a Noun,
) -> Result<Rc<Noun>, TTanks> {
    let (op, formula) = formula.as_cell().ok_or(TTanks::new())?;
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
            let op = op.ok_or(TTanks::new())?;
            stacker::maybe_grow(32 * 1024, 1024 * 1024, || tar_u32(ctx, subj, op, formula))
        }
    }
}

pub fn eval_gate(
    ctx: &mut InterpreterContext,
    gate_factory: &Rc<Noun>,
) -> Result<Rc<Noun>, TTanks> {
    let pulled_gate = tar(ctx, ctx.nouns.sig.clone(), &gate_factory)?;
    eval_pulled_gate(ctx, pulled_gate)
}

pub fn slam(
    ctx: &mut InterpreterContext,
    gate_factory: &Rc<Noun>,
    sample: &Rc<Noun>,
) -> Result<Rc<Noun>, TTanks> {
    let gate = tar(ctx, ctx.nouns.sig.clone(), &gate_factory)?;

    eval_pulled_gate(ctx, replace_sample(&gate, sample)?)
}

/// This is useful for evaluaing gates produced by running `.foo/nock gate-name` in the dojo.
/// Also for calling gates from within jets.
pub fn eval_pulled_gate(ctx: &mut InterpreterContext, gate: Rc<Noun>) -> Result<Rc<Noun>, TTanks> {
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
) -> Result<Rc<Noun>, TTanks> {
    eval_pulled_gate(ctx, replace_sample(gate, sample)?)
}

pub fn replace_sample(gate: &Rc<Noun>, sample: &Rc<Noun>) -> Result<Rc<Noun>, TTanks> {
    let (battery, payload) = gate.as_cell().ok_or(TTanks::new())?;
    let (_sample, context) = payload.as_cell().ok_or(TTanks::new())?;

    Ok(cell(battery, &cell(sample, context)))
}
