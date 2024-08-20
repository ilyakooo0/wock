use num_integer::Integer;
use std::{collections::BTreeMap, rc::Rc};

use crate::{
    interpreter::InterpreterContext,
    noun::{self, cell, Atom, Noun},
};

pub type Jets = BTreeMap<noun::Hash, Jet>;

pub fn generate_jets() -> Jets {
    BTreeMap::from([
        (15030307987607209070707551851657016156, ADD),
        (63755436425185291665709128251960810903, DEC),
        (103625312163793685360054974742842323626, DVR),
        (184655757819184698409025865360091499300, DIV),
        (78626429405957535773915737685773609505, GTE),
        (278010947678490337096911683131743337690, GTH),
        (306661504112892911468962748662595874369, LTE),
        (219735893577820642504602723917694385141, LTH),
        (175456549196765081095821926674251751751, MAX),
        (236402571827443459563375333060817874032, MIN),
        (74509797090527672995505213760834630343, MOD),
        (254997877510438802608262569354903382802, MUL),
        (44659582293430635022123827294854022894, SUB),
    ])
}

pub type Jet = fn(ctx: &InterpreterContext, Rc<Noun>) -> Rc<Noun>;

static BINARY_ATOM: fn(Rc<Noun>, fn(&Atom, &Atom) -> Atom) -> Rc<Noun> =
    |n: Rc<Noun>, f: fn(&Atom, &Atom) -> Atom| {
        let Noun::Cell { p, q, .. } = (*n).clone() else {
            panic!()
        };
        Rc::new(Noun::Atom(f(p.as_atom().unwrap(), q.as_atom().unwrap())))
    };

static BINARY_ATOM_LOOB: fn(&InterpreterContext, Rc<Noun>, fn(&Atom, &Atom) -> bool) -> Rc<Noun> =
    |ctx: &InterpreterContext, n: Rc<Noun>, f: fn(&Atom, &Atom) -> bool| {
        let Noun::Cell { p, q, .. } = (*n).clone() else {
            panic!()
        };

        if f(p.as_atom().unwrap(), q.as_atom().unwrap()) {
            ctx.nouns.sig.clone()
        } else {
            ctx.nouns.one.clone()
        }
    };

static BINARY_ATOM_PAIR: fn(Rc<Noun>, fn(&Atom, &Atom) -> (Atom, Atom)) -> Rc<Noun> =
    |n: Rc<Noun>, f: fn(&Atom, &Atom) -> (Atom, Atom)| {
        let Noun::Cell { p, q, .. } = (*n).clone() else {
            panic!()
        };
        let (a, b) = f(p.as_atom().unwrap(), q.as_atom().unwrap());
        cell(Rc::new(Noun::Atom(a)), Rc::new(Noun::Atom(b)))
    };

static ADD: Jet = |_ctx, n| BINARY_ATOM(n, |a, b| a + b);
static DEC: Jet = |_ctx, n| Rc::new(Noun::Atom(n.as_atom().unwrap() - 1u32));
static DVR: Jet = |_ctx, n| BINARY_ATOM_PAIR(n, |a, b| a.div_rem(&b));
static DIV: Jet = |_ctx, n| BINARY_ATOM(n, |a, b| a / b);
static GTE: Jet = |ctx, n| BINARY_ATOM_LOOB(ctx, n, |a, b| a >= b);
static GTH: Jet = |ctx, n| BINARY_ATOM_LOOB(ctx, n, |a, b| a > b);
static LTE: Jet = |ctx, n| BINARY_ATOM_LOOB(ctx, n, |a, b| a <= b);
static LTH: Jet = |ctx, n| BINARY_ATOM_LOOB(ctx, n, |a, b| a < b);
static MAX: Jet = |_ctx, n| BINARY_ATOM(n, |a, b| if a > b { a.clone() } else { b.clone() });
static MIN: Jet = |_ctx, n| BINARY_ATOM(n, |a, b| if a < b { a.clone() } else { b.clone() });
static MOD: Jet = |_ctx, n| BINARY_ATOM(n, |a, b| a % b);
static MUL: Jet = |_ctx, n| BINARY_ATOM(n, |a, b| a * b);
static SUB: Jet = |_ctx, n| BINARY_ATOM(n, |a, b| a - b);
