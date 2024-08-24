use core::panic;
use num_integer::Integer;
use std::{
    cmp::Ordering,
    collections::BTreeMap,
    iter::{once, repeat_n, zip},
    rc::Rc,
};

use crate::{
    interpreter::{slam, InterpreterContext},
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
        (189893948398582289331214200623955451738, CAP),
        (156410165781489962142326056842465219913, MAS),
        (319842446776547752542981884135289299212, PEG),
        (14996897817720580650336412401915082021, FAND),
        (233946487723094976680797572797152464005, FIND),
        (215071416104742929357520800152622002342, FLOP),
        (127000845647123308421702512578230020206, GULF),
        (224124632134128248273067864092142254001, INTO),
        (22365719601138689598430859488399820563, JOIN),
        (211253539934007667533839299235896640130, LENT),
        (104838119266193725126177792605193739824, LEVY),
        (136665047491594821996133547576675424469, LIEN),
        (134383540064775397895881415210519251023, MURN),
        (206209714924463645290290379601675245373, REAP),
        (266278769253331237088430651317345081031, REAR),
        (146282575960509029931248602154898675358, REEL),
        (218516926094890651045754335042357820192, ROLL),
        (89970054366691388746503743197917964564, SCAG),
        (68845187332605602565057027619381114427, SKID),
        (51010231444191054729255601633316498529, SKIM),
        (144878877047435484372311285666595397017, SKIP),
        (22344664364706022641079203486385841813, SLAG),
        (288515703007942177334798811660651439897, SNAG),
        (322006565952434408151902864454572452802, SNIP),
        (290837622185731774795798043954046795930, SNOC),
        (255402796382159268271460433298209603428, SORT),
        (10111398404342329752608067595798409804, SPIN),
        (209194119090564757355985847956576215234, SPUN),
        (103985097318911737222583881947453560061, TURN),
        (216618478373489138062093083914898130454, WELD),
        (161636738411171172446435323238280743696, WELP),
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

static CAP: Jet = |ctx, n| {
    let Noun::Atom(n) = (*n).clone() else {
        panic!()
    };
    CAP_ATOM(ctx, n)
};
static CAP_ATOM: fn(&InterpreterContext, Atom) -> Rc<Noun> = |ctx, n: Atom| {
    let mut n_iter = n.iter_u32_digits();

    match n_iter.len() {
        0 => panic!(),
        1 => CAP_U32(ctx, n_iter.next().unwrap()),
        _ => CAP_ATOM(ctx, n >> 1),
    }
};
static CAP_U32: fn(ctx: &InterpreterContext, u32) -> Rc<Noun> = |ctx, n| match n {
    0 | 1 => panic!(),
    2 => ctx.nouns.two.clone(),
    3 => ctx.nouns.three.clone(),
    n => CAP_U32(ctx, n >> 1),
};

static MAS: Jet = |ctx, n| {
    let Noun::Atom(n) = (*n).clone() else {
        panic!()
    };
    Rc::new(Noun::Atom(MAS_ATOM(ctx, n)))
};
static MAS_ATOM: fn(&InterpreterContext, Atom) -> Atom = |ctx, n| {
    let mut n_iter = n.iter_u32_digits();

    match n_iter.len() {
        0 => panic!(),
        1 => Atom::new(vec![MAS_U32(n_iter.next().unwrap())]),
        _ => (&n & &ctx.big_uints.one) + (MAS_ATOM(ctx, &n >> 1) << 1),
    }
};
static MAS_U32: fn(u32) -> u32 = |n| match n {
    0 | 1 => panic!(),
    2 | 3 => 1,
    _ => (n & 1) + (MAS_U32(n >> 1) << 1),
};

static PEG: Jet = |ctx, n| {
    let Noun::Cell { p: a, q: b, .. } = (*n).clone() else {
        panic!()
    };

    let a = a.as_atom().unwrap();
    let b = b.as_atom().unwrap();

    if *a == Atom::ZERO {
        panic!()
    };

    Rc::new(Noun::Atom(PEG_ATOM(ctx, a, b.clone())))
};
static PEG_ATOM: fn(&InterpreterContext, &Atom, Atom) -> Atom = |ctx, a, b| {
    let mut b_iter = b.iter_u32_digits();

    match b_iter.len() {
        0 => panic!(),
        1 => PEG_U32(a, b_iter.next().unwrap()),
        _ => (&b & &ctx.big_uints.one) + (PEG_ATOM(ctx, a, b >> 1) << 1),
    }
};
static PEG_U32: fn(&Atom, u32) -> Atom = |a, b| match b {
    0 => panic!(),
    1 => a.clone(),
    2 => a << 1,
    3 => (a << 1) + 1u32,
    _ => (b & 1) + (PEG_U32(a, b >> 1) << 1),
};

static FAND: Jet = |ctx, n| {
    let (nedl, hstk) = n.as_cell().unwrap();
    if nedl.is_sig() || hstk.is_sig() {
        return ctx.nouns.sig.clone();
    }

    let nedl: Vec<Rc<Noun>> = nedl.list_iter().collect();

    let mut result: Vec<u32> = Vec::new();

    let mut iter = zip(0.., hstk.list_iter());

    let mut inner_iter = iter.clone();

    while let Option::Some((i, _)) = iter.next() {
        if inner_iter
            .map(|(_, x)| x)
            .take(nedl.len())
            .collect::<Vec<_>>()
            == nedl
        {
            result.push(i);
        }

        inner_iter = iter.clone();
    }

    Noun::list(result.iter().map(|a| Noun::from_u32(*a)))
};

static FIND: Jet = |ctx, n| {
    let (nedl, hstk) = n.as_cell().unwrap();
    if nedl.is_sig() || hstk.is_sig() {
        return ctx.nouns.sig.clone();
    }

    let nedl: Vec<Rc<Noun>> = nedl.list_iter().collect();

    let mut iter = zip(0.., hstk.list_iter());

    let mut inner_iter = iter.clone();

    while let Option::Some((i, _)) = iter.next() {
        if inner_iter
            .map(|(_, x)| x)
            .take(nedl.len())
            .collect::<Vec<_>>()
            == nedl
        {
            return Rc::new(Noun::from_u32(i)).unit();
        }

        inner_iter = iter.clone();
    }

    return ctx.nouns.sig.clone();
};

static FLOP: Jet = |ctx, n| {
    n.list_iter()
        .fold(ctx.nouns.sig.clone(), |tail, head| cell(head, tail))
};

static GULF: Jet = |_ctx, n| {
    let (from, to) = n.as_cell().unwrap();

    let mut from = (*from.as_atom().unwrap()).clone();
    let to = to.as_atom().unwrap().clone();

    if from > to {
        panic!()
    }

    let mut tmp = Vec::new();

    while from <= to {
        tmp.push(from.clone());
        from += 1u32;
    }

    Noun::list(tmp.iter().rev().map(|a| Noun::Atom(a.clone())))
};

static INTO: Jet = |_ctx, n| {
    let (a, b) = n.as_cell().unwrap();
    let (b, c) = b.as_cell().unwrap();

    let mut b_iter = b.as_atom().unwrap().iter_u32_digits();

    match b_iter.len() {
        0 => cell(c, a),
        1 => {
            let l = a.list_iter();
            let b = b_iter.next().unwrap() as usize;
            Noun::list_refs(
                l.clone()
                    .take(b)
                    .chain(once(c))
                    .chain(l.skip(b))
                    .collect::<Vec<_>>()
                    .iter()
                    .map(|x| x.clone()),
            )
        }
        _ => Noun::list_refs(
            a.list_iter()
                .chain(once(c))
                .collect::<Vec<_>>()
                .iter()
                .map(|x| x.clone()),
        ),
    }
};

static JOIN: Jet = |_ctx, n| {
    let (sep, list) = n.as_cell().unwrap();
    Noun::list_refs(
        list.list_iter()
            .intersperse(sep)
            .collect::<Vec<_>>()
            .iter()
            .map(|x| x.clone()),
    )
};

static LENT: Jet = |_ctx, n| Rc::new(Noun::from_u32(n.list_iter().fold(0u32, |l, _| l + 1)));

static LEVY: Jet = |ctx, n| {
    let (a, b) = n.as_cell().unwrap();
    if a.list_iter()
        .all(|n| slam(ctx, b.clone(), n) == ctx.nouns.y)
    {
        ctx.nouns.y.clone()
    } else {
        ctx.nouns.n.clone()
    }
};

static LIEN: Jet = |ctx, n| {
    let (a, b) = n.as_cell().unwrap();
    if a.list_iter()
        .any(|n| slam(ctx, b.clone(), n) == ctx.nouns.y)
    {
        ctx.nouns.y.clone()
    } else {
        ctx.nouns.n.clone()
    }
};

static MURN: Jet = |ctx, n| {
    let (a, b) = n.as_cell().unwrap();

    Noun::list_refs(
        a.list_iter()
            .filter_map(|n| slam(ctx, b.clone(), n).as_unit())
            .collect::<Vec<_>>()
            .iter()
            .map(|x| x.clone()),
    )
};

static REAP: Jet = |_ctx, n| {
    let (a, b) = n.as_cell().unwrap();

    Noun::list_refs(repeat_n(b, a.as_u32().unwrap() as usize))
};

static REAR: Jet = |_ctx, n| n.list_iter().last().unwrap();

static REEL: Jet = |ctx, n| {
    let (list, gate) = n.as_cell().unwrap();

    list.list_iter().collect::<Vec<_>>().iter().rfold(
        gate.clone().gate_sample().unwrap().as_cell().unwrap().1,
        |acc, next| slam(ctx, gate.clone(), cell(next.clone(), acc)),
    )
};

static ROLL: Jet = |ctx, n| {
    let (list, gate) = n.as_cell().unwrap();

    list.list_iter().fold(
        gate.clone().gate_sample().unwrap().as_cell().unwrap().1,
        |acc, next| slam(ctx, gate.clone(), cell(next.clone(), acc)),
    )
};

static SCAG: Jet = |_ctx, n| {
    let (a, b) = n.as_cell().unwrap();

    Noun::list_refs(
        b.list_iter()
            .take(a.as_u32().unwrap() as usize)
            .collect::<Vec<_>>()
            .iter()
            .map(|x| x.clone()),
    )
};

static SKID: Jet = |ctx, n| {
    let (list, gate) = n.as_cell().unwrap();

    let (ys, ns): (Vec<Rc<Noun>>, _) = list
        .list_iter()
        .partition(|n| slam(ctx, gate.clone(), n.clone()).is_y());

    cell(
        Noun::list_refs(ys.iter().cloned()),
        Noun::list_refs(ns.iter().cloned()),
    )
};

static SKIM: Jet = |ctx, n| {
    let (list, gate) = n.as_cell().unwrap();

    Noun::list_refs(
        list.list_iter()
            .filter(|n| slam(ctx, gate.clone(), n.clone()).is_y())
            .collect::<Vec<_>>()
            .iter()
            .cloned(),
    )
};

static SKIP: Jet = |ctx, n| {
    let (list, gate) = n.as_cell().unwrap();

    Noun::list_refs(
        list.list_iter()
            .filter(|n| slam(ctx, gate.clone(), n.clone()).is_n())
            .collect::<Vec<_>>()
            .iter()
            .cloned(),
    )
};

static SLAG: Jet = |ctx, n| {
    let (a, list) = n.as_cell().unwrap();

    let mut a_iter = a.as_atom().unwrap().iter_u32_digits();

    match a_iter.len() {
        0 => list,
        1 => Noun::list_refs(
            list.list_iter()
                .skip(a_iter.next().unwrap() as usize)
                .collect::<Vec<_>>()
                .iter()
                .cloned(),
        ),
        _ => ctx.nouns.sig.clone(),
    }
};

static SNAG: Jet = |_ctx, n| {
    let (a, list) = n.as_cell().unwrap();

    list.list_iter().nth(a.as_u32().unwrap() as usize).unwrap()
};

static SNIP: Jet = |_ctx, n| {
    Noun::list_refs(
        n.list_iter()
            .collect::<Vec<_>>()
            .iter()
            .rev()
            .skip(1)
            .rev()
            .cloned(),
    )
};

static SNOC: Jet = |_ctx, n| {
    let (list, b) = n.as_cell().unwrap();

    Noun::list_refs(
        list.list_iter()
            .chain(once(b))
            .collect::<Vec<_>>()
            .iter()
            .cloned(),
    )
};

static SORT: Jet = |ctx, n| {
    let (list, lth) = n.as_cell().unwrap();

    let mut v: Vec<_> = list.list_iter().collect();

    v.sort_by(|a, b| {
        if slam(ctx, lth.clone(), cell(a.clone(), b.clone())).is_y() {
            Ordering::Less
        } else if slam(ctx, lth.clone(), cell(b.clone(), a.clone())).is_y() {
            Ordering::Greater
        } else {
            Ordering::Equal
        }
    });

    Noun::list_refs(v.iter().cloned())
};

static SPIN: Jet = |ctx, n| {
    let (list, b) = n.as_cell().unwrap();
    let (mut acc, gate) = b.as_cell().unwrap();

    let mut res = Vec::new();

    for n in list.list_iter() {
        let (el, new_acc) = slam(ctx, gate.clone(), cell(n, acc)).as_cell().unwrap();
        acc = new_acc;
        res.push(el);
    }

    cell(Noun::list_refs(res.iter().cloned()), acc)
};

static SPUN: Jet = |ctx, n| {
    let (list, gate) = n.as_cell().unwrap();

    let mut res = Vec::new();
    let mut acc = gate.clone().gate_sample().unwrap().as_cell().unwrap().1;

    for n in list.list_iter() {
        let (el, new_acc) = slam(ctx, gate.clone(), cell(n, acc)).as_cell().unwrap();
        acc = new_acc;
        res.push(el);
    }

    cell(Noun::list_refs(res.iter().cloned()), acc)
};

static TURN: Jet = |ctx, n| {
    let (list, gate) = n.as_cell().unwrap();
    Noun::list_refs(
        list.list_iter()
            .map(|x| slam(ctx, gate.clone(), x))
            .collect::<Vec<_>>()
            .iter()
            .cloned(),
    )
};

static WELD: Jet = |_ctx, n| {
    let (a, b) = n.as_cell().unwrap();

    Noun::list_refs(
        a.list_iter()
            .chain(b.list_iter())
            .collect::<Vec<_>>()
            .iter()
            .cloned(),
    )
};

static WELP: Jet = WELD;
