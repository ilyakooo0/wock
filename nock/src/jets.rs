use core::panic;
use murmur3::murmur3_32;
use num_integer::Integer;
use std::{
    collections::BTreeMap,
    io::Read,
    iter::{once, repeat_n, zip},
    rc::Rc,
};

use crate::{
    interpreter::{eval_gate, slam, InterpreterContext},
    noun::{self, cell, Atom, Edge, Hair, Nail, Noun},
};

fn traverse_iter_option<T: Clone, I: Iterator<Item = Option<T>>>(xs: I) -> Option<Vec<T>> {
    let mut target: Vec<T> = Vec::new();

    for x in xs {
        target.push(x.clone()?)
    }

    Some(target)
}

pub type Jets = BTreeMap<noun::Hash, Jet>;
pub type DoubleJets = BTreeMap<noun::Hash, DoubleJet>;

pub fn generate_double_jets() -> DoubleJets {
    BTreeMap::from([(210326504639957723220404138306543145797, JUST)])
}

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
        // (255402796382159268271460433298209603428, SORT),
        (10111398404342329752608067595798409804, SPIN),
        (209194119090564757355985847956576215234, SPUN),
        (103985097318911737222583881947453560061, TURN),
        (216618478373489138062093083914898130454, WELD),
        (161636738411171172446435323238280743696, WELP),
        (90634096513747887582230817128332929026, ZING),
        (59352245691598674621226842217946522940, BEX),
        (102718716366123785010912772034318125155, END),
        (233905026649167301608240871123208487851, CAN),
        (88906713429699361214497715048276521868, CAT),
        (194217032002582779752566874231896746840, MET),
        (132848439781269254342687684309700447927, CUT),
        (184331043431916214773639930303095454137, FIL),
        (104452959965816676316166435675758878521, LSH),
        (84358434946425438379820219100952030132, RSH),
        (49642221456321333759873032692286222867, RAP),
        (92956425869648916777812729456192604084, REP),
        (302196507237993702819920888576650809366, REV),
        (111192034817574564051316537756194881830, RIP),
        (194882429025651072335997865399571359986, RUN),
        (52890757765966568893620799383929397067, RUT),
        (189837220965666741450798472864087835815, SEW),
        (122271149828189266412416527758198690539, SWP),
        (315388312266248574424227833154020891850, XEB),
        (26880808488368366377851132561579207892, CON),
        (311027486926441687148059942599447850754, DIS),
        (330927609077697663248483044495963838437, MIX),
        (39579736615539990724554083585557839000, NOT),
        (3482046397915506566435874021936376320, LAST),
        (124976064672625230427889101290245200507, TRIP),
        (4078279288073906183633157465551067635, MUK),
    ])
}

pub type Jet = fn(ctx: &InterpreterContext, Rc<Noun>) -> Option<Rc<Noun>>;
pub type DoubleJet = fn(ctx: &InterpreterContext, Rc<Noun>, Rc<Noun>) -> Option<Rc<Noun>>;

static BINARY_ATOM: fn(Rc<Noun>, fn(&Atom, &Atom) -> Atom) -> Option<Rc<Noun>> =
    |n: Rc<Noun>, f: fn(&Atom, &Atom) -> Atom| {
        let (p, q) = n.as_cell()?;
        Some(Rc::new(Noun::Atom(f(p.as_atom()?, q.as_atom()?))))
    };

static BINARY_ATOM_LOOB: fn(
    &InterpreterContext,
    Rc<Noun>,
    fn(&Atom, &Atom) -> bool,
) -> Option<Rc<Noun>> = |ctx: &InterpreterContext, n: Rc<Noun>, f: fn(&Atom, &Atom) -> bool| {
    let (p, q) = n.as_cell()?;

    if f(p.as_atom()?, q.as_atom()?) {
        Some(ctx.nouns.sig.clone())
    } else {
        Some(ctx.nouns.one.clone())
    }
};

static BINARY_ATOM_PAIR: fn(Rc<Noun>, fn(&Atom, &Atom) -> (Atom, Atom)) -> Option<Rc<Noun>> =
    |n: Rc<Noun>, f: fn(&Atom, &Atom) -> (Atom, Atom)| {
        let (p, q) = n.as_cell()?;

        let (a, b) = f(p.as_atom()?, q.as_atom()?);
        Some(cell(Rc::new(Noun::Atom(a)), Rc::new(Noun::Atom(b))))
    };

static ADD: Jet = |_ctx, n| BINARY_ATOM(n, |a, b| a + b);
static DEC: Jet = |_ctx, n| Some(Rc::new(Noun::Atom(n.as_atom()? - 1u32)));
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
    let n = n.as_atom()?;
    CAP_ATOM(ctx, n.clone())
};

static CAP_ATOM: fn(&InterpreterContext, Atom) -> Option<Rc<Noun>> = |ctx, n: Atom| {
    let mut n_iter = n.iter_u32_digits();

    match n_iter.len() {
        0 => None,
        1 => CAP_U32(ctx, n_iter.next().unwrap()),
        _ => CAP_ATOM(ctx, n >> 1),
    }
};
static CAP_U32: fn(ctx: &InterpreterContext, u32) -> Option<Rc<Noun>> = |ctx, n| match n {
    0 | 1 => None,
    2 => Some(ctx.nouns.two.clone()),
    3 => Some(ctx.nouns.three.clone()),
    n => CAP_U32(ctx, n >> 1),
};

static MAS: Jet = |ctx, n| {
    let n = n.as_atom()?;
    Some(Rc::new(Noun::Atom(MAS_ATOM(ctx, n.clone())?)))
};
static MAS_ATOM: fn(&InterpreterContext, Atom) -> Option<Atom> = |ctx, n| {
    let mut n_iter = n.iter_u32_digits();

    match n_iter.len() {
        0 => None,
        1 => match n_iter.next().unwrap() {
            1 => None,
            x => Some(Atom::new(vec![MAS_U32(x)])),
        },
        _ => Some((&n & &ctx.big_uints.one) + (MAS_ATOM(ctx, &n >> 1)? << 1)),
    }
};
static MAS_U32: fn(u32) -> u32 = |n| match n {
    0 | 1 => panic!(),
    2 | 3 => 1,
    _ => (n & 1) + (MAS_U32(n >> 1) << 1),
};

static PEG: Jet = |ctx, n| {
    let (a, b) = n.as_cell()?;

    let a = a.as_atom()?;
    let b = b.as_atom()?;

    if *a == Atom::ZERO {
        return None;
    };

    Some(Rc::new(Noun::Atom(PEG_ATOM(ctx, a, b.clone())?)))
};
static PEG_ATOM: fn(&InterpreterContext, &Atom, Atom) -> Option<Atom> = |ctx, a, b| {
    let mut b_iter = b.iter_u32_digits();

    match b_iter.len() {
        0 => None,
        1 => Some(PEG_U32(a, b_iter.next().unwrap())),
        _ => Some((&b & &ctx.big_uints.one) + (PEG_ATOM(ctx, a, b >> 1)? << 1)),
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
    let (nedl, hstk) = n.as_cell()?;
    if nedl.is_sig() || hstk.is_sig() {
        return Some(ctx.nouns.sig.clone());
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

    Some(Noun::list(result.iter().map(|a| Noun::from_u32(*a))))
};

static FIND: Jet = |ctx, n| {
    let (nedl, hstk) = n.as_cell()?;
    if nedl.is_sig() || hstk.is_sig() {
        return Some(ctx.nouns.sig.clone());
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
            return Some(Rc::new(Noun::from_u32(i)).unit());
        }

        inner_iter = iter.clone();
    }

    return Some(ctx.nouns.sig.clone());
};

static FLOP: Jet = |ctx, n| {
    Some(
        n.list_iter()
            .fold(ctx.nouns.sig.clone(), |tail, head| cell(head, tail)),
    )
};

static GULF: Jet = |_ctx, n| {
    let (from, to) = n.as_cell()?;

    let mut from = (*from.as_atom()?).clone();
    let to = to.as_atom()?.clone();

    if from > to {
        return None;
    }

    let mut tmp = Vec::new();

    while from <= to {
        tmp.push(from.clone());
        from += 1u32;
    }

    Some(Noun::list(tmp.iter().rev().map(|a| Noun::Atom(a.clone()))))
};

static INTO: Jet = |_ctx, n| {
    let (a, b) = n.as_cell()?;
    let (b, c) = b.as_cell()?;

    let mut b_iter = b.as_atom()?.iter_u32_digits();

    match b_iter.len() {
        0 => Some(cell(c, a)),
        1 => {
            let l = a.list_iter();
            let b = b_iter.next().unwrap() as usize;
            Some(Noun::list_refs(
                l.clone()
                    .take(b)
                    .chain(once(c))
                    .chain(l.skip(b))
                    .collect::<Vec<_>>()
                    .iter()
                    .map(|x| x.clone()),
            ))
        }
        _ => Some(Noun::list_refs(
            a.list_iter()
                .chain(once(c))
                .collect::<Vec<_>>()
                .iter()
                .map(|x| x.clone()),
        )),
    }
};

static JOIN: Jet = |_ctx, n| {
    let (sep, list) = n.as_cell()?;
    Some(Noun::list_refs(
        list.list_iter()
            .intersperse(sep)
            .collect::<Vec<_>>()
            .iter()
            .cloned(),
    ))
};

static LENT: Jet = |_ctx, n| {
    Some(Rc::new(Noun::from_u32(
        n.list_iter().fold(0u32, |l, _| l + 1),
    )))
};

static LEVY: Jet = |ctx, n| {
    let (a, b) = n.as_cell()?;
    if traverse_iter_option(a.list_iter().map(|n| slam(ctx, b.clone(), n)))?
        .iter()
        .all(|x| x.is_y())
    {
        Some(ctx.nouns.y.clone())
    } else {
        Some(ctx.nouns.n.clone())
    }
};

static LIEN: Jet = |ctx, n| {
    let (a, b) = n.as_cell()?;
    if traverse_iter_option(a.list_iter().map(|n| slam(ctx, b.clone(), n)))?
        .iter()
        .any(|x| x.is_y())
    {
        Some(ctx.nouns.y.clone())
    } else {
        Some(ctx.nouns.n.clone())
    }
};

static MURN: Jet = |ctx, n| {
    let (a, b) = n.as_cell()?;

    Some(Noun::list_refs(
        a.list_iter()
            .filter_map(|n| slam(ctx, b.clone(), n)?.as_unit())
            .collect::<Vec<_>>()
            .iter()
            .map(|x| x.clone()),
    ))
};

static REAP: Jet = |_ctx, n| {
    let (a, b) = n.as_cell()?;

    Some(Noun::list_refs(repeat_n(b, a.as_u32().unwrap() as usize)))
};

static REAR: Jet = |_ctx, n| n.list_iter().last();

static REEL: Jet = |ctx, n| {
    let (list, gate) = n.as_cell()?;

    list.list_iter().collect::<Vec<_>>().iter().rfold(
        Some(gate.clone().gate_sample()?.as_cell()?.1),
        |acc, next| slam(ctx, gate.clone(), cell(next.clone(), acc?)),
    )
};

static ROLL: Jet = |ctx, n| {
    let (list, gate) = n.as_cell()?;

    list.list_iter().fold(
        Some(gate.clone().gate_sample()?.as_cell()?.1),
        |acc, next| slam(ctx, gate.clone(), cell(next.clone(), acc?)),
    )
};

static SCAG: Jet = |_ctx, n| {
    let (a, b) = n.as_cell()?;

    Some(Noun::list_refs(
        b.list_iter()
            .take(a.as_u32().unwrap() as usize)
            .collect::<Vec<_>>()
            .iter()
            .map(|x| x.clone()),
    ))
};

static SKID: Jet = |ctx, n| {
    let (list, gate) = n.as_cell()?;

    let slammed_n = traverse_iter_option(
        list.list_iter()
            .map(|n| Some((slam(ctx, gate.clone(), n.clone())?, n))),
    )?;
    let (ys, ns): (Vec<_>, _) = slammed_n.iter().partition(|(l, _)| l.is_y());

    Some(cell(
        Noun::list_refs(ys.iter().map(|(_, x)| x).cloned()),
        Noun::list_refs(ns.iter().map(|(_, x)| x).cloned()),
    ))
};

static SKIM: Jet = |ctx, n| {
    let (list, gate) = n.as_cell()?;

    Some(Noun::list_refs(
        traverse_iter_option(
            list.list_iter()
                .map(|n| Some((slam(ctx, gate.clone(), n.clone())?, n))),
        )?
        .iter()
        .filter(|(l, _)| l.is_y())
        .map(|(_, x)| x)
        .cloned(),
    ))
};

static SKIP: Jet = |ctx, n| {
    let (list, gate) = n.as_cell()?;

    Some(Noun::list_refs(
        traverse_iter_option(
            list.list_iter()
                .map(|n| Some((slam(ctx, gate.clone(), n.clone())?, n))),
        )?
        .iter()
        .filter(|(l, _)| l.is_y())
        .map(|(_, x)| x)
        .cloned(),
    ))
};

static SLAG: Jet = |ctx, n| {
    let (a, list) = n.as_cell()?;

    let mut a_iter = a.as_atom()?.iter_u32_digits();

    match a_iter.len() {
        0 => Some(list),
        1 => Some(Noun::list_refs(
            list.list_iter()
                .skip(a_iter.next().unwrap() as usize)
                .collect::<Vec<_>>()
                .iter()
                .cloned(),
        )),
        _ => Some(ctx.nouns.sig.clone()),
    }
};

static SNAG: Jet = |_ctx, n| {
    let (a, list) = n.as_cell()?;

    list.list_iter().nth(a.as_u32().unwrap() as usize)
};

static SNIP: Jet = |_ctx, n| {
    Some(Noun::list_refs(
        n.list_iter()
            .collect::<Vec<_>>()
            .iter()
            .rev()
            .skip(1)
            .rev()
            .cloned(),
    ))
};

static SNOC: Jet = |_ctx, n| {
    let (list, b) = n.as_cell()?;

    Some(Noun::list_refs(
        list.list_iter()
            .chain(once(b))
            .collect::<Vec<_>>()
            .iter()
            .cloned(),
    ))
};

// static SORT: Jet = |ctx, n| {
//     let (list, lth) = n.as_cell().unwrap();

//     let mut v: Vec<_> = list.list_iter().collect();

//     v.sort_by(|a, b| {
//         if slam(ctx, lth.clone(), cell(a.clone(), b.clone())) == ctx.nouns.some_y {
//             Ordering::Less
//         } else if slam(ctx, lth.clone(), cell(b.clone(), a.clone())) == ctx.nouns.some_y {
//             Ordering::Greater
//         } else {
//             Ordering::Equal
//         }
//     });

//     Some(Noun::list_refs(v.iter().cloned()))
// };

static SPIN: Jet = |ctx, n| {
    let (list, b) = n.as_cell()?;
    let (mut acc, gate) = b.as_cell()?;

    let mut res = Vec::new();

    for n in list.list_iter() {
        let (el, new_acc) = slam(ctx, gate.clone(), cell(n, acc))?.as_cell()?;
        acc = new_acc;
        res.push(el);
    }

    Some(cell(Noun::list_refs(res.iter().cloned()), acc))
};

static SPUN: Jet = |ctx, n| {
    let (list, gate) = n.as_cell()?;

    let mut res = Vec::new();
    let mut acc = gate.clone().gate_sample()?.as_cell()?.1;

    for n in list.list_iter() {
        let (el, new_acc) = slam(ctx, gate.clone(), cell(n, acc))?.as_cell()?;
        acc = new_acc;
        res.push(el);
    }

    Some(cell(Noun::list_refs(res.iter().cloned()), acc))
};

static TURN: Jet = |ctx, n| {
    let (list, gate) = n.as_cell()?;
    Some(Noun::list_refs(
        traverse_iter_option(list.list_iter().map(|x| slam(ctx, gate.clone(), x)))?
            .iter()
            .cloned(),
    ))
};

static WELD: Jet = |_ctx, n| {
    let (a, b) = n.as_cell()?;

    Some(Noun::list_refs(
        a.list_iter()
            .chain(b.list_iter())
            .collect::<Vec<_>>()
            .iter()
            .cloned(),
    ))
};

static WELP: Jet = WELD;

static ZING: Jet = |_ctx, n| {
    Some(Noun::list_refs(
        n.list_iter()
            .map(|l| l.list_iter())
            .flatten()
            .collect::<Vec<_>>()
            .iter()
            .cloned(),
    ))
};

static BEX: Jet = |ctx, n| {
    Some(Rc::new(Noun::Atom(
        &ctx.big_uints.one << n.as_u32().unwrap(),
    )))
};

static END: Jet = |ctx, n| {
    let (a, b) = n.as_cell()?;
    let bite = a.as_bite();

    let bits = bite.bits();

    let mask = (&ctx.big_uints.one << bits) - &ctx.big_uints.one;

    Some(Rc::new(Noun::Atom(mask & b.as_atom()?)))
};

static CAN: Jet = |ctx, n| {
    let (a, b) = n.as_cell()?;
    let bloq = a.as_u32().unwrap();
    let bloq_ = 2u32.pow(bloq);

    let atom: Option<Atom> = b.list_iter().collect::<Vec<_>>().iter().rfold(
        Some(ctx.big_uints.zero.clone()),
        |acc, next| {
            let (p, q) = next.as_cell()?;
            let step = p.as_u32().unwrap();
            let bits = bloq_ * step;
            let mask = (&ctx.big_uints.one << bits) - &ctx.big_uints.one;

            Some((acc? << bits) | (mask & q.as_atom()?))
        },
    );
    Some(Rc::new(Noun::Atom(atom?)))
};

static CAT: Jet = |_ctx, n| {
    let (bloq, b) = n.as_cell()?;
    let (b, c) = b.as_cell()?;

    let bloq = bloq.as_u32()?;
    let b = b.as_atom()?;
    let c = c.as_atom()?;

    Some(Rc::new(Noun::Atom(
        (c << (met(bloq, &b) * 2u32.pow(bloq))) | b,
    )))
};

static MET: Jet = |_ctx, n| {
    let (bloq, b) = n.as_cell()?;
    let b: Atom = b.as_atom()?.clone();
    let bloq = bloq.as_u32().unwrap();

    Some(Rc::new(Noun::from_u32(met(bloq, &b))))
};

fn met(bloq: u32, a: &Atom) -> u32 {
    (a.bits() as u32).div_ceil(2u32.pow(bloq))
}

static CUT: Jet = |ctx, n| {
    let (a, b) = n.as_cell()?;
    let (b, d) = b.as_cell()?;
    let (b, c) = b.as_cell()?;

    let bits = 2u32.pow(a.as_u32().unwrap());

    let mask = (&ctx.big_uints.one << (bits * c.as_u32().unwrap())) - &ctx.big_uints.one;

    Some(Rc::new(Noun::Atom(
        (d.as_atom()? >> (bits * b.as_u32().unwrap())) & mask,
    )))
};

static FIL: Jet = |ctx, n| {
    let (a, b) = n.as_cell()?;
    let (b, c) = b.as_cell()?;

    let bits = 2u32.pow(a.as_u32().unwrap());

    let mask = (&ctx.big_uints.one << bits) - &ctx.big_uints.one;

    let src = c.as_atom()? & mask;

    let mut target = ctx.big_uints.zero.clone();

    for _ in 0..b.as_u32().unwrap() {
        target = (target << bits) | &src;
    }

    Some(Rc::new(Noun::Atom(target)))
};

static LSH: Jet = |_ctx, n| {
    let (a, b) = n.as_cell()?;
    let a = a.as_bite();
    let b = b.as_atom()?;

    Some(Rc::new(Noun::Atom(b << a.bits())))
};

static RSH: Jet = |_ctx, n| {
    let (a, b) = n.as_cell()?;
    let a = a.as_bite();
    let b = b.as_atom()?;

    Some(Rc::new(Noun::Atom(b >> a.bits())))
};

static RAP: Jet = |ctx, n| {
    let (bloq, b) = n.as_cell()?;
    let bloq = bloq.as_u32().unwrap();
    let bits = 2u32.pow(bloq);

    let mut target = ctx.big_uints.zero.clone();

    for el in b.list_iter().collect::<Vec<_>>().iter().rev() {
        let el = el.as_atom()?;
        if el != &ctx.big_uints.zero {
            target = (target << (bits * met(bloq, &el))) | el;
        }
    }

    Some(Rc::new(Noun::Atom(target)))
};

static REP: Jet = |ctx, n| {
    let (bite, b) = n.as_cell()?;
    let bite = bite.as_bite();

    let bits = bite.bits();
    let mask = (&ctx.big_uints.one << bits) - &ctx.big_uints.one;

    let mut target = ctx.big_uints.zero.clone();

    for el in b.list_iter().collect::<Vec<_>>().iter().rev() {
        target = (target << bits) | (el.as_atom()? & &mask);
    }

    Some(Rc::new(Noun::Atom(target)))
};

static REV: Jet = |ctx, n| {
    let (bloq, b) = n.as_cell()?;
    let (len, dat) = b.as_cell()?;
    let bloq = bloq.as_u32().unwrap();
    let len = len.as_u32().unwrap();
    let mut dat: Atom = dat.as_atom()?.clone();

    let bits = 2u32.pow(bloq);
    let mask = (&ctx.big_uints.one << bits) - &ctx.big_uints.one;

    let mut target = ctx.big_uints.zero.clone();

    for _ in 0..len {
        target = (target << bits) | (&mask & &dat);
        dat = dat >> bits;
    }

    Some(Rc::new(Noun::Atom(target)))
};

static RIP: Jet = |ctx, n| {
    let (bite, b) = n.as_cell()?;
    let bite = bite.as_bite();
    let mut b: Atom = b.as_atom()?.clone();

    let bits = bite.bits();
    let mask = (&ctx.big_uints.one << bits) - &ctx.big_uints.one;

    let mut target = Vec::new();

    while b > ctx.big_uints.zero {
        target.push(Rc::new(Noun::Atom(&mask & &b)));
        b = b >> bits;
    }

    Some(Noun::list_refs(target.iter().cloned()))
};

static RUN: Jet = |ctx, n| {
    let (bite, b) = n.as_cell()?;
    let (b, gate) = b.as_cell()?;

    let bite = bite.as_bite();
    let mut b: Atom = b.as_atom()?.clone();

    let bits = bite.bits();
    let mask = (&ctx.big_uints.one << bits) - &ctx.big_uints.one;

    let mut target = ctx.big_uints.zero.clone();

    while b > ctx.big_uints.zero {
        target = (target << bits)
            | ((*slam(ctx, gate.clone(), Rc::new(Noun::Atom(&mask & &b)))?).as_atom()? & &mask);
        b = b >> bits;
    }

    Some(Rc::new(Noun::Atom(target)))
};

static RUT: Jet = |ctx, n| {
    let (bite, b) = n.as_cell()?;
    let (b, gate) = b.as_cell()?;

    let bite = bite.as_bite();
    let mut b: Atom = b.as_atom()?.clone();

    let bits = bite.bits();
    let mask = (&ctx.big_uints.one << bits) - &ctx.big_uints.one;

    let mut target = Vec::new();

    while b > ctx.big_uints.zero {
        target
            .push((*slam(ctx, gate.clone(), Rc::new(Noun::Atom(&mask & &b)))?).as_atom()? & &mask);
        b = b >> bits;
    }

    Some(Noun::list(target.iter().cloned().map(Noun::Atom)))
};

static SEW: Jet = |ctx, n| {
    let (a, b) = n.as_cell()?;
    let (b, recipient) = b.as_cell()?;
    let (sew_start, c) = b.as_cell()?;
    let (number_of_blocks, donor) = c.as_cell()?;
    let recipient = recipient.as_atom()?;
    let donor = donor.as_atom()?;

    let bloq = a.as_u32().unwrap();
    let bits = 2u32.pow(bloq);
    let mask = ((&ctx.big_uints.one << (bits * number_of_blocks.as_u32().unwrap()))
        - &ctx.big_uints.one)
        << (sew_start.as_u32().unwrap() * bits);

    let apendix = recipient & &mask;

    let target = (recipient ^ apendix) | (donor & mask);

    Some(Rc::new(Noun::Atom(target)))
};

static SWP: Jet = |ctx, n| {
    let (bloq, b) = n.as_cell()?;
    let bloq = bloq.as_u32().unwrap();
    let bits = 2u32.pow(bloq);
    let mask = (&ctx.big_uints.one << bits) - &ctx.big_uints.one;

    let mut source = b.as_atom()?.clone();
    let mut target = ctx.big_uints.zero.clone();

    while source > ctx.big_uints.zero {
        target = (target << bits) | (&source & &mask);
        source = source >> bits;
    }

    Some(Rc::new(Noun::Atom(target)))
};

static XEB: Jet = |_ctx, n| {
    let mut a = n.as_atom()?.clone();

    let mut c = 0;

    while a > Atom::ZERO {
        a = a >> 1;
        c += 1;
    }

    Some(Rc::new(Noun::from_u32(c)))
};

static CON: Jet = |_ctx, n| BINARY_ATOM(n, |a, b| a | b);
static DIS: Jet = |_ctx, n| BINARY_ATOM(n, |a, b| a & b);
static MIX: Jet = |_ctx, n| BINARY_ATOM(n, |a, b| a ^ b);

static NOT: Jet = |ctx, n| {
    let (a, b) = n.as_cell()?;
    let (b, c) = b.as_cell()?;

    let bloq = a.as_u32().unwrap();
    let bits = 2u32.pow(bloq);
    let mask = (&ctx.big_uints.one << (bits * b.as_u32().unwrap())) - &ctx.big_uints.one;

    let target = c.as_atom()? ^ &mask;

    Some(Rc::new(Noun::Atom(target)))
};

static LAST: Jet = |_ctx, n| {
    let (a, b) = n.as_cell()?;
    let a = a.as_hair()?;
    let b = b.as_hair()?;
    Some(Noun::from_hair(last(&a, &b)))
};

fn last<'a>(a: &'a Hair, b: &'a Hair) -> &'a Hair {
    if a.line == b.line {
        if a.column > b.column {
            a
        } else {
            b
        }
    } else {
        if a.line > b.line {
            a
        } else {
            b
        }
    }
}

static TRIP: Jet = |_ctx, n| {
    let a = n.as_atom()?;
    Some(Noun::list(
        a.to_bytes_le().iter().map(|b| Noun::from_u32(*b as u32)),
    ))
};

static MUK: Jet = |_ctx, n| {
    let (seed, len) = n.as_cell()?;
    let (len, key) = len.as_cell()?;
    let seed = seed.as_u32()?;
    let len = len.as_u32()?;
    let key = key.as_atom()?;

    let key: Vec<_> = key
        .to_bytes_le()
        .iter()
        .take(len as usize)
        .cloned()
        .collect();

    Some(Rc::new(Noun::from_u32(
        murmur3_32(&mut &*key, seed).unwrap(),
    )))
};

static JUST: DoubleJet = |ctx, n, m| {
    let char = n.as_atom()?;
    let nail = m.as_nail()?;
    if nail.rest == ctx.nouns.sig {
        Some(fail(nail).as_noun())
    } else {
        let (i, _) = nail.rest.as_cell()?;
        let i = i.as_atom()?;
        if i == char {
            Some(next(nail)?.as_noun())
        } else {
            Some(fail(nail).as_noun())
        }
    }
};

fn next(nail: Nail) -> Option<Edge> {
    if *nail.rest == Noun::SIG {
        Some(fail(nail))
    } else {
        let (i, t) = nail.rest.as_cell()?;
        let zac = lust(i.as_atom()?.clone(), nail.hair);
        // cell(zac, cell(Rc::new(Noun::SIG), cell(i, cell(zac, t))))
        Some(Edge {
            hair: zac.clone(),
            result: Some((i, Nail { hair: zac, rest: t })),
        })
    }
}

fn lust(char: Atom, hair: Hair) -> Hair {
    if char == Atom::from_slice(&vec![10]) {
        Hair {
            line: hair.line + 1,
            column: 1,
        }
    } else {
        Hair {
            line: hair.line,
            column: hair.column + 1,
        }
    }
}

fn fail(nail: Nail) -> Edge {
    Edge {
        hair: nail.hair,
        result: None,
    }
}
