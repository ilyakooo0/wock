use std::{cell::Cell, cmp::Ordering, fmt::Write, rc::Rc};

use num_bigint::BigUint;
use std::fmt;
use xxhash_rust::xxh3::xxh3_128;

use crate::jets::mum;

pub type Atom = BigUint;

pub type Hash = u128;
pub type Mug = u32;

#[derive(Clone, Debug, Eq)]
pub enum Noun {
    Atom(Atom),
    Cell {
        p: Rc<Noun>,
        q: Rc<Noun>,
        hash: Cell<Option<Hash>>,
        mug: Cell<Option<Mug>>,
    },
}

impl Ord for Noun {
    fn cmp(&self, other: &Self) -> Ordering {
        compare_nouns(self, other)
    }
}

impl PartialOrd for Noun {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(compare_nouns(self, other))
    }
}

fn compare_nouns(p: &Noun, q: &Noun) -> Ordering {
    p.hash().cmp(&q.hash())
}

impl PartialEq for Noun {
    fn eq(&self, other: &Self) -> bool {
        self.hash().eq(&other.hash())
    }
}

impl<'a> Noun {
    pub const SIG: Noun = Noun::Atom(BigUint::ZERO);

    pub fn rc(self: Self) -> Rc<Self> {
        Rc::new(self)
    }

    pub fn from_u32(a: u32) -> Noun {
        Noun::Atom(Atom::new(vec![a]))
    }

    pub fn list<I: Iterator<Item = Noun> + DoubleEndedIterator>(l: I) -> Rc<Noun> {
        l.rfold(Rc::new(Noun::SIG), |tail, head| cell(&Rc::new(head), &tail))
    }

    pub fn list_refs<I: Iterator<Item = Rc<Noun>> + DoubleEndedIterator>(l: I) -> Rc<Noun> {
        l.rfold(Rc::new(Noun::SIG), |tail, head| cell(&head, &tail))
    }

    pub fn unit(self: Rc<Self>) -> Rc<Noun> {
        cell(&Rc::new(Noun::SIG), &self)
    }

    pub fn from_unit(unit: Option<Rc<Noun>>) -> Rc<Noun> {
        match unit {
            None => Rc::new(Noun::SIG),
            Some(x) => Noun::unit(x),
        }
    }

    pub fn as_atom(self: &Self) -> Option<&Atom> {
        match self {
            Noun::Atom(a) => Option::Some(a),
            _ => Option::None,
        }
    }

    pub fn as_u32(self: &Self) -> Option<u32> {
        let mut iter = self.as_atom()?.iter_u32_digits();

        match iter.len() {
            0 => Some(0),
            1 => Some(iter.next().unwrap()),
            _ => Option::None,
        }
    }

    pub fn gate_sample(self: &Rc<Self>) -> Option<&Rc<Self>> {
        let (_battery, payload) = self.as_cell()?;
        let (sample, _context) = payload.as_cell()?;
        Option::Some(sample)
    }

    pub fn as_cell(self: &'a Self) -> Option<(&'a Rc<Noun>, &'a Rc<Noun>)> {
        match self {
            Noun::Cell { p, q, .. } => Option::Some((p, q)),
            _ => Option::None,
        }
    }

    pub fn as_cell_cloned(self: &'a Self) -> Option<(Rc<Noun>, Rc<Noun>)> {
        match self {
            Noun::Cell { p, q, .. } => Option::Some((p.clone(), q.clone())),
            _ => Option::None,
        }
    }

    pub fn tuple_2(self: &Self) -> Option<(&Rc<Noun>, &Rc<Noun>)> {
        self.as_cell()
    }

    pub fn tuple_3(self: &Self) -> Option<(&Rc<Noun>, &Rc<Noun>, &Rc<Noun>)> {
        let (a, b) = self.as_cell()?;
        let (b, c) = b.as_cell()?;

        Some((a, b, c))
    }
    pub fn tuple_4(self: &Self) -> Option<(&Rc<Noun>, &Rc<Noun>, &Rc<Noun>, &Rc<Noun>)> {
        let (a, b) = self.as_cell()?;
        let (b, c) = b.as_cell()?;
        let (c, d) = c.as_cell()?;

        Some((a, b, c, d))
    }

    pub fn tuple_5(self: &Self) -> Option<(&Rc<Noun>, &Rc<Noun>, &Rc<Noun>, &Rc<Noun>, &Rc<Noun>)> {
        let (a, b) = self.as_cell()?;
        let (b, c) = b.as_cell()?;
        let (c, d) = c.as_cell()?;
        let (d, e) = d.as_cell()?;

        Some((a, b, c, d, e))
    }

    pub fn tuple_6(
        self: &Self,
    ) -> Option<(
        &Rc<Noun>,
        &Rc<Noun>,
        &Rc<Noun>,
        &Rc<Noun>,
        &Rc<Noun>,
        &Rc<Noun>,
    )> {
        let (a, b) = self.as_cell()?;
        let (b, c) = b.as_cell()?;
        let (c, d) = c.as_cell()?;
        let (d, e) = d.as_cell()?;
        let (e, f) = e.as_cell()?;

        Some((a, b, c, d, e, f))
    }

    pub fn is_sig(self: &Self) -> bool {
        match self {
            Noun::Atom(n) if *n == Atom::ZERO => true,
            _ => false,
        }
    }

    pub fn is_y(self: &Self) -> bool {
        self.is_sig()
    }

    pub fn is_n(self: &Self) -> bool {
        !self.is_sig()
    }

    pub fn list_iter(self: &'a Rc<Self>) -> NounListIterator<'a> {
        NounListIterator { noun: self }
    }

    /// panics if malformed
    pub fn as_unit(self: &Self) -> Option<&Rc<Self>> {
        if self.is_sig() {
            Option::None
        } else {
            Option::Some(self.as_cell().unwrap().1)
        }
    }

    pub fn hash(self: &Self) -> Hash {
        match self {
            Noun::Cell { hash, p, q, .. } => match hash.get() {
                Some(x) => x,
                None => {
                    let hash_value =
                        stacker::maybe_grow(32 * 1024, 1024 * 1024, || hash_pair(p, q));
                    hash.replace(Some(hash_value));
                    hash_value
                }
            },
            Noun::Atom(a) => xxh3_128(&*a.to_bytes_le()),
        }
    }

    pub fn mug(self: &Self) -> Mug {
        match self {
            Noun::Cell { mug, p, q, .. } => match mug.get() {
                Some(x) => x,
                None => {
                    let mug_value = stacker::maybe_grow(32 * 1024, 1024 * 1024, || {
                        mum(0xDEADBEEF, 0xFFFE, &Atom::new(vec![p.mug(), q.mug()]))
                    });
                    mug.replace(Some(mug_value));
                    mug_value
                }
            },
            Self::Atom(a) => mum(0xCAFEBABE, 0x7FFF, a),
        }
    }

    /// returns the hash and the current sample
    pub fn hash_gate(self: &Self) -> (Hash, &Rc<Noun>) {
        let (battery, payload) = self.as_cell().unwrap();
        let (sample, context) = payload.as_cell().unwrap();

        (hash_pair(battery, context), sample)
    }

    /// returns the hash and the current sample
    pub fn hash_double_gate(self: &Self) -> (Hash, &Rc<Noun>, &Rc<Noun>) {
        let (battery, payload) = self.as_cell().unwrap();
        let (sample_2, context) = payload.as_cell().unwrap();
        let (inner_hash, sample_1) = context.hash_gate();

        (
            hash_tripple(battery.clone(), inner_hash),
            sample_1,
            sample_2,
        )
    }

    pub fn as_bite(self: &Self) -> Bite {
        match self {
            Noun::Atom(_) => Bite {
                bloq: self.as_u32().unwrap(),
                step: 1,
            },

            Noun::Cell {
                p: bloq, q: step, ..
            } => Bite {
                bloq: bloq.as_u32().unwrap(),
                step: step.as_u32().unwrap(),
            },
        }
    }

    pub fn from_bytes(bytes: &[u8]) -> Self {
        Noun::Atom(Atom::from_bytes_le(bytes))
    }

    pub fn as_bytes(self: &Self) -> Option<Vec<u8>> {
        let a = self.as_atom()?;
        Some(a.to_bytes_le())
    }

    pub fn as_hair(self: &Self) -> Option<Hair> {
        let (line, column) = self.as_cell()?;
        Some(Hair {
            line: line.as_u32().unwrap(),
            column: column.as_u32().unwrap(),
        })
    }

    pub fn from_hair(hair: &Hair) -> Rc<Self> {
        cell(
            &Rc::new(Noun::from_u32(hair.line)),
            &Rc::new(Noun::from_u32(hair.column)),
        )
    }

    pub fn as_nail(self: &Self) -> Option<Nail> {
        let (hair, tape) = self.as_cell()?;
        Some(Nail {
            hair: hair.as_hair()?,
            rest: tape,
        })
    }

    pub fn as_edge(self: &Self) -> Option<Edge> {
        let (hair, result) = self.as_cell()?;
        let hair = hair.as_hair()?;
        let result = result.as_unit().map(|x| {
            let (result, nail) = x.as_cell()?;
            let nail = nail.as_nail()?;
            Some((result, nail))
        });
        let result = match result {
            Some(result) => result,
            None => None?,
        };
        Some(Edge { hair, result })
    }
}

pub trait AsNoun {
    fn as_noun(self: &Self) -> Rc<Noun>;
}

impl AsNoun for Noun {
    fn as_noun(self: &Self) -> Rc<Noun> {
        Rc::new(self.clone())
    }
}

impl AsNoun for Rc<Noun> {
    fn as_noun(self: &Self) -> Rc<Noun> {
        self.clone()
    }
}

impl<A, B> AsNoun for (A, B)
where
    A: AsNoun,
    B: AsNoun,
{
    fn as_noun(self: &Self) -> Rc<Noun> {
        let (a, b) = self;

        cell(&a.as_noun(), &b.as_noun())
    }
}

impl<A, B, C> AsNoun for (A, B, C)
where
    A: AsNoun,
    B: AsNoun,
    C: AsNoun,
{
    fn as_noun(self: &Self) -> Rc<Noun> {
        let (a, b, c) = self;

        cell(&a.as_noun(), &cell(&b.as_noun(), &c.as_noun()))
    }
}

impl<A, B, C, D> AsNoun for (A, B, C, D)
where
    A: AsNoun,
    B: AsNoun,
    C: AsNoun,
    D: AsNoun,
{
    fn as_noun(self: &Self) -> Rc<Noun> {
        let (a, b, c, d) = self;

        cell(
            &a.as_noun(),
            &cell(&b.as_noun(), &cell(&c.as_noun(), &d.as_noun())),
        )
    }
}

impl<A, B, C, D, E> AsNoun for (A, B, C, D, E)
where
    A: AsNoun,
    B: AsNoun,
    C: AsNoun,
    D: AsNoun,
    E: AsNoun,
{
    fn as_noun(self: &Self) -> Rc<Noun> {
        let (a, b, c, d, e) = self;

        cell(
            &a.as_noun(),
            &cell(
                &b.as_noun(),
                &cell(&c.as_noun(), &cell(&d.as_noun(), &e.as_noun())),
            ),
        )
    }
}

impl<A, B, C, D, E, F> AsNoun for (A, B, C, D, E, F)
where
    A: AsNoun,
    B: AsNoun,
    C: AsNoun,
    D: AsNoun,
    E: AsNoun,
    F: AsNoun,
{
    fn as_noun(self: &Self) -> Rc<Noun> {
        let (a, b, c, d, e, f) = self;

        cell(
            &a.as_noun(),
            &cell(
                &b.as_noun(),
                &cell(
                    &c.as_noun(),
                    &cell(&d.as_noun(), &cell(&e.as_noun(), &f.as_noun())),
                ),
            ),
        )
    }
}

impl From<(Rc<Noun>, Rc<Noun>)> for Noun {
    fn from(value: (Rc<Noun>, Rc<Noun>)) -> Self {
        naked_cell(&value.0, &value.1)
    }
}

impl From<(Rc<Noun>, Rc<Noun>, Rc<Noun>)> for Noun {
    fn from(value: (Rc<Noun>, Rc<Noun>, Rc<Noun>)) -> Self {
        naked_cell(&value.0, &cell(&value.1, &value.2))
    }
}

impl From<(Rc<Noun>, Rc<Noun>, Rc<Noun>, Rc<Noun>)> for Noun {
    fn from(value: (Rc<Noun>, Rc<Noun>, Rc<Noun>, Rc<Noun>)) -> Self {
        naked_cell(&value.0, &cell(&value.1, &cell(&value.2, &value.3)))
    }
}

impl From<(Rc<Noun>, Rc<Noun>, Rc<Noun>, Rc<Noun>, Rc<Noun>)> for Noun {
    fn from(value: (Rc<Noun>, Rc<Noun>, Rc<Noun>, Rc<Noun>, Rc<Noun>)) -> Self {
        naked_cell(
            &value.0,
            &cell(&value.1, &cell(&value.2, &cell(&value.3, &value.4))),
        )
    }
}

impl From<(Rc<Noun>, Rc<Noun>, Rc<Noun>, Rc<Noun>, Rc<Noun>, Rc<Noun>)> for Noun {
    fn from(value: (Rc<Noun>, Rc<Noun>, Rc<Noun>, Rc<Noun>, Rc<Noun>, Rc<Noun>)) -> Self {
        naked_cell(
            &value.0,
            &cell(
                &value.1,
                &cell(&value.2, &cell(&value.3, &cell(&value.4, &value.5))),
            ),
        )
    }
}

impl
    From<(
        Rc<Noun>,
        Rc<Noun>,
        Rc<Noun>,
        Rc<Noun>,
        Rc<Noun>,
        Rc<Noun>,
        Rc<Noun>,
    )> for Noun
{
    fn from(
        value: (
            Rc<Noun>,
            Rc<Noun>,
            Rc<Noun>,
            Rc<Noun>,
            Rc<Noun>,
            Rc<Noun>,
            Rc<Noun>,
        ),
    ) -> Self {
        naked_cell(
            &value.0,
            &cell(
                &value.1,
                &cell(
                    &value.2,
                    &cell(&value.3, &cell(&value.4, &cell(&value.5, &value.6))),
                ),
            ),
        )
    }
}

impl From<Noun> for Option<(Rc<Noun>, Rc<Noun>)> {
    fn from(value: Noun) -> Self {
        value.as_cell_cloned()
    }
}

impl From<Noun> for Option<(Rc<Noun>, Rc<Noun>, Rc<Noun>)> {
    fn from(value: Noun) -> Self {
        let (a, b) = value.as_cell_cloned()?;
        let (b, c) = b.as_cell_cloned()?;
        Some((a, b, c))
    }
}

impl From<Noun> for Option<(Rc<Noun>, Rc<Noun>, Rc<Noun>, Rc<Noun>)> {
    fn from(value: Noun) -> Self {
        let (a, b) = value.as_cell_cloned()?;
        let (b, c) = b.as_cell_cloned()?;
        let (c, d) = c.as_cell_cloned()?;
        Some((a, b, c, d))
    }
}

impl From<Noun> for Option<(Rc<Noun>, Rc<Noun>, Rc<Noun>, Rc<Noun>, Rc<Noun>)> {
    fn from(value: Noun) -> Self {
        let (a, b) = value.as_cell_cloned()?;
        let (b, c) = b.as_cell_cloned()?;
        let (c, d) = c.as_cell_cloned()?;
        let (d, e) = d.as_cell_cloned()?;
        Some((a, b, c, d, e))
    }
}

impl From<Noun> for Option<(Rc<Noun>, Rc<Noun>, Rc<Noun>, Rc<Noun>, Rc<Noun>, Rc<Noun>)> {
    fn from(value: Noun) -> Self {
        let (a, b) = value.as_cell_cloned()?;
        let (b, c) = b.as_cell_cloned()?;
        let (c, d) = c.as_cell_cloned()?;
        let (d, e) = d.as_cell_cloned()?;
        let (e, f) = e.as_cell_cloned()?;
        Some((a, b, c, d, e, f))
    }
}

impl From<Noun>
    for Option<(
        Rc<Noun>,
        Rc<Noun>,
        Rc<Noun>,
        Rc<Noun>,
        Rc<Noun>,
        Rc<Noun>,
        Rc<Noun>,
    )>
{
    fn from(value: Noun) -> Self {
        let (a, b) = value.as_cell_cloned()?;
        let (b, c) = b.as_cell_cloned()?;
        let (c, d) = c.as_cell_cloned()?;
        let (d, e) = d.as_cell_cloned()?;
        let (e, f) = e.as_cell_cloned()?;
        let (f, g) = f.as_cell_cloned()?;
        Some((a, b, c, d, e, f, g))
    }
}

impl From<&[u8]> for Noun {
    fn from(value: &[u8]) -> Self {
        Noun::from_bytes(value)
    }
}

#[derive(Clone)]
pub struct Edge<'a> {
    pub hair: Hair,
    pub result: Option<(&'a Rc<Noun>, Nail<'a>)>,
}

impl<'a> Edge<'a> {
    pub fn as_noun(self: &Self) -> Rc<Noun> {
        let foo = match &self.result {
            None => &Rc::new(Noun::SIG),
            Some((result, nail)) => {
                let foo = cell(&result, &nail.as_noun());
                &cell(&Rc::new(Noun::SIG), &foo)
            }
        };
        cell(&self.hair.as_noun(), foo)
    }
}

#[derive(Clone)]
pub struct Hair {
    pub line: u32,
    pub column: u32,
}

impl Hair {
    pub fn as_noun(self: &Self) -> Rc<Noun> {
        cell(
            &Rc::new(Noun::from_u32(self.line)),
            &Rc::new(Noun::from_u32(self.column)),
        )
    }
}

#[derive(Clone)]
pub struct Nail<'a> {
    pub hair: Hair,
    pub rest: &'a Rc<Noun>,
}

impl<'a> Nail<'a> {
    pub fn as_noun(self: &Self) -> Rc<Noun> {
        cell(&self.hair.as_noun(), &self.rest)
    }
}

#[derive(Clone)]
pub struct Bite {
    pub bloq: u32,
    pub step: u32,
}

impl Bite {
    pub fn bits(self: &Self) -> u32 {
        2u32.pow(self.bloq) * self.step
    }
}

#[derive(Clone)]
pub struct NounListIterator<'a> {
    noun: &'a Rc<Noun>,
}

impl<'a> Iterator for NounListIterator<'a> {
    type Item = &'a Rc<Noun>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.noun.is_sig() {
            Option::None
        } else {
            let (p, q) = self.noun.as_cell().unwrap();
            self.noun = q;
            Option::Some(p)
        }
    }
}

impl fmt::Display for Noun {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write_noun(f, self, false)
    }
}

fn write_noun(f: &mut fmt::Formatter, noun: &Noun, is_rhs: bool) -> fmt::Result {
    match noun {
        Noun::Cell { p, q, .. } if is_rhs => {
            write_noun(f, p, false)?;
            f.write_char(' ')?;
            write_noun(f, q, true)
        }
        Noun::Cell { p, q, .. } => {
            f.write_char('[')?;
            write_noun(f, p, false)?;
            f.write_char(' ')?;
            write_noun(f, q, true)?;
            f.write_char(']')
        }
        Noun::Atom(a) => {
            let atom_bytes = a.to_bytes_le();
            if atom_bytes.len() > 1 && atom_bytes.into_iter().all(|c| (c > 33) && c < 126) {
                f.write_char('%')?;
                let cord = unsafe { String::from_utf8_unchecked(a.to_bytes_le()) };
                f.write_str(&*cord)
            } else if a == &BigUint::ZERO {
                f.write_char('~')
            } else {
                let mut result = String::new();
                let mut counter = 0;
                for char in a.to_string().chars().rev() {
                    if counter == 3 {
                        result.push('.');
                        counter = 0;
                    }
                    counter += 1;
                    result.push(char);
                }
                f.write_str(&*result.chars().rev().collect::<String>())
            }
        }
    }
}

pub fn cell(p: &Rc<Noun>, q: &Rc<Noun>) -> Rc<Noun> {
    Rc::new(naked_cell(p, q))
}

pub fn naked_cell(p: &Rc<Noun>, q: &Rc<Noun>) -> Noun {
    Noun::Cell {
        p: p.clone(),
        q: q.clone(),
        hash: Cell::new(None),
        mug: Cell::new(None),
    }
}

fn hash_pair(p: &Rc<Noun>, q: &Rc<Noun>) -> Hash {
    xxh3_128(&[p.hash().to_le_bytes(), q.hash().to_le_bytes()].as_flattened())
}

fn hash_tripple(p: Rc<Noun>, q: Hash) -> Hash {
    xxh3_128(&[p.hash().to_le_bytes(), q.to_le_bytes()].as_flattened())
}
