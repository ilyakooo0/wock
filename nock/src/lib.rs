#![feature(iter_intersperse)]
#![feature(let_chains)]

use std::rc::Rc;

use noun::Noun;
pub mod cue;
pub mod interpreter;
pub mod jam;
mod jets;
pub mod noun;

pub type Vase = (Rc<Noun>, Rc<Noun>);
