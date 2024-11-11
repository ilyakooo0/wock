#![feature(iter_intersperse)]
#![feature(let_chains)]

use core::str;
use std::{borrow::Borrow, rc::Rc};

use interpreter::{eval_pulled_gate, slam_pulled_gate, tar, InterpreterContext, TTank, TTanks};
use noun::{cell, Noun};
pub mod cue;
pub mod interpreter;
pub mod jam;
mod jets;
pub mod noun;

pub type Vase = (Rc<Noun>, Rc<Noun>);
