#![feature(iter_intersperse)]
#![feature(let_chains)]
#![feature(iter_repeat_n)]
pub mod cue;
pub mod interpreter;
pub mod jam;
mod jets;
pub mod noun;
mod utils;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn greet() {}
