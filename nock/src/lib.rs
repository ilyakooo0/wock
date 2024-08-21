#![feature(iter_intersperse)]
pub mod interpreter;
mod jets;
pub mod noun;
pub mod serialization;
mod utils;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn greet() {}
