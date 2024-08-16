use ares::hamt::Hamt;
use ares::interpreter::{interpret, Context};
use ares::jets::cold::Cold;
use ares::jets::hot::{Hot, URBIT_HOT_STATE};
use ares::jets::warm::Warm;
use ares::mem::NockStack;
use ares::newt::Newt;
use ares::noun::*;
use ares::serialization::cue;
use core::panic;
use std::io::{self, Read};

fn generate_context() -> Context {
    let mut stack = NockStack::new(2048 << 15, 0);
    let newt = Newt::new();
    let cache = Hamt::<Noun>::new(&mut stack);

    let mut cold = Cold::new(&mut stack);

    let hot = Hot::init(&mut stack, URBIT_HOT_STATE);
    let warm = Warm::init(&mut stack, &mut cold, &hot);

    return Context {
        stack,
        newt,
        cold,
        warm,
        hot,
        cache,
        scry_stack: D(0),
        trace_info: None,
    };
}

fn main() {
    let mut ctx = generate_context();

    let mut jammed_input = Vec::new();

    match io::stdin().read_to_end(&mut jammed_input) {
        Err(err) => panic!("{err}"),
        Ok(_) => (),
    };

    let jammed_atom = unsafe {
        IndirectAtom::new_raw_bytes_ref(&mut ctx.stack, jammed_input.as_slice()).as_atom()
    };

    let noun = cue(&mut ctx.stack, jammed_atom);

    let call_gate = Cell::new_tuple(&mut ctx.stack, &[D(9), D(2), D(0), D(1)]).as_noun();

    let res = interpret(&mut ctx, noun, call_gate).expect("bad exec");

    println!("{res}");
}
