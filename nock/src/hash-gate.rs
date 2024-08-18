use nock::{noun::*, serialization::cue_bytes};
use std::io::{self, Read};

fn main() {
    let mut jammed_input = Vec::new();
    match io::stdin().read_to_end(&mut jammed_input) {
        Err(err) => panic!("{err}"),
        Ok(_) => (),
    };

    let gate = cue_bytes(&jammed_input);

    let Noun::Cell {
        p: battery,
        q: payload,
        ..
    } = (*gate).clone()
    else {
        panic!()
    };

    let Noun::Cell {
        p: _sample,
        q: context,
        ..
    } = (*payload).clone()
    else {
        panic!()
    };

    let hash = cell(battery, context).hash();

    println!("{hash}");
}
