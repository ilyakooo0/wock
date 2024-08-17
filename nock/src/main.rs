use std::io::{self, Read};
use nock::{interpreter::eval_gate, serialization::cue_bytes};

fn main () {
    let mut jammed_input = Vec::new();
    match io::stdin().read_to_end(&mut jammed_input) {
        Err(err) => panic!("{err}"),
        Ok(_) => (),
    };

    let gate = cue_bytes(&jammed_input);

    let result = eval_gate(gate);

    println!("{result}");
}