use nock::{
    interpreter::{eval_gate, generate_interpreter_context},
    serialization::cue_bytes,
};
use std::io::{self, Read};

fn main() {
    let mut jammed_input = Vec::new();
    match io::stdin().read_to_end(&mut jammed_input) {
        Err(err) => panic!("{err}"),
        Ok(_) => (),
    };

    let gate = cue_bytes(&jammed_input);

    let result = eval_gate(&generate_interpreter_context(), gate).unwrap();

    println!("{result}");
}
