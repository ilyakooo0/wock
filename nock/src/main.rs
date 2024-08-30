use bitvec::ptr::read_unaligned;
use clap::{command, error::Result, Parser, Subcommand};
use nock::{
    cue::cue_bytes,
    interpreter::{self, eval_gate, generate_interpreter_context, slam},
    jam::jam_to_bytes,
    noun::{Atom, Noun},
};
use spinoff::Spinner;
use std::{
    borrow::Borrow,
    fs::File,
    io::{self, stdin, stdout, Read, Write},
    os::unix::fs::FileExt,
    panic::catch_unwind,
    path::PathBuf,
    process::exit,
    rc::Rc,
};

#[derive(Parser)]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: NockCommand,
}

#[derive(Subcommand)]
enum NockCommand {
    Build {
        #[arg(value_name = "FILE.hoon")]
        root: PathBuf,
        #[arg(short, long, value_name = "FILE.nock")]
        output: PathBuf,
    },
    Run {
        #[command(subcommand)]
        command: RunCommand,
    },
    Cycle,
    HashGate {
        #[arg(value_name = "FILE.nock")]
        gate: PathBuf,
    },
    HashDoubleGate {
        #[arg(value_name = "FILE.nock")]
        gate: PathBuf,
    },
    EvalGate {
        #[arg(value_name = "FILE.nock")]
        gate: PathBuf,
    },
}

#[derive(Subcommand)]
enum RunCommand {
    Interact {
        #[arg(value_name = "FILE.nock")]
        gate: PathBuf,
    },
}

fn main() -> Result<(), std::io::Error> {
    let cli = Cli::parse();

    match cli.command {
        NockCommand::Run { command } => match command {
            RunCommand::Interact { gate } => interact(gate)?,
        },
        NockCommand::Build { root, output } => {
            build(root, output).unwrap();
        }
        NockCommand::Cycle => {
            let mut source = Vec::new();
            stdin().read_to_end(&mut source).unwrap();
            let foo = cue_bytes(&source);
            let bat = jam_to_bytes(foo);
            stdout().write(&bat).unwrap();
        }
        NockCommand::HashGate { gate } => {
            let gate = read_nock(&gate)?;
            let (hash, _sample) = gate.hash_gate();

            println!("{hash}");
        }
        NockCommand::HashDoubleGate { gate } => {
            let gate = read_nock(&gate)?;
            let (hash, _sample_1, _sample_2) = gate.hash_double_gate();

            println!("{hash}");
        }
        NockCommand::EvalGate { gate } => {
            let gate = read_nock(&gate).unwrap();

            let result = eval_gate(&generate_interpreter_context(), gate).unwrap();

            println!("{result}");
        }
    }

    Ok(())
}

fn interact(gate_file: PathBuf) -> Result<(), std::io::Error> {
    let mut spinner = new_spinner(format!("Loading {gate_file:#?}"));
    let Ok(gate) = read_nock(&gate_file) else {
        spinner.fail(&format!("Failed to load {gate_file:#?}"));
        exit(1);
    };
    spinner.success(&format!("Loaded {gate_file:#?}"));

    let source = get_stdin().unwrap();

    let mut spinner = new_spinner(String::from("Slamming gate..."));
    let Some(slam) = slam(&generate_interpreter_context(), gate, source) else {
        spinner.fail("Gate execution failed");
        exit(1);
    };
    let Some(target) = slam.as_bytes() else {
        spinner.fail("The gate did not produce a valid atom");
        exit(1);
    };
    spinner.success("Gate slammed successfully");

    stdout().write_all(&target)?;

    Ok(())
}

fn read_nock(nock_file: &PathBuf) -> Result<Rc<Noun>, std::io::Error> {
    Ok(cue_bytes(&read_file(nock_file)?))
}

fn read_file(nock_file: &PathBuf) -> Result<Vec<u8>, std::io::Error> {
    let mut file = File::open(nock_file)?;
    let mut contents = Vec::new();
    file.read_to_end(&mut contents)?;
    Ok(contents)
}

fn get_stdin() -> Result<Rc<Noun>, std::io::Error> {
    let mut contents = Vec::new();
    stdin().read_to_end(&mut contents)?;
    Ok(Rc::new(Noun::from_bytes(&contents)))
}

fn new_spinner(txt: String) -> Spinner {
    Spinner::new_with_stream(spinoff::spinners::Dots, txt, None, spinoff::Streams::Stderr)
}

fn build(root: PathBuf, output: PathBuf) -> Result<(), std::io::Error> {
    let make_nock = include_bytes!("../res/make.nock");
    let make = cue_bytes(make_nock);

    let root = read_file(&root)?;

    let hoon_hoon = include_bytes!("../res/hoon.hoon");

    let mut source = Vec::new();

    source.extend_from_slice(b"=>\n");
    source.extend_from_slice(hoon_hoon);
    source.extend_from_slice(&root);

    let slam_result = slam(
        &generate_interpreter_context(),
        make,
        Rc::new(Noun::Atom(Atom::from_bytes_le(&source))),
    )
    .unwrap();

    let mut output_file = File::create(output)?;
    File::write_all(&mut output_file, &jam_to_bytes(slam_result)).unwrap();
    Ok(())
}
// [%7 <hoon/hoon> <make-result>]
