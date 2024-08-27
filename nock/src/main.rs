use clap::{command, error::Result, Parser, Subcommand};
use nock::{
    cue::cue_bytes,
    interpreter::{self, eval_gate, generate_interpreter_context, slam},
    jam::jam_to_bytes,
    noun::Noun,
};
use spinoff::Spinner;
use std::{
    borrow::Borrow,
    fs::File,
    io::{self, stdin, stdout, Read, Write},
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
        NockCommand::Build { root, output } => todo!(),
        NockCommand::Cycle => {
            let mut source = Vec::new();
            stdin().read_to_end(&mut source).unwrap();
            let foo = cue_bytes(&source);
            let bat = jam_to_bytes(foo);
            stdout().write(&bat).unwrap();
        }
    }

    Ok(())

    // let mut jammed_input = Vec::new();
    // match io::stdin().read_to_end(&mut jammed_input) {
    //     Err(err) => panic!("{err}"),
    //     Ok(_) => (),
    // };

    // let gate = cue_bytes(&jammed_input);

    // let result = eval_gate(&generate_interpreter_context(), gate);

    // println!("{result}");
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
    let mut file = File::open(nock_file)?;
    let mut contents = Vec::new();
    file.read_to_end(&mut contents)?;
    Ok(cue_bytes(&contents))
}

fn get_stdin() -> Result<Rc<Noun>, std::io::Error> {
    let mut contents = Vec::new();
    stdin().read_to_end(&mut contents)?;
    Ok(Rc::new(Noun::from_bytes(&contents)))
}

fn new_spinner(txt: String) -> Spinner {
    Spinner::new_with_stream(spinoff::spinners::Dots, txt, None, spinoff::Streams::Stderr)
}
