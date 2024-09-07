use clap::{command, error::Result, Parser, Subcommand};
use nock::interpreter::{eval_pulled_gate, slam_pulled_gate, tar, InterpreterContext};
use nock::{
    cue::cue_bytes,
    interpreter::{generate_interpreter_context, slam},
    jam::jam_to_bytes,
    noun::{cell, Atom, Noun},
};
use spinoff::Spinner;
use std::str::FromStr;
use std::{
    fs::File,
    io::{stdin, stdout, Read, Write},
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
    #[command(about = "Compiles a single Hoon file to Nock")]
    Compile {
        #[arg(value_name = "FILE.hoon")]
        root: PathBuf,
        #[arg(short, long, value_name = "FILE.nock")]
        output: PathBuf,
    },
    #[command(about = "Runs compiled Nock")]
    Run {
        #[command(subcommand)]
        command: RunCommand,
    },
    HashGate {
        #[arg(value_name = "FILE.nock")]
        gate: PathBuf,
    },
    HashDoubleGate {
        #[arg(value_name = "FILE.nock")]
        gate: PathBuf,
    },
    Eval {
        #[arg(value_name = "FILE.nock")]
        nock: PathBuf,
    },
}

#[derive(Subcommand)]
enum RunCommand {
    #[command(
        about = "Prints the result of slamming all of standard input as a cord against the supplied compiled gate. Expects the gate to also return a cord."
    )]
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
        NockCommand::Compile { root, output } => {
            compile(root, output).unwrap();
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
        NockCommand::Eval { nock } => {
            let mut ctx = generate_interpreter_context();

            let nock = read_nock_or_compile(&mut ctx, nock)?;

            let sig = ctx.nouns.sig.clone();
            let result = tar(&mut ctx, sig, &nock).unwrap();

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
    let Some(slam) = slam(&mut generate_interpreter_context(), &gate, &source) else {
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

fn compile(root: PathBuf, output: PathBuf) -> Result<(), std::io::Error> {
    let mut spinner = new_spinner(format!("Loading Hoon compiler"));

    let mut ctx = generate_interpreter_context();

    let nock = compile_to_nock(&mut ctx, &mut spinner, root)?;

    spinner.update_text(format!("Writing output to {output:#?}"));

    let mut output_file = File::create(output.clone())?;
    File::write_all(&mut output_file, &jam_to_bytes(&nock)).unwrap();

    spinner.success(&*format!("Compiled {output:#?} successfully"));

    Ok(())
}

fn compile_to_nock(
    ctx: &mut InterpreterContext,
    spinner: &mut Spinner,
    root: PathBuf,
) -> Result<Rc<Noun>, std::io::Error> {
    let hoon_nock = cue_bytes(include_bytes!("../res/hoon.nock"));
    let (hoon_type, hoon) = hoon_nock.as_cell().unwrap();
    let ride = cue_bytes(include_bytes!("../res/ride.nock"));
    let comb = cue_bytes(include_bytes!("../res/comb.nock"));

    spinner.update_text(format!("Compiling {root:#?}"));

    let root_source = read_file(&root)?;

    let target = slam_pulled_gate(
        ctx,
        &ride,
        &cell(
            hoon_type,
            &Rc::new(Noun::Atom(Atom::from_bytes_le(&root_source))),
        ),
    )
    .unwrap();

    let (_target_type, target_nock) = target.as_cell().unwrap();

    spinner.update_text(format!("Combining Nock formulas"));

    Ok(slam_pulled_gate(ctx, &comb, &cell(hoon, target_nock)).unwrap())
}

fn read_nock_or_compile(
    ctx: &mut InterpreterContext,
    path: PathBuf,
) -> Result<Rc<Noun>, std::io::Error> {
    if path.extension().and_then(|x| x.to_str()) == Some("hoon") {
        let mut spinner = new_spinner(String::new());
        let nock = compile_to_nock(ctx, &mut spinner, path.clone())?;
        spinner.success(&*format!("Compiled {path:#?} successfully"));
        Ok(nock)
    } else {
        read_nock(&path)
    }
}
