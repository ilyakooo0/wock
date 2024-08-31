use clap::{command, error::Result, Parser, Subcommand};
use nock::interpreter::{eval_gate, eval_pulled_gate, slam_pulled_gate};
use nock::{
    cue::cue_bytes,
    interpreter::{generate_interpreter_context, slam},
    jam::jam_to_bytes,
    noun::{cell, Atom, Noun},
};
use spinoff::Spinner;
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
        NockCommand::Cycle => {
            let mut source = Vec::new();
            eprintln!("reading");
            stdin().read_to_end(&mut source).unwrap();
            eprintln!("cueing");
            let foo = cue_bytes(&source);
            // eprintln!("{foo}");
            eprintln!("jamming");
            let bat = jam_to_bytes(&foo);
            eprintln!("writing");
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

            let result = eval_pulled_gate(&generate_interpreter_context(), &gate).unwrap();

            println!("{result}");
        }
        NockCommand::Eval { nock } => {
            let nock = read_nock(&nock).unwrap();

            let ctx = generate_interpreter_context();
            let result = eval_pulled_gate(&ctx, &nock).unwrap();

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
    let Some(slam) = slam(&generate_interpreter_context(), &gate, &source) else {
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
    println!("reaming");
    let root_ast = {
        let ream_nock = include_bytes!("../res/ream.nock");
        let ream_gate = cue_bytes(ream_nock);

        let root_source = read_file(&root)?;

        slam_pulled_gate(
            &generate_interpreter_context(),
            &ream_gate,
            &Rc::new(Noun::Atom(Atom::from_bytes_le(&root_source))),
        )
        .unwrap()
    };

    println!("Loading hoon/hoon");

    let ast = {
        let hoon_hoon_ast_nock = include_bytes!("../res/hoon-hoon-ast.nock");
        let hoon_hoon_ast = cue_bytes(hoon_hoon_ast_nock);
        cell(
            &Rc::new(Noun::Atom(Atom::from_bytes_le(b"tsgl"))),
            &cell(&hoon_hoon_ast, &root_ast),
        )
    };

    println!("minting");

    let mint_gate = cue_bytes(include_bytes!("../res/mint.nock"));
    let minted_ast =
        slam_pulled_gate(&generate_interpreter_context(), &mint_gate, &root_ast).unwrap();
    let (_type, nock) = minted_ast.as_cell().unwrap();

    let mut output_file = File::create(output)?;
    File::write_all(&mut output_file, &jam_to_bytes(nock)).unwrap();
    Ok(())
}
