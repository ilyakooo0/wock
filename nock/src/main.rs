use clap::{command, error::Result, Parser, Subcommand};
use core::str;
use nock::interpreter::{
    ram_ttanks, slam_pulled_gate, tar, InterpreterContext, TTanks, JET_COUNTER,
};
use nock::{
    cue::cue_bytes,
    interpreter::{generate_interpreter_context, slam},
    jam::jam_to_bytes,
    noun::{cell, Atom, Noun},
};
use spinoff::Spinner;
use std::cell::LazyCell;
use std::{
    fs::File,
    io::{stdin, stdout, Read, Write},
    path::PathBuf,
    process::exit,
    rc::Rc,
};

static GENERATE_URBIT: fn() -> Rc<Noun> = || cue_bytes(include_bytes!("../res/urbit.jam"));

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

fn print_jet_counter() {
    let counter = JET_COUNTER.lock().unwrap();
    let mut builder = tabled::builder::Builder::new();
    builder.push_record(vec!["jet", "number of calls"]);
    let mut counters = counter.iter().collect::<Vec<_>>();
    counters.sort_unstable_by_key(|x| x.1);
    for (key, count) in counters {
        let key = String::from_utf8(key.to_bytes_le()).unwrap();
        builder.push_record(vec![key, count.to_string()]);
    }
    println!(
        "\n{}",
        builder
            .build()
            .with(tabled::settings::style::Style::rounded())
    );
}

fn main() -> Result<(), std::io::Error> {
    if cfg!(feature = "count-jets") {
        ctrlc::set_handler(|| {
            print_jet_counter();
            exit(1);
        })
        .unwrap();
    }

    let urbit = LazyCell::new(GENERATE_URBIT);
    let mut ctx = InterpreterContext {
        // slog: |_| {},
        ..generate_interpreter_context()
    };

    let cli = Cli::parse();

    match cli.command {
        NockCommand::Run { command } => match command {
            RunCommand::Interact { gate: gate_file } => {
                let mut spinner = new_spinner(format!("Loading {gate_file:#?}"));

                let (_typ, nok) = read_nock_or_compile(&mut spinner, &mut ctx, &urbit, &gate_file)?;

                spinner.success(&format!("Loaded {gate_file:#?}"));

                let source = get_stdin().unwrap();

                let mut spinner = new_spinner(String::from("Slamming gate..."));
                let slam = match slam(&mut ctx, &nok, &source) {
                    Ok(slam) => slam,
                    Err(tanks) => {
                        spinner.fail("Gate execution failed");

                        let mut spinner = new_spinner(String::from("Building trace"));
                        let trace = ram_ttanks(&mut ctx, tanks);
                        spinner.success("Trace built");
                        println!("{trace}");
                        exit(1);
                    }
                };
                let Some(target) = slam.as_bytes() else {
                    spinner.fail("The gate did not produce a valid atom");
                    exit(1);
                };
                spinner.success("Gate slammed successfully");

                stdout().write_all(&target)?;
            }
        },
        NockCommand::Compile { root, output } => {
            let mut spinner = new_spinner(format!("Compiling {root:#?}"));

            let nock = compile_to_nock(&mut ctx, &urbit, &mut spinner, root)?;

            spinner.update_text(format!("Writing output to {output:#?}"));

            let mut output_file = File::create(output.clone())?;
            File::write_all(&mut output_file, &jam_to_bytes(&nock)).unwrap();

            spinner.success(&*format!("Compiled {output:#?} successfully"));

            ()
        }
        NockCommand::HashGate { gate } => {
            let (_type, gate) = read_nock(&gate)?;
            let (hash, _sample) = gate.hash_gate();

            println!("{hash}");
        }
        NockCommand::HashDoubleGate { gate } => {
            let (_type, gate) = read_nock(&gate)?;
            let (hash, _sample_1, _sample_2) = gate.hash_double_gate();

            println!("{hash}");
        }
        NockCommand::Eval { nock } => {
            let mut spinner = new_spinner(format!("Loading {nock:#?}"));

            let (_typ, nok) = read_nock_or_compile(&mut spinner, &mut ctx, &urbit, &nock)?;

            let sig = ctx.nouns.sig.clone();
            let result = tar(&mut ctx, &sig, &nok).unwrap();

            println!("{result}");
        }
    }

    if cfg!(feature = "count-jets") {
        print_jet_counter()
    }

    Ok(())
}

/// Reads a `vase` from a `.nock` file.
fn read_nock(nock_file: &PathBuf) -> Result<(Rc<Noun>, Rc<Noun>), std::io::Error> {
    let nock = cue_bytes(&read_file(nock_file)?);
    let (typ, nok) = nock.as_cell().unwrap();
    Ok((typ.clone(), nok.clone()))
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

fn compile_to_nock(
    ctx: &mut InterpreterContext,
    urbit: &LazyCell<Rc<Noun>>,
    spinner: &mut Spinner,
    root: PathBuf,
) -> Result<Rc<Noun>, std::io::Error> {
    let hoon = slam_pulled_gate(
        ctx,
        &urbit,
        &cell(&Rc::new(Noun::from_bytes(b"hoon")), &ctx.nouns.sig),
    );
    let hoon = or_wash_fail(ctx, hoon, spinner, "Failed to read Hoon standard library");
    let (hoon_type, hoon) = hoon.as_cell().unwrap();

    spinner.update_text(format!("Compiling {root:#?}"));

    let root_source = read_file(&root)?;

    let target = slam_pulled_gate(
        ctx,
        &urbit,
        &cell(
            &Rc::new(Noun::from_bytes(b"ride")),
            &cell(
                hoon_type,
                &Rc::new(Noun::Atom(Atom::from_bytes_le(&root_source))),
            ),
        ),
    );
    let target = or_wash_fail(ctx, target, spinner, "Compilation failed");

    let (target_type, target_nock) = target.as_cell().unwrap();

    spinner.update_text(format!("Combining Nock formulas"));

    let combined_nock = slam_pulled_gate(
        ctx,
        &urbit,
        &cell(
            &Rc::new(Noun::from_bytes(b"comb")),
            &cell(hoon, target_nock),
        ),
    );
    let combined_nock = cell(
        target_type,
        &or_wash_fail(ctx, combined_nock, spinner, "Failed to combine Nock"),
    );
    Ok(combined_nock)
}

fn read_nock_or_compile(
    spinner: &mut Spinner,
    ctx: &mut InterpreterContext,
    urbit: &LazyCell<Rc<Noun>>,
    path: &PathBuf,
) -> Result<(Rc<Noun>, Rc<Noun>), std::io::Error> {
    if path.extension().and_then(|x| x.to_str()) == Some("hoon") {
        let nock = compile_to_nock(ctx, urbit, spinner, path.clone())?;
        let (typ, nok) = nock.as_cell().unwrap();
        Ok((typ.clone(), nok.clone()))
    } else {
        read_nock(&path)
    }
}

fn or_wash_fail(
    ctx: &mut InterpreterContext,
    source: Result<Rc<Noun>, TTanks>,
    spinner: &mut Spinner,
    fail_err: &str,
) -> Rc<Noun> {
    match source {
        Ok(source) => source,
        Err(tanks) => {
            spinner.fail(fail_err);
            let washed_tanks = ram_ttanks(ctx, tanks);
            eprintln!("{}", washed_tanks);
            exit(1)
        }
    }
}
