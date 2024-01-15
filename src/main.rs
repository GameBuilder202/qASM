use std::{
    fs,
    io::{Read, Write},
};

use clap::{ArgAction, Parser};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};

pub mod ast;
pub mod emulator;

use ast::*;
use emulator::Emulator;

fn main() {
    let args = Args::parse();
    let mut f = fs::File::open(&args.input_file).unwrap();
    let mut contents = String::new();
    f.read_to_string(&mut contents).unwrap();

    let mut files = SimpleFiles::new();
    files.add(&args.input_file, &contents);
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = Config::default();

    let (prog, diags) = ast::parse(&contents, 0);

    for diag in &diags {
        term::emit(&mut writer.lock(), &config, &files, diag).unwrap()
    }

    if !diags.is_empty() {
        std::process::exit(1)
    }

    if let Ok(prog) = prog {
        let mut emulator = Emulator::new(&prog);
        let mut results = vec![vec![0u64; 1 << prog.headers.1]; prog.headers.3];

        if args.print_emu_state {
            let res = emulator.run();
            if let Err(diag) = res {
                term::emit(&mut writer.lock(), &config, &files, &diag).unwrap();
                std::process::exit(3)
            }

            println!("{}", emulator);
            return;
        }

        if args.classical {
            let res = emulator.run();
            if let Err(diag) = res {
                term::emit(&mut writer.lock(), &config, &files, &diag).unwrap();
                std::process::exit(3)
            }

            println!("Registers data:");
            print!("Reg:");
            for i in 0..prog.headers.3 {
                print!("\tcr{}", i)
            }
            println!();
            print!("Data:");
            let data = emulator.get_cregs_state();
            for reg in data {
                print!("\t{:01$b}", reg.get_val().0, prog.headers.1)
            }
            println!();

            let mut mem_file = fs::File::options()
                .write(true)
                .create(true)
                .truncate(true)
                .open("mem.bin")
                .unwrap();

            let mem = emulator.get_mem_state();
            for chunk in mem.chunks(16) {
                for data in chunk {
                    write!(mem_file, "{:01$b} ", data.get_val().0, prog.headers.1).unwrap()
                }
                writeln!(mem_file).unwrap()
            }

            return;
        }

        for _ in 0..args.shots {
            let res = emulator.run();
            if let Err(diag) = res {
                term::emit(&mut writer.lock(), &config, &files, &diag).unwrap();
                std::process::exit(3)
            }

            let cregs = emulator.get_cregs_state();
            for (creg, res) in cregs.iter().zip(results.iter_mut()) {
                let val1 = creg.get_val().0;
                res[val1 as usize] += 1
            }

            emulator.reset()
        }

        println!("creg statistics:");
        for (i, result) in results.iter().enumerate() {
            println!("creg {} stat:", i);
            let sum = result.iter().sum::<u64>() as f64;
            for (j, res) in result.iter().enumerate() {
                println!(
                    "[STATE {:01$b}] freq: {2};\tprob: {3:.5}",
                    j,
                    prog.headers.1,
                    res,
                    (*res as f64) / sum
                )
            }
            println!("-------------------")
        }
    } else if let Err(e) = prog {
        let diag = Diagnostic::error();
        match e {
            ResolveError::UndefinedLabel(name, s) => term::emit(
                &mut writer.lock(),
                &config,
                &files,
                &diag
                    .with_message(format!("Reference to undefined label \"{}\"", name))
                    .with_labels(vec![Label::primary(s.file, s.span)]),
            )
            .unwrap(),

            ResolveError::ParseError(diag) => {
                term::emit(&mut writer.lock(), &config, &files, &diag).unwrap()
            }
        }

        std::process::exit(2)
    }
}

#[derive(Parser)]
struct Args {
    /// The input .qASM file
    input_file: String,

    /// Number of times to run the program for statistics information
    #[clap(short = 's', long = "shots", default_value_t = 1024)]
    shots: u64,

    /// Print the statevectors of the quantum registers and terminate the program
    #[clap(short = 'p', long = "print-state", default_value_t = false, action = ArgAction::SetTrue)]
    print_emu_state: bool,

    #[clap(short = 'c', long = "classical", default_value_t = false, action = ArgAction::SetTrue)]
    classical: bool,
}

fn _escaped(c: char) -> String {
    match c {
        '\n' => String::from("\\n"),
        '\r' => String::from("\\r"),
        _ => c.to_string(),
    }
}
