use std::{fs, io::Read};

use chumsky::Parser as _;
use clap::{ArgAction, Parser};

pub mod ast;
pub mod emulator;
pub mod parser;

use emulator::Emulator;

fn main() {
    let args = Args::parse();
    let mut f = fs::File::open(args.input_file).unwrap();
    let mut contents = String::new();
    f.read_to_string(&mut contents).unwrap();

    let ast = parser::parser().parse(contents);

    if let Ok(prog) = ast {
        let mut emulator = Emulator::new(&prog);
        let mut results = vec![vec![0u64; 1 << prog.headers.1]; prog.headers.3];

        if args.print_emu_state {
            emulator.run().unwrap();
            println!("{}", emulator);
            return;
        }

        for _ in 0..args.shots {
            emulator.run().unwrap();

            let cregs = emulator.get_cregs_state();
            for (creg, res) in cregs.iter().zip(results.iter_mut()) {
                let val1 = creg.get_val().0;
                res[val1 as usize] += 1
            }

            emulator.reset()
        }

        println!("creg statistics:");
        for (i, result) in (0..results.len()).zip(results.iter()) {
            println!("creg {} stat:", i);
            let sum = result.iter().sum::<u64>() as f64;
            for (j, res) in (0..result.len()).zip(result.iter()) {
                println!(
                    "[STATE {:01$b}] freq: {2};\tprob: {3:.5}",
                    j,
                    prog.headers.1 as usize,
                    res,
                    (*res as f64) / sum
                )
            }
            println!("-------------------")
        }
    }
}

#[derive(Parser)]
struct Args {
    /// The input .qASM file
    input_file: String,

    /// Number of times to run the program for statistics information
    #[clap(short = 's', long = "shots", default_value_t = 1024)]
    shots: u64,

    /// Print the statevectors of the quantum registers and terminate the program (note: measurement operations are not ignored, use before adding measurements)
    #[clap(short = 'p', long = "print-state", default_value_t = false, action = ArgAction::SetTrue)]
    print_emu_state: bool,
}
