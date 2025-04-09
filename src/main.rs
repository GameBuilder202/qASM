use std::{
    fs,
    io::{Read, Write},
};

use clap::{ArgAction, Parser};

use codespan_reporting::{
    files::SimpleFiles,
    term::{
        self, Config,
        termcolor::{ColorChoice, StandardStream},
    },
};

use q_asm::ast;
use q_asm::emulator::Emulator;

fn main() {
    let args = Args::parse();
    let mut f = fs::File::open(&args.input_file).unwrap();
    let mut contents = String::new();
    f.read_to_string(&mut contents).unwrap();

    let mut files = SimpleFiles::new();
    files.add(&args.input_file, &contents);
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = Config::default();

    let prog = ast::parse(&contents, 0, args.classical);
    if let Err(diags) = prog {
        for diag in &diags {
            term::emit(&mut writer.lock(), &config, &files, diag).unwrap()
        }
        std::process::exit(1)
    }

    let prog = prog.unwrap();
    if let Err(diag) = prog {
        term::emit(&mut writer.lock(), &config, &files, &diag).unwrap();
        std::process::exit(2)
    }

    let prog = prog.unwrap();
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
        println!("Program output:");
        std::io::stdout().write_all(&emulator.stdout).unwrap();

        println!("Registers data:");
        print!("Reg:");
        for i in 0..prog.headers.3 {
            print!("\tcr{}", i)
        }
        println!();
        print!("Data:");
        let data = emulator.get_cregs_state();
        let digits = prog.headers.1 / 4;
        for reg in data {
            print!("\t0x{:01$x}", reg.get_val().0, digits)
        }
        println!();

        let mut mem_file = fs::File::options()
            .write(true)
            .create(true)
            .truncate(true)
            .open("mem.bin")
            .unwrap();

        let mem = emulator.get_mem_state();
        let digits = prog.headers.1.div_ceil(8) * 2;
        for chunk in mem.chunks(16) {
            for data in chunk {
                write!(mem_file, "{:01$x} ", data.get_val().0, digits).unwrap()
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
            if res == &0 {
                continue;
            }

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

    /// Run in "classical mode", which runs the program once, then prints classical register data to stdout and dumps memory to a file
    #[clap(short = 'c', long = "classical", default_value_t = false, action = ArgAction::SetTrue)]
    classical: bool,
}
