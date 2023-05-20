use std::{fs, io::Read};

use chumsky::Parser as _;
use clap::Parser;

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
    println!("{:#?}", ast);
    println!("----------------------");

    if let Ok(prog) = ast {
        let mut emulator = Emulator::new(&prog);
        emulator.run().unwrap();
        println!("{}", emulator)
    }
}

#[derive(Parser)]
struct Args {
    input_file: String,
}
