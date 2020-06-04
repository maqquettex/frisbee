pub mod vm;
pub mod instruction;
pub mod assembler;
pub mod parser_core;
pub mod parser;

use crate::parser::parse;
use crate::assembler::AssemblerInstruction;

use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("\x1b[0;31mPlease enter path to file!\x1b[0m");
        return;
    }

    let filename: &String = args.get(1).unwrap();
    let contents = fs::read_to_string(filename)
        .expect("Something went wrong reading the file {}");

    let lines = contents.split("\n");

    let mut program: Vec<AssemblerInstruction> = vec![];

    for (i, line) in lines.enumerate() {
        if line == "\n" || line.len() == 0 {
            continue;
        }

        match parse(line) {
            Ok((_, asmi)) => program.push(asmi),
            Err(_) => println!("Error on line {} ", i+1) // enumerate starts from 0
        }
    }

}