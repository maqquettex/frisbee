pub mod vm;
pub mod instruction;
pub mod assembler;
pub mod parser_core;
pub mod parser;

use crate::parser::load_instr;

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

    let lines = contents.split("\n").filter(|x| x != &"\n" && x.len() != 0);


    lines.for_each(|line| {
        let res = crate::load_instr(line).unwrap().1;
        println!("{:?}", res);
    });
}