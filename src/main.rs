pub mod vm;
pub mod instruction;
pub mod parser;

use std::env;
use std::fs;
use parser::load_instuction;

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
        let res = load_instuction(line).unwrap().1;
        println!("{:?}", res);
    });
}