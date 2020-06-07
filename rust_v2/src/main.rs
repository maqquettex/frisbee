pub mod vm;
pub mod instruction;
pub mod assembler;
pub mod parser_core;
pub mod parser;

use std::convert::TryFrom;

use crate::parser::parse;
use crate::assembler::{AssemblerInstruction, Operand};
use crate::vm::VM;

use std::env;
use std::fs;

fn operand_to_vec(op: &Operand, v: &mut Vec<u8>) {
    match op {
        Operand::Register(x) => v.push(x.clone()),
        Operand::IntegerValue(i) => {
            let converted = i16::try_from(i.clone()).expect(&format!("Out-of-range integer: {}", i));
            let [b1, b2] = converted.to_be_bytes();
            v.push(b1);
            v.push(b2);
        }
    };
}

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

    let mut prog_bytes: Vec<u8> = vec![];
    for asmi in program {
        prog_bytes.push(asmi.opcode as u8);
        asmi.operand1.map(|o| operand_to_vec(&o, &mut prog_bytes));
        asmi.operand2.map(|o| operand_to_vec(&o, &mut prog_bytes));
        asmi.operand3.map(|o| operand_to_vec(&o, &mut prog_bytes));
        if prog_bytes.len() % 4 != 0 {
            for _ in 0..(4 - prog_bytes.len() % 4) {
                prog_bytes.push(0);
            }
        }
        
    }
    println!("--- PRG ---");
    for (i, _) in prog_bytes.iter().enumerate().step_by(4) {
        println!("{:02X} {:02X} {:02X} {:02X}", prog_bytes[i], prog_bytes[i+1], prog_bytes[i+2], prog_bytes[i+3]);
    }
    println!("-----------");

    let mut test_vm = VM::new();
    test_vm.program = prog_bytes;
    test_vm.run();
}
