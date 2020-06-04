extern crate nom;
use crate::assembler::{AssemblerInstruction, Operand};
use crate::parser_core::{register, int_value};
use crate::instruction::Opcode;

use nom::{
  alt,
  named,
  tag_no_case,
  do_parse,
  character::complete::multispace1 as ws,
};


crate::named_binary!(load_instr, Opcode::LOAD, "load", register, int_value);
crate::named_ternary!(add_instr, Opcode::ADD, "add", register, register, register);
crate::named_ternary!(sub_instr, Opcode::SUB, "sub", register, register, register);
crate::named_ternary!(mul_instr, Opcode::MUL, "mul", register, register, register);
crate::named_ternary!(div_instr, Opcode::DIV, "div", register, register, register);
crate::named_unary!(jmp_instr, Opcode::JMP, "jmp", register);
crate::named_unary!(jmpf_instr, Opcode::JMPF, "jmpf", register);
crate::named_unary!(jmpb_instr, Opcode::JMPB, "jmpb", register);
crate::named_ternary!(eq_instr, Opcode::EQ, "eq", register, register, register);
crate::named_ternary!(neq_instr, Opcode::NEQ, "neq", register, register, register);
crate::named_ternary!(gt_instr, Opcode::GT, "gt", register, register, register);
crate::named_ternary!(lt_instr, Opcode::LT, "lt", register, register, register);
crate::named_binary!(jeq_instr, Opcode::JEQ, "jeq", register, register);
crate::named_binary!(jneq_instr, Opcode::JNEQ, "jneq", register, register);


named!(pub parse<&str, AssemblerInstruction>,
  alt!(
      load_instr
    | add_instr
    | sub_instr
    | mul_instr
    | div_instr
    | jmp_instr
    | jmpf_instr
    | jmpb_instr
    | eq_instr
    | neq_instr
    | gt_instr
    | lt_instr
    | jeq_instr
    | jneq_instr
  )
);





#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parse_case_insensitive_load() {
    let expected = AssemblerInstruction{
      opcode: Opcode::LOAD,
      operand1: Some(Operand::Register(255)),
      operand2: Some(Operand::IntegerValue(100)),
      operand3: None
    };
    assert_eq!(parse("load %FF #100"), Ok(("", expected.clone())));
    assert_eq!(parse("LOAD %FF #100"), Ok(("", expected.clone())));
  }

  #[test]
  fn parse_add() {
    let expected = AssemblerInstruction{
      opcode: Opcode::ADD,
      operand1: Some(Operand::Register(1)),
      operand2: Some(Operand::Register(2)),
      operand3: Some(Operand::Register(255)),
    };
    assert_eq!(parse("add %01 %02 %FF"), Ok(("", expected)));
    assert!(parse("add %1 %02 %FF").is_err());
  }
}