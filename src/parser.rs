extern crate nom;
use crate::assembler::{AssemblerInstruction, Operand};
use crate::parser_core::{register, int_value};
use crate::instruction::Opcode;

use nom::{
  named,
  tag_no_case,
  do_parse,
  character::complete::multispace1 as ws,
};

crate::named_binary!(load_instr, Opcode::LOAD, "load", register, int_value);



#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parse_instruction_one() {
    let expected = AssemblerInstruction{
      opcode: Opcode::LOAD,
      operand1: Some(Operand::Register(255)),
      operand2: Some(Operand::IntegerValue(100)),
      operand3: None
    };
    assert_eq!(load_instr("load %FF #100"), Ok(("", expected.clone())));
    assert_eq!(load_instr("LOAD %FF #100"), Ok(("", expected.clone())));
  }
}