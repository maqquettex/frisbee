extern crate nom;
use crate::assembler::{Operand};

use nom::{
  IResult,
  named,
  tag,
  do_parse,
  bytes::complete::{tag, take_while_m_n,},
  character::complete::digit1,
  combinator::map_res
};


#[macro_export]
macro_rules! named_unary {
  ($name: ident, $opc: expr, $tag: expr, $op1_p: ident) => (
    named!(pub $name<&str, AssemblerInstruction>, 
      do_parse!(
        o: tag_no_case!($tag) >> ws >> op1: $op1_p >> (
          AssemblerInstruction{opcode: $opc, operand1: Some(op1), operand2: None, operand3: None}
        )
      )
    );
  );
}

#[macro_export]
macro_rules! named_binary {
  ($name: ident, $opc: expr, $tag: expr, $op1_p: ident, $op2_p: ident) => (
    named!(pub $name<&str, AssemblerInstruction>, 
      do_parse!(
        o: tag_no_case!($tag) >> ws >> op1: $op1_p >> ws >> op2: $op2_p >> (
          AssemblerInstruction{opcode: $opc, operand1: Some(op1), operand2: Some(op2), operand3: None}
        )
      )
    );
  );
}

macro_rules! named_ternary {
  ($name: ident, $opc: expr, $tag: expr, $op1_p: ident, $op2_p: ident, $op3_p: ident) => (
    named!(pub $name<&str, AssemblerInstruction>, 
      do_parse!(
        o: tag_no_case!($tag) >> ws >> op1: $op1_p >> ws >> op2: $op2_p >> ws >> op3: $op3_p >>(
          AssemblerInstruction{opcode: $opc, operand1: Some(op1), operand2: Some(op2), operand3: Some(op3)}
        )
      )
    );
  );
}



named!(pub int_value<&str, Operand>,
    do_parse!(
        tag!("#") >>
        reg_num: digit1 >>
        (Operand::IntegerValue(reg_num.parse::<i32>().unwrap()))
    )
);

fn hex_primary(input: &str) -> IResult<&str, u8> {
  return map_res(
    take_while_m_n(2, 2, |c: char| c.is_digit(16)),
    |i| u8::from_str_radix(i, 16)
  )(input)
}


pub fn register(input: &str) -> IResult<&str, Operand> {
  let (input, _) = tag("%")(input)?;
  let (input, register_num) = hex_primary(input)?;

  return Ok((input, Operand::Register(register_num) ));
}


#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parse_color() {
    assert_eq!(hex_primary("FF"), Ok(("", 255)));
    assert_eq!(hex_primary("0A"), Ok(("", 10)));
    assert_eq!(hex_primary("12"), Ok(("", 18)));
  }

  #[test]
  fn parse_register() {
    assert_eq!(register("%ff"), Ok(("", Operand::Register(255))));
    assert_eq!(register("%FF"), Ok(("", Operand::Register(255))));
    assert_eq!(register("%0A"), Ok(("", Operand::Register(10))));
    assert_eq!(register("%12"), Ok(("", Operand::Register(18))));
  }

  #[test]
  fn parse_register_bad() {
    assert!(register("%XX").is_err());
    assert!(register("FF").is_err());
    assert!(register("@FF").is_err());
  }

  #[test]
  fn parse_int_value() {
    assert_eq!(int_value("#1000"), Ok(("", Operand::IntegerValue(1000))));
    assert_eq!(int_value("#321"), Ok(("", Operand::IntegerValue(321))));
    assert_eq!(int_value("#001"), Ok(("", Operand::IntegerValue(1))));
    
    assert!(int_value("#FF").is_err());
    assert!(int_value("100").is_err());
  }

}