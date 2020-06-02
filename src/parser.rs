extern crate nom;
use crate::instruction::Opcode;

use nom::{
  IResult,
  named,
  tag,
  tag_no_case,
  do_parse,
  bytes::complete::{tag, take_while_m_n,},
  character::complete::{digit1, multispace1},
  combinator::map_res
};

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Register(u8),
    IntegerValue(i32),
}

#[derive(Debug, PartialEq, Clone)]
pub struct AssemblerInstruction {
    opcode: Opcode,
    operand1: Option<Operand>,
    operand2: Option<Operand>,
    operand3: Option<Operand>,
}

named!(pub load_instuction<&str, AssemblerInstruction>,
  do_parse!(
      o: tag_no_case!("load") >> r: register_w >> i: int_value_w >> (
        AssemblerInstruction{opcode: Opcode::LOAD, operand1: Some(r), operand2: Some(i), operand3: None}
      )
  )
);





named!(int_value<&str, Operand>,
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


fn register(input: &str) -> IResult<&str, Operand> {
  let (input, _) = tag("%")(input)?;
  let (input, register_num) = hex_primary(input)?;

  return Ok((input, Operand::Register(register_num) ));
}
named!(register_w<&str, Operand>, do_parse!(multispace1 >> r: register >> (r)));
named!(int_value_w<&str, Operand>, do_parse!(multispace1 >> i: int_value >> (i)));



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

  #[test]
  fn parse_instruction_one() {
    let expected = AssemblerInstruction{
      opcode: Opcode::LOAD,
      operand1: Some(Operand::Register(255)),
      operand2: Some(Operand::IntegerValue(100)),
      operand3: None
    };
    assert_eq!(
      load_instuction("load %FF #100"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      load_instuction("LOAD %FF #100"),
      Ok(("", expected.clone()))
    );
  }
}