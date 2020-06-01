#[derive(Debug, PartialEq)]
pub enum Opcode {
  HLT = 0,
  LOAD = 1,
  ADD = 2,
  SUB = 3,
  MUL = 4,
  DIV = 5,
  JMP = 6,
  JMPF = 7,
  JMPB = 8,
  EQ = 9,
  NEQ = 10,
  GT = 11,
  LT = 12,
  JEQ = 13,
  JNEQ = 14,
  IGL = 255,
}

#[derive(Debug, PartialEq)]
pub struct Instruction {
  opcode: Opcode
}


impl Instruction {
  pub fn new(opcode: Opcode) -> Instruction {
    Instruction {
      opcode: opcode
    }
  }
}
impl From<Opcode> for u8 {
  fn from(v: Opcode) -> Self {
    return v as u8;
    
}
}
impl From<u8> for Opcode {
  fn from(v: u8) -> Self {
      match v {
        0 => Opcode::HLT,
        1 => Opcode::LOAD,
        2 => Opcode::ADD,
        3 => Opcode::SUB,
        4 => Opcode::MUL,
        5 => Opcode::DIV,
        6 => Opcode::JMP,
        7 => Opcode::JMPF,
        8 => Opcode::JMPB,
        9 => Opcode::EQ,
        10 => Opcode::NEQ,
        11 => Opcode::GT,
        12 => Opcode::LT,
        13 => Opcode::JEQ,
        14 => Opcode::JNEQ,
        _ => Opcode::IGL,
      }
  }
}



  #[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_create_hlt() {
      let opcode = Opcode::HLT;
      assert_eq!(opcode, Opcode::HLT);
  }

  #[test]
  fn test_create_instruction() {
    let instruction = Instruction::new(Opcode::HLT);
    assert_eq!(instruction.opcode, Opcode::HLT);
  }
}
