#[derive(Debug, PartialEq)]
pub enum Opcode {
  HLT = 0,
  LOAD = 1,
  ADD = 2,
  SUB = 3,
  MUL = 4,
  DIV = 5,
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
