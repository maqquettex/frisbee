use strum_macros::EnumIter;
use strum::IntoEnumIterator;

#[derive(Clone, Debug, PartialEq, EnumIter)]
pub enum Opcode {
  HLT = 0x0,
  LOAD = 0x1,
  ADD = 0x2,
  SUB = 0x3,
  MUL = 0x4,
  DIV = 0x5,
  JMP = 0x6,
  JMPF = 0x7,
  JMPB = 0x8,
  EQ = 0x9,
  NEQ = 0xA,
  GT = 0xB,
  LT = 0xC,
  JEQ = 0xD,
  JNEQ = 0xE,
  IGL = 0xFF,
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
        0x0 => Opcode::HLT,
        0x1 => Opcode::LOAD,
        0x2 => Opcode::ADD,
        0x3 => Opcode::SUB,
        0x4 => Opcode::MUL,
        0x5 => Opcode::DIV,
        0x6 => Opcode::JMP,
        0x7 => Opcode::JMPF,
        0x8 => Opcode::JMPB,
        0x9 => Opcode::EQ,
        0xA => Opcode::NEQ,
        0xB => Opcode::GT,
        0xC => Opcode::LT,
        0xD => Opcode::JEQ,
        0xE => Opcode::JNEQ,
        _ => Opcode::IGL,
      }
  }
}



  #[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_check_conversion() {
      for opcode in Opcode::iter() {
        let casted = Opcode::from(opcode.clone() as u8);
        assert_eq!(casted, opcode);
      }

      // Some unknown values, make sure casted to IGL
      assert_eq!(Opcode::from(244), Opcode::IGL);
      assert_eq!(Opcode::from(254), Opcode::IGL);
      assert_eq!(Opcode::from(253), Opcode::IGL);
  }
  
  #[test]
  fn test_create_hlt() {
      let instr = Instruction::new(Opcode::HLT);
      assert_eq!(instr.opcode, Opcode::HLT);
  }

  #[test]
  fn test_create_instruction() {
    let instruction = Instruction::new(Opcode::HLT);
    assert_eq!(instruction.opcode, Opcode::HLT);
  }
}
