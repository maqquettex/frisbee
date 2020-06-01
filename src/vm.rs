use crate::instruction::Opcode;

pub struct VM {
  registers: [i32; 32],
  pc: usize,
  program: Vec<u8>,
  remainder: i32,
}

impl VM {
  pub fn new() -> VM {
      VM {
          registers: [0; 32],
          pc: 0,
          program: vec![],
          remainder: 0,
      }
  }

  fn next_opcode(&mut self) -> Opcode {
    let opcode = Opcode::from(self.program[self.pc]);
    self.pc += 1;
    return opcode;
  }

  fn next_8_bits(&mut self) -> u8 {
    let result = self.program[self.pc];
    self.pc += 1;
    return result;
  }

  fn next_16_bits(&mut self) -> u16 {
    let result = ((self.program[self.pc] as u16) << 8) | self.program[self.pc + 1] as u16;
    self.pc += 2;
    return result;
  }

  pub fn run(&mut self) {
    let mut is_done = false;
    while !is_done {
        is_done = self.execute_instruction();
    }
  }

  /// Executes one instruction. Meant to allow for more controlled execution of the VM
  pub fn run_once(&mut self) {
    self.execute_instruction();
  }


  fn execute_instruction(&mut self) -> bool {
    if self.pc >= self.program.len() {
        return true;
    }
    match self.next_opcode() {
        Opcode::LOAD => {
            let register = self.next_8_bits() as usize;
            let number = self.next_16_bits() as u32;
            self.registers[register] = number as i32;
        },
        Opcode::HLT => {
            println!("HLT encountered");
        },
        Opcode::ADD => {
          let register1 = self.registers[self.next_8_bits() as usize];
          let register2 = self.registers[self.next_8_bits() as usize];
          self.registers[self.next_8_bits() as usize] = register1 + register2;
        },
        Opcode::SUB => {
          let register1 = self.registers[self.next_8_bits() as usize];
          let register2 = self.registers[self.next_8_bits() as usize];
          self.registers[self.next_8_bits() as usize] = register1 - register2;
        },
        Opcode::MUL => {
          let register1 = self.registers[self.next_8_bits() as usize];
          let register2 = self.registers[self.next_8_bits() as usize];
          self.registers[self.next_8_bits() as usize] = register1 * register2;
        },
        Opcode::DIV => {
          let register1 = self.registers[self.next_8_bits() as usize];
          let register2 = self.registers[self.next_8_bits() as usize];
          self.registers[self.next_8_bits() as usize] = register1 / register2;
          self.remainder = (register1 % register2) as i32;
        },
        Opcode::JMP => {
          let target = self.registers[self.next_8_bits() as usize];
          self.pc = target as usize;
        },
        Opcode::JMPF => {
          let value = self.registers[self.next_8_bits() as usize];
          self.pc += value as usize;
        }
        Opcode::JMPB => {
          let value = self.registers[self.next_8_bits() as usize];
          self.pc -= value as usize;
        },
        Opcode::EQ => {
          let register1 = self.registers[self.next_8_bits() as usize];
          let register2 = self.registers[self.next_8_bits() as usize];
          let register3_i = self.next_8_bits() as usize;
          if register1 == register2 {
              self.registers[register3_i] = 1;
          } else {
              self.registers[register3_i] = 0 ;
          }
        },
        Opcode::NEQ => {
          let register1 = self.registers[self.next_8_bits() as usize];
          let register2 = self.registers[self.next_8_bits() as usize];
          let register3_i = self.next_8_bits() as usize;
          if register1 != register2 {
              self.registers[register3_i] = 1;
          } else {
              self.registers[register3_i] = 0 ;
          }
        },
        Opcode::GT => {
          let register1 = self.registers[self.next_8_bits() as usize];
          let register2 = self.registers[self.next_8_bits() as usize];
          let register3_i = self.next_8_bits() as usize;
          if register1 > register2 {
              self.registers[register3_i] = 1;
          } else {
              self.registers[register3_i] = 0 ;
          }
        },
        Opcode::LT => {
          let register1 = self.registers[self.next_8_bits() as usize];
          let register2 = self.registers[self.next_8_bits() as usize];
          let register3_i = self.next_8_bits() as usize;
          if register1 < register2 {
              self.registers[register3_i] = 1;
          } else {
              self.registers[register3_i] = 0 ;
          }
        },
        Opcode::JEQ => {
          let cond = self.registers[self.next_8_bits() as usize];
          let target = self.registers[self.next_8_bits() as usize];
          if cond == 1 {
              self.pc = target as usize;
          }
        },
        Opcode::JNEQ => {
          let cond = self.registers[self.next_8_bits() as usize];
          let target = self.registers[self.next_8_bits() as usize];
          if cond == 0 {
              self.pc = target as usize;
          }
        },
        _ => {
            println!("Unknown opcode");
            return true;
        }
    }
    return false;
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_create_vm() {
      let test_vm = VM::new();
      assert_eq!(test_vm.registers[0], 0);
      assert_eq!(test_vm.registers[31], 0);
      assert_eq!(test_vm.pc, 0);
      assert_eq!(test_vm.program, vec![]);
  }
  
  #[test]
  fn test_opcode_hlt() {
    let mut test_vm = VM::new();
    test_vm.program = vec![Opcode::HLT as u8,0,0,0];
    test_vm.run_once();
    assert_eq!(test_vm.pc, 1);
  }

  #[test]
  fn test_opcode_igl() {
    let mut test_vm = VM::new();
    let test_bytes = vec![200,0,0,0];
    test_vm.program = test_bytes;
    test_vm.run_once();
    assert_eq!(test_vm.pc, 1);
  }

  #[test]
  fn test_load_opcode() {
    let mut test_vm = VM::new();
    test_vm.program = vec![Opcode::LOAD as u8, 0, 1, 244]; // Remember, this is how we represent 500 using two u8s in little endian format
    test_vm.run_once();
    assert_eq!(test_vm.registers[0], 500);
  }

  #[test]
  fn test_add_opcode() {
    let mut test_vm = VM::new();
    test_vm.registers[0] = 2;
    test_vm.registers[1] = 3;
    test_vm.program = vec![Opcode::ADD as u8, 0, 1, 3]; // Remember, this is how we represent 500 using two u8s in little endian format
    test_vm.run_once();
    assert_eq!(test_vm.registers[3], 5);
  }

  #[test]
  fn test_sub_opcode() {
    let mut test_vm = VM::new();
    test_vm.registers[0] = 2;
    test_vm.registers[1] = 3;
    test_vm.program = vec![Opcode::SUB as u8, 0, 1, 2]; // Remember, this is how we represent 500 using two u8s in little endian format
    test_vm.run_once();
    assert_eq!(test_vm.registers[2], -1);
  }

  #[test]
  fn test_mul_opcode() {
    let mut test_vm = VM::new();
    test_vm.registers[0] = 2;
    test_vm.registers[1] = 3;
    test_vm.program = vec![Opcode::MUL as u8, 0, 1, 2]; // Remember, this is how we represent 500 using two u8s in little endian format
    test_vm.run_once();
    assert_eq!(test_vm.registers[2], 6);
  }

  #[test]
  fn test_div_opcode() {
    let mut test_vm = VM::new();
    test_vm.registers[0] = 7;
    test_vm.registers[1] = 3;
    test_vm.program = vec![Opcode::DIV as u8, 0, 1, 2]; // Remember, this is how we represent 500 using two u8s in little endian format
    test_vm.run_once();
    assert_eq!(test_vm.registers[2], 2);
    assert_eq!(test_vm.remainder, 1);
  }

  #[test]
  fn test_jmp_opcode() {
    let mut test_vm = VM::new();
      test_vm.registers[0] = 1;
      test_vm.program = vec![Opcode::JMP as u8, 0, 0, 0];
      test_vm.run_once();
      assert_eq!(test_vm.pc, 1);
  }

  #[test]
  fn test_jmpf_opcode() {
      let mut test_vm = VM::new();
      test_vm.registers[0] = 2;
      test_vm.program = vec![Opcode::JMPF as u8, 0, 0, 0, 0];
      test_vm.run_once();
      assert_eq!(test_vm.pc, 4);
  }

  #[test]
  fn test_jmpb_opcode() {
      let mut test_vm = VM::new();
      test_vm.registers[1] = 2;
      test_vm.program = vec![0, 0, Opcode::JMPB as u8, 1, 0, 0,];
      test_vm.run_once();
      assert_eq!(test_vm.pc, 1);
  }

  #[test]
  fn test_eq_opcode() {
      let mut test_vm = VM::new();
      test_vm.registers[1] = 2;
      test_vm.registers[2] = 2;
      test_vm.program = vec![Opcode::EQ as u8, 1, 2, 3, Opcode::EQ as u8, 2, 3, 4];
      test_vm.run_once();
      assert_eq!(test_vm.registers[3], 1);
      assert_eq!(test_vm.registers[4], 0);
      test_vm.run_once();
      assert_eq!(test_vm.registers[4], 0);
  }

  #[test]
  fn test_jeq_opcode() {
    let mut test_vm = VM::new();
    test_vm.registers[1] = 1;
    test_vm.registers[2] = 4;
    test_vm.program = vec![Opcode::JEQ as u8, 1, 2, 0, Opcode::JEQ as u8, 0, 0];
    test_vm.run_once();
    assert_eq!(test_vm.pc, 4);
    test_vm.run_once();
    assert_eq!(test_vm.pc, 7);
  }

  #[test]
  fn test_jneq_opcode() {
    let mut test_vm = VM::new();
    test_vm.registers[0] = 1;
    test_vm.registers[1] = 0;
    test_vm.registers[2] = 4;
    test_vm.program = vec![Opcode::JNEQ as u8, 1, 2, 0, Opcode::JNEQ as u8, 0, 0];
    test_vm.run_once();
    assert_eq!(test_vm.pc, 4);
    test_vm.run_once();
    assert_eq!(test_vm.pc, 7);
  }
}