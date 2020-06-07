use crate::instruction::Opcode;


#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Register(u8),
    IntegerValue(i32),
}

#[derive(Debug, PartialEq, Clone)]
pub struct AssemblerInstruction {
    pub opcode: Opcode,
    pub operand1: Option<Operand>,
    pub operand2: Option<Operand>,
    pub operand3: Option<Operand>,
}
