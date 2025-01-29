#[derive(Debug, Clone)]
pub struct Program {
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    LoadA(RegisterOperand, Operand),
    StoreA(RegisterOperand, Operand),
    LoadB(RegisterOperand, Operand, Operand),
    StoreB(RegisterOperand, Operand, Operand),
    Move(RegisterOperand, Operand),
    Add(RegisterOperand, Operand, Operand),
    Sub(RegisterOperand, Operand, Operand),
    Mul(RegisterOperand, Operand, Operand),
    // FAdd(Register, Operand, Operand),
    // FSub(Register, Operand, Operand),
    // FMul(Register, Operand, Operand),
    Jump(Operand),
    Beq(Operand, Operand, Operand),
    Blt(Operand, Operand, Operand),
    Bgt(Operand, Operand, Operand),
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Int(i32),
    // Float(f32),
    UInt(usize),
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Reg(RegisterOperand),
    Imm(ImmediateOperand),
}

#[derive(Debug, Clone, Copy)]
pub struct RegisterOperand {
    pub reg_num: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct ImmediateOperand {
    pub value: Value,
}
