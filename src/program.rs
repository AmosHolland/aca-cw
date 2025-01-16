#[derive(Debug)]
pub struct Program {
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Load(RegisterOperand, Operand),
    Store(RegisterOperand, Operand),
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

#[derive(Debug)]
pub enum Value {
    Int(i32),
    // Float(f32),
    UInt(usize),
}

#[derive(Debug)]
pub enum Operand {
    Reg(RegisterOperand),
    Imm(ImmediateOperand),
}

#[derive(Debug)]
pub struct RegisterOperand {
    pub reg_num: usize,
}

#[derive(Debug)]
pub struct ImmediateOperand {
    pub value: Value,
}
