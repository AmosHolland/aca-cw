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
    Div(RegisterOperand, Operand, Operand),
    Jump(Operand),
    Beq(ImmediateOperand, Operand, Operand),
    Blt(ImmediateOperand, Operand, Operand),
    Bgt(ImmediateOperand, Operand, Operand),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum InstructionType {
    Mem,
    Alu,
    Jmp,
}

impl Instruction {
    pub fn get_write_reg(&self) -> Option<usize> {
        match self {
            Instruction::LoadA(reg_opr, _) => Some(reg_opr.reg_num),
            Instruction::LoadB(reg_opr, _, _) => Some(reg_opr.reg_num),
            Instruction::Move(reg_opr, _) => Some(reg_opr.reg_num),
            Instruction::Add(reg_opr, _, _) => Some(reg_opr.reg_num),
            Instruction::Sub(reg_opr, _, _) => Some(reg_opr.reg_num),
            Instruction::Mul(reg_opr, _, _) => Some(reg_opr.reg_num),
            Instruction::Div(reg_opr, _, _) => Some(reg_opr.reg_num),
            _ => None,
        }
    }

    pub fn is_branch(&self) -> bool {
        matches!(
            self,
            Instruction::Jump(..)
                | Instruction::Beq(..)
                | Instruction::Blt(..)
                | Instruction::Bgt(..)
        )
    }

    pub fn n_cycles(&self) -> usize {
        match self {
            Instruction::LoadA(_, _) => 4,
            Instruction::LoadB(_, _, _) => 5,
            Instruction::StoreB(_, _, _) => 2,
            Instruction::Mul(_, _, _) => 2,
            Instruction::Div(_, _, _) => 12,
            _ => 1,
        }
    }

    pub fn get_type(&self) -> InstructionType {
        match self {
            Instruction::LoadA(..) => InstructionType::Mem,
            Instruction::StoreA(..) => InstructionType::Mem,
            Instruction::LoadB(..) => InstructionType::Mem,
            Instruction::StoreB(..) => InstructionType::Mem,
            Instruction::Move(..) => InstructionType::Alu,
            Instruction::Add(..) => InstructionType::Alu,
            Instruction::Sub(..) => InstructionType::Alu,
            Instruction::Mul(..) => InstructionType::Alu,
            Instruction::Div(..) => InstructionType::Alu,
            Instruction::Jump(..) => InstructionType::Jmp,
            Instruction::Blt(..) => InstructionType::Jmp,
            Instruction::Beq(..) => InstructionType::Jmp,
            Instruction::Bgt(..) => InstructionType::Jmp,
        }
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::LoadA(reg_opr, opr) => write!(f, "LDA {reg_opr} {opr}"),
            Instruction::StoreA(reg_opr, opr) => write!(f, "STA {reg_opr} {opr}"),
            Instruction::LoadB(reg_opr, opr, opr1) => write!(f, "LDB {reg_opr} {opr} {opr1}"),
            Instruction::StoreB(reg_opr, opr, opr1) => write!(f, "STB {reg_opr} {opr}, {opr1}"),
            Instruction::Move(reg_opr, opr) => write!(f, "MV {reg_opr} {opr}"),
            Instruction::Add(reg_opr, opr, opr1) => write!(f, "ADD {reg_opr} {opr}, {opr1}"),
            Instruction::Sub(reg_opr, opr, opr1) => write!(f, "SUB {reg_opr} {opr}, {opr1}"),
            Instruction::Mul(reg_opr, opr, opr1) => write!(f, "MUL {reg_opr} {opr}, {opr1}"),
            Instruction::Div(reg_opr, opr, opr1) => write!(f, "DIV {reg_opr} {opr}, {opr1}"),
            Instruction::Jump(opr) => write!(f, "JMP {opr}"),
            Instruction::Beq(opr, opr1, opr2) => write!(f, "BEQ {opr} {opr1} {opr2}"),
            Instruction::Blt(opr, opr1, opr2) => write!(f, "BLT {opr} {opr1} {opr2}"),
            Instruction::Bgt(opr, opr1, opr2) => write!(f, "BGT {opr} {opr1} {opr2}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Int(i32),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{i}"),
        }
    }
}

impl Value {
    pub fn as_usize(&self) -> usize {
        let Value::Int(i) = self;
        (*i).try_into()
            .expect("Tried to convert incopatible value into usize.")
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Reg(RegisterOperand),
    Imm(ImmediateOperand),
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Reg(reg_opr) => write!(f, "{reg_opr}"),
            Operand::Imm(imm_opr) => write!(f, "{imm_opr}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct RegisterOperand {
    pub reg_num: usize,
}

impl std::fmt::Display for RegisterOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "R{0}", self.reg_num)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ImmediateOperand {
    pub value: Value,
}

impl std::fmt::Display for ImmediateOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{0}", self.value)
    }
}
