use std::{fmt::write, path::Display};

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

impl Instruction {
    fn get_write_reg(&self) -> Option<usize> {
        match self {
            Instruction::LoadA(reg_opr, _) => Some(reg_opr.reg_num),
            Instruction::StoreA(reg_opr, _) => Some(reg_opr.reg_num),
            Instruction::LoadB(reg_opr, _, _) => Some(reg_opr.reg_num),
            Instruction::StoreB(reg_opr, _, _) => Some(reg_opr.reg_num),
            Instruction::Move(reg_opr, _) => Some(reg_opr.reg_num),
            Instruction::Add(reg_opr, _, _) => Some(reg_opr.reg_num),
            Instruction::Sub(reg_opr, _, _) => Some(reg_opr.reg_num),
            Instruction::Mul(reg_opr, _, _) => Some(reg_opr.reg_num),
            _ => None,
        }
    }

    fn get_read_regs(&self) -> Option<Vec<usize>> {
        match self {
            Instruction::LoadA(_, operand) => find_reg_operands(vec![*operand]),
            Instruction::StoreA(_, operand) => find_reg_operands(vec![*operand]),
            Instruction::LoadB(_, operand, operand1) => {
                find_reg_operands(vec![*operand, *operand1])
            }
            Instruction::StoreB(_, operand, operand1) => {
                find_reg_operands(vec![*operand, *operand1])
            }
            Instruction::Move(_, operand) => find_reg_operands(vec![*operand]),
            Instruction::Add(_, operand, operand1) => find_reg_operands(vec![*operand, *operand1]),
            Instruction::Sub(_, operand, operand1) => find_reg_operands(vec![*operand, *operand1]),
            Instruction::Mul(_, operand, operand1) => find_reg_operands(vec![*operand, *operand1]),
            Instruction::Jump(operand) => find_reg_operands(vec![*operand]),
            Instruction::Beq(operand, operand1, operand2) => {
                find_reg_operands(vec![*operand, *operand1, *operand2])
            }
            Instruction::Blt(operand, operand1, operand2) => {
                find_reg_operands(vec![*operand, *operand1, *operand2])
            }
            Instruction::Bgt(operand, operand1, operand2) => {
                find_reg_operands(vec![*operand, *operand1, *operand2])
            }
        }
    }

    pub fn blocks(&self, other: &Self) -> bool {
        if let Some(write_reg) = self.get_write_reg() {
            if let Some(regs) = other.get_read_regs() {
                regs.contains(&write_reg)
            } else {
                false
            }
        } else {
            false
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
            Instruction::Jump(opr) => write!(f, "JMP {opr}"),
            Instruction::Beq(opr, opr1, opr2) => write!(f, "BEQ {opr} {opr1} {opr2}"),
            Instruction::Blt(opr, opr1, opr2) => write!(f, "BLT {opr} {opr1} {opr2}"),
            Instruction::Bgt(opr, opr1, opr2) => write!(f, "BGT {opr} {opr1} {opr2}"),
        }
    }
}

fn find_reg_operands(oprs: Vec<Operand>) -> Option<Vec<usize>> {
    let mut regs = Vec::new();
    for opr in oprs {
        if let Operand::Reg(reg) = opr {
            regs.push(reg.reg_num);
        }
    }

    if regs.is_empty() {
        None
    } else {
        Some(regs)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Int(i32),
    // Float(f32),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{i}"),
        }
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
