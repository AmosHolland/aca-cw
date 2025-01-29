use crate::program::{ImmediateOperand, Instruction, Operand, Value};

#[derive(Clone, Copy, Debug)]
pub enum Work {
    Mem(MemWork),
    Compute(ComputeWork),
    Branch(BranchWork),
}

#[derive(Clone, Copy, Debug)]
pub struct MemWork {
    pub operation: MemOp,
    pub reg: usize,
    pub index: Operand,
    pub base: Option<Operand>,
}

#[derive(Clone, Copy, Debug)]
pub enum MemOp {
    Load,
    Store,
}

#[derive(Clone, Copy, Debug)]
pub struct ComputeWork {
    pub operation: ComputeOp,
    pub x: Operand,
    pub y: Operand,
    pub dest: usize,
}

#[derive(Clone, Copy, Debug)]
pub enum ComputeOp {
    Add,
    Sub,
    Mul,
}

#[derive(Clone, Copy, Debug)]
pub struct BranchWork {
    pub operation: Option<CompareOp>,
    pub x: Option<Operand>,
    pub y: Option<Operand>,
    pub t: Operand,
}

#[derive(Clone, Copy, Debug)]
pub enum CompareOp {
    Eq,
    Lt,
    Gt,
}

pub fn decode(instruction: Instruction) -> Work {
    match instruction {
        Instruction::LoadA(register_operand, operand) => Work::Mem(MemWork {
            operation: MemOp::Load,
            reg: register_operand.reg_num,
            index: operand,
            base: None,
        }),
        Instruction::StoreA(register_operand, operand) => Work::Mem(MemWork {
            operation: MemOp::Store,
            reg: register_operand.reg_num,
            index: operand,
            base: None,
        }),
        Instruction::LoadB(register_operand, operand1, operand2) => Work::Mem(MemWork {
            operation: MemOp::Load,
            reg: register_operand.reg_num,
            index: operand1,
            base: Some(operand2),
        }),
        Instruction::StoreB(register_operand, operand1, operand2) => Work::Mem(MemWork {
            operation: MemOp::Store,
            reg: register_operand.reg_num,
            index: operand1,
            base: Some(operand2),
        }),
        Instruction::Move(register_operand, operand) => Work::Compute(ComputeWork {
            operation: ComputeOp::Add,
            x: operand,
            y: Operand::Imm(ImmediateOperand {
                value: Value::Int(0),
            }),
            dest: register_operand.reg_num,
        }),
        Instruction::Add(register_operand, operand1, operand2) => Work::Compute(ComputeWork {
            operation: ComputeOp::Add,
            x: operand1,
            y: operand2,
            dest: register_operand.reg_num,
        }),
        Instruction::Sub(register_operand, operand1, operand2) => Work::Compute(ComputeWork {
            operation: ComputeOp::Sub,
            x: operand1,
            y: operand2,
            dest: register_operand.reg_num,
        }),
        Instruction::Mul(register_operand, operand1, operand2) => Work::Compute(ComputeWork {
            operation: ComputeOp::Mul,
            x: operand1,
            y: operand2,
            dest: register_operand.reg_num,
        }),
        Instruction::Jump(operand) => Work::Branch(BranchWork {
            operation: None,
            x: None,
            y: None,
            t: operand,
        }),
        Instruction::Beq(operand1, operand2, operand3) => Work::Branch(BranchWork {
            operation: Some(CompareOp::Eq),
            x: Some(operand2),
            y: Some(operand3),
            t: operand1,
        }),
        Instruction::Blt(operand1, operand2, operand3) => Work::Branch(BranchWork {
            operation: Some(CompareOp::Lt),
            x: Some(operand2),
            y: Some(operand3),
            t: operand1,
        }),
        Instruction::Bgt(operand1, operand2, operand3) => Work::Branch(BranchWork {
            operation: Some(CompareOp::Gt),
            x: Some(operand2),
            y: Some(operand3),
            t: operand1,
        }),
    }
}
