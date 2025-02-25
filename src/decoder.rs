use crate::program::{ImmediateOperand, Instruction, Operand, Value};

#[derive(Clone, Copy, Debug)]
pub struct Work {
    pub cycles: usize,
    pub job: Job,
}

#[derive(Clone, Copy, Debug)]
pub enum Job {
    Mem(MemJob),
    Compute(ComputeJob),
    Branch(BranchJob),
}

#[derive(Clone, Copy, Debug)]
pub struct MemJob {
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
pub struct ComputeJob {
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
    Div,
}

#[derive(Clone, Copy, Debug)]
pub struct BranchJob {
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
    let job = match instruction {
        Instruction::LoadA(register_operand, operand) => Job::Mem(MemJob {
            operation: MemOp::Load,
            reg: register_operand.reg_num,
            index: operand,
            base: None,
        }),
        Instruction::StoreA(register_operand, operand) => Job::Mem(MemJob {
            operation: MemOp::Store,
            reg: register_operand.reg_num,
            index: operand,
            base: None,
        }),
        Instruction::LoadB(register_operand, operand1, operand2) => Job::Mem(MemJob {
            operation: MemOp::Load,
            reg: register_operand.reg_num,
            index: operand1,
            base: Some(operand2),
        }),
        Instruction::StoreB(register_operand, operand1, operand2) => Job::Mem(MemJob {
            operation: MemOp::Store,
            reg: register_operand.reg_num,
            index: operand1,
            base: Some(operand2),
        }),
        Instruction::Move(register_operand, operand) => Job::Compute(ComputeJob {
            operation: ComputeOp::Add,
            x: operand,
            y: Operand::Imm(ImmediateOperand {
                value: Value::Int(0),
            }),
            dest: register_operand.reg_num,
        }),
        Instruction::Add(register_operand, operand1, operand2) => Job::Compute(ComputeJob {
            operation: ComputeOp::Add,
            x: operand1,
            y: operand2,
            dest: register_operand.reg_num,
        }),
        Instruction::Sub(register_operand, operand1, operand2) => Job::Compute(ComputeJob {
            operation: ComputeOp::Sub,
            x: operand1,
            y: operand2,
            dest: register_operand.reg_num,
        }),
        Instruction::Mul(register_operand, operand1, operand2) => Job::Compute(ComputeJob {
            operation: ComputeOp::Mul,
            x: operand1,
            y: operand2,
            dest: register_operand.reg_num,
        }),
        Instruction::Div(register_operand, operand1, operand2) => Job::Compute(ComputeJob {
            operation: ComputeOp::Div,
            x: operand1,
            y: operand2,
            dest: register_operand.reg_num,
        }),
        Instruction::Jump(operand) => Job::Branch(BranchJob {
            operation: None,
            x: None,
            y: None,
            t: operand,
        }),
        Instruction::Beq(operand1, operand2, operand3) => Job::Branch(BranchJob {
            operation: Some(CompareOp::Eq),
            x: Some(operand2),
            y: Some(operand3),
            t: operand1,
        }),
        Instruction::Blt(operand1, operand2, operand3) => Job::Branch(BranchJob {
            operation: Some(CompareOp::Lt),
            x: Some(operand2),
            y: Some(operand3),
            t: operand1,
        }),
        Instruction::Bgt(operand1, operand2, operand3) => Job::Branch(BranchJob {
            operation: Some(CompareOp::Gt),
            x: Some(operand2),
            y: Some(operand3),
            t: operand1,
        }),
    };

    let cycles = match instruction {
        Instruction::LoadA(_, _) => 4,
        Instruction::LoadB(_, _, _) => 5,
        Instruction::StoreB(_, _, _) => 2,
        Instruction::Mul(_, _, _) => 2,
        Instruction::Div(_, _, _) => 12,
        _ => 1,
    };

    Work { job, cycles }
}
