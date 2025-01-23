use core::panic;

use crate::program::{Instruction, Operand, Value};

#[derive(Clone, Copy)]
pub enum Work {
    Mem(MemWork),
    Alu(ALUWork),
    Branch(BranchWork),
}

#[derive(Clone, Copy)]
pub struct MemWork {}

#[derive(Clone, Copy)]
pub struct ALUWork {
    pub operation: ALUOp,
    pub x: i32,
    pub y: i32,
    pub dest: usize,
}

#[derive(Clone, Copy)]
pub enum ALUOp {
    Add,
    Sub,
    Mul,
}

#[derive(Clone, Copy)]
pub struct BranchWork {}

pub fn decode(instruction: Instruction, arf: &[i32; crate::ARF_SIZE]) -> Work {
    match instruction {
        Instruction::Load(register_operand, operand) => todo!(),
        Instruction::Store(register_operand, operand) => todo!(),
        Instruction::Move(register_operand, operand) => Work::Alu(ALUWork {
            operation: ALUOp::Add,
            x: decode_alu_operand(operand, arf),
            y: 0,
            dest: register_operand.reg_num,
        }),
        Instruction::Add(register_operand, operand1, operand2) => Work::Alu(ALUWork {
            operation: ALUOp::Add,
            x: decode_alu_operand(operand1, arf),
            y: decode_alu_operand(operand2, arf),
            dest: register_operand.reg_num,
        }),
        Instruction::Sub(register_operand, operand1, operand2) => Work::Alu(ALUWork {
            operation: ALUOp::Sub,
            x: decode_alu_operand(operand1, arf),
            y: decode_alu_operand(operand2, arf),
            dest: register_operand.reg_num,
        }),
        Instruction::Mul(register_operand, operand1, operand2) => Work::Alu(ALUWork {
            operation: ALUOp::Mul,
            x: decode_alu_operand(operand1, arf),
            y: decode_alu_operand(operand2, arf),
            dest: register_operand.reg_num,
        }),
        Instruction::Jump(operand) => todo!(),
        Instruction::Beq(operand1, operand2, operand3) => todo!(),
        Instruction::Blt(operand1, operand2, operand3) => todo!(),
        Instruction::Bgt(operand1, operand2, operand3) => todo!(),
    }
}

fn decode_alu_operand(operand: Operand, arf: &[i32; crate::ARF_SIZE]) -> i32 {
    if let Value::Int(n) = decode_operand(operand, arf) {
        return n;
    }

    panic!("Attempted to do ALU operations on unsigned int.")
}

fn decode_operand(operand: Operand, arf: &[i32; crate::ARF_SIZE]) -> Value {
    match operand {
        Operand::Reg(reg_operand) => Value::Int(arf[reg_operand.reg_num]),
        Operand::Imm(imm_operand) => imm_operand.value,
    }
}
