use crate::decoder::{CompareOp, ComputeOp};

pub fn alu_compute(op: ComputeOp, x: i32, y: i32) -> i32 {
    match op {
        crate::decoder::ComputeOp::Add => x + y,
        crate::decoder::ComputeOp::Sub => x - y,
        crate::decoder::ComputeOp::Mul => x * y,
        crate::decoder::ComputeOp::Div => x / y,
    }
}

pub fn alu_compare(op: CompareOp, x: i32, y: i32) -> bool {
    match op {
        CompareOp::Eq => x == y,
        CompareOp::Lt => x < y,
        CompareOp::Gt => x > y,
    }
}
