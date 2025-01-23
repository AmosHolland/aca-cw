use crate::decoder::ALUWork;

pub fn alu_execute(work: ALUWork) -> (usize, i32) {
    let result = match work.operation {
        crate::decoder::ALUOp::Add => work.x + work.y,
        crate::decoder::ALUOp::Sub => work.x - work.y,
        crate::decoder::ALUOp::Mul => work.x * work.y,
    };

    (work.dest, result)
}
