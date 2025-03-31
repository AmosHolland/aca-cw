use crate::{
    cpu::ExecOperand,
    program::{Instruction, Operand, Value},
};

#[derive(Clone, Copy, Debug)]
pub enum Job {
    Mem(MemJob),
    Compute(ComputeJob),
    Branch(BranchJob),
}

impl Job {
    pub fn is_ready(&self) -> bool {
        match self {
            Job::Mem(job) => job.is_ready(),
            Job::Compute(job) => {
                matches!(job.x, ExecOperand::Value(..)) && matches!(job.y, ExecOperand::Value(..))
            }
            Job::Branch(job) => job.is_ready(),
        }
    }

    pub fn update_operands(&mut self, station_id: usize, value: ExecOperand) {
        match self {
            Job::Mem(job) => job.update_operands(station_id, value),
            Job::Compute(job) => job.update_operands(station_id, value),
            Job::Branch(job) => job.update_operands(station_id, value),
        }
    }
}

impl std::fmt::Display for Job {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Job::Mem(job) => write!(f, "{job}"),
            Job::Compute(job) => write!(f, "{job}"),
            Job::Branch(job) => write!(f, "{job}"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct MemJob {
    pub operation: MemOp,
    pub dst: Option<usize>,
    pub src: Option<ExecOperand>,
    pub index: ExecOperand,
    pub base: Option<ExecOperand>,
}

impl MemJob {
    fn is_ready(&self) -> bool {
        let mut ready = matches!(self.index, ExecOperand::Value(..));
        if let Some(src) = self.src {
            ready = ready && matches!(src, ExecOperand::Value(..));
        }
        if let Some(base) = self.base {
            ready = ready && matches!(base, ExecOperand::Value(..));
        }
        ready
    }

    fn update_operands(&mut self, station_id: usize, value: ExecOperand) {
        if let Some(src) = self.src {
            if src.is_ref_to_id(station_id) {
                self.src = Some(value)
            }
        }

        if self.index.is_ref_to_id(station_id) {
            self.index = value
        }

        if let Some(base) = self.base {
            if base.is_ref_to_id(station_id) {
                self.base = Some(value)
            }
        }
    }
}

impl std::fmt::Display for MemJob {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let base = match self.base {
            None => "".to_string(),
            Some(ex_op) => format!("{ex_op} + "),
        };
        match self.operation {
            MemOp::Load => write!(
                f,
                "R{0} = Mem[{base}{1}]",
                self.dst.expect("Load without destination."),
                self.index
            ),
            MemOp::Store => write!(
                f,
                "Mem[{base}{1}] = {0}",
                self.src.expect("Store without source."),
                self.index
            ),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum MemOp {
    Load,
    Store,
}

#[derive(Clone, Copy, Debug)]
pub struct ComputeJob {
    pub operation: ComputeOp,
    pub x: ExecOperand,
    pub y: ExecOperand,
    pub dest: usize,
}

impl ComputeJob {
    fn update_operands(&mut self, station_id: usize, value: ExecOperand) {
        if self.x.is_ref_to_id(station_id) {
            self.x = value
        }

        if self.y.is_ref_to_id(station_id) {
            self.y = value
        }
    }
}

impl std::fmt::Display for ComputeJob {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "R{0} = {1} {2} {3}",
            self.dest, self.x, self.operation, self.y
        )
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ComputeOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl std::fmt::Display for ComputeOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ComputeOp::Add => write!(f, "+"),
            ComputeOp::Sub => write!(f, "-"),
            ComputeOp::Mul => write!(f, "*"),
            ComputeOp::Div => write!(f, "/"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct BranchJob {
    pub operation: Option<CompareOp>,
    pub x: Option<ExecOperand>,
    pub y: Option<ExecOperand>,
    pub t: ExecOperand,
}

impl BranchJob {
    fn is_ready(&self) -> bool {
        let mut ready = matches!(self.t, ExecOperand::Value(..));
        if let Some(x) = self.x {
            ready = ready && matches!(x, ExecOperand::Value(..));
        }
        if let Some(y) = self.y {
            ready = ready && matches!(y, ExecOperand::Value(..));
        }
        ready
    }

    fn update_operands(&mut self, station_id: usize, value: ExecOperand) {
        if let Some(x) = self.x {
            if x.is_ref_to_id(station_id) {
                self.x = Some(value)
            }
        }

        if let Some(base) = self.y {
            if base.is_ref_to_id(station_id) {
                self.y = Some(value)
            }
        }

        if self.t.is_ref_to_id(station_id) {
            self.t = value;
        }
    }
}

impl std::fmt::Display for BranchJob {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let condition = match self.operation {
            None => "".to_string(),
            Some(op) => format!(
                " if {0} {op} {1}",
                self.x.expect("conditional branch missing operand"),
                self.y.expect("conditional branch missing operand")
            ),
        };

        write!(f, "PC = {0}{condition}", self.t)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum CompareOp {
    Eq,
    Lt,
    Gt,
}

impl std::fmt::Display for CompareOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompareOp::Eq => write!(f, "=="),
            CompareOp::Lt => write!(f, "<"),
            CompareOp::Gt => write!(f, ">"),
        }
    }
}

pub fn decode(instruction: Instruction, arf: &[ExecOperand]) -> Job {
    match instruction {
        Instruction::LoadA(register_operand, operand) => Job::Mem(MemJob {
            operation: MemOp::Load,
            dst: Some(register_operand.reg_num),
            src: None,
            index: decode_operand(operand, arf),
            base: None,
        }),
        Instruction::StoreA(register_operand, operand) => Job::Mem(MemJob {
            operation: MemOp::Store,
            dst: None,
            src: Some(arf[register_operand.reg_num]),
            index: decode_operand(operand, arf),
            base: None,
        }),
        Instruction::LoadB(register_operand, operand1, operand2) => Job::Mem(MemJob {
            operation: MemOp::Load,
            dst: Some(register_operand.reg_num),
            src: None,
            index: decode_operand(operand1, arf),
            base: Some(decode_operand(operand2, arf)),
        }),
        Instruction::StoreB(register_operand, operand1, operand2) => Job::Mem(MemJob {
            operation: MemOp::Store,
            dst: None,
            src: Some(arf[register_operand.reg_num]),
            index: decode_operand(operand1, arf),
            base: Some(decode_operand(operand2, arf)),
        }),
        Instruction::Move(register_operand, operand) => Job::Compute(ComputeJob {
            operation: ComputeOp::Add,
            x: decode_operand(operand, arf),
            y: ExecOperand::Value(0),
            dest: register_operand.reg_num,
        }),
        Instruction::Add(register_operand, operand1, operand2) => Job::Compute(ComputeJob {
            operation: ComputeOp::Add,
            x: decode_operand(operand1, arf),
            y: decode_operand(operand2, arf),
            dest: register_operand.reg_num,
        }),
        Instruction::Sub(register_operand, operand1, operand2) => Job::Compute(ComputeJob {
            operation: ComputeOp::Sub,
            x: decode_operand(operand1, arf),
            y: decode_operand(operand2, arf),
            dest: register_operand.reg_num,
        }),
        Instruction::Mul(register_operand, operand1, operand2) => Job::Compute(ComputeJob {
            operation: ComputeOp::Mul,
            x: decode_operand(operand1, arf),
            y: decode_operand(operand2, arf),
            dest: register_operand.reg_num,
        }),
        Instruction::Div(register_operand, operand1, operand2) => Job::Compute(ComputeJob {
            operation: ComputeOp::Div,
            x: decode_operand(operand1, arf),
            y: decode_operand(operand2, arf),
            dest: register_operand.reg_num,
        }),
        Instruction::Jump(operand) => Job::Branch(BranchJob {
            operation: None,
            x: None,
            y: None,
            t: decode_operand(operand, arf),
        }),
        Instruction::Beq(operand1, operand2, operand3) => Job::Branch(BranchJob {
            operation: Some(CompareOp::Eq),
            x: Some(decode_operand(operand2, arf)),
            y: Some(decode_operand(operand3, arf)),
            t: decode_operand(operand1, arf),
        }),
        Instruction::Blt(operand1, operand2, operand3) => Job::Branch(BranchJob {
            operation: Some(CompareOp::Lt),
            x: Some(decode_operand(operand2, arf)),
            y: Some(decode_operand(operand3, arf)),
            t: decode_operand(operand1, arf),
        }),
        Instruction::Bgt(operand1, operand2, operand3) => Job::Branch(BranchJob {
            operation: Some(CompareOp::Gt),
            x: Some(decode_operand(operand2, arf)),
            y: Some(decode_operand(operand3, arf)),
            t: decode_operand(operand1, arf),
        }),
    }
}

fn decode_operand(operand: Operand, arf: &[ExecOperand]) -> ExecOperand {
    match operand {
        Operand::Reg(reg_opr) => arf[reg_opr.reg_num],
        Operand::Imm(imm_opr) => {
            let Value::Int(i) = imm_opr.value;
            ExecOperand::Value(i)
        }
    }
}
