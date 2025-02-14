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
