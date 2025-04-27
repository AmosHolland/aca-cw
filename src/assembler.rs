use std::{collections::HashMap, fs};

use crate::program::{self, ImmediateOperand, Value};

pub struct Assembler {
    symbol_table: HashMap<String, i32>,
    lines: Vec<String>,
}

impl Assembler {
    pub fn new() -> Self {
        Assembler {
            symbol_table: HashMap::new(),
            lines: Vec::new(),
        }
    }

    pub fn assemble(&mut self, filename: &str) -> program::Program {
        self.symbol_table = HashMap::new();

        let code = fs::read_to_string(filename).expect("Could not read input file.");

        let new_lines = code
            .split('\n')
            .map(|s| -> String { s.to_owned() })
            .collect();

        self.lines = new_lines;

        self.parse_labels();
        self.parse_code()
    }

    fn parse_labels(&mut self) {
        let mut addr_offset = 0;
        for line in self.lines.iter() {
            if !line.is_empty() && line[0..1].eq(".") {
                let parts: Vec<&str> = line.split(' ').collect();

                if parts.len() != 1 {
                    panic!("Encountered invalid label ({line}) while parsing code.")
                }

                let label = &parts[0][1..];

                if label.is_empty() {
                    panic!("Encountered empty label while parsing code.")
                }

                self.symbol_table.insert(label.to_owned(), addr_offset);
            }
            addr_offset += line_real_instructions(line);
        }
    }

    fn parse_code(&self) -> program::Program {
        let mut instructions: Vec<program::Instruction> = Vec::new();
        for line in self.lines.iter() {
            if !line.is_empty() {
                let result = self.parse_line(line);

                if let Some(new_insts) = result {
                    for inst in new_insts {
                        instructions.push(inst);
                    }
                }
            }
        }

        program::Program { instructions }
    }

    fn parse_line(&self, line: &str) -> Option<Vec<program::Instruction>> {
        let parts: Vec<&str> = line.split(' ').collect();
        let op = parts[0];

        if op[0..1].eq(".") || op[0..1].eq("/") {
            return None;
        }

        let op_stats = (op, parts.len() - 1);

        match op_stats {
            ("LDA", 2) => Some(vec![program::Instruction::LoadA(
                self.parse_reg(parts[1]),
                self.parse_operand(parts[2]),
            )]),
            ("STA", 2) => Some(vec![program::Instruction::StoreA(
                self.parse_reg(parts[1]),
                self.parse_operand(parts[2]),
            )]),
            ("LDB", 3) => Some(vec![program::Instruction::LoadB(
                self.parse_reg(parts[1]),
                self.parse_operand(parts[2]),
                self.parse_operand(parts[3]),
            )]),
            ("STB", 3) => Some(vec![program::Instruction::StoreB(
                self.parse_reg(parts[1]),
                self.parse_operand(parts[2]),
                self.parse_operand(parts[3]),
            )]),
            ("MV", 2) => Some(vec![program::Instruction::Move(
                self.parse_reg(parts[1]),
                self.parse_operand(parts[2]),
            )]),
            ("ADD", 3) => Some(vec![program::Instruction::Add(
                self.parse_reg(parts[1]),
                self.parse_operand(parts[2]),
                self.parse_operand(parts[3]),
            )]),
            ("SUB", 3) => Some(vec![program::Instruction::Sub(
                self.parse_reg(parts[1]),
                self.parse_operand(parts[2]),
                self.parse_operand(parts[3]),
            )]),
            ("MUL", 3) => Some(vec![program::Instruction::Mul(
                self.parse_reg(parts[1]),
                self.parse_operand(parts[2]),
                self.parse_operand(parts[3]),
            )]),
            ("DIV", 3) => Some(vec![program::Instruction::Div(
                self.parse_reg(parts[1]),
                self.parse_operand(parts[2]),
                self.parse_operand(parts[3]),
            )]),
            ("JMP", 1) => Some(vec![program::Instruction::Jump(
                self.parse_operand(parts[1]),
            )]),
            ("BEQ", 3) => Some(vec![program::Instruction::Beq(
                self.parse_addr(parts[1]),
                self.parse_operand(parts[2]),
                self.parse_operand(parts[3]),
            )]),
            ("BLT", 3) => Some(vec![program::Instruction::Blt(
                self.parse_addr(parts[1]),
                self.parse_operand(parts[2]),
                self.parse_operand(parts[3]),
            )]),
            ("BGT", 3) => Some(vec![program::Instruction::Bgt(
                self.parse_addr(parts[1]),
                self.parse_operand(parts[2]),
                self.parse_operand(parts[3]),
            )]),
            ("PUSH", 2) => {
                let value = self.parse_reg(parts[1]);
                let target = self.parse_reg(parts[2]);
                let immediate_1 = program::Operand::Imm(ImmediateOperand {
                    value: Value::Int(1),
                });
                Some(vec![
                    program::Instruction::Add(target, program::Operand::Reg(target), immediate_1),
                    program::Instruction::StoreA(value, program::Operand::Reg(target)),
                ])
            }
            ("POP", 2) => {
                let value = self.parse_reg(parts[1]);
                let target = self.parse_reg(parts[2]);
                let immediate_1 = program::Operand::Imm(ImmediateOperand {
                    value: Value::Int(1),
                });
                Some(vec![
                    program::Instruction::LoadA(value, program::Operand::Reg(target)),
                    program::Instruction::Sub(target, program::Operand::Reg(target), immediate_1),
                ])
            }
            _ => panic!("Encountered a malformed instruction ({line}) while parsing code."),
        }
    }

    fn parse_operand(&self, operand: &str) -> program::Operand {
        match &operand[0..1] {
            "R" => program::Operand::Reg(self.parse_reg(operand)),
            "#" => program::Operand::Imm(self.parse_imm(&operand[1..])),
            "_" => program::Operand::Imm(self.parse_label(&operand[1..])),
            _ => panic!("Encountered malformed operand ({operand}) while parsing code."),
        }
    }

    fn parse_addr(&self, operand: &str) -> program::ImmediateOperand {
        match &operand[0..1] {
            "#" => self.parse_imm(&operand[1..]),
            "_" => self.parse_label(&operand[1..]),
            _ => panic!("Encountered invalud address operand ({operand}) while parsing code."),
        }
    }

    fn parse_reg(&self, reg: &str) -> program::RegisterOperand {
        if !reg[0..1].eq("R") {
            panic!("Encountered malformed register while parsing code.")
        };

        let reg_num = reg[1..]
            .parse::<usize>()
            .expect("Encountered invalid register number while parsing code.");

        if reg_num >= crate::ARF_SIZE {
            panic!("Encountered out of range register number ({reg_num}) while parsing code.")
        }

        program::RegisterOperand { reg_num }
    }

    fn parse_imm(&self, imm: &str) -> program::ImmediateOperand {
        let value = program::Value::Int(
            imm.parse::<i32>()
                .expect("Encountered invalid register number while parsing code."),
        );

        program::ImmediateOperand { value }
    }

    fn parse_label(&self, label: &str) -> program::ImmediateOperand {
        if !self.symbol_table.contains_key(label) {
            panic!("Encountered undefined label {label} while parsing code.");
        }

        let value = program::Value::Int(self.symbol_table[label]);
        program::ImmediateOperand { value }
    }
}

fn line_real_instructions(line: &str) -> i32 {
    if line.is_empty() || line[0..1].eq(".") || line[0..1].eq("/") {
        0
    } else {
        let parts: Vec<&str> = line.split(' ').collect();
        let op = parts[0];
        match op {
            "PUSH" => 2,
            "POP" => 2,
            _ => 1,
        }
    }
}
