use std::{collections::HashMap, fs};

use crate::program::{self};

pub struct Assembler {
    symbol_table: HashMap<String, usize>,
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
            if !line_is_nop(line) {
                addr_offset += 1;
            }
        }
    }

    fn parse_code(&self) -> program::Program {
        let mut instructions: Vec<program::Instruction> = Vec::new();
        for line in self.lines.iter() {
            if !line.is_empty() {
                let result = self.parse_line(line);

                if let Some(instruction) = result {
                    instructions.push(instruction);
                }
            }
        }

        program::Program { instructions }
    }

    fn parse_line(&self, line: &str) -> Option<program::Instruction> {
        let parts: Vec<&str> = line.split(' ').collect();
        let op = parts[0];

        if op[0..1].eq(".") {
            return None;
        }

        let op_stats = (op, parts.len() - 1);

        match op_stats {
            ("LD", 2) => Some(program::Instruction::Load(
                self.parse_reg(parts[1]),
                self.parse_operand(parts[2]),
            )),
            ("ST", 2) => Some(program::Instruction::Store(
                self.parse_reg(parts[1]),
                self.parse_operand(parts[2]),
            )),
            ("MV", 2) => Some(program::Instruction::Move(
                self.parse_reg(parts[1]),
                self.parse_operand(parts[2]),
            )),
            ("ADD", 3) => Some(program::Instruction::Add(
                self.parse_reg(parts[1]),
                self.parse_operand(parts[2]),
                self.parse_operand(parts[3]),
            )),
            ("SUB", 3) => Some(program::Instruction::Sub(
                self.parse_reg(parts[1]),
                self.parse_operand(parts[2]),
                self.parse_operand(parts[3]),
            )),
            ("MUL", 3) => Some(program::Instruction::Mul(
                self.parse_reg(parts[1]),
                self.parse_operand(parts[2]),
                self.parse_operand(parts[3]),
            )),
            ("JMP", 1) => Some(program::Instruction::Jump(self.parse_operand(parts[1]))),
            ("BEQ", 3) => Some(program::Instruction::Beq(
                self.parse_operand(parts[1]),
                self.parse_operand(parts[2]),
                self.parse_operand(parts[3]),
            )),
            ("BLT", 3) => Some(program::Instruction::Blt(
                self.parse_operand(parts[1]),
                self.parse_operand(parts[2]),
                self.parse_operand(parts[3]),
            )),
            ("BGT", 3) => Some(program::Instruction::Bgt(
                self.parse_operand(parts[1]),
                self.parse_operand(parts[2]),
                self.parse_operand(parts[3]),
            )),
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

    fn parse_reg(&self, reg: &str) -> program::RegisterOperand {
        if !reg[0..1].eq("R") {
            panic!("Encountered malformed register while parsing code.")
        };

        let reg_num = reg[1..]
            .parse::<usize>()
            .expect("Encountered invalid register number while parsing code.");

        if reg_num >= crate::ISA_N_REG {
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

        let value = program::Value::UInt(self.symbol_table[label]);
        program::ImmediateOperand { value }
    }
}

fn line_is_nop(line: &str) -> bool {
    line.is_empty() || line[0..1].eq(".")
}
