use std::usize;

use text_io::read;

use crate::alu::{alu_compare, alu_compute};
use crate::decoder::{decode, BranchWork, ComputeWork, MemOp, MemWork, Work};
use crate::memory::Memory;
use crate::program::{Instruction, Operand, Program, Value};
use crate::{ARF_SIZE, PRF_SIZE};

pub struct Cpu {
    arf: [i32; ARF_SIZE],
    prf: [i32; PRF_SIZE],
    pc: usize,
    memory: Memory,
    debug: bool,
}

pub type Address = usize;
pub type Writeback = (usize, i32);

#[derive(Default)]
struct Pipeline {
    fetch: Option<Address>,
    decode: Option<Instruction>,
    execute: Option<Work>,
    writeback: Option<Writeback>,
}

impl std::fmt::Display for Pipeline {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "
        To fetch     : {0:?}\n
        To decode    : {1:?}\n
        To execute   : {2:?}\n
        To writeback : {3:?}\n
        ",
            self.fetch, self.decode, self.execute, self.writeback
        )
    }
}

impl Cpu {
    pub fn new(debug: bool) -> Self {
        Cpu {
            arf: [0; ARF_SIZE],
            prf: [0; PRF_SIZE],
            pc: 0,
            memory: Memory::new(),
            debug,
        }
    }

    pub fn run(&mut self, program: Program) {
        self.arf = [0; ARF_SIZE];
        self.prf = [0; PRF_SIZE];

        let program_length = program.instructions.len();
        self.memory.load_program(program);

        let mut cycles = 0;

        let mut curr_pipeline = Pipeline {
            fetch: Some(0),
            decode: None,
            execute: None,
            writeback: None,
        };

        while self.pc < program_length {
            if self.debug {
                println!("Pipeline before cycle {0}:", cycles + 1);
                println!("{curr_pipeline}\n");
                println!("PC Value: {0}\n", self.pc);
                self.display_reg_state();
                println!();
                let _: String = read!();
            }
            curr_pipeline = self.run_cycle(curr_pipeline);
            cycles += 1;
        }

        println!("Program run in {cycles} cycles.");
        self.display_reg_state();

        for i in 399..411 {
            println!("Memory Location {i}: {0}", self.memory.load(i));
        }
    }
    fn run_cycle(&mut self, start_pipeline: Pipeline) -> Pipeline {
        let mut next_pipeline = Pipeline::default();

        if let Some(addr) = start_pipeline.fetch {
            next_pipeline.decode = Some(self.memory.fetch(addr));
        }
        if let Some(instr) = start_pipeline.decode {
            next_pipeline.execute = Some(decode(instr))
        }
        if let Some(work) = start_pipeline.execute {
            next_pipeline.writeback = match work {
                Work::Mem(mem_work) => {
                    let wb = self.handle_mem_work(mem_work);
                    if wb.is_none() {
                        self.pc += 1;
                        next_pipeline.fetch = Some(self.pc);
                    }
                    wb
                }
                Work::Compute(comp_work) => self.handle_compute_work(comp_work),
                Work::Branch(branch_work) => {
                    self.handle_branch_work(branch_work);
                    next_pipeline.fetch = Some(self.pc);
                    None
                }
            }
        }

        if let Some((reg, value)) = start_pipeline.writeback {
            self.arf[reg] = value;
            self.pc += 1;
            next_pipeline.fetch = Some(self.pc);
        };

        next_pipeline
    }

    fn handle_compute_work(&self, work: ComputeWork) -> Option<Writeback> {
        let x = self.evaluate_compute_operand(work.x);
        let y = self.evaluate_compute_operand(work.y);

        Some((work.dest, alu_compute(work.operation, x, y)))
    }

    fn handle_branch_work(&mut self, work: BranchWork) {
        let x = self.evaluate_compute_operand(work.x.expect("Conditional branch without operands"));
        let y = self.evaluate_compute_operand(work.y.expect("Conditional branch without operands"));
        let t = self.evaluate_address_operand(work.t);

        let branch = match work.operation {
            None => true,
            Some(op) => alu_compare(op, x, y),
        };

        if branch {
            self.pc = t;
        } else {
            self.pc += 1;
        }
    }

    fn handle_mem_work(&mut self, work: MemWork) -> Option<Writeback> {
        let base = match work.base {
            Some(opr) => self.evaluate_address_operand(opr),
            None => 0,
        };
        let index = self.evaluate_address_operand(work.index);

        let address = base + index;

        match work.operation {
            MemOp::Load => {
                let value = self.memory.load(address);
                Some((work.reg, value))
            }
            MemOp::Store => {
                let value = self.arf[work.reg];
                self.memory.store(address, value);
                None
            }
        }
    }

    fn evaluate_compute_operand(&self, opr: Operand) -> i32 {
        match opr {
            Operand::Reg(reg_opr) => self.arf[reg_opr.reg_num],
            Operand::Imm(imm_opr) => {
                if let Value::Int(i) = imm_opr.value {
                    i
                } else {
                    panic!("attempted to do compute on a usize")
                }
            }
        }
    }

    fn evaluate_address_operand(&self, opr: Operand) -> usize {
        match opr {
            Operand::Reg(reg_opr) => self.arf[reg_opr.reg_num].try_into().unwrap(),
            Operand::Imm(imm_opr) => match imm_opr.value {
                Value::Int(i) => i.try_into().expect("Invalid int given as address"),
                Value::UInt(i) => i,
            },
        }
    }

    fn display_reg_state(&self) {
        println!("Register values:");
        for (i, r) in self.arf.iter().enumerate() {
            println!("Register {i}: {r}");
        }
    }
}
