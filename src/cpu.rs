use std::default;
use std::usize;

use crate::alu;
use crate::alu::alu_execute;
use crate::decoder::decode;
use crate::decoder::Work;
use crate::memory::Memory;
use crate::program::Instruction;
use crate::program::Program;
use crate::ARF_SIZE;
use crate::PRF_SIZE;

struct CPU {
    arf: [i32; ARF_SIZE],
    prf: [i32; PRF_SIZE],
    pc: usize,
    memory: Memory,
}

#[derive(Default)]
struct Pipeline {
    fetch: Option<usize>,
    decode: Option<Instruction>,
    execute: Option<Work>,
    writeback: Option<(usize, i32)>,
}

impl CPU {
    fn new() -> Self {
        CPU {
            arf: [0; ARF_SIZE],
            prf: [0; PRF_SIZE],
            pc: 0,
            memory: Memory::new(),
        }
    }

    fn run(&mut self, program: Program) {
        self.arf = [0; ARF_SIZE];
        self.prf = [0; PRF_SIZE];
        self.memory = Memory::new();

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
            let mut next_pipeline = Pipeline::default();

            if let Some(addr) = curr_pipeline.fetch {
                next_pipeline.decode = Some(self.memory.fetch(addr));
            }
            if let Some(instr) = curr_pipeline.decode {
                next_pipeline.execute = Some(decode(instr, &self.arf))
            }
            if let Some(work) = curr_pipeline.execute {
                next_pipeline.writeback = Some(match work {
                    Work::Mem(mem_work) => todo!(),
                    Work::Alu(aluwork) => alu_execute(aluwork),
                    Work::Branch(branch_work) => todo!(),
                })
            }
            if let Some((reg, value)) = curr_pipeline.writeback {
                self.arf[reg] = value;
            }

            self.pc += 1;
            cycles += 1;
        }
    }
}
