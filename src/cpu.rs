use crate::alu::{alu_compare, alu_compute};
use crate::debug::RuntimeCommand;
use crate::decoder::{decode, BranchJob, ComputeJob, Job, MemJob, MemOp, Work};
use crate::memory::Memory;
use crate::program::{Instruction, Operand, Program, Value};
use crate::{debug, ARF_SIZE, PRF_SIZE};

pub struct Cpu {
    arf: [i32; ARF_SIZE],
    prf: [i32; PRF_SIZE],
    pc: usize,
    pub memory: Memory,
    debug: bool,
    pipeline: Pipeline,
}

pub type Address = usize;
pub type Writeback = (usize, i32);

#[derive(Default, Clone, Copy)]
struct Pipeline {
    fetch: Option<Address>,
    decode: Option<Instruction>,
    execute: Option<(Work, Instruction)>,
    writeback: Option<(Writeback, Instruction)>,
}

impl Pipeline {
    fn is_empty(&self) -> bool {
        self.fetch.is_none()
            && self.decode.is_none()
            && self.execute.is_none()
            && self.writeback.is_none()
    }
}

impl std::fmt::Display for Pipeline {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fetch_text = if let Some(pc) = self.fetch {
            format!("instruction {pc}")
        } else {
            "None".to_string()
        };

        let decode_text = if let Some(inst) = self.decode {
            format!("{inst}")
        } else {
            "None".to_string()
        };

        let execute_text = if let Some((work, inst)) = self.execute {
            format!("{inst}, {0} cycle(s) left", work.cycles)
        } else {
            "None".to_string()
        };

        let wb_text = if let Some(((reg, val), inst)) = self.writeback {
            format!("{val} to {reg}, result of {inst}")
        } else {
            "None".to_string()
        };

        write!(
            f,
            "Fetch     : {fetch_text}\nDecode    : {decode_text}\nExecute   : {execute_text}\nWriteback : {wb_text}\n",
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
            pipeline: Pipeline::default(),
        }
    }

    fn debug_repl(&self) {
        let mut rl = rustyline::DefaultEditor::new().expect("");
        if rl.load_history("runtime_history.txt").is_err() {
            println!("No previous history.");
        }

        loop {
            let readline = rl.readline(">> ");
            match readline {
                Ok(line) => {
                    let _ = rl.add_history_entry(line.as_str());
                    let parsed_line = debug::parse_runtime_command_string(line);

                    match parsed_line {
                        Err(err) => println!("Invalid command: {err}"),
                        Ok(command) => {
                            self.debug_command_output(command);
                            if let RuntimeCommand::Step = command {
                                break;
                            }
                        }
                    }
                }
                _ => panic!("Program halted prematurely."),
            }
        }
        let _ = rl.save_history("history.txt");
    }

    fn debug_command_output(&self, command: RuntimeCommand) {
        match command {
            RuntimeCommand::ShowRegisters => self.display_reg_state(),
            RuntimeCommand::ShowRegister(n) => println!("R{n} : {0}", self.arf[n]),
            RuntimeCommand::ShowMemory((n1, n2)) => self.display_memory(n1, n2),
            RuntimeCommand::ShowPipeline => println!("Current pipeline: \n{0}", self.pipeline),
            RuntimeCommand::ShowPC => println!("PC : {0}", self.pc),
            _ => (),
        }
    }

    pub fn run(&mut self, program: Program) {
        self.arf = [0; ARF_SIZE];
        self.prf = [0; PRF_SIZE];

        self.memory.load_program(program);

        let mut cycles = 0;

        self.pipeline = Pipeline {
            fetch: Some(0),
            decode: None,
            execute: None,
            writeback: None,
        };

        while !self.pipeline.is_empty() {
            if self.debug {
                println!("Pipeline before cycle {0}:", cycles + 1);
                println!("{0}", self.pipeline);
                println!("PC Value: {0}\n", self.pc);
                self.display_reg_state();
                self.debug_repl();
            }
            self.pipeline = self.run_cycle();
            cycles += 1;
        }

        println!("Program run in {cycles} cycles.");
        self.display_reg_state();
    }

    fn run_cycle(&mut self) -> Pipeline {
        let start_pipeline = self.pipeline;
        let mut next_pipeline = Pipeline::default();
        let mut jumped = false;

        if let Some((work, inst)) = start_pipeline.execute {
            if work.cycles == 1 {
                next_pipeline.writeback = match work.job {
                    Job::Mem(mem_work) => {
                        let wb_opt = self.handle_mem_work(mem_work);
                        wb_opt.map(|wb| (wb, inst))
                    }
                    Job::Compute(comp_work) => Some((self.handle_compute_work(comp_work), inst)),
                    Job::Branch(branch_work) => {
                        jumped = self.handle_branch_work(branch_work);
                        None
                    }
                }
            } else {
                next_pipeline.execute = Some((
                    Work {
                        cycles: work.cycles - 1,
                        job: work.job,
                    },
                    inst,
                ))
            }
        }

        if let Some(instr) = start_pipeline.decode {
            if next_pipeline.execute.is_some() {
                next_pipeline.decode = Some(instr)
            } else if let Some((_, instr1)) = next_pipeline.writeback {
                if instr1.blocks(&instr) {
                    next_pipeline.decode = Some(instr)
                } else {
                    next_pipeline.execute = Some((decode(instr), instr))
                }
            } else {
                next_pipeline.execute = Some((decode(instr), instr))
            }
        }

        if let Some(addr) = start_pipeline.fetch {
            if next_pipeline.decode.is_some() {
                next_pipeline.fetch = Some(addr);
            } else if !jumped {
                next_pipeline.decode = self.memory.fetch(self.pc);
                if next_pipeline.decode.is_some() {
                    self.pc += 1;
                    next_pipeline.fetch = Some(self.pc);
                }
            }
        }

        if let Some(((reg, value), _)) = start_pipeline.writeback {
            self.arf[reg] = value;
        };

        if jumped {
            next_pipeline = Pipeline {
                fetch: Some(self.pc),
                decode: None,
                execute: None,
                writeback: None,
            }
        }

        next_pipeline
    }

    fn handle_compute_work(&self, work: ComputeJob) -> Writeback {
        let x = self.evaluate_compute_operand(work.x);
        let y = self.evaluate_compute_operand(work.y);
        (work.dest, alu_compute(work.operation, x, y))
    }

    fn handle_branch_work(&mut self, work: BranchJob) -> bool {
        let t = self.evaluate_address_operand(work.t);

        let branch = match work.operation {
            None => true,
            Some(op) => {
                let x = self
                    .evaluate_compute_operand(work.x.expect("Conditional branch without operands"));
                let y = self
                    .evaluate_compute_operand(work.y.expect("Conditional branch without operands"));
                alu_compare(op, x, y)
            }
        };

        if branch {
            self.pc = t;
        };

        branch
    }

    fn handle_mem_work(&mut self, work: MemJob) -> Option<Writeback> {
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
                let Value::Int(i) = imm_opr.value;
                i
            }
        }
    }

    fn evaluate_address_operand(&self, opr: Operand) -> usize {
        match opr {
            Operand::Reg(reg_opr) => self.arf[reg_opr.reg_num]
                .try_into()
                .expect("Invalid int given as address"),
            Operand::Imm(imm_opr) => match imm_opr.value {
                Value::Int(i) => i.try_into().expect("Invalid int given as address"),
            },
        }
    }

    fn display_reg_state(&self) {
        for (i, r) in self.arf.iter().enumerate() {
            println!("R{i}: {r}");
        }
    }

    fn display_memory(&self, start: usize, end: usize) {
        for i in start..end {
            println!("M[{i}] : {0}", self.memory.load(i));
        }
    }
}
