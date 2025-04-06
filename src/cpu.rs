use core::panic;
use std::collections::{HashMap, VecDeque};

use crate::alu::{alu_compare, alu_compute};
use crate::debug::RuntimeCommand;
use crate::decoder::{decode, BranchJob, ComputeJob, Destination, Job, MemJob, MemOp};
use crate::execution_units::ExecutionUnits;
use crate::memory::Memory;
use crate::program::{Instruction, InstructionType, Program};
use crate::reservation_station::ReservationStation;
use crate::rob::{BasicROBEntry, BranchROBEntry, ROBEntry, ReorderBuffer};
use crate::{debug, CPUParams, ARF_SIZE};

pub type Address = usize;

// TODO
// - stop copying pipelines, implement a CPU wide flush?
// - we could also split some of our cycle execution into different functions

pub struct Cpu {
    arf: [i32; ARF_SIZE],
    arf_flags: [Option<usize>; ARF_SIZE],
    pc: usize,
    pub memory: Memory,
    debug: bool,
    station_map: HashMap<InstructionType, usize>,
    pipeline: Pipeline,
    params: CPUParams,
}

#[derive(Clone)]
struct Pipeline {
    next_instruction: Option<Address>,
    decode_queue: VecDeque<(usize, Instruction)>,
    res_stations: Vec<ReservationStation>,
    eus: Vec<ExecutionUnits>,
    write_result: Vec<(usize, i32)>,
    rob: ReorderBuffer,
}

#[derive(Clone, Copy, Debug)]
pub enum ExecOperand {
    Value(i32),
    Ref(usize),
}

impl ExecOperand {
    pub fn is_ref_to_id(&self, station_id: usize) -> bool {
        match self {
            Self::Value(..) => false,
            Self::Ref(id) => *id == station_id,
        }
    }

    fn as_int(&self) -> i32 {
        match self {
            ExecOperand::Value(i) => *i,
            ExecOperand::Ref(..) => panic!("Attempted to compute with an unresolved operand."),
        }
    }

    fn as_usize(&self) -> usize {
        self.as_int()
            .try_into()
            .expect("Invalid number given as address.")
    }
}

impl std::fmt::Display for ExecOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExecOperand::Value(i) => write!(f, "{i}"),
            ExecOperand::Ref(n) => write!(f, "Ref({n})"),
        }
    }
}

impl Pipeline {
    fn is_empty(&self) -> bool {
        for station in self.res_stations.iter() {
            if !station.is_empty() {
                return false;
            }
        }
        self.decode_queue.is_empty()
            && self.next_instruction.is_none()
            && self.rob.is_empty()
            && self.write_result.is_empty()
    }

    fn clear(&mut self) {
        self.next_instruction = None;

        self.decode_queue = VecDeque::new();

        for station in self.res_stations.iter_mut() {
            station.clear();
        }

        for eu in self.eus.iter_mut() {
            eu.clear();
        }

        self.rob.clear();

        self.write_result = Vec::new();
    }
}

impl std::fmt::Display for Pipeline {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut fmt_string = "".to_string();

        fmt_string = format!(
            "{fmt_string}Next Instruction: {0:?}\n",
            self.next_instruction
        );

        fmt_string = format!("{fmt_string}\n\n\nDecode Queue:\n",);
        for (addr, inst) in self.decode_queue.iter() {
            fmt_string = format!("{fmt_string}{addr}: {inst}\n")
        }

        fmt_string = format!("{fmt_string}\n\n\nReorder Buffer:\n",);
        for (i, entry_opt) in self.rob.entries.iter().enumerate() {
            match entry_opt {
                None => fmt_string = format!("{fmt_string}\n{i}: None"),
                Some(entry) => fmt_string = format!("{fmt_string}\n{i}: {entry}"),
            }
        }

        for (i, station) in self.res_stations.iter().enumerate() {
            fmt_string = format!("{fmt_string}\n\n\nReservation station #{i}:\n");
            fmt_string = format!("{fmt_string}{station}\n")
        }

        fmt_string = format!("{fmt_string}\n\n\nALU:\n\n");
        fmt_string = format!("{fmt_string}{0}", self.eus[0]);

        fmt_string = format!("{fmt_string}\n\n\nMemory Unit:\n\n");
        fmt_string = format!("{fmt_string}{0}", self.eus[1]);

        fmt_string = format!("{fmt_string}\n\n\nBranch Unit:\n\n");
        fmt_string = format!("{fmt_string}{0}", self.eus[2]);

        for (rob_id, value) in self.write_result.iter() {
            fmt_string = format!("{fmt_string}ROB[{rob_id}].value = {value} \n")
        }

        write!(f, "{fmt_string}")
    }
}

impl Cpu {
    pub fn new(debug: bool, params: CPUParams) -> Self {
        let mut res_stations = vec![ReservationStation::new(1); params.res_params.n_stations];
        for (i, size) in params.res_params.station_sizes.iter().enumerate() {
            res_stations[i] = ReservationStation::new(*size)
        }

        let eus = vec![
            ExecutionUnits::new(params.alu_n, InstructionType::Alu),
            ExecutionUnits::new(params.mem_n, InstructionType::Mem),
            ExecutionUnits::new(params.jmp_n, InstructionType::Jmp),
        ];

        let mut station_map = HashMap::new();
        station_map.insert(InstructionType::Alu, params.res_params.alu_station);
        station_map.insert(InstructionType::Mem, params.res_params.mem_station);
        station_map.insert(InstructionType::Jmp, params.res_params.jmp_station);

        let pipeline = Pipeline {
            next_instruction: Some(0),
            decode_queue: VecDeque::new(),
            res_stations,
            eus,
            write_result: Vec::new(),
            rob: ReorderBuffer::new(params.rob_size),
        };

        Cpu {
            arf: [0; ARF_SIZE],
            arf_flags: [None; ARF_SIZE],
            pc: 0,
            memory: Memory::new(),
            debug,
            station_map,
            pipeline,
            params,
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
        let _ = rl.save_history("runtime_history.txt");
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

        self.memory.load_program(program);

        let mut cycles = 0;

        while !self.pipeline.is_empty() {
            if self.debug {
                println!("Pipeline before cycle {0}:", cycles + 1);
                println!("{0}", self.pipeline);
                println!("PC Value: {0}\n", self.pc);
                self.debug_repl();
            }
            self.pipeline = self.run_cycle();
            cycles += 1;
        }

        println!("Program run in {cycles} cycles.");
        self.display_reg_state();
    }

    fn run_cycle(&mut self) -> Pipeline {
        // Set up temporary state.
        let mut pipeline = self.pipeline.clone();

        let mut next_arf = self.arf;
        let mut next_arf_flags = self.arf_flags;

        let mut cdb = Vec::new();
        let mut rob_updates = Vec::new();

        // Write-result (defers to CDB so changes don't take affect until next cycle)
        while let Some((rob_id, result)) = pipeline.write_result.pop() {
            cdb.push((rob_id, result));
        }

        // Execute - collect any new jobs, resolve any complete jobs
        // new jobs are stpped in the same cycle to simulate forwarding
        for eus in &mut pipeline.eus.iter_mut() {
            let station_id = self.station_map[&eus.get_type()];
            let station = &mut pipeline.res_stations[station_id];

            for _ in 0..eus.n_free_slots() {
                if let Some(id) = station.get_ready_job(eus.get_type()) {
                    let slot = station.get_slot(id);
                    eus.add_work(id, slot.inst.n_cycles());
                    station.start_job(id);
                }
            }

            let finished_ids = eus.step_units();

            for id in finished_ids {
                let slot = station.get_slot(id);
                station.finish_job(id);

                let rob_id = slot.rob_id;

                let wb = match slot.job {
                    Job::Mem(job) => {
                        let ROBEntry::Basic(entry) = pipeline.rob.get_entry(rob_id) else {
                            panic!("Mem instruction has non-basic ROB entry.");
                        };
                        let (value, new_entry_opt) = self.handle_mem_job(job, entry);
                        if let Some(new_entry) = new_entry_opt {
                            rob_updates.push((rob_id, ROBEntry::Basic(new_entry)));
                        }
                        Some(value)
                    }
                    Job::Compute(job) => Some(self.handle_compute_job(job)),
                    Job::Branch(job) => {
                        let ROBEntry::Branch(entry) = pipeline.rob.get_entry(rob_id) else {
                            panic!("Branch instruction has non-branch ROB entry.");
                        };
                        rob_updates
                            .push((rob_id, ROBEntry::Branch(self.handle_branch_job(job, entry))));
                        None
                    }
                };

                if let Some(val) = wb {
                    pipeline.write_result.push((rob_id, val));
                }
            }
        }

        // Decode - if there's room for the next instruction to execute then push it down the pipeline
        // this involves adding it to reservation stations and the ROB
        if let Some((i_addr, inst)) = pipeline.decode_queue.pop_front() {
            let job = decode(inst, i_addr, &self.arf, &self.arf_flags, &pipeline.rob);
            let job_type = inst.get_type();
            let station = &mut pipeline.res_stations[self.station_map[&job_type]];

            if !station.is_full() && !pipeline.rob.is_full() {
                let entry = match job {
                    Job::Mem(mem_job) => {
                        ROBEntry::Basic(BasicROBEntry::new(inst, mem_job.try_get_destination()))
                    }
                    Job::Compute(compute_job) => ROBEntry::Basic(BasicROBEntry::new(
                        inst,
                        Some(compute_job.get_destination()),
                    )),
                    Job::Branch(_) => ROBEntry::Branch(BranchROBEntry {
                        instruction: inst,
                        ready: false,
                        predicted_taken: false,
                        taken: None,
                        real_target: None,
                    }),
                };

                let rob_id = pipeline.rob.push_back(entry);

                station.add_job(job, inst, rob_id);
                if let Some(r) = inst.get_write_reg() {
                    next_arf_flags[r] = Some(rob_id);
                }
            } else {
                pipeline.decode_queue.push_front((i_addr, inst));
            }
        }

        // Fetch - grab the next instruction, push it to decode queue if it can, and increment PC
        // Currently this is doing assume not taken branch prediction, will need to be modified when we get proper prediction
        // (as this will be the point where predicitons are made)
        if let Some(addr) = pipeline.next_instruction {
            if pipeline.decode_queue.len() >= self.params.decode_buff_n {
                pipeline.next_instruction = Some(addr);
            } else {
                pipeline.next_instruction = None;
                let next_instr = self.memory.fetch(addr);
                if let Some(instr) = next_instr {
                    pipeline.decode_queue.push_back((addr, instr));
                    self.pc += 1;
                    pipeline.next_instruction = Some(self.pc);
                }
            }
        }

        // Commit stage - examine front ROB entry, if it's ready commit it.
        // Keep going with commits until we encounter an instruction that is not ready.
        loop {
            let Some((entry, id)) = pipeline.rob.peek_front() else {
                break;
            };

            if !entry.is_ready() {
                break;
            }

            match entry {
                ROBEntry::Branch(entry) => {
                    let target = entry
                        .real_target
                        .expect("Should have a confirmed target to be ready.");

                    let taken = entry
                        .taken
                        .expect("Should know if taken or not to be ready.");

                    if entry.predicted_taken != taken {
                        pipeline.clear();
                        next_arf_flags = [None; ARF_SIZE];
                        cdb = Vec::new();
                        rob_updates = Vec::new();
                        pipeline.next_instruction = Some(target);
                        self.pc = target;
                        break;
                    } else {
                        pipeline.rob.pop_front();
                    }
                }
                ROBEntry::Basic(entry) => {
                    let value = entry
                        .value
                        .expect("Should have a confirmed target to be ready.");
                    let destination = entry
                        .destination
                        .expect("Should have confirmed destination to be ready.");
                    match destination {
                        Destination::Reg(r) => {
                            next_arf[r] = value;
                            if let Some(tag) = next_arf_flags[r] {
                                if tag == id {
                                    next_arf_flags[r] = None
                                }
                            }
                        }
                        Destination::Mem(addr) => {
                            self.memory.store(addr, value);
                        }
                    }
                    pipeline.rob.pop_front();
                }
            }
        }

        while let Some((rob_id, value)) = cdb.pop() {
            pipeline.rob.update_value(rob_id, value);
            for station in pipeline.res_stations.iter_mut() {
                station.update_operands(rob_id, value);
            }
        }

        while let Some((rob_id, entry)) = rob_updates.pop() {
            pipeline.rob.update_entry(rob_id, entry);
        }

        self.arf = next_arf;
        self.arf_flags = next_arf_flags;
        pipeline
    }

    fn handle_compute_job(&self, job: ComputeJob) -> i32 {
        let x = job.x.as_int();
        let y = job.y.as_int();
        alu_compute(job.operation, x, y)
    }

    fn handle_branch_job(&self, job: BranchJob, mut entry: BranchROBEntry) -> BranchROBEntry {
        let taken = match job.operation {
            None => true,
            Some(op) => {
                let x = job
                    .x
                    .expect("Conditional x`branch without operands")
                    .as_int();
                let y = job.y.expect("Conditional branch without operands").as_int();
                alu_compare(op, x, y)
            }
        };

        entry.ready = true;
        entry.taken = Some(taken);
        if taken {
            entry.real_target = Some(job.t_taken.as_usize());
        } else {
            entry.real_target = Some(job.t_untaken);
        }

        entry
    }

    fn handle_mem_job(
        &mut self,
        job: MemJob,
        mut entry: BasicROBEntry,
    ) -> (i32, Option<BasicROBEntry>) {
        let base = match job.base {
            Some(opr) => opr.as_usize(),
            None => 0,
        };
        let index = job.index.as_usize();

        let address = base + index;

        match job.operation {
            MemOp::Load => {
                let value = self.memory.load(address);
                (value, None)
            }
            MemOp::Store => {
                let operand = job.src.expect("no source on memory store");
                let ExecOperand::Value(value) = operand else {
                    panic!("Executed an instruction with unresolved operands")
                };

                entry.destination = Some(Destination::Mem(address));
                (value, Some(entry))
            }
        }
    }

    fn display_reg_state(&self) {
        for (i, r) in self.arf.iter().enumerate() {
            println!(
                "R{i}: {r}, {0}",
                if let Some(tag) = self.arf_flags[i] {
                    format!("{tag}")
                } else {
                    "None".to_string()
                }
            );
        }
    }

    fn display_memory(&self, start: usize, end: usize) {
        for i in start..end {
            println!("M[{i}] : {0}", self.memory.load(i));
        }
    }
}
