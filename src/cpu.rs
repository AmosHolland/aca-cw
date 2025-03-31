use core::panic;
use std::collections::VecDeque;

use crate::alu::{alu_compare, alu_compute};
use crate::debug::RuntimeCommand;
use crate::decoder::{decode, BranchJob, ComputeJob, Job, MemJob, MemOp};
use crate::memory::Memory;
use crate::program::{Instruction, Program};
use crate::{debug, CPUParams, ARF_SIZE};

pub struct Cpu {
    arf: [ExecOperand; ARF_SIZE],
    pc: usize,
    pub memory: Memory,
    debug: bool,
    pipeline: Pipeline,
    params: CPUParams,
}

pub type Address = usize;
pub type Writeback = (usize, ExecOperand);

#[derive(Debug, Clone)]
pub struct ExecutionUnits {
    size: usize,
    units: Vec<Option<(usize, usize)>>,
    free_slots: Vec<usize>,
}

impl ExecutionUnits {
    fn new(size: usize) -> Self {
        let units = vec![None; size];
        let free_slots = Vec::from_iter(0..size);
        ExecutionUnits {
            size,
            units,
            free_slots,
        }
    }

    fn add_work(&mut self, station_id: usize, cycles: usize) {
        let Some(i) = self.free_slots.pop() else {
            panic!("Attempted to add work to full set of execution units.")
        };

        self.units[i] = Some((station_id, cycles))
    }

    fn n_free_slots(&self) -> usize {
        self.free_slots.len()
    }

    fn step_units(&mut self) -> Vec<usize> {
        let mut finished_stations = Vec::new();

        for i in 0..self.size {
            if let Some((id, cycles)) = self.units[i] {
                if cycles == 1 {
                    finished_stations.push(id);
                    self.units[i] = None;
                    self.free_slots.push(i);
                } else {
                    self.units[i] = Some((id, cycles - 1));
                }
            }
        }
        finished_stations
    }
}

impl std::fmt::Display for ExecutionUnits {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut fmt_string = "Execution Units:".to_string();
        for (i, opt) in self.units.iter().enumerate() {
            let str_body = match opt {
                Some((id, cycles)) => {
                    format!("station {id}, {cycles} cycles left")
                }
                None => "".to_string(),
            };
            fmt_string = format!("{fmt_string}\n {i} : {str_body}");
        }

        write!(f, "{fmt_string}")
    }
}

#[derive(Debug, Clone)]
pub struct ReservationStation {
    start_id: usize,
    end_id: usize,
    free_stations: Vec<usize>,
    stations: Vec<Option<(Job, bool, Instruction)>>,
}

impl ReservationStation {
    fn new(start_id: usize, size: usize) -> Self {
        let free_stations = Vec::from_iter(0..size);
        let stations = vec![None; size];

        ReservationStation {
            start_id,
            end_id: start_id + size,
            free_stations,
            stations,
        }
    }

    fn add_job(&mut self, job: Job, instr: Instruction) -> usize {
        let Some(idx) = self.free_stations.pop() else {
            panic!("Tried to push to a full reservation station.")
        };
        self.stations[idx] = Some((job, false, instr));
        idx + self.start_id
    }

    fn get_ready_job(&self) -> Option<usize> {
        for (i, j) in self.stations.iter().enumerate() {
            if let Some((job, busy, ..)) = j {
                if job.is_ready() && !busy {
                    return Some(i + self.start_id);
                }
            }
        }
        None
    }

    fn get_job(&self, id: usize) -> (Job, bool, Instruction) {
        let Some(pair) = self.stations[id - self.start_id] else {
            panic!("Tried to access empty reservation station.")
        };
        pair
    }

    fn start_job(&mut self, id: usize) {
        let Some((job, busy, instr)) = self.stations[id - self.start_id] else {
            panic!("Tried to access empty reservation station.")
        };
        if busy {
            panic!("Attempted to start an already running job")
        }
        self.stations[id - self.start_id] = Some((job, true, instr));
    }

    fn finish_job(&mut self, id: usize) {
        self.stations[id - self.start_id] = None;
        self.free_stations.push(id - self.start_id);
    }

    fn update_operands(&mut self, station_id: usize, value: ExecOperand) {
        for (.., j) in self.stations.iter_mut().enumerate() {
            if let Some((job, ..)) = j {
                job.update_operands(station_id, value);
            }
        }
    }

    fn is_empty(&self) -> bool {
        for job_opt in self.stations.iter() {
            if job_opt.is_some() {
                return false;
            }
        }
        true
    }

    fn is_full(&self) -> bool {
        for job_opt in self.stations.iter() {
            if job_opt.is_none() {
                return false;
            }
        }
        true
    }

    fn id_in_range(&self, id: usize) -> bool {
        id >= self.start_id && id < self.end_id
    }
}

impl std::fmt::Display for ReservationStation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut fmt_string = "Reservation Stations:".to_string();
        for (i, opt) in self.stations.iter().enumerate() {
            let str_body = match opt {
                Some((job, busy, inst)) => {
                    format!("{job} ({inst}) {0}", if *busy { "*" } else { "" })
                }
                None => "".to_string(),
            };
            fmt_string = format!("{fmt_string}\n {0} : {str_body}", i + self.start_id);
        }

        write!(f, "{fmt_string}")
    }
}

#[derive(Clone)]
struct Pipeline {
    next_instruction: Option<Address>,
    branch_target: Option<Address>,
    decode_queue: VecDeque<Instruction>,
    alu_station: ReservationStation,
    alu_eus: ExecutionUnits,
    mem_station: ReservationStation,
    mem_eus: ExecutionUnits,
    jmp_station: ReservationStation,
    jmp_eus: ExecutionUnits,
    writeback: Vec<(Writeback, usize, Instruction)>,
    cdb: Vec<(usize, ExecOperand)>,
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
        self.next_instruction.is_none()
            && self.decode_queue.is_empty()
            && self.writeback.is_empty()
            && self.alu_station.is_empty()
            && self.mem_station.is_empty()
            && self.jmp_station.is_empty()
    }

    fn finish_job(&mut self, station_id: usize) {
        if self.alu_station.id_in_range(station_id) {
            self.alu_station.finish_job(station_id);
        } else if self.mem_station.id_in_range(station_id) {
            self.mem_station.finish_job(station_id);
        } else if self.jmp_station.id_in_range(station_id) {
            self.jmp_station.finish_job(station_id);
        } else {
            panic!("Attempted to stop a job not associated with any reservation station.")
        }
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
        for inst in self.decode_queue.iter() {
            fmt_string = format!("{fmt_string}{inst}\n")
        }

        fmt_string = format!("{fmt_string}\n\n\nALU:\n\n");
        fmt_string = format!("{fmt_string}{0}\n\n", self.alu_station);
        fmt_string = format!("{fmt_string}{0}", self.alu_eus);

        fmt_string = format!("{fmt_string}\n\n\nMemory Unit:\n\n");
        fmt_string = format!("{fmt_string}{0}\n\n", self.mem_station);
        fmt_string = format!("{fmt_string}{0}", self.mem_eus);

        fmt_string = format!("{fmt_string}\n\n\nBranch Unit:\n\n");
        fmt_string = format!("{fmt_string}{0}\n\n", self.jmp_station);
        fmt_string = format!("{fmt_string}{0}", self.jmp_eus);

        fmt_string = format!("{fmt_string}\n\n\nWriteback:\n");
        for ((r, val), _, inst) in self.writeback.iter() {
            fmt_string = format!("{fmt_string}R{r} = {val} ({inst}) \n")
        }

        write!(f, "{fmt_string}")
    }
}

impl Cpu {
    pub fn new(debug: bool, params: CPUParams) -> Self {
        let alu_station = ReservationStation::new(0, params.alu_res);
        let mem_station = ReservationStation::new(params.alu_res, params.mem_res);
        let jmp_station = ReservationStation::new(params.alu_res + params.mem_res, params.jmp_res);

        let init_pipeline = Pipeline {
            next_instruction: Some(0),
            branch_target: None,
            decode_queue: VecDeque::new(),
            alu_station,
            alu_eus: ExecutionUnits::new(params.alu_n),
            mem_station,
            mem_eus: ExecutionUnits::new(params.mem_n),
            jmp_station,
            jmp_eus: ExecutionUnits::new(params.jmp_n),
            writeback: Vec::new(),
            cdb: Vec::new(),
        };

        Cpu {
            arf: [ExecOperand::Value(0); ARF_SIZE],
            pc: 0,
            memory: Memory::new(),
            debug,
            pipeline: init_pipeline,
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
        self.arf = [ExecOperand::Value(0); ARF_SIZE];

        self.memory.load_program(program);

        let mut cycles = 0;

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
        let mut pipeline = self.pipeline.clone();
        let mut next_arf = self.arf;

        while let Some(((r, v), station_id, _)) = pipeline.writeback.pop() {
            match next_arf[r] {
                ExecOperand::Value(..) => next_arf[r] = v,
                ExecOperand::Ref(id) => {
                    if id == station_id {
                        next_arf[r] = v
                    }
                }
            }
            pipeline.cdb.push((station_id, v));
            pipeline.finish_job(station_id);
        }

        let mut eu_types = [
            (&mut pipeline.alu_station, &mut pipeline.alu_eus),
            (&mut pipeline.mem_station, &mut pipeline.mem_eus),
            (&mut pipeline.jmp_station, &mut pipeline.jmp_eus),
        ];

        for (station, eus) in &mut eu_types {
            for _ in 0..eus.n_free_slots() {
                if let Some(station_id) = station.get_ready_job() {
                    let (.., instr) = station.get_job(station_id);
                    eus.add_work(station_id, instr.n_cycles());
                    station.start_job(station_id);
                }
            }

            let finished_ids = eus.step_units();

            for id in finished_ids {
                let (job, _, instr) = station.get_job(id);
                let wb = match job {
                    Job::Mem(mem_job) => self.handle_mem_work(mem_job),
                    Job::Compute(compute_job) => Some(self.handle_compute_work(compute_job)),
                    Job::Branch(branch_job) => {
                        self.handle_branch_work(branch_job);
                        pipeline.branch_target = Some(self.pc);
                        None
                    }
                };

                if let Some((r, val)) = wb {
                    pipeline.writeback.push(((r, val), id, instr));
                } else {
                    station.finish_job(id);
                }
            }
        }

        if let Some(inst) = pipeline.decode_queue.pop_front() {
            let job = decode(inst, &self.arf);
            let station = match job {
                Job::Mem(..) => &mut pipeline.mem_station,
                Job::Compute(..) => &mut pipeline.alu_station,
                Job::Branch(..) => &mut pipeline.jmp_station,
            };

            if !station.is_full() {
                let station_id = station.add_job(job, inst);
                if let Some(r) = inst.get_write_reg() {
                    next_arf[r] = ExecOperand::Ref(station_id);
                }
            } else {
                pipeline.decode_queue.push_front(inst);
            }
        }

        if let Some(addr) = pipeline.next_instruction {
            if pipeline.decode_queue.len() >= self.params.decode_buff_n {
                pipeline.next_instruction = Some(addr);
            } else {
                pipeline.next_instruction = None;
                let next_instr = self.memory.fetch(self.pc);
                if let Some(instr) = next_instr {
                    pipeline.decode_queue.push_back(instr);
                    if !instr.is_branch() {
                        self.pc += 1;
                        pipeline.next_instruction = Some(self.pc);
                    }
                }
            }
        }

        if pipeline.branch_target.is_some() {
            pipeline.next_instruction = pipeline.branch_target;
            pipeline.branch_target = None;
        }

        while let Some((station_id, value)) = pipeline.cdb.pop() {
            pipeline.alu_station.update_operands(station_id, value);
            pipeline.mem_station.update_operands(station_id, value);
            pipeline.jmp_station.update_operands(station_id, value);
        }

        self.arf = next_arf;
        pipeline
    }

    fn handle_compute_work(&self, work: ComputeJob) -> Writeback {
        let x = work.x.as_int();
        let y = work.y.as_int();
        (
            work.dest,
            ExecOperand::Value(alu_compute(work.operation, x, y)),
        )
    }

    fn handle_branch_work(&mut self, work: BranchJob) {
        let t = work.t.as_usize();

        let branch = match work.operation {
            None => true,
            Some(op) => {
                let x = work
                    .x
                    .expect("Conditional branch without operands")
                    .as_int();
                let y = work
                    .y
                    .expect("Conditional branch without operands")
                    .as_int();
                alu_compare(op, x, y)
            }
        };

        if branch {
            self.pc = t;
        } else {
            self.pc += 1;
        };
    }

    fn handle_mem_work(&mut self, work: MemJob) -> Option<Writeback> {
        let base = match work.base {
            Some(opr) => opr.as_usize(),
            None => 0,
        };
        let index = work.index.as_usize();

        let address = base + index;

        match work.operation {
            MemOp::Load => {
                let value = self.memory.load(address);
                Some((
                    work.dst.expect("no destination on memory load"),
                    ExecOperand::Value(value),
                ))
            }
            MemOp::Store => {
                let value = work.src.expect("no source on memory store");
                self.memory.store(address, value.as_int());
                None
            }
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
