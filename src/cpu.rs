use core::{panic, task};
use std::collections::{HashMap, VecDeque};
use std::mem::{self, take};

use crate::alu::{alu_compare, alu_compute};
use crate::debug::RuntimeCommand;
use crate::decoder::{decode, BranchJob, ComputeJob, Destination, Job, MemJob, MemOp};
use crate::memory::{Memory, MEM_SIZE};
use crate::program::{Instruction, InstructionType, Program};
use crate::{debug, CPUParams, ARF_SIZE};

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

pub type Address = usize;
pub type Writeback = (usize, ExecOperand);

#[derive(Debug, Clone, Copy)]
pub enum ROBEntry {
    Branch(BranchROBEntry),
    Basic(BasicROBEntry),
}

impl ROBEntry {
    fn is_ready(&self) -> bool {
        match self {
            ROBEntry::Branch(e) => e.ready,
            ROBEntry::Basic(e) => e.ready,
        }
    }

    fn get_instruction(&self) -> Instruction {
        match self {
            ROBEntry::Branch(e) => e.instruction,
            ROBEntry::Basic(e) => e.instruction,
        }
    }

    fn get_value(&self) -> Option<i32> {
        match self {
            ROBEntry::Branch(_) => panic!("Tried to access value of a branch entry."),
            ROBEntry::Basic(entry) => entry.value,
        }
    }

    fn give_value(&self, value: i32) -> Result<ROBEntry, String> {
        match self {
            ROBEntry::Branch(_) => Err("Cannot update branch entry value.".to_string()),
            ROBEntry::Basic(entry) => {
                let mut new_entry = entry.clone();
                new_entry.value = Some(value);
                new_entry.ready = true;
                Ok(ROBEntry::Basic(new_entry))
            }
        }
    }
}

impl std::fmt::Display for ROBEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ROBEntry::Branch(entry) => write!(f, "{entry}"),
            ROBEntry::Basic(entry) => write!(f, "{entry}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BranchROBEntry {
    instruction: Instruction,
    ready: bool,
    predicted_taken: bool,
    taken: Option<bool>,
    real_target: Option<usize>,
}

impl std::fmt::Display for BranchROBEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let taken = if let Some(bool) = self.taken {
            format!("{bool}")
        } else {
            format!("None")
        };

        let target = if let Some(pc) = self.real_target {
            format!("{pc}")
        } else {
            format!("None")
        };

        write!(
            f,
            "inst: {0}, ready: {1}, predicted taken: {2}, taken: {taken}, real target: {target}",
            self.instruction, self.ready, self.predicted_taken
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BasicROBEntry {
    instruction: Instruction,
    ready: bool,
    value: Option<i32>,
    destination: Option<Destination>,
}

impl std::fmt::Display for BasicROBEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let destination = if let Some(dest) = self.destination {
            format!("{dest}")
        } else {
            format!("None")
        };

        let value = if let Some(value) = self.value {
            format!("{value}")
        } else {
            format!("None")
        };

        write!(
            f,
            "inst: {0}, ready: {1}, destination: {destination}, value: {value}",
            self.instruction, self.ready
        )
    }
}

impl BasicROBEntry {
    fn new(inst: Instruction, dest: Option<Destination>) -> Self {
        BasicROBEntry {
            instruction: inst,
            ready: false,
            value: None,
            destination: dest,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ROB {
    entries: Vec<Option<ROBEntry>>,
    head: usize,
    tail: usize,
    size: usize,
    empty: bool,
}

impl ROB {
    fn new(size: usize) -> Self {
        let entries = vec![None; size];

        ROB {
            entries,
            head: 0,
            tail: 0,
            size,
            empty: true,
        }
    }

    fn push_back(&mut self, entry: ROBEntry) -> usize {
        if self.head == self.tail && !self.empty {
            panic!("Tried to push to a full ROB")
        }

        let id = self.tail;

        self.entries[id] = Some(entry);
        self.tail = (self.tail + 1) % self.size;
        self.empty = false;

        id
    }

    fn pop_front(&mut self) -> (ROBEntry, usize) {
        if self.empty {
            panic!("Tried to pop from an empty ROB");
        };

        let entry = self.entries[self.head];
        let tag = self.head;

        self.entries[self.head] = None;
        self.head = (self.head + 1) % self.size;
        self.empty = self.head == self.tail;

        (entry.expect(""), tag)
    }

    fn peek_front(&mut self) -> Option<(ROBEntry, usize)> {
        match self.entries[self.head] {
            None => None,
            Some(entry) => Some((entry, self.head)),
        }
    }

    fn is_empty(&self) -> bool {
        self.empty
    }

    fn is_full(&self) -> bool {
        self.head == self.tail && !self.empty
    }

    fn get_entry(&self, id: usize) -> ROBEntry {
        let Some(entry) = self.entries[id] else {
            panic!("Tried to get from empty rob entry")
        };

        entry
    }

    fn clear(&mut self) {
        for i in 0..self.size {
            self.entries[i] = None;
        }

        self.empty = true;
        self.head = 0;
        self.tail = 0;
    }

    fn update_value(&mut self, rob_id: usize, value: i32) {
        let Some(entry) = self.entries[rob_id] else {
            panic!("Tried to update empty rob entry");
        };

        match entry.give_value(value) {
            Ok(new_entry) => self.entries[rob_id] = Some(new_entry),
            Err(err) => panic!("{err}"),
        }
    }

    fn update_entry(&mut self, rob_id: usize, entry: ROBEntry) {
        if self.entries[rob_id].is_none() {
            panic!("Cannot update an empty entry {rob_id}.");
        }

        self.entries[rob_id] = Some(entry);
    }

    pub fn get_value(&self, rob_id: usize) -> Option<i32> {
        let Some(entry) = self.entries[rob_id] else {
            panic!("Tried to access empty entry {rob_id}.")
        };

        entry.get_value()
    }
}

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

    fn clear(&mut self) {
        self.units = vec![None; self.size];
        self.free_slots = Vec::from_iter(0..self.size);
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

#[derive(Debug, Clone, Copy)]
pub struct ReservationSlot {
    job: Job,
    running: bool,
    inst: Instruction,
}

impl ReservationSlot {
    fn new(job: Job, inst: Instruction) -> Self {
        ReservationSlot {
            job,
            running: false,
            inst,
        }
    }

    fn is_ready(&self) -> bool {
        !self.running && self.job.is_ready()
    }
}

#[derive(Debug, Clone)]
pub struct ReservationStation {
    size: usize,
    stations: HashMap<usize, ReservationSlot>,
}

impl ReservationStation {
    fn new(size: usize) -> Self {
        let stations = HashMap::new();

        ReservationStation { size, stations }
    }

    fn add_job(&mut self, job: Job, inst: Instruction, rob_id: usize) {
        if self.stations.len() == self.size {
            panic!("Tried to push to a full reservation station.")
        };
        self.stations
            .insert(rob_id, ReservationSlot::new(job, inst));
    }

    fn get_ready_job(&self) -> Option<usize> {
        for (rob_id, slot) in self.stations.iter() {
            if slot.is_ready() {
                return Some(*rob_id);
            }
        }
        None
    }

    fn get_job(&self, rob_id: usize) -> ReservationSlot {
        let Some(slot) = self.stations.get(&rob_id) else {
            panic!("Tried to access job not in reservation station.")
        };

        *slot
    }

    fn start_job(&mut self, rob_id: usize) {
        let Some(slot) = self.stations.get(&rob_id) else {
            panic!("Tried to access empty reservation station.")
        };
        if slot.running {
            panic!("Attempted to start an already running job")
        }
        self.stations.insert(
            rob_id,
            ReservationSlot {
                job: slot.job,
                running: true,
                inst: slot.inst,
            },
        );
    }

    fn finish_job(&mut self, rob_id: usize) {
        self.stations.remove(&rob_id);
    }

    fn update_operands(&mut self, tag: usize, value: i32) {
        for (_, slot) in self.stations.iter_mut() {
            slot.job.update_operands(tag, value);
        }
    }

    fn is_empty(&self) -> bool {
        self.stations.len() == 0
    }

    fn is_full(&self) -> bool {
        self.stations.len() == self.size
    }

    fn id_in_range(&self, id: usize) -> bool {
        self.stations.contains_key(&id)
    }

    fn clear(&mut self) {
        self.stations = HashMap::new();
    }
}

impl std::fmt::Display for ReservationStation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut fmt_string = "Reservation Stations:".to_string();
        for (i, slot) in self.stations.iter() {
            fmt_string = format!(
                "{fmt_string}\n {i} : {0} ({1}) {2}",
                slot.job,
                slot.inst,
                if slot.running { "*" } else { "" }
            );
        }

        write!(f, "{fmt_string}")
    }
}

#[derive(Clone)]
struct Pipeline {
    next_instruction: Option<Address>,
    decode_queue: VecDeque<(usize, Instruction)>,
    res_stations: Vec<ReservationStation>,
    alu_eus: ExecutionUnits,
    mem_eus: ExecutionUnits,
    jmp_eus: ExecutionUnits,
    write_result: Vec<(usize, i32)>,
    rob: ROB,
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

        self.alu_eus.clear();
        self.jmp_eus.clear();
        self.mem_eus.clear();

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
        fmt_string = format!("{fmt_string}{0}", self.alu_eus);

        fmt_string = format!("{fmt_string}\n\n\nMemory Unit:\n\n");
        fmt_string = format!("{fmt_string}{0}", self.mem_eus);

        fmt_string = format!("{fmt_string}\n\n\nBranch Unit:\n\n");
        fmt_string = format!("{fmt_string}{0}", self.jmp_eus);

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

        let mut station_map = HashMap::new();
        station_map.insert(InstructionType::Alu, params.res_params.alu_station);
        station_map.insert(InstructionType::Mem, params.res_params.mem_station);
        station_map.insert(InstructionType::Jmp, params.res_params.jmp_station);

        let pipeline = Pipeline {
            next_instruction: Some(0),
            decode_queue: VecDeque::new(),
            res_stations,
            alu_eus: ExecutionUnits::new(params.alu_n),
            mem_eus: ExecutionUnits::new(params.mem_n),
            jmp_eus: ExecutionUnits::new(params.jmp_n),
            write_result: Vec::new(),
            rob: ROB::new(params.rob_size),
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
        let mut pipeline = self.pipeline.clone();

        let mut next_arf = self.arf;
        let mut next_arf_flags = self.arf_flags;

        let mut cdb = Vec::new();
        let mut rob_updates = Vec::new();

        //TODO
        // - Move branch not taken target out of ROB into Job
        // - Then test and debug IG

        while let Some((rob_id, result)) = pipeline.write_result.pop() {
            cdb.push((rob_id, result));

            let entry = pipeline.rob.get_entry(rob_id);

            pipeline.res_stations[self.station_map[&entry.get_instruction().get_type()]]
                .finish_job(rob_id);
        }

        let mut eu_types = [
            (self.params.res_params.alu_station, &mut pipeline.alu_eus),
            (self.params.res_params.mem_station, &mut pipeline.mem_eus),
            (self.params.res_params.jmp_station, &mut pipeline.jmp_eus),
        ];

        for (station_id, eus) in &mut eu_types {
            let station = &mut pipeline.res_stations[*station_id];
            for _ in 0..eus.n_free_slots() {
                if let Some(rob_id) = station.get_ready_job() {
                    let slot = station.get_job(rob_id);
                    eus.add_work(rob_id, slot.inst.n_cycles());
                    station.start_job(rob_id);
                }
            }

            let finished_ids = eus.step_units();

            for id in finished_ids {
                let slot = station.get_job(id);
                let wb = match slot.job {
                    Job::Mem(job) => {
                        let ROBEntry::Basic(entry) = pipeline.rob.get_entry(id) else {
                            panic!("Mem instruction has non-basic ROB entry.");
                        };
                        let (value, new_entry_opt) = self.handle_mem_job(job, entry);
                        if let Some(new_entry) = new_entry_opt {
                            rob_updates.push((id, ROBEntry::Basic(new_entry)));
                        }
                        Some(value)
                    }
                    Job::Compute(job) => Some(self.handle_compute_job(job)),
                    Job::Branch(job) => {
                        let ROBEntry::Branch(entry) = pipeline.rob.get_entry(id) else {
                            panic!("Branch instruction has non-branch ROB entry.");
                        };
                        rob_updates
                            .push((id, ROBEntry::Branch(self.handle_branch_job(job, entry))));
                        None
                    }
                };

                if let Some(val) = wb {
                    pipeline.write_result.push((id, val));
                } else {
                    station.finish_job(id);
                }
            }
        }

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
