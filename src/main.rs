use std::collections::HashMap;

use debug::PreRuntimeCommand;
use prediction::{BranchPredictor, TwoBitPredictor};
use rand::seq::SliceRandom;

mod alu;
mod assembler;
mod cpu;
mod debug;
mod decoder;
mod execution_units;
mod memory;
mod prediction;
mod program;
mod reservation_station;
mod rob;

const ARF_SIZE: usize = 16;

#[derive(Clone)]
struct CPUParams {
    decode_buff_n: usize,
    alu_n: usize,
    mem_n: usize,
    jmp_n: usize,
    res_params: ReservationParams,
    rob_size: usize,
    predictor: BranchPredictor,
    jmp_buff_size: usize,
}

enum ReservationInfo {
    Shared(usize),
    Separate(usize, usize, usize),
}

#[derive(Clone)]
struct ReservationParams {
    n_stations: usize,
    station_sizes: Vec<usize>,
    alu_station: usize,
    mem_station: usize,
    jmp_station: usize,
}

impl ReservationParams {
    fn new(info: ReservationInfo) -> Self {
        match info {
            ReservationInfo::Shared(n) => ReservationParams {
                n_stations: 1,
                station_sizes: vec![n],
                alu_station: 0,
                mem_station: 0,
                jmp_station: 0,
            },
            ReservationInfo::Separate(alu_n, mem_n, jmp_n) => ReservationParams {
                n_stations: 3,
                station_sizes: vec![alu_n, mem_n, jmp_n],
                alu_station: 0,
                mem_station: 1,
                jmp_station: 2,
            },
        }
    }
}

fn main() {
    run_repl();
}

fn run_repl() {
    let cpu_params = CPUParams {
        decode_buff_n: 8,
        alu_n: 1,
        mem_n: 1,
        jmp_n: 1,
        res_params: ReservationParams::new(ReservationInfo::Separate(1, 1, 1)),
        rob_size: 6,
        predictor: BranchPredictor::TwoBit(TwoBitPredictor {
            static_forward: false,
            static_backward: false,
            prediction_buffer: HashMap::new(),
        }),
        jmp_buff_size: 8,
    };

    let mut rl = rustyline::DefaultEditor::new().expect("");
    if rl.load_history("repl_history.txt").is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());

                let parsed_line = debug::parse_pre_runtime_command_string(line);
                let Ok(command) = parsed_line else {
                    println!("Invalid command.");
                    continue;
                };

                let (program, debug) = match command {
                    PreRuntimeCommand::Run(s) => (s, false),
                    PreRuntimeCommand::Debug(s) => (s, true),
                };

                match program {
                    _ if program == *"vector_add" => test_vec_add(debug, cpu_params.clone()),
                    _ if program == *"fibonacci" => test_fib(debug, cpu_params.clone()),
                    _ if program == *"quicksort" => test_quicksort(debug, cpu_params.clone()),
                    _ => println!("Unknown program: {program}"),
                }
            }
            _ => break,
        }
        let _ = rl.save_history("repl_history.txt");
    }
}

fn test_vec_add(debug: bool, params: CPUParams) {
    let mut assembler = assembler::Assembler::new();
    let program = assembler.assemble("benchmarks/vector_add");

    let mut cpu = cpu::Cpu::new(debug, params);
    let test_data = [5; 10];
    cpu.memory.load_array(&test_data, 0);
    cpu.memory.load_array(&test_data, 10);

    cpu.run(program);

    for i in 0..30 {
        println!("Memory Location {i}: {0}", cpu.memory.load(i));
    }
}

fn test_fib(debug: bool, params: CPUParams) {
    let mut assembler = assembler::Assembler::new();
    let program = assembler.assemble("benchmarks/fibonacci");

    let mut cpu = cpu::Cpu::new(debug, params);
    cpu.run(program);

    for i in 0..20 {
        println!("Memory Location {i}: {0}", cpu.memory.load(i));
    }
}

fn test_quicksort(debug: bool, params: CPUParams) {
    let mut assembler = assembler::Assembler::new();
    let program = assembler.assemble("benchmarks/quicksort");

    let mut cpu = cpu::Cpu::new(debug, params);

    let array_info: [i32; 2] = [1000, 10];
    let mut array: [i32; 10] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];

    let mut shuffle_rng = rand::rng();
    array.shuffle(&mut shuffle_rng);

    cpu.memory.load_array(&array_info, 0);
    cpu.memory.load_array(&array, 1000);

    cpu.run(program);

    for i in 0..20 {
        println!("Memory Location {i}: {0}", cpu.memory.load(i));
    }
    println!();
    for i in 1000..1010 {
        println!("Memory Location {i}: {0}", cpu.memory.load(i));
    }
}
