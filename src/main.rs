use std::{collections::HashMap, hash::Hash};

use debug::PreRuntimeCommand;
use prediction::{BranchPredictor, OneBitPredictor, TwoBitPredictor};
use profiling::Profiler;
use rand::seq::SliceRandom;

mod alu;
mod assembler;
mod cpu;
mod debug;
mod decoder;
mod execution_units;
mod memory;
mod prediction;
mod profiling;
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
        alu_n: 3,
        mem_n: 3,
        jmp_n: 1,
        res_params: ReservationParams::new(ReservationInfo::Shared(8)),
        rob_size: 12,
        predictor: BranchPredictor::TwoBit(TwoBitPredictor {
            static_backward: true,
            static_forward: false,
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

                let (program, debug, n_opt) = match command {
                    PreRuntimeCommand::Run(s) => (s, false, None),
                    PreRuntimeCommand::Debug(s) => (s, true, None),
                    PreRuntimeCommand::Test(n) => ("".to_string(), true, Some(n)),
                };

                if let Some(n) = n_opt {
                    run_all_tests(n);
                } else {
                    match program {
                        _ if program == *"vector_add" => {
                            test_vec_add(debug, cpu_params.clone(), false);
                        }
                        _ if program == *"fibonacci" => {
                            test_fib(debug, cpu_params.clone(), false);
                        }
                        _ if program == *"quicksort" => {
                            test_quicksort(debug, cpu_params.clone(), false);
                        }
                        _ if program == *"gcd" => {
                            test_gcd(debug, cpu_params.clone(), false);
                        }
                        _ => println!("Unknown program: {program}"),
                    }
                }
            }
            _ => break,
        }
        let _ = rl.save_history("repl_history.txt");
    }
}

fn run_all_tests(n_runs: usize) {
    test_s(n_runs, "vector_add".to_string());
    test_s(n_runs, "fibonacci".to_string());
    test_s(n_runs, "quicksort".to_string());
    test_s(n_runs, "gcd".to_string());
}

fn test_s(n_runs: usize, name: String) {
    let test_params = CPUParams {
        decode_buff_n: 8,
        alu_n: 3,
        mem_n: 3,
        jmp_n: 1,
        res_params: ReservationParams::new(ReservationInfo::Shared(8)),
        rob_size: 32,
        predictor: BranchPredictor::TwoBit(TwoBitPredictor {
            static_backward: true,
            static_forward: false,
            prediction_buffer: HashMap::new(),
        }),
        jmp_buff_size: 8,
    };

    let mut total_icp = 0.0;
    let mut total_bpr = 0.0;

    for _ in 0..n_runs {
        let p = match name {
            _ if name == *"vector_add" => test_vec_add(false, test_params.clone(), true),
            _ if name == *"fibonacci" => test_fib(false, test_params.clone(), true),
            _ if name == *"quicksort" => test_quicksort(false, test_params.clone(), true),
            _ if name == *"gcd" => test_gcd(false, test_params.clone(), true),
            _ => panic!("bad test name"),
        };

        total_icp += p.get_icp();
        total_bpr += p.get_bpr();
    }

    println!(
        "{name} | icp : {0} | bpr : {1}",
        total_icp / n_runs as f32,
        total_bpr / n_runs as f32
    )
}

fn test_vec_add(debug: bool, params: CPUParams, quiet: bool) -> Profiler {
    let mut assembler = assembler::Assembler::new();
    let program = assembler.assemble("benchmarks/vector_add");

    let mut cpu = cpu::Cpu::new(debug, params);
    let test_data = [5; 100];
    cpu.memory.load_array(&test_data, 0);
    cpu.memory.load_array(&test_data, 10);

    let p = cpu.run(program, quiet);
    if !quiet {
        for i in 0..30 {
            println!("Memory Location {i}: {0}", cpu.memory.load(i));
        }
    }
    p
}

fn test_fib(debug: bool, params: CPUParams, quiet: bool) -> Profiler {
    let mut assembler = assembler::Assembler::new();
    let program = assembler.assemble("benchmarks/fibonacci");

    let mut cpu = cpu::Cpu::new(debug, params);
    let p = cpu.run(program, quiet);

    if !quiet {
        for i in 0..20 {
            println!("Memory Location {i}: {0}", cpu.memory.load(i));
        }
    }

    p
}

fn test_quicksort(debug: bool, params: CPUParams, quiet: bool) -> Profiler {
    let mut assembler = assembler::Assembler::new();
    let program = assembler.assemble("benchmarks/quicksort");

    let mut cpu = cpu::Cpu::new(debug, params);

    let array_info: [i32; 2] = [1000, 100];
    let mut array: [i32; 100] = [0; 100];
    for i in 0..100 {
        array[i] = i.try_into().expect("");
    }

    let mut shuffle_rng = rand::rng();
    array.shuffle(&mut shuffle_rng);

    cpu.memory.load_array(&array_info, 0);
    cpu.memory.load_array(&array, 1000);

    let p = cpu.run(program, quiet);

    if !quiet {
        println!();
        for i in 1000..1010 {
            println!("Memory Location {i}: {0}", cpu.memory.load(i));
        }
    }
    p
}

fn test_gcd(debug: bool, params: CPUParams, quiet: bool) -> Profiler {
    let mut assembler = assembler::Assembler::new();
    let program = assembler.assemble("benchmarks/gcd");

    let mut cpu = cpu::Cpu::new(debug, params);
    cpu.run(program, quiet)
}
