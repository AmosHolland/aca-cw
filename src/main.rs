use debug::PreRuntimeCommand;
use rand::seq::SliceRandom;

mod alu;
mod assembler;
mod cpu;
mod debug;
mod decoder;
mod memory;
mod program;

const ARF_SIZE: usize = 16;

const BASE_PARAMS: CPUParams = CPUParams {
    decode_buff_n: 8,
    alu_n: 1,
    mem_n: 1,
    jmp_n: 1,
    alu_res: 1,
    mem_res: 1,
    jmp_res: 1,
};

struct CPUParams {
    decode_buff_n: usize,
    alu_n: usize,
    mem_n: usize,
    jmp_n: usize,
    alu_res: usize,
    mem_res: usize,
    jmp_res: usize,
}

fn main() {
    run_repl();
}

fn run_repl() {
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
                    _ if program == *"vector_add" => test_vec_add(debug),
                    _ if program == *"fibonacci" => test_fib(debug),
                    _ if program == *"quicksort" => test_quicksort(debug),
                    _ => println!("Unknown program: {program}"),
                }
            }
            _ => break,
        }
    }
}

fn test_vec_add(debug: bool) {
    let mut assembler = assembler::Assembler::new();
    let program = assembler.assemble("benchmarks/vector_add");

    let mut cpu = cpu::Cpu::new(debug, BASE_PARAMS);
    let test_data = [5; 10];
    cpu.memory.load_array(&test_data, 0);
    cpu.memory.load_array(&test_data, 10);

    cpu.run(program);

    for i in 0..30 {
        println!("Memory Location {i}: {0}", cpu.memory.load(i));
    }
}

fn test_fib(debug: bool) {
    let mut assembler = assembler::Assembler::new();
    let program = assembler.assemble("benchmarks/fibonacci");

    let mut cpu = cpu::Cpu::new(debug, BASE_PARAMS);
    cpu.run(program);

    for i in 0..20 {
        println!("Memory Location {i}: {0}", cpu.memory.load(i));
    }
}

fn test_quicksort(debug: bool) {
    let mut assembler = assembler::Assembler::new();
    let program = assembler.assemble("benchmarks/quicksort");

    let mut cpu = cpu::Cpu::new(debug, BASE_PARAMS);

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
