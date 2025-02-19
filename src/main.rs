use rand::seq::SliceRandom;

mod alu;
mod assembler;
mod cpu;
mod decoder;
mod memory;
mod program;

const ARF_SIZE: usize = 16;
const PRF_SIZE: usize = 16;

fn main() {
    test_fib(true);
}

fn test_vec_add(debug: bool) {
    let mut assembler = assembler::Assembler::new();
    let program = assembler.assemble("benchmarks/vector_add");

    let mut cpu = cpu::Cpu::new(debug);
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

    let mut cpu = cpu::Cpu::new(debug);
    cpu.run(program);

    for i in 0..20 {
        println!("Memory Location {i}: {0}", cpu.memory.load(i));
    }
}

fn test_quicksort(debug: bool) {
    let mut assembler = assembler::Assembler::new();
    let program = assembler.assemble("benchmarks/quicksort");

    let mut cpu = cpu::Cpu::new(debug);

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
