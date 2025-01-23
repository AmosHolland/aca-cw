mod alu;
mod assembler;
mod cpu;
mod decoder;
mod memory;
mod program;

const ARF_SIZE: usize = 16;
const PRF_SIZE: usize = 16;

fn main() {
    let mut assembler = assembler::Assembler::new();
    let program = assembler.assemble("bingus");
    println!("{program:?}");
}
