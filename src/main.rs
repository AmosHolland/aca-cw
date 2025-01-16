mod assembler;
mod program;

const ISA_N_REG: usize = 16;

fn main() {
    let mut assembler = assembler::Assembler::new();
    let program = assembler.assemble("bingus");
    println!("{program:?}");
}
