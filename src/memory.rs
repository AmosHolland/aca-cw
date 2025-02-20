use crate::program::{Instruction, Program};

pub const MEM_SIZE: usize = 4096;
pub struct Memory {
    data: [i32; MEM_SIZE],
    program: Program,
}

impl Memory {
    pub fn new() -> Self {
        Memory {
            data: [0; MEM_SIZE],
            program: Program {
                instructions: Vec::new(),
            },
        }
    }

    pub fn load_array(&mut self, array: &[i32], start: usize) {
        let n = array.len();
        let m = self.data.len();
        if n + start >= m {
            panic!("Attempted to load an array of size {n} into memory of size {m} at position {start} (array would overflow).")
        }

        self.data[start..start + n].copy_from_slice(array);
    }

    pub fn load_program(&mut self, program: Program) {
        self.program = program.clone();
    }

    pub fn fetch(&self, pc: usize) -> Option<Instruction> {
        self.program.instructions.get(pc).copied()
    }

    pub fn store(&mut self, address: usize, value: i32) {
        if address >= self.data.len() {
            panic!("Attempted to store into address {address} which is outside of memory.")
        }

        self.data[address] = value;
    }

    pub fn load(&self, address: usize) -> i32 {
        if address >= self.data.len() {
            panic!("Attempted to load from address {address} which is outside of memory.")
        }

        self.data[address]
    }
}
