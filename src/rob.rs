use crate::{decoder::Destination, program::Instruction};

#[derive(Debug, Clone)]
pub struct ROB {
    pub entries: Vec<Option<ROBEntry>>,
    head: usize,
    tail: usize,
    size: usize,
    empty: bool,
}

impl ROB {
    pub fn new(size: usize) -> Self {
        let entries = vec![None; size];

        ROB {
            entries,
            head: 0,
            tail: 0,
            size,
            empty: true,
        }
    }

    pub fn push_back(&mut self, entry: ROBEntry) -> usize {
        if self.head == self.tail && !self.empty {
            panic!("Tried to push to a full ROB")
        }

        let id = self.tail;

        self.entries[id] = Some(entry);
        self.tail = (self.tail + 1) % self.size;
        self.empty = false;

        id
    }

    pub fn pop_front(&mut self) -> (ROBEntry, usize) {
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

    pub fn peek_front(&mut self) -> Option<(ROBEntry, usize)> {
        match self.entries[self.head] {
            None => None,
            Some(entry) => Some((entry, self.head)),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.empty
    }

    pub fn is_full(&self) -> bool {
        self.head == self.tail && !self.empty
    }

    pub fn get_entry(&self, id: usize) -> ROBEntry {
        let Some(entry) = self.entries[id] else {
            panic!("Tried to get from empty rob entry")
        };

        entry
    }

    pub fn clear(&mut self) {
        for i in 0..self.size {
            self.entries[i] = None;
        }

        self.empty = true;
        self.head = 0;
        self.tail = 0;
    }

    pub fn update_value(&mut self, rob_id: usize, value: i32) {
        let Some(entry) = self.entries[rob_id] else {
            panic!("Tried to update empty rob entry");
        };

        match entry.give_value(value) {
            Ok(new_entry) => self.entries[rob_id] = Some(new_entry),
            Err(err) => panic!("{err}"),
        }
    }

    pub fn update_entry(&mut self, rob_id: usize, entry: ROBEntry) {
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

#[derive(Debug, Clone, Copy)]
pub enum ROBEntry {
    Branch(BranchROBEntry),
    Basic(BasicROBEntry),
}

impl ROBEntry {
    pub fn is_ready(&self) -> bool {
        match self {
            ROBEntry::Branch(e) => e.ready,
            ROBEntry::Basic(e) => e.ready,
        }
    }

    pub fn get_instruction(&self) -> Instruction {
        match self {
            ROBEntry::Branch(e) => e.instruction,
            ROBEntry::Basic(e) => e.instruction,
        }
    }

    pub fn get_value(&self) -> Option<i32> {
        match self {
            ROBEntry::Branch(_) => panic!("Tried to access value of a branch entry."),
            ROBEntry::Basic(entry) => entry.value,
        }
    }

    pub fn give_value(&self, value: i32) -> Result<ROBEntry, String> {
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
    pub instruction: Instruction,
    pub ready: bool,
    pub predicted_taken: bool,
    pub taken: Option<bool>,
    pub real_target: Option<usize>,
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
    pub instruction: Instruction,
    pub ready: bool,
    pub value: Option<i32>,
    pub destination: Option<Destination>,
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
    pub fn new(inst: Instruction, dest: Option<Destination>) -> Self {
        BasicROBEntry {
            instruction: inst,
            ready: false,
            value: None,
            destination: dest,
        }
    }
}
