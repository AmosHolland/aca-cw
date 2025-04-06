use crate::{
    decoder::Job,
    program::{Instruction, InstructionType},
};

#[derive(Debug, Clone)]
pub struct ReservationStation {
    stations: Vec<Option<ReservationSlot>>,
    free_slots: Vec<usize>,
    size: usize,
}

impl ReservationStation {
    pub fn new(size: usize) -> Self {
        let stations = vec![None; size];
        let free_slots = Vec::from_iter(0..size);

        ReservationStation {
            stations,
            free_slots,
            size,
        }
    }

    pub fn add_job(&mut self, job: Job, inst: Instruction, rob_id: usize) {
        let Some(i) = self.free_slots.pop() else {
            panic!("Tried to push to a full reservation station.")
        };

        self.stations[i] = Some(ReservationSlot::new(job, inst, rob_id));
    }

    pub fn get_ready_job(&self, inst_type: InstructionType) -> Option<usize> {
        for (i, slot_opt) in self.stations.iter().enumerate() {
            if let Some(slot) = slot_opt {
                if slot.is_ready() && slot.inst.get_type() == inst_type {
                    return Some(i);
                }
            }
        }
        None
    }

    pub fn get_slot(&self, id: usize) -> ReservationSlot {
        let Some(slot_opt) = self.stations.get(id) else {
            panic!("Tried to access slot not in reservation station.")
        };

        let Some(slot) = *slot_opt else {
            panic!("Tried to access an empty reservation slot.")
        };

        slot
    }

    pub fn start_job(&mut self, id: usize) {
        let Some(slot_opt) = self.stations.get_mut(id) else {
            panic!("Tried to access slot not in reservation station.")
        };
        let Some(slot) = slot_opt else {
            panic!("Tried to access an empty reservation slot.")
        };
        if slot.running {
            panic!("Attempted to start an already running job")
        }
        slot.running = true;
    }

    pub fn finish_job(&mut self, id: usize) {
        let Some(slot_opt) = self.stations.get(id) else {
            panic!("Tried to finish slot not in reservation station.")
        };
        let Some(slot) = slot_opt else {
            panic!("Tried to access an empty reservation slot.")
        };
        if !slot.running {
            panic!("Attempted to stop a job that wasn't running")
        }

        self.stations[id] = None;
        self.free_slots.push(id);
    }

    pub fn update_operands(&mut self, tag: usize, value: i32) {
        for slot in self.stations.iter_mut().flatten() {
            slot.job.update_operands(tag, value);
        }
    }

    pub fn is_empty(&self) -> bool {
        self.free_slots.len() == self.size
    }

    pub fn is_full(&self) -> bool {
        self.free_slots.is_empty()
    }

    pub fn clear(&mut self) {
        self.stations = vec![None; self.size];
        self.free_slots = Vec::from_iter(0..self.size);
    }
}

impl std::fmt::Display for ReservationStation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut fmt_string = "Reservation Stations:".to_string();
        for (i, slot) in self.stations.iter().enumerate() {
            match slot {
                None => fmt_string = format!("{fmt_string}\n {i} : None"),
                Some(slot) => {
                    fmt_string = format!(
                        "{fmt_string}\n {i} : {0} ({1}) [{2}] {3}",
                        slot.job,
                        slot.inst,
                        slot.rob_id,
                        if slot.running { "*" } else { "" }
                    )
                }
            }
        }

        write!(f, "{fmt_string}")
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ReservationSlot {
    pub job: Job,
    pub running: bool,
    pub inst: Instruction,
    pub rob_id: usize,
}

impl ReservationSlot {
    fn new(job: Job, inst: Instruction, rob_id: usize) -> Self {
        ReservationSlot {
            job,
            running: false,
            inst,
            rob_id,
        }
    }

    fn is_ready(&self) -> bool {
        !self.running && self.job.is_ready()
    }
}
