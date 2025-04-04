use std::collections::HashMap;

use crate::{
    decoder::Job,
    program::{Instruction, InstructionType},
};

#[derive(Debug, Clone)]
pub struct ReservationStation {
    size: usize,
    stations: HashMap<usize, ReservationSlot>,
}

impl ReservationStation {
    pub fn new(size: usize) -> Self {
        let stations = HashMap::new();

        ReservationStation { size, stations }
    }

    pub fn add_job(&mut self, job: Job, inst: Instruction, rob_id: usize) {
        if self.stations.len() == self.size {
            panic!("Tried to push to a full reservation station.")
        };
        self.stations
            .insert(rob_id, ReservationSlot::new(job, inst));
    }

    pub fn get_ready_job(&self, inst_type: InstructionType) -> Option<usize> {
        for (rob_id, slot) in self.stations.iter() {
            if slot.is_ready() && slot.inst.get_type() == inst_type {
                return Some(*rob_id);
            }
        }
        None
    }

    pub fn get_job(&self, rob_id: usize) -> ReservationSlot {
        let Some(slot) = self.stations.get(&rob_id) else {
            panic!("Tried to access job not in reservation station.")
        };

        *slot
    }

    pub fn start_job(&mut self, rob_id: usize) {
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

    pub fn finish_job(&mut self, rob_id: usize) {
        self.stations.remove(&rob_id);
    }

    pub fn update_operands(&mut self, tag: usize, value: i32) {
        for (_, slot) in self.stations.iter_mut() {
            slot.job.update_operands(tag, value);
        }
    }

    pub fn is_empty(&self) -> bool {
        self.stations.len() == 0
    }

    pub fn is_full(&self) -> bool {
        self.stations.len() == self.size
    }

    pub fn id_in_range(&self, id: usize) -> bool {
        self.stations.contains_key(&id)
    }

    pub fn clear(&mut self) {
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

#[derive(Debug, Clone, Copy)]
pub struct ReservationSlot {
    pub job: Job,
    pub running: bool,
    pub inst: Instruction,
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
