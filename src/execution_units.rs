use crate::program::InstructionType;

#[derive(Debug, Clone)]
pub struct ExecutionUnits {
    size: usize,
    inst_type: InstructionType,
    units: Vec<Option<(usize, usize)>>,
    free_slots: Vec<usize>,
}

impl ExecutionUnits {
    pub fn new(size: usize, inst_type: InstructionType) -> Self {
        let units = vec![None; size];
        let free_slots = Vec::from_iter(0..size);
        ExecutionUnits {
            size,
            inst_type,
            units,
            free_slots,
        }
    }

    pub fn get_type(&self) -> InstructionType {
        self.inst_type
    }

    pub fn add_work(&mut self, station_id: usize, cycles: usize) {
        let Some(i) = self.free_slots.pop() else {
            panic!("Attempted to add work to full set of execution units.")
        };

        self.units[i] = Some((station_id, cycles))
    }

    pub fn n_free_slots(&self) -> usize {
        self.free_slots.len()
    }

    pub fn step_units(&mut self) -> Vec<usize> {
        let mut finished_stations = Vec::new();

        for i in 0..self.size {
            if let Some((id, cycles)) = self.units[i] {
                if cycles == 1 {
                    finished_stations.push(id);
                    self.units[i] = None;
                    self.free_slots.push(i);
                } else {
                    self.units[i] = Some((id, cycles - 1));
                }
            }
        }
        finished_stations
    }

    pub fn clear(&mut self) {
        self.units = vec![None; self.size];
        self.free_slots = Vec::from_iter(0..self.size);
    }
}

impl std::fmt::Display for ExecutionUnits {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut fmt_string = "Execution Units:".to_string();
        for (i, opt) in self.units.iter().enumerate() {
            let str_body = match opt {
                Some((id, cycles)) => {
                    format!("station {id}, {cycles} cycles left")
                }
                None => "".to_string(),
            };
            fmt_string = format!("{fmt_string}\n {i} : {str_body}");
        }

        write!(f, "{fmt_string}")
    }
}
