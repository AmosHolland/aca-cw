#[derive(Clone)]
pub struct Profiler {
    cycles: usize,
    instructions_executed: usize,
    correct_predictions: usize,
    incorrect_predictions: usize,
}

impl Profiler {
    pub fn new() -> Self {
        Profiler {
            cycles: 0,
            instructions_executed: 0,
            correct_predictions: 0,
            incorrect_predictions: 0,
        }
    }

    pub fn inc_cycles(&mut self) {
        self.cycles += 1;
    }

    pub fn inc_instructions(&mut self) {
        self.instructions_executed += 1;
    }

    pub fn inc_correct_pred(&mut self) {
        self.correct_predictions += 1;
    }

    pub fn inc_incorrect_pred(&mut self) {
        self.incorrect_predictions += 1;
    }

    pub fn get_icp(&self) -> f32 {
        self.instructions_executed as f32 / self.cycles as f32
    }

    pub fn get_bpr(&self) -> f32 {
        (self.correct_predictions as f32)
            / ((self.correct_predictions + self.incorrect_predictions) as f32)
    }

    pub fn report(&self) {
        println!(
            "Executed {0} instructions in {1} cycles!",
            self.instructions_executed, self.cycles
        );
        println!(
            "IPC {0}",
            self.instructions_executed as f32 / self.cycles as f32
        );
        println!(
            "correct prediction rate: {0}",
            (self.correct_predictions as f32)
                / ((self.correct_predictions + self.incorrect_predictions) as f32)
        )
    }
}
