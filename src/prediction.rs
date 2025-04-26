use std::{collections::HashMap, hash::Hash};

#[derive(Clone)]
pub enum BranchPredictor {
    NotTaken,
    Taken,
    ForwardTaken,
    BackwardTaken,
    OneBit(OneBitPredictor),
    TwoBit(TwoBitPredictor),
}

impl BranchPredictor {
    pub fn predict(&self, pc: usize, target: usize) -> usize {
        let taken = match self {
            BranchPredictor::NotTaken => static_predict(pc, target, false, false),
            BranchPredictor::Taken => static_predict(pc, target, true, true),
            BranchPredictor::ForwardTaken => static_predict(pc, target, true, false),
            BranchPredictor::BackwardTaken => static_predict(pc, target, false, true),
            BranchPredictor::OneBit(pred) => pred.predict(pc, target),
            BranchPredictor::TwoBit(pred) => pred.predict(pc, target),
        };

        if taken {
            target
        } else {
            pc + 1
        }
    }

    pub fn update(&mut self, pc: usize, taken: bool) {
        match self {
            BranchPredictor::OneBit(pred) => pred.update(pc, taken),
            BranchPredictor::TwoBit(pred) => pred.update(pc, taken),
            _ => (),
        }
    }
}

fn static_predict(pc: usize, target: usize, forward_taken: bool, backward_taken: bool) -> bool {
    (pc < target && forward_taken) || (pc > target && backward_taken)
}

#[derive(Clone)]
pub struct OneBitPredictor {
    pub static_forward: bool,
    pub static_backward: bool,
    pub prediction_buffer: HashMap<usize, bool>,
}

impl OneBitPredictor {
    fn predict(&self, pc: usize, target: usize) -> bool {
        if self.prediction_buffer.contains_key(&pc) {
            self.prediction_buffer[&pc]
        } else {
            static_predict(pc, target, self.static_forward, self.static_backward)
        }
    }

    fn update(&mut self, pc: usize, taken: bool) {
        self.prediction_buffer.insert(pc, taken);
    }
}

#[derive(Clone, Copy)]
pub enum TwoBitState {
    StronglyTaken,
    WeaklyTaken,
    WeaklyNotTaken,
    StronglyNotTaken,
}

impl TwoBitState {
    fn update(&self, taken: bool) -> TwoBitState {
        if taken {
            match self {
                TwoBitState::StronglyTaken => Self::StronglyTaken,
                TwoBitState::WeaklyTaken => Self::StronglyTaken,
                TwoBitState::WeaklyNotTaken => Self::WeaklyTaken,
                TwoBitState::StronglyNotTaken => Self::WeaklyNotTaken,
            }
        } else {
            match self {
                TwoBitState::StronglyTaken => Self::WeaklyTaken,
                TwoBitState::WeaklyTaken => Self::WeaklyNotTaken,
                TwoBitState::WeaklyNotTaken => Self::StronglyNotTaken,
                TwoBitState::StronglyNotTaken => Self::StronglyNotTaken,
            }
        }
    }

    fn is_taken(&self) -> bool {
        match self {
            TwoBitState::StronglyTaken => true,
            TwoBitState::WeaklyTaken => true,
            TwoBitState::WeaklyNotTaken => false,
            TwoBitState::StronglyNotTaken => false,
        }
    }
}

#[derive(Clone)]
pub struct TwoBitPredictor {
    pub static_forward: bool,
    pub static_backward: bool,
    pub prediction_buffer: HashMap<usize, TwoBitState>,
}

impl TwoBitPredictor {
    fn predict(&self, pc: usize, target: usize) -> bool {
        if self.prediction_buffer.contains_key(&pc) {
            self.prediction_buffer[&pc].is_taken()
        } else {
            static_predict(pc, target, self.static_forward, self.static_backward)
        }
    }

    fn update(&mut self, pc: usize, taken: bool) {
        if self.prediction_buffer.contains_key(&pc) {
            let curr_state = self.prediction_buffer[&pc];
            self.prediction_buffer.insert(pc, curr_state.update(taken));
        } else if taken {
            self.prediction_buffer.insert(pc, TwoBitState::WeaklyTaken);
        } else {
            self.prediction_buffer
                .insert(pc, TwoBitState::WeaklyNotTaken);
        }
    }
}

pub struct JumpTargetBuffer {
    size: usize,
    buffer: Vec<usize>,
}

impl JumpTargetBuffer {
    pub fn new(size: usize) -> Self {
        JumpTargetBuffer {
            size,
            buffer: vec![0; size],
        }
    }

    pub fn predict(&self, pc: usize) -> usize {
        self.buffer[pc % self.size]
    }

    pub fn update(&mut self, pc: usize, target: usize) {
        self.buffer[pc % self.size] = target
    }
}
