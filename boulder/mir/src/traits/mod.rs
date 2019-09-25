use std::fmt;

use crate::{binop, meta};

mod update;

pub use update::*;

pub trait MirState: fmt::Debug + Clone {
    type StepMeta: UpdateStepIds + fmt::Debug + Clone;
    type Binop: binop::Binop<Self>;
}

#[derive(Debug, Clone)]
pub struct InitialMirState;

impl MirState for InitialMirState {
    type StepMeta = ();
    type Binop = binop::ExtendedBinop;
}

#[derive(Debug, Clone)]
pub struct FinalMirState;

impl MirState for FinalMirState {
    type StepMeta = meta::LastNeeded<()>;
    type Binop = binop::ReducedBinop;
}
