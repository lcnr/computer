use tindex::TVec;

use shared_id::{BlockId, FunctionId, StepId};

pub enum Binop {
    Add,
    Sub,
}

pub enum Action {
    Invert(StepId),
    Binop(Binop, StepId, StepId),
}

pub struct MatchArm {
    pub pat: u8,
    pub target: Option<BlockId>,
    pub args: Vec<StepId>,
}

pub enum Terminator {
    Goto(Option<BlockId>, Vec<StepId>),
    Match(StepId, Vec<MatchArm>),
}

pub struct Lir<'a> {
    pub functions: TVec<FunctionId, Function<'a>>,
}

pub struct Function<'a> {
    pub name: &'a str,
    pub blocks: TVec<BlockId, Block>,
}

pub struct Block {
    input_len: usize,
    steps: TVec<StepId, Action>,
    terminator: Terminator,
}
