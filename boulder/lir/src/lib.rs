use tindex::TVec;

use shared_id::{BlockId, FunctionId, LocationId, StepId};

pub enum Binop {
    Add,
    Sub,
}

pub enum Action {
    Invert(LocationId, LocationId),
    Binop {
        op: Binop, 
        l: LocationId, 
        r: LocationId, 
        out: LocationId
    },
    FunctionCall { 
        args: Vec<LocationId>,
        ret: Vec<LocationId>,
    }
}

pub struct MatchArm {
    pub pat: u8,
    pub target: Option<BlockId>,
    pub args: Vec<LocationId>,
}

pub enum Terminator {
    Goto(Option<BlockId>, Vec<LocationId>),
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
    pub input_len: usize,
    pub memory_len: usize,
    pub steps: TVec<StepId, Action>,
    pub terminator: Terminator,
}
