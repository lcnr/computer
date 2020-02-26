use tindex::TVec;

use shared_id::{BlockId, FunctionId, LocationId, StepId};

mod display;

#[derive(Debug, Clone, Copy)]
pub enum Binop {
    Add,
    Sub,
    Shl,
    Shr,
    Eq,
    Neq,
    Gt,
    Gte,
    BitOr,
    BitAnd,
    BitXor,
}

#[derive(Debug, Clone)]
pub enum Action {
    Invert(LocationId, LocationId),
    Debug(LocationId),
    LoadInput(usize, LocationId),
    LoadConstant(u8, LocationId),
    Binop {
        op: Binop,
        l: LocationId,
        r: LocationId,
        out: LocationId,
    },
    FunctionCall {
        id: FunctionId,
        args: Vec<LocationId>,
        ret: Vec<LocationId>,
    },
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pat: u8,
    pub target: Option<BlockId>,
    pub args: Vec<LocationId>,
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Goto(Option<BlockId>, Vec<LocationId>),
    Match(LocationId, Vec<MatchArm>),
}

#[derive(Debug, Clone)]
pub struct Lir<'a> {
    pub functions: TVec<FunctionId, Function<'a>>,
}

#[derive(Debug, Clone)]
pub struct FunctionContext {
    pub export: bool,
    pub test: bool,
    pub hidden: bool,
}

#[derive(Debug, Clone)]
pub struct Function<'a> {
    pub name: &'a str,
    pub ctx: FunctionContext,
    pub blocks: TVec<BlockId, Block>,
    pub return_length: usize,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub input_len: usize,
    pub memory_len: usize,
    pub steps: TVec<StepId, Action>,
    pub terminator: Terminator,
}
