use tindex::TVec;

use shared_id::{BlockId, FunctionId, LocationId, StepId};

mod display;
mod optimize;
mod traits;
mod validate;

/// FIXME: reduce restrictions of binops
/// e.g. 0 & invalid == 0
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Binop {
    /// both arguments must be valid
    Add,
    /// both arguments must be valid
    Sub,
    /// both arguments must be valid
    Shl,
    /// both arguments must be valid
    Shr,
    /// both arguments must be valid
    Eq,
    /// both arguments must be valid
    Neq,
    /// both arguments must be valid
    Gt,
    /// both arguments must be valid
    Gte,
    /// both arguments must be valid
    BitOr,
    /// both arguments must be valid
    BitAnd,
    /// both arguments must be valid
    BitXor,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    /// input must be a valid byte.
    Invert(LocationId, LocationId),
    /// input may not be a valid byte.
    Move(LocationId, LocationId),
    /// input may not be a valid byte.
    Debug(LocationId),
    /// input may not be a valid byte.
    LoadInput(usize, LocationId),
    /// input must be a valid byte.
    LoadConstant(u8, LocationId),
    /// see binop docs for soundness constraints.
    Binop {
        op: Binop,
        l: LocationId,
        r: LocationId,
        out: LocationId,
    },
    /// args and ret may both not be valid bytes.
    FunctionCall {
        id: FunctionId,
        args: Vec<LocationId>,
        ret: Vec<LocationId>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchArm {
    pub pat: u8,
    pub target: Option<BlockId>,
    pub args: Vec<LocationId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator {
    Goto(Option<BlockId>, Vec<LocationId>),
    Match(LocationId, Vec<MatchArm>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Context {
    pub true_replacement: u8,
    pub false_replacement: u8,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lir<'a> {
    pub ctx: Context,
    pub functions: TVec<FunctionId, Function<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionContext {
    pub export: bool,
    pub test: bool,
    pub hidden: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function<'a> {
    pub name: &'a str,
    pub ctx: FunctionContext,
    pub blocks: TVec<BlockId, Block>,
    pub return_length: usize,
}

impl<'a> Function<'a> {
    pub fn input_len(&self) -> usize {
        self.blocks[BlockId(0)].input_len
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub input_len: usize,
    pub memory_len: usize,
    pub steps: TVec<StepId, Action>,
    pub terminator: Terminator,
}
