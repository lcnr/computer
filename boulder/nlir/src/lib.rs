use tindex::TVec;

use shared_id::{BlockId, NodeId, InputId};

pub struct State(pub NodeId);

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

pub enum SelectArm {
    pat: u8,
}

pub enum Node {
    Input(InputId),
    Invert(NodeId),
    BlackBox(NodeId),
    Debug(NodeId, State),
    Constant(u8),
    Binop(Binop, NodeId, NodeId),
    FunctionCall(FunctionId, TVec<InputId, Option<NodeId>>, Option<State>),
    ReturnValue(NodeId),
    SelfBlock(TVec<InputId, Option<NodeId>),
    Match(NodeId)
    Exit(Vec<NodeId>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionContext {
    pub inline: bool,
    pub export: bool,
    pub test: bool,
    pub hidden: bool,
}

pub struct Function<'a> {
    pub name: &'a str,
    pub ctx: FunctionContext,
    pub nodes: TVec<BlockId, Block>,
    /// Does this function either potentionally
    /// not terminate or have sideeffects?
    pub has_sideeffects: bool,

pub struct Block {
    pub nodes: TVec<NodeId, Node>,
    pub input_count: usize,
    pub return_count: usize,
}
