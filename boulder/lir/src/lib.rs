#[cfg(feature = "profiler")]
#[macro_use]
extern crate thread_profiler;

use tindex::{tvec, TBitSet, TVec};

use shared_id::{BlockId, FunctionId, InputId, LocationId, StepId};

macro_rules! l {
    ($lir:expr, $f:expr, $b:expr, $s:expr) => {
        $lir.functions[$f].blocks[$b].steps[$s]
    };
    ($lir:expr, $f:expr, $b:expr) => {
        $lir.functions[$f].blocks[$b]
    };
    ($lir:expr, $f:expr) => {
        $lir.functions[$f]
    };
}

mod display;
mod optimize;
mod reachability;
pub mod traits;
mod validate;

use crate::traits::{Reads, Writes};

#[derive(Debug, Clone, Copy)]
pub enum Memory {
    Byte(u8),
    Unknown,
    Undefined,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoolOp {
    Eq,
    Gt,
    Gte,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Binop {
    Add,
    Sub,
    Shl,
    Shr,
    Logic(BoolOp, u8, u8),
    BitOr,
    BitAnd,
    BitXor,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Arg {
    Byte(u8),
    Location(LocationId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    /// input must be a valid byte.
    Invert(LocationId, LocationId),
    /// Prevent optimizing the output based on the input
    BlackBox(LocationId, LocationId),
    /// Input must be a valid byte.
    ///
    /// While moving invalid data is not that dangerous,
    /// it is not guaranteed that `mem[i] == mem[o]` is true
    /// after a move if `mem[i]` was previously undefined.
    Move(Arg, LocationId),
    /// input may not be a valid byte.
    Debug(LocationId),
    /// see binop docs for soundness constraints.
    Binop {
        op: Binop,
        l: Arg,
        r: Arg,
        out: LocationId,
    },
    /// args and ret may both not be valid bytes.
    FunctionCall {
        id: FunctionId,
        args: TVec<InputId, Option<Arg>>,
        ret: Vec<Option<LocationId>>,
    },
    /// Does nothing, can just be removed.
    Noop,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MatchArm {
    pub pat: u8,
    pub target: Option<BlockId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator {
    Goto(Option<BlockId>),
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

impl<'a> Lir<'a> {
    pub fn may_recurse(&self, f: FunctionId, b: BlockId, s: StepId) -> bool {
        let mut called = TBitSet::new();
        match self.functions[f].blocks[b].steps[s] {
            Action::FunctionCall { id, .. } => {
                if id == f {
                    return true;
                }
                called.add(id);
                self.functions[id].requires(self, &mut called, f)
            }
            ref e => unreachable!("may_recurse called on an unexpected step: {:?}", e),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionContext {
    pub inline: bool,
    pub export: bool,
    pub test: bool,
    pub hidden: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function<'a> {
    pub name: &'a str,
    pub ctx: FunctionContext,
    pub blocks: TVec<BlockId, Block>,
    pub memory_len: usize,
    pub return_len: usize,
    pub input_len: usize,
}

impl<'a> Function<'a> {
    /// Checks whether this function depends on the function `f`.
    ///
    /// In case `f` is `FunctionId::invalid()`, `visited` contains all
    /// required functions.
    pub fn requires(
        &self,
        lir: &Lir<'a>,
        visited: &mut TBitSet<FunctionId>,
        f: FunctionId,
    ) -> bool {
        for step in self.blocks.iter().flat_map(|b| b.steps.iter()) {
            if let Action::FunctionCall { id, .. } = *step {
                if id == f {
                    return true;
                } else if !visited.get(id) {
                    visited.add(id);
                    if lir.functions[id].requires(lir, visited, f) {
                        return true;
                    }
                }
            }
        }

        false
    }

    pub fn used_locations(&self, block: BlockId, after: StepId) -> TBitSet<LocationId> {
        use crate::reachability::Reachability;

        let mut l = tvec![Reachability::Ignore; self.memory_len];

        self.blocks[block].steps[after].writes(|id| l[id].write());

        for step in self.blocks[block].steps[after + 1..].iter() {
            step.reads(|id| {
                l[id].read();
            });
            step.writes(|id| {
                l[id].write();
            });
        }

        l.iter_mut()
            .zip(self.terminator_reachability(block))
            .for_each(|(l, o)| l.append(o));

        l.index_iter()
            .filter(|&i| l[i] == Reachability::Access)
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub steps: TVec<StepId, Action>,
    pub terminator: Terminator,
}
