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
mod traits;
mod validate;

use crate::traits::{Reads, Writes};

#[derive(Debug, Clone, Copy)]
pub enum Memory {
    Byte(u8),
    Unknown,
    Undefined,
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Arg {
    Byte(u8),
    Location(LocationId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    /// input must be a valid byte.
    Invert(LocationId, LocationId),
    /// Input must be a valid byte.
    ///
    /// While moving invalid data is not that dangerous,
    /// it is not guaranteed that `mem[i] == mem[o]` is true
    /// after a move if `mem[i]` was previously undefined.
    Move(LocationId, LocationId),
    /// input may not be a valid byte.
    Debug(LocationId),
    /// input must be a valid byte.
    LoadConstant(u8, LocationId),
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchArm {
    pub pat: u8,
    pub target: Option<BlockId>,
    pub args: TVec<InputId, Option<Arg>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator {
    Goto(Option<BlockId>, TVec<InputId, Option<Arg>>),
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
        self.blocks[BlockId(0)].inputs.len()
    }

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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub inputs: TVec<InputId, LocationId>,
    pub memory_len: usize,
    pub steps: TVec<StepId, Action>,
    pub terminator: Terminator,
}

impl Block {
    pub fn used_locations(&self, after: StepId) -> TBitSet<LocationId> {
        #[derive(Clone, Copy, PartialEq, Eq)]
        enum Location {
            Unknown,
            Overwritten,
            Needed,
        }

        let mut l = tvec![Location::Unknown; self.memory_len];

        self.steps[after].writes(|id| {
            if let Location::Unknown = l[id] {
                l[id] = Location::Overwritten
            }
        });

        for step in self.steps[after + 1..].iter() {
            step.reads(|id| {
                if let Location::Unknown = l[id] {
                    l[id] = Location::Needed
                }
            });
            step.writes(|id| {
                if let Location::Unknown = l[id] {
                    l[id] = Location::Overwritten
                }
            });
        }

        self.terminator.reads(|id| {
            if let Location::Unknown = l[id] {
                l[id] = Location::Needed
            }
        });

        l.index_iter()
            .filter(|&i| l[i] == Location::Needed)
            .collect()
    }
}
