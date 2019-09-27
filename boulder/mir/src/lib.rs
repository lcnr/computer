use std::{
    fmt,
    ops::{Index, IndexMut},
};

use tindex::TVec;

pub mod binop;
mod display;
mod index;
pub mod meta;
pub mod optimize;
pub mod to_asm;
pub mod traits;
pub mod validate;

use binop::Binop;
use traits::{MirState, UpdateStepIds};

#[allow(dead_code)]
mod ty {
    use super::TypeId;

    pub const EMPTY_TYPE_ID: TypeId = TypeId(0);
    pub const NEVER_TYPE_ID: TypeId = TypeId(1);
    pub const BOOL_TYPE_ID: TypeId = TypeId(4);
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Uninhabited,
    Unit,
    U8,
    U16,
    U32,
    Struct(TVec<FieldId, TypeId>),
    Sum(Vec<TypeId>),
}

#[derive(Debug, Clone)]
pub enum Object {
    Unit,
    U8(u8),
    U16(u16),
    U32(u32),
    Struct(Vec<Object>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldId(usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StepId(usize);

impl StepId {
    pub fn invalid() -> Self {
        StepId(std::usize::MAX)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionId(usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator {
    Return(StepId),
    Goto(BlockId, Vec<StepId>),
    Match(StepId, Vec<(TypeId, BlockId, Vec<Option<StepId>>)>),
}

#[derive(Debug, Clone)]
pub enum Action {
    Extend(StepId),
    LoadInput(usize),
    LoadConstant(Object),
    CallFunction(FunctionId, Vec<StepId>),
    FieldAccess(StepId, FieldId),
    Binop(Binop, StepId, StepId),
}

#[derive(Debug, Clone)]
pub struct Step<M: MirState> {
    pub ty: TypeId,
    pub action: Action,
    pub meta: M::StepMeta,
}

impl<M: MirState<StepMeta = ()>> Step<M> {
    pub fn new(ty: TypeId, action: Action) -> Self {
        Self {
            meta: (),
            ty,
            action,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function<M: MirState> {
    pub name: Box<str>,
    pub blocks: TVec<BlockId, Block<M>>,
    pub ret: TypeId,
}

impl<M: MirState> Index<BlockId> for Function<M> {
    type Output = Block<M>;

    fn index(&self, id: BlockId) -> &Self::Output {
        &self.blocks[id]
    }
}

impl<M: MirState> IndexMut<BlockId> for Function<M> {
    fn index_mut(&mut self, id: BlockId) -> &mut Self::Output {
        &mut self.blocks[id]
    }
}

impl<M: MirState> Function<M> {
    pub fn new(name: Box<str>, ret: TypeId) -> Self {
        Self {
            name,
            blocks: TVec::new(),
            ret,
        }
    }

    pub fn add_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len());
        self.blocks.push(Block::new());
        id
    }

    pub fn args(&self) -> &[TypeId] {
        if let Some(first) = self.blocks.first() {
            &first.input
        } else {
            &[]
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block<M: MirState> {
    pub input: Vec<TypeId>,
    pub steps: TVec<StepId, Step<M>>,
    pub terminator: Terminator,
}

impl<M: MirState> Index<StepId> for Block<M> {
    type Output = Step<M>;

    fn index(&self, id: StepId) -> &Self::Output {
        &self.steps[id]
    }
}

impl<M: MirState> IndexMut<StepId> for Block<M> {
    fn index_mut(&mut self, id: StepId) -> &mut Self::Output {
        &mut self.steps[id]
    }
}

impl<M: MirState<StepMeta = ()>> Block<M> {
    pub fn add_step(&mut self, ty: TypeId, action: Action) -> StepId {
        let step = Step::new(ty, action);
        self.steps.push(step)
    }

    pub fn add_input(&mut self, ty: TypeId) -> StepId {
        let id = self.add_step(ty, Action::LoadInput(self.input.len()));
        self.input.push(ty);
        id
    }
}

impl<M: MirState> Block<M> {
    pub fn new() -> Self {
        Self {
            input: Vec::new(),
            steps: TVec::new(),
            terminator: Terminator::Return(StepId::invalid()),
        }
    }

    pub fn add_terminator(&mut self, terminator: Terminator) {
        assert_eq!(
            self.terminator,
            Terminator::Return(StepId::invalid()),
            "{:?}",
            self
        );
        self.terminator = terminator;
    }
}

#[derive(Debug, Clone)]
pub struct Mir<M: MirState> {
    pub types: TVec<TypeId, Type>,
    pub functions: TVec<FunctionId, Function<M>>,
}

impl<M: MirState> Mir<M> {
    pub fn step_count(&self) -> usize {
        self.functions
            .iter()
            .flat_map(|f| f.blocks.iter())
            .flat_map(|b| b.steps.iter())
            .count()
    }
}

impl<M: MirState> Index<TypeId> for Mir<M> {
    type Output = Type;

    fn index(&self, id: TypeId) -> &Self::Output {
        &self.types[id]
    }
}

impl<M: MirState> IndexMut<TypeId> for Mir<M> {
    fn index_mut(&mut self, id: TypeId) -> &mut Self::Output {
        &mut self.types[id]
    }
}

impl<M: MirState> Index<FunctionId> for Mir<M> {
    type Output = Function<M>;

    fn index(&self, id: FunctionId) -> &Self::Output {
        &self.functions[id]
    }
}

impl<M: MirState> IndexMut<FunctionId> for Mir<M> {
    fn index_mut(&mut self, id: FunctionId) -> &mut Self::Output {
        &mut self.functions[id]
    }
}
