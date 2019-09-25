use std::{
    fmt,
    ops::{Index, IndexMut},
};

pub mod binop;
mod display;
pub mod meta;
pub mod optimize;
pub mod to_asm;
pub mod traits;
pub mod validate;

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
    Struct(Vec<TypeId>),
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
pub struct TypeId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StepId(pub usize);

impl StepId {
    pub fn invalid() -> Self {
        StepId(std::usize::MAX)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator {
    Return(StepId),
    Goto(BlockId, Vec<StepId>),
    Match(StepId, Vec<(TypeId, BlockId, Vec<Option<StepId>>)>),
}

#[derive(Debug, Clone)]
pub enum Action<M: MirState> {
    Extend(StepId),
    LoadInput(usize),
    LoadConstant(Object),
    CallFunction(FunctionId, Vec<StepId>),
    FieldAccess(StepId, FieldId),
    Binop(M::Binop, StepId, StepId),
}

#[derive(Debug, Clone)]
pub struct Step<M: MirState> {
    pub ty: TypeId,
    pub action: Action<M>,
    pub meta: M::StepMeta,
}

impl<M: MirState<StepMeta = ()>> Step<M> {
    pub fn new(ty: TypeId, action: Action<M>) -> Self {
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
    pub content: Vec<Block<M>>,
    pub ret: TypeId,
}

impl<M: MirState> Index<BlockId> for Function<M> {
    type Output = Block<M>;

    fn index(&self, id: BlockId) -> &Self::Output {
        &self.content[id.0]
    }
}

impl<M: MirState> IndexMut<BlockId> for Function<M> {
    fn index_mut(&mut self, id: BlockId) -> &mut Self::Output {
        &mut self.content[id.0]
    }
}

impl<M: MirState> Function<M> {
    pub fn new(name: Box<str>, ret: TypeId) -> Self {
        Self {
            name,
            content: Vec::new(),
            ret,
        }
    }

    pub fn add_block(&mut self) -> BlockId {
        let id = BlockId(self.content.len());
        self.content.push(Block::new());
        id
    }

    pub fn args(&self) -> &[TypeId] {
        if let Some(first) = self.content.first() {
            &first.input
        } else {
            &[]
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block<M: MirState> {
    pub input: Vec<TypeId>,
    pub content: Vec<Step<M>>,
    pub terminator: Terminator,
}

impl<M: MirState> Index<StepId> for Block<M> {
    type Output = Step<M>;

    fn index(&self, id: StepId) -> &Self::Output {
        &self.content[id.0]
    }
}

impl<M: MirState> IndexMut<StepId> for Block<M> {
    fn index_mut(&mut self, id: StepId) -> &mut Self::Output {
        &mut self.content[id.0]
    }
}

impl<M: MirState<StepMeta = ()>> Block<M> {
    pub fn add_step(&mut self, ty: TypeId, action: Action<M>) -> StepId {
        let step = Step::new(ty, action);
        let id = StepId(self.content.len());
        self.content.push(step);
        id
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
            content: Vec::new(),
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
    pub types: Vec<Type>,
    pub functions: Vec<Function<M>>,
}

impl<M: MirState> Mir<M> {
    pub fn step_count(&self) -> usize {
        self.functions
            .iter()
            .flat_map(|f| f.content.iter())
            .flat_map(|b| b.content.iter())
            .count()
    }
}

impl<M: MirState> Index<TypeId> for Mir<M> {
    type Output = Type;

    fn index(&self, id: TypeId) -> &Self::Output {
        &self.types[id.0]
    }
}

impl<M: MirState> IndexMut<TypeId> for Mir<M> {
    fn index_mut(&mut self, id: TypeId) -> &mut Self::Output {
        &mut self.types[id.0]
    }
}

impl<M: MirState> Index<FunctionId> for Mir<M> {
    type Output = Function<M>;

    fn index(&self, id: FunctionId) -> &Self::Output {
        &self.functions[id.0]
    }
}

impl<M: MirState> IndexMut<FunctionId> for Mir<M> {
    fn index_mut(&mut self, id: FunctionId) -> &mut Self::Output {
        &mut self.functions[id.0]
    }
}
