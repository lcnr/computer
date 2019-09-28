use std::ops::{Index, IndexMut};

use tindex::TVec;

use shared_id::{FieldId, FunctionId, TypeId};

pub mod binop;
mod display;
mod index;
pub mod optimize;
pub mod traits;
pub mod validate;

use binop::Binop;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StepId(usize);

impl StepId {
    pub fn invalid() -> Self {
        StepId(std::usize::MAX)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(usize);

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
pub struct Step {
    pub ty: TypeId,
    pub action: Action,
}

impl Step {
    pub fn new(ty: TypeId, action: Action) -> Self {
        Self { ty, action }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Box<str>,
    pub blocks: TVec<BlockId, Block>,
    pub ret: TypeId,
}

impl Index<BlockId> for Function {
    type Output = Block;

    fn index(&self, id: BlockId) -> &Self::Output {
        &self.blocks[id]
    }
}

impl IndexMut<BlockId> for Function {
    fn index_mut(&mut self, id: BlockId) -> &mut Self::Output {
        &mut self.blocks[id]
    }
}

impl Function {
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
pub struct Block {
    pub input: Vec<TypeId>,
    pub steps: TVec<StepId, Step>,
    pub terminator: Terminator,
}

impl Index<StepId> for Block {
    type Output = Step;

    fn index(&self, id: StepId) -> &Self::Output {
        &self.steps[id]
    }
}

impl IndexMut<StepId> for Block {
    fn index_mut(&mut self, id: StepId) -> &mut Self::Output {
        &mut self.steps[id]
    }
}

impl Block {
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

impl Block {
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
pub struct Mir {
    pub types: TVec<TypeId, Type>,
    pub functions: TVec<FunctionId, Function>,
}

impl Mir {
    pub fn step_count(&self) -> usize {
        self.functions
            .iter()
            .flat_map(|f| f.blocks.iter())
            .flat_map(|b| b.steps.iter())
            .count()
    }
}

impl Index<TypeId> for Mir {
    type Output = Type;

    fn index(&self, id: TypeId) -> &Self::Output {
        &self.types[id]
    }
}

impl IndexMut<TypeId> for Mir {
    fn index_mut(&mut self, id: TypeId) -> &mut Self::Output {
        &mut self.types[id]
    }
}

impl Index<FunctionId> for Mir {
    type Output = Function;

    fn index(&self, id: FunctionId) -> &Self::Output {
        &self.functions[id]
    }
}

impl IndexMut<FunctionId> for Mir {
    fn index_mut(&mut self, id: FunctionId) -> &mut Self::Output {
        &mut self.functions[id]
    }
}
