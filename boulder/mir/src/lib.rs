use std::{
    convert::identity,
    ops::{Index, IndexMut},
};

use tindex::{bitset::TBitSet, TSlice, TVec};

use shared_id::{FieldId, FunctionId, TypeId};

pub mod binop;
mod display;
mod index;
pub mod optimize;
pub mod traits;
pub mod validate;

use crate::binop::Binop;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Uninhabited,
    Unit,
    U8,
    U16,
    U32,
    Struct(TVec<FieldId, TypeId>),
    Sum(TBitSet<TypeId>),
}

impl Type {
    pub fn is_subtype(ty: TypeId, of: TypeId, types: &TSlice<TypeId, Type>) -> bool {
        if ty == of {
            true
        } else if let &Type::Sum(ref options) = &types[of] {
            if let &Type::Sum(ref t) = &types[ty] {
                t.iter().all(|ty| options.get(ty))
            } else {
                options.get(ty)
            }
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Unit,
    U8(u8),
    U16(u16),
    U32(u32),
    Struct(TVec<FieldId, Object>),
    Variant(TypeId, Box<Object>),
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

impl BlockId {
    pub fn invalid() -> Self {
        BlockId(std::usize::MAX)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator {
    Return(StepId),
    Goto(BlockId, Vec<StepId>),
    Match(StepId, Vec<(TypeId, BlockId, Vec<Option<StepId>>)>),
}

impl Terminator {
    fn invalid() -> Self {
        Terminator::Return(StepId::invalid())
    }

    pub fn used_blocks(&self, used: &mut TBitSet<BlockId>) {
        match self {
            &Terminator::Return(_) => (),
            &Terminator::Goto(block, _) => used.add(block),
            &Terminator::Match(_, ref arms) => {
                arms.iter().for_each(|arm| used.add(arm.1));
            }
        }
    }

    pub fn used_steps(&self, used: &mut TBitSet<StepId>) {
        match self {
            &Terminator::Return(id) => used.add(id),
            &Terminator::Goto(_, ref steps) => steps.iter().for_each(|&s| used.add(s)),
            &Terminator::Match(step, ref arms) => {
                used.add(step);
                arms.iter().for_each(|arm| {
                    arm.2
                        .iter()
                        .copied()
                        .filter_map(identity)
                        .for_each(|s| used.add(s))
                });
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperation {
    Invert,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    Extend(StepId),
    LoadInput(usize),
    LoadConstant(Object),
    InitializeStruct(TypeId, TVec<FieldId, StepId>),
    CallFunction(FunctionId, Vec<StepId>),
    FieldAccess(StepId, FieldId),
    UnaryOperation(UnaryOperation, StepId),
    Binop(Binop, StepId, StepId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Step {
    pub ty: TypeId,
    pub action: Action,
}

impl Step {
    pub fn new(ty: TypeId, action: Action) -> Self {
        Self { ty, action }
    }

    pub fn used_steps(&self, used: &mut TBitSet<StepId>) {
        match &self.action {
            &Action::Extend(id) => used.add(id),
            &Action::InitializeStruct(_, ref fields) => fields.iter().for_each(|&s| used.add(s)),
            &Action::CallFunction(_, ref steps) => steps.iter().for_each(|&s| used.add(s)),
            &Action::FieldAccess(id, _) => used.add(id),
            &Action::UnaryOperation(_, expr) => used.add(expr),
            &Action::Binop(_, a, b) => {
                used.add(a);
                used.add(b);
            }
            &Action::LoadInput(_) | &Action::LoadConstant(_) => (),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Box<str>,
    pub attributes: Vec<Box<str>>,
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
    pub fn new(name: Box<str>, attributes: Vec<Box<str>>, ret: TypeId) -> Self {
        Self {
            name,
            attributes,
            blocks: TVec::new(),
            ret,
        }
    }

    pub fn add_block(&mut self) -> BlockId {
        self.blocks.push(Block::new())
    }

    pub fn clone_block(&mut self, block: BlockId) -> BlockId {
        let block = self[block].clone();
        self.blocks.push(block)
    }

    pub fn args(&self) -> &[TypeId] {
        if let Some(first) = self.blocks.first() {
            &first.input
        } else {
            &[]
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
            terminator: Terminator::invalid(),
        }
    }

    pub fn add_terminator(&mut self, terminator: Terminator) {
        assert_eq!(self.terminator, Terminator::invalid(), "{:?}", self);
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

    pub fn get_function(&self, name: &str) -> Option<FunctionId> {
        self.functions
            .iter()
            .position(|f| f.name.as_ref() == name)
            .map(FunctionId::from)
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
