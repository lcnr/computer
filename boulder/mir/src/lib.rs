use std::{
    collections::HashSet,
    mem,
    ops::{Index, IndexMut},
};

use tindex::TVec;

use shared_id::{FieldId, FunctionId, TypeId};

pub mod binop;
mod display;
mod index;
pub mod optimize;
pub mod traits;
pub mod validate;

use crate::{binop::Binop, traits::UpdateStepIds};

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

impl Terminator {
    fn invalid() -> Self {
        Terminator::Return(StepId::invalid())
    }
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

    pub fn used_steps(&self, used: &mut HashSet<StepId>) {
        match &self.action {
            &Action::Extend(id) => mem::drop(used.insert(id)),
            &Action::CallFunction(_, ref steps) => {
                steps.iter().for_each(|&s| mem::drop(used.insert(s)))
            }
            &Action::FieldAccess(id, _) => mem::drop(used.insert(id)),
            &Action::Binop(_, a, b) => {
                used.insert(a);
                used.insert(b);
            }
            &Action::LoadInput(_) | &Action::LoadConstant(_) => (),
        }
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
        self.blocks.push(Block::new())
    }

    pub fn args(&self) -> &[TypeId] {
        if let Some(first) = self.blocks.first() {
            &first.input
        } else {
            &[]
        }
    }

    pub fn split_block(&mut self, block: BlockId, after: StepId) -> BlockId {
        let mut used = HashSet::new();
        for step in self[block].steps[after..].iter().skip(1) {
            step.used_steps(&mut used);
        }
        let used = used
            .iter()
            .filter(|&&t| t <= after)
            .copied()
            .collect::<Vec<_>>();

        let new = self.add_block();
        for &step in used.iter() {
            let ty = self[block][step].ty;
            self[new].add_input(ty);
        }

        let mut content = self[block].steps.split_off(StepId(after.0 + 1));
        self[new].steps.append(&mut content);
        let terminator = mem::replace(&mut self[block].terminator, Terminator::invalid());
        self.blocks[new].add_terminator(terminator);

        self[new].update_step_ids(&mut |id| {
            if *id > after {
                id.0 = id.0 + used.len() - (after.0 + 1);
            } else {
                id.0 = used.iter().position(|i| id == i).unwrap();
            }
        });

        self[block].terminator = Terminator::Goto(new, used);

        new
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
