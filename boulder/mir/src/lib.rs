#![allow(clippy::new_without_default)]

#[cfg(feature = "profiler")]
#[macro_use]
extern crate thread_profiler;

#[macro_use]
extern crate tindex;

use std::{
    convert::identity,
    ops::{Index, IndexMut},
};

use tindex::{bitset::TBitSet, TSlice, TVec};

use shared_id::{FieldId, FunctionId, TypeId};

pub mod binop;
pub mod ctx;
mod display;
pub mod traits;
pub mod transform;
pub mod validate;

use crate::{
    binop::Binop,
    ctx::{Context, FunctionContext},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Uninhabited,
    Unit,
    U8,
    U16,
    U32,
    Struct(TVec<FieldId, TypeId>),
    Union(TBitSet<TypeId>),
    Sum(TBitSet<TypeId>),
}

impl Type {
    pub fn size(&self, types: &TSlice<TypeId, Type>) -> usize {
        match self {
            Type::Unit => 0,
            Type::Uninhabited => 0,
            Type::U8 => 1,
            Type::Struct(fields) => fields.iter().map(|&f| types[f].size(types)).sum(),
            Type::Union(fields) => fields
                .iter()
                .map(|f| types[f].size(types))
                .max()
                .expect("empty union"),
            Type::U16 | Type::U32 | Type::Sum(_) => unreachable!(),
        }
    }

    pub fn expect_sum(&self) -> &TBitSet<TypeId> {
        if let Self::Sum(options) = self {
            options
        } else {
            panic!("expected sum, found {:?}", self)
        }
    }

    pub fn expect_union(&self) -> &TBitSet<TypeId> {
        if let Self::Union(options) = self {
            options
        } else {
            panic!("expected union, found {:?}", self)
        }
    }

    pub fn expect_struct(&self) -> &TSlice<FieldId, TypeId> {
        if let Self::Struct(fields) = self {
            fields
        } else {
            panic!("expected struct, found {:?}", self)
        }
    }

    pub fn is_union(&self) -> bool {
        if let Self::Union(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_subtype(ty: TypeId, of: TypeId, types: &TSlice<TypeId, Type>) -> bool {
        if ty == of {
            true
        } else if let Type::Sum(ref options) = types[of] {
            if let Type::Sum(ref t) = types[ty] {
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
    Field(TypeId, Box<Object>),
    Undefined,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchArm<T> {
    pub pat: T,
    pub target: Option<shared_id::BlockId>,
    pub args: Vec<Option<shared_id::StepId>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator {
    Goto(Option<shared_id::BlockId>, Vec<shared_id::StepId>),
    Match(shared_id::StepId, Vec<MatchArm<TypeId>>),
    MatchByte(shared_id::StepId, Vec<MatchArm<u8>>),
}

impl Terminator {
    fn invalid() -> Self {
        Terminator::Goto(None, Vec::new())
    }

    pub fn used_blocks(&self, used: &mut TBitSet<shared_id::BlockId>) {
        match self {
            Terminator::Goto(None, _) => (),
            &Terminator::Goto(Some(block), _) => used.add(block),
            &Terminator::Match(_, ref arms) => {
                arms.iter()
                    .filter_map(|arm| arm.target)
                    .for_each(|arm| used.add(arm));
            }
            Terminator::MatchByte(_, arms) => {
                arms.iter()
                    .filter_map(|arm| arm.target)
                    .for_each(|arm| used.add(arm));
            }
        }
    }

    pub fn used_steps(&self, used: &mut TBitSet<shared_id::StepId>) {
        match self {
            Terminator::Goto(_, steps) => steps.iter().for_each(|&s| used.add(s)),
            &Terminator::Match(step, ref arms) => {
                used.add(step);
                arms.iter().for_each(|arm| {
                    arm.args
                        .iter()
                        .copied()
                        .filter_map(identity)
                        .for_each(|s| used.add(s))
                });
            }
            &Terminator::MatchByte(step, ref arms) => {
                used.add(step);
                arms.iter().for_each(|arm| {
                    arm.args
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
    ToBytes,
    FromBytes,
    Debug,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LangItemState {
    Unresolved,
    BinopResolved,
    ToBytesResolved,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    Extend(shared_id::StepId),
    Reduce(shared_id::StepId),
    LoadInput(usize),
    LoadConstant(Object),
    InitializeStruct(TVec<FieldId, shared_id::StepId>),
    InitializeUnion(shared_id::StepId),
    CallFunction(FunctionId, Vec<shared_id::StepId>),
    StructFieldAccess(shared_id::StepId, FieldId),
    UnionFieldAccess(shared_id::StepId),
    UnaryOperation(UnaryOperation, shared_id::StepId),
    Binop(Binop, shared_id::StepId, shared_id::StepId),
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

    pub fn used_steps(&self, used: &mut TBitSet<shared_id::StepId>) {
        match &self.action {
            &Action::Extend(id)
            | &Action::Reduce(id)
            | &Action::StructFieldAccess(id, _)
            | &Action::InitializeUnion(id)
            | &Action::UnionFieldAccess(id)
            | &Action::UnaryOperation(_, id) => used.add(id),
            &Action::InitializeStruct(ref fields) => fields.iter().for_each(|&s| used.add(s)),
            &Action::CallFunction(_, ref steps) => steps.iter().for_each(|&s| used.add(s)),
            &Action::Binop(_, a, b) => {
                used.add(a);
                used.add(b);
            }
            &Action::LoadInput(_) | &Action::LoadConstant(_) => (),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function<'a> {
    pub name: &'a str,
    pub ctx: FunctionContext,
    pub blocks: TVec<shared_id::BlockId, Block>,
    pub ret: TypeId,
}

impl<'a> Index<shared_id::BlockId> for Function<'a> {
    type Output = Block;

    fn index(&self, id: shared_id::BlockId) -> &Self::Output {
        &self.blocks[id]
    }
}

impl<'a> IndexMut<shared_id::BlockId> for Function<'a> {
    fn index_mut(&mut self, id: shared_id::BlockId) -> &mut Self::Output {
        &mut self.blocks[id]
    }
}

impl<'a> Function<'a> {
    pub fn new(name: &'a str, ctx: FunctionContext, ret: TypeId) -> Self {
        Self {
            name,
            ctx,
            blocks: TVec::new(),
            ret,
        }
    }

    pub fn add_block(&mut self) -> shared_id::BlockId {
        self.blocks.push(Block::new())
    }

    pub fn clone_block(&mut self, block: shared_id::BlockId) -> shared_id::BlockId {
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

    pub fn used_functions(&self, used: &mut TBitSet<FunctionId>) {
        for step in self.blocks.iter().flat_map(|b| b.steps.iter()) {
            if let Action::CallFunction(id, _) = step.action {
                used.add(id)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub input: Vec<TypeId>,
    pub steps: TVec<shared_id::StepId, Step>,
    pub terminator: Terminator,
}

impl Index<shared_id::StepId> for Block {
    type Output = Step;

    fn index(&self, id: shared_id::StepId) -> &Self::Output {
        &self.steps[id]
    }
}

impl IndexMut<shared_id::StepId> for Block {
    fn index_mut(&mut self, id: shared_id::StepId) -> &mut Self::Output {
        &mut self.steps[id]
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

    pub fn add_step(&mut self, ty: TypeId, action: Action) -> shared_id::StepId {
        let step = Step::new(ty, action);
        self.steps.push(step)
    }

    pub fn add_input(&mut self, ty: TypeId) -> shared_id::StepId {
        let id = self.add_step(ty, Action::LoadInput(self.input.len()));
        self.input.push(ty);
        id
    }

    pub fn add_terminator(&mut self, terminator: Terminator) {
        assert_eq!(self.terminator, Terminator::invalid(), "{:?}", self);
        self.terminator = terminator;
    }
}

#[derive(Debug, Clone)]
pub struct Mir<'a> {
    pub types: TVec<TypeId, Type>,
    pub functions: TVec<FunctionId, Function<'a>>,
    pub ctx: Context,
}

impl<'a> Mir<'a> {
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
            .position(|f| f.name == name)
            .map(FunctionId::from)
    }
}

impl<'a> Index<TypeId> for Mir<'a> {
    type Output = Type;

    fn index(&self, id: TypeId) -> &Self::Output {
        &self.types[id]
    }
}

impl<'a> IndexMut<TypeId> for Mir<'a> {
    fn index_mut(&mut self, id: TypeId) -> &mut Self::Output {
        &mut self.types[id]
    }
}

impl<'a> Index<FunctionId> for Mir<'a> {
    type Output = Function<'a>;

    fn index(&self, id: FunctionId) -> &Self::Output {
        &self.functions[id]
    }
}

impl<'a> IndexMut<FunctionId> for Mir<'a> {
    fn index_mut(&mut self, id: FunctionId) -> &mut Self::Output {
        &mut self.functions[id]
    }
}
