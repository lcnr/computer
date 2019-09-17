use std::ops::{Index, IndexMut};

mod display;
pub mod optimize;
pub mod validate;

#[allow(dead_code)]
mod ty {
    use super::TypeId;

    pub const EMPTY_TYPE_ID: TypeId = TypeId(0);
    pub const UNINHABITED_TYPE_ID: TypeId = TypeId(1);
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub usize);

#[derive(Debug, Clone)]
pub enum Action {
    Extend(StepId),
    LoadInput(usize),
    LoadConstant(Object),
    Return(StepId),
    CallFunction(FunctionId, Vec<StepId>),
    FieldAccess(StepId, FieldId),
    Add(StepId, StepId),
    Sub(StepId, StepId),
    Mul(StepId, StepId),
    Div(StepId, StepId),
    Lt(StepId, StepId),
    BitOr(StepId, StepId),
    Goto(BlockId, Vec<StepId>),
    Match(StepId, Vec<(TypeId, BlockId, Vec<StepId>)>),
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
    pub content: Vec<Block>,
}

impl Index<BlockId> for Function {
    type Output = Block;

    fn index(&self, id: BlockId) -> &Self::Output {
        &self.content[id.0]
    }
}

impl IndexMut<BlockId> for Function {
    fn index_mut(&mut self, id: BlockId) -> &mut Self::Output {
        &mut self.content[id.0]
    }
}

impl Function {
    pub fn new(name: Box<str>) -> Self {
        Self {
            name,
            content: Vec::new(),
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
pub struct Block {
    pub input: Vec<TypeId>,
    pub content: Vec<Step>,
}

impl Index<StepId> for Block {
    type Output = Step;

    fn index(&self, id: StepId) -> &Self::Output {
        &self.content[id.0]
    }
}

impl IndexMut<StepId> for Block {
    fn index_mut(&mut self, id: StepId) -> &mut Self::Output {
        &mut self.content[id.0]
    }
}

impl Block {
    pub fn new() -> Self {
        Self {
            input: Vec::new(),
            content: Vec::new(),
        }
    }

    pub fn add_step(&mut self, ty: TypeId, action: Action) -> StepId {
        let step = Step { ty, action };
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

#[derive(Debug, Clone)]
pub struct Mir {
    pub types: Vec<Type>,
    pub functions: Vec<Function>,
}

impl Index<TypeId> for Mir {
    type Output = Type;

    fn index(&self, id: TypeId) -> &Self::Output {
        &self.types[id.0]
    }
}

impl IndexMut<TypeId> for Mir {
    fn index_mut(&mut self, id: TypeId) -> &mut Self::Output {
        &mut self.types[id.0]
    }
}

impl Index<FunctionId> for Mir {
    type Output = Function;

    fn index(&self, id: FunctionId) -> &Self::Output {
        &self.functions[id.0]
    }
}

impl IndexMut<FunctionId> for Mir {
    fn index_mut(&mut self, id: FunctionId) -> &mut Self::Output {
        &mut self.functions[id.0]
    }
}
