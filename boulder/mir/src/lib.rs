mod display;

#[derive(Debug, Clone)]
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
    Uninhabited,
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StepId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub usize);

#[derive(Debug, Clone)]
pub enum Action {
    LoadInput(usize),
    LoadConstant(Object),
    Return(StepId),
    CallFunction(FunctionId, Vec<StepId>),
    FieldAccess(StepId, FieldId),
    Add(StepId, StepId),
    Sub(StepId, StepId),
    Mul(StepId, StepId),
    Div(StepId, StepId),
    BitOr(StepId, StepId),
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
    pub content: Vec<Block>,
}

impl Function {
    pub fn new() -> Self {
        Self {
            content: Vec::new(),
        }
    }

    pub fn add_block(&mut self, block: Block) -> BlockId {
        let id = BlockId(self.content.len());
        self.content.push(block);
        id
    }

    pub fn block(&mut self, id: BlockId) -> &mut Block {
        &mut self.content[id.0]
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

impl Block {
    pub fn new() -> Self {
        Self {
            input: Vec::new(),
            content: Vec::new(),
        }
    }

    pub fn add_step(&mut self, step: Step) -> StepId {
        let id = StepId(self.content.len());
        self.content.push(step);
        id
    }

    pub fn get_step(&self, id: StepId) -> Step {
        self.content[id.0].clone()
    }

    pub fn add_input(&mut self, ty: TypeId) -> StepId {
        let id = self.add_step(Step {
            ty,
            action: Action::LoadInput(self.input.len()),
        });
        self.input.push(ty);
        id
    }
}

#[derive(Debug, Clone)]
pub struct Mir {
    pub types: Vec<Type>,
    pub functions: Vec<Function>,
}
