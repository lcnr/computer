#[derive(Debug, Clone)]
pub enum Type {
    Empty,
    Never,
    U8,
    U16,
    U32,
}

#[derive(Debug, Clone)]
pub enum Object {
    Empty,
    Never,
    U8(u8),
    U16(u16),
    U32(u32),
}

impl Object {
    pub fn ty(&self) -> Type {
        match self {
            Object::Empty => Type::Empty,
            Object::Never => Type::Never,
            Object::U8(_) => Type::U8,
            Object::U16(_) => Type::U16,
            Object::U32(_) => Type::U32,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StepId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

#[derive(Debug, Clone)]
pub enum Action {
    LoadInput(usize),
    LoadConstant(Object),
    Return(StepId),
    Add(StepId, StepId),
    Sub(StepId, StepId),
    Mul(StepId, StepId),
    Div(StepId, StepId),
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
