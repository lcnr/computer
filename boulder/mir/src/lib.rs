#[derive(Debug, Clone)]
pub enum Type {
    Empty,
    U8,
    U16,
    U32,
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
}

#[derive(Debug, Clone)]
pub struct Step {
    ty: TypeId,
    action: Action,
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
