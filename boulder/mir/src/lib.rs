pub enum Ty {
    U8,
    U16,
    U32,
}

pub struct TypeId(usize);
pub struct ValueId(usize);
pub struct BlockId(usize);


pub enum Action {
    
}

pub struct Block {
    pub inputs: usize,
    pub values: Vec<TypeId>,
    pub content: Vec<Action>,
}

pub struct Mir {
    pub types: Vec<Ty>,
    pub blocks: Vec<Block>,
}