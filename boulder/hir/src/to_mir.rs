use boulder_core::CompileError;

use crate::Function;

pub struct MirBuilder {
    functions: Vec<mir::BlockId>,
    types: Vec<mir::TypeId>,
}

impl MirBuilder {
    pub fn new(functions: Vec<Function>) -> Self {
        Self {
            functions: Vec::new(),
            types: Vec::new(),
        }
    }

    pub fn build(self) -> Result<mir::Mir, CompileError> {
        unimplemented!()
    }
}