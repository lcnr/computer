use diagnostics::CompileError;

use crate::{Function, TypeId, VariableId};

pub struct MirBuilder {
    functions: Vec<mir::BlockId>,
    types: Vec<mir::TypeId>,
}

impl MirBuilder {
    pub fn new(functions: Vec<Function<'_, VariableId, TypeId>>) -> Self {
        Self {
            functions: Vec::new(),
            types: Vec::new(),
        }
    }

    pub fn build(self) -> Result<mir::Mir, CompileError> {
        unimplemented!()
    }
}