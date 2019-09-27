use tindex::TIndex;

use crate::{TypeId, FieldId, FunctionId, BlockId, StepId};

impl From<usize> for TypeId {
    fn from(v: usize) -> Self {
        Self(v)
    }
}

impl TIndex for TypeId {
    fn as_index(self) -> usize {
        self.0
    }
}

impl From<usize> for StepId {
    fn from(v: usize) -> Self {
        Self(v)
    }
}

impl TIndex for StepId {
    fn as_index(self) -> usize {
        self.0
    }
}

impl From<usize> for FieldId {
    fn from(v: usize) -> Self {
        Self(v)
    }
}

impl TIndex for FieldId {
    fn as_index(self) -> usize {
        self.0
    }
}

impl From<usize> for FunctionId {
    fn from(v: usize) -> Self {
        Self(v)
    }
}

impl TIndex for FunctionId {
    fn as_index(self) -> usize {
        self.0
    }
}


impl From<usize> for BlockId {
    fn from(v: usize) -> Self {
        Self(v)
    }
}

impl TIndex for BlockId {
    fn as_index(self) -> usize {
        self.0
    }
}