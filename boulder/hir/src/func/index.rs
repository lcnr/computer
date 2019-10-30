use tindex::TIndex;

use crate::func::{ScopeId, VariableId};

impl From<usize> for ScopeId {
    fn from(v: usize) -> Self {
        Self(v)
    }
}

impl TIndex for ScopeId {
    fn as_index(&self) -> usize {
        self.0
    }
}

impl From<usize> for VariableId {
    fn from(v: usize) -> Self {
        Self(v)
    }
}

impl TIndex for VariableId {
    fn as_index(&self) -> usize {
        self.0
    }
}
