use tindex::TIndex;

use crate::{BlockId, StepId};

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
