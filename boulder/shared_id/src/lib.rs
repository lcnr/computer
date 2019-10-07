use std::fmt;

use tindex::TIndex;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(usize);

pub const EMPTY_TYPE_ID: TypeId = TypeId(0);
pub const NEVER_TYPE_ID: TypeId = TypeId(1);
pub const TRUE_TYPE_ID: TypeId = TypeId(2);
pub const FALSE_TYPE_ID: TypeId = TypeId(3);
pub const BOOL_TYPE_ID: TypeId = TypeId(4);
pub const U8_TYPE_ID: TypeId = TypeId(5);
pub const U16_TYPE_ID: TypeId = TypeId(6);
pub const U32_TYPE_ID: TypeId = TypeId(7);

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

impl fmt::Display for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionId(usize);

impl From<usize> for FunctionId {
    fn from(v: usize) -> Self {
        Self(v)
    }
}

impl fmt::Display for FunctionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

impl TIndex for FunctionId {
    fn as_index(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldId(usize);

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
