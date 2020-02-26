use std::{fmt, ops::Add};

use tindex::TIndex;

pub const EMPTY_TYPE_ID: TypeId = TypeId(0);
pub const NEVER_TYPE_ID: TypeId = TypeId(1);
pub const TRUE_TYPE_ID: TypeId = TypeId(2);
pub const FALSE_TYPE_ID: TypeId = TypeId(3);
pub const BOOL_TYPE_ID: TypeId = TypeId(4);
pub const U8_TYPE_ID: TypeId = TypeId(5);
pub const U16_TYPE_ID: TypeId = TypeId(6);
pub const U16_BYTES_TYPE_ID: TypeId = TypeId(7);
pub const U32_TYPE_ID: TypeId = TypeId(8);
pub const U32_BYTES_TYPE_ID: TypeId = TypeId(9);

macro_rules! t {
    ($($n:ident $(: $tag:expr)?),+) => {
        $(
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub struct $n(pub usize);

            impl From<usize> for $n {
                fn from(v: usize) -> Self {
                    Self(v)
                }
            }

            impl TIndex for $n {
                fn as_index(&self) -> usize {
                    self.0
                }
            }

            $(
                impl fmt::Display for $n {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        write!(f, "{}{}", $tag, self.0)
                    }
                }
            )?
        )+
    }
}

t!(TypeId: "%", FunctionId: "#", FieldId, BlockId, StepId, LocationId: "@");

impl BlockId {
    pub fn invalid() -> Self {
        BlockId(std::usize::MAX)
    }
}

impl StepId {
    pub fn invalid() -> Self {
        StepId(std::usize::MAX)
    }

    pub fn replacement(n: usize) -> Self {
        StepId(std::usize::MAX - n)
    }
}

impl Add<usize> for StepId {
    type Output = Self;

    fn add(self, other: usize) -> Self {
        StepId(self.0 + other)
    }
}

impl Add<usize> for LocationId {
    type Output = Self;

    fn add(self, other: usize) -> Self {
        LocationId(self.0 + other)
    }
}
