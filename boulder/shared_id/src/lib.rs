use std::{
    fmt,
    ops::{Add, Sub},
};

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

macro_rules! invalid {
    ($($n:ident),+) => {
        $(
            impl $n {
                pub const fn invalid() -> Self {
                    $n(usize::max_value())
                }
            }
        )+
    }
}

macro_rules! add {
    ($($n:ident),+) => {
        $(
            impl Add<usize> for $n {
                type Output = Self;

                fn add(self, other: usize) -> Self {
                    $n(self.0 + other)
                }
            }

            impl Add<$n> for $n {
                type Output = Self;

                fn add(self, other: $n) -> Self {
                    $n(self.0 + other.0)
                }
            }
        )+
    }
}

macro_rules! sub {
    ($($n:ident),+) => {
        $(
            impl Sub<usize> for $n {
                type Output = Self;

                fn sub(self, other: usize) -> Self {
                    $n(self.0 - other)
                }
            }

            impl Sub<$n> for $n {
                type Output = Self;

                fn sub(self, other: $n) -> Self {
                    $n(self.0 - other.0)
                }
            }
        )+
    }
}

t!(BlockId: '~', EntityId, FieldId, FunctionId: '#', TypeId: '%', StepId: '$', InputId, LocationId: '@', TagId);

invalid!(BlockId, FunctionId, StepId);

add!(BlockId, InputId, FunctionId, LocationId, StepId, TagId);

sub!(BlockId, FunctionId);
impl StepId {
    pub fn replacement(n: usize) -> Self {
        StepId(std::usize::MAX - n)
    }
}
