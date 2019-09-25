use std::fmt;

use crate::{MirState, Step};

pub trait Binop<M: MirState>: fmt::Display + fmt::Debug + Clone {
    fn validate(&self, this: &Step<M>, a: &Step<M>, b: &Step<M>);
}

#[derive(Debug, Clone, Copy)]
pub enum ExtendedBinop {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    BitOr,
}

impl<M: MirState> Binop<M> for ExtendedBinop {
    fn validate(&self, this: &Step<M>, a: &Step<M>, b: &Step<M>) {
        match self {
            Self::Add | Self::Sub | Self::Mul | Self::Div => {
                assert_eq!(a.ty, b.ty);
                assert_eq!(a.ty, this.ty);
            }
            Self::Lt => {
                assert_eq!(a.ty, b.ty);
                // TODO: check `this.ty == Bool`
            }
            Self::BitOr => unimplemented!("bitor"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ReducedBinop {
    Add,
    Sub,
    Lt,
    BitOr,
}

impl<M: MirState> Binop<M> for ReducedBinop {
    fn validate(&self, this: &Step<M>, a: &Step<M>, b: &Step<M>) {
        match self {
            Self::Add | Self::Sub => {
                assert_eq!(a.ty, b.ty);
                assert_eq!(a.ty, this.ty);
            }
            Self::Lt => {
                assert_eq!(a.ty, b.ty);
                // TODO: check `this.ty == Bool`
            }
            Self::BitOr => unimplemented!("bitor"),
        }
    }
}
