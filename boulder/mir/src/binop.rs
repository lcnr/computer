use std::fmt;

use crate::{traits::UpdateStepIds, Action, Block, Function, Mir, MirState, Step, StepId};

#[derive(Debug, Clone, Copy)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    BitOr,
}

impl Binop {
    pub fn validate<M: MirState>(&self, this: &Step<M>, a: &Step<M>, b: &Step<M>) {
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

impl<M: MirState<StepMeta = ()>> Mir<M> {
    pub fn reduce_binops(&mut self) {
        for function in self.functions.iter_mut() {
            for block in function.blocks.iter_mut() {
                let mut i = StepId(0);
                while i.0 < block.steps.len() {
                    match &block.steps[i].action {
                        Action::Binop(Binop::Mul, a, b) => {}
                        _ => (),
                    }

                    i.0 += 1;
                }
            }
        }
    }
}
