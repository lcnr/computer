use shared_id::BOOL_TYPE_ID;

use crate::{Action, Mir, Step, StepId};

#[derive(Debug, Clone, Copy)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    BitOr,
    BitAnd,
}

impl Binop {
    pub fn validate(&self, this: &Step, a: &Step, b: &Step) {
        match self {
            Self::Add | Self::Sub | Self::Mul | Self::Div | Self::BitOr | Self::BitAnd => {
                assert_eq!(a.ty, b.ty);
                assert_eq!(a.ty, this.ty);
            }
            Self::Lt => {
                assert_eq!(a.ty, b.ty);
                assert_eq!(this.ty, BOOL_TYPE_ID);
            }
        }
    }
}

impl Mir {
    pub fn reduce_binops(&mut self) {
        for function in self.functions.iter_mut() {
            for block in function.blocks.iter_mut() {
                let mut i = StepId(0);
                while i.0 < block.steps.len() {
                    match &block.steps[i].action {
                        Action::Binop(Binop::Mul, _a, _b) => {}
                        _ => (),
                    }

                    i.0 += 1;
                }
            }
        }
    }
}
