use shared_id::{BlockId, FunctionId, StepId, BOOL_TYPE_ID, U8_TYPE_ID};

use crate::{Action, Mir, Step, Type};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Shl,
    Shr,
    Eq,
    Neq,
    Gt,
    Gte,
    BitOr,
    BitAnd,
    BitXor,
}

impl Binop {
    pub fn validate(self, this: &Step, a: &Step, b: &Step, e2b: bool) {
        match self {
            Self::Add
            | Self::Sub
            | Self::Mul
            | Self::Div
            | Self::Rem
            | Self::Shl
            | Self::Shr
            | Self::BitOr
            | Self::BitAnd
            | Self::BitXor => {
                assert_eq!(a.ty, b.ty);
                assert_eq!(a.ty, this.ty);
            }
            Self::Eq | Self::Neq | Self::Gt | Self::Gte => {
                assert_eq!(a.ty, b.ty);
                if e2b {
                    assert_eq!(this.ty, U8_TYPE_ID);
                } else {
                    assert_eq!(this.ty, BOOL_TYPE_ID);
                }
            }
        }
    }
}

impl<'a> Mir<'a> {
    fn reduce_binops_in_block(&mut self, function: FunctionId, block: BlockId) {
        #[cfg(feature = "profiler")]
        profile_scope!("reduce_binops_in_block");
        let function = &mut self.functions[function];

        let mut i = StepId(0);
        while i.0 < function[block].steps.len() {
            match function[block].steps[i].action {
                Action::Binop(Binop::Mul, a, b) => {
                    let op_ty = function[block].steps[i].ty;
                    function[block].steps[i].action = Action::CallFunction(
                        match self.types[op_ty] {
                            Type::U8 => self.ctx.mul8,
                            Type::U16 => self.ctx.mul16,
                            Type::U32 => self.ctx.mul32,
                            _ => unreachable!("non integer multiplication"),
                        },
                        vec![a, b],
                    );
                }
                Action::Binop(Binop::Div, a, b) => {
                    let op_ty = function[block].steps[i].ty;
                    function[block].steps[i].action = Action::CallFunction(
                        match self.types[op_ty] {
                            Type::U8 => self.ctx.div8,
                            Type::U16 => self.ctx.div16,
                            Type::U32 => self.ctx.div32,
                            _ => unreachable!("non integer division"),
                        },
                        vec![a, b],
                    );
                }
                Action::Binop(Binop::Rem, a, b) => {
                    let op_ty = function[block].steps[i].ty;
                    function[block].steps[i].action = Action::CallFunction(
                        match self.types[op_ty] {
                            Type::U8 => self.ctx.rem8,
                            Type::U16 => self.ctx.rem16,
                            Type::U32 => self.ctx.rem32,
                            _ => unreachable!("non integer remainder"),
                        },
                        vec![a, b],
                    );
                }
                _ => {}
            }

            i.0 += 1;
        }
    }

    pub fn reduce_binops(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("reduce_binops");
        for function in 0..self.functions.len() {
            let function = FunctionId::from(function);
            let mut block = BlockId(0);
            while block.0 < self[function].blocks.len() {
                self.reduce_binops_in_block(function, block);
                block.0 += 1;
            }
        }
    }
}
