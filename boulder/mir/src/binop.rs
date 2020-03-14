use shared_id::{
    BlockId, FunctionId, StepId, BOOL_TYPE_ID, FALSE_TYPE_ID, TRUE_TYPE_ID, U8_TYPE_ID,
};

use crate::{ctx::Context, Action, Mir, Object, Step, Type};

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

fn bool_op<F>(ctx: &Context, l: Object, r: Object, f: F) -> Option<Object>
where
    F: FnOnce(bool, bool) -> bool,
{
    if let (Object::Variant(l, v), Object::Variant(r, u)) = (l, r) {
        if (*v == Object::Unit && *u == Object::Unit)
            && (l == TRUE_TYPE_ID || l == FALSE_TYPE_ID)
            && (r == TRUE_TYPE_ID || r == FALSE_TYPE_ID)
        {
            Some(ctx.bool_to_object(f(l == TRUE_TYPE_ID, r == TRUE_TYPE_ID)))
        } else {
            None
        }
    } else {
        None
    }
}

impl Binop {
    pub fn execute(self, ctx: &Context, lhs: Object, rhs: Object) -> Option<Object> {
        match self {
            Binop::Add => match (lhs, rhs) {
                (Object::U8(l), Object::U8(r)) => l.checked_add(r).map(Object::U8),
                (Object::U16(l), Object::U16(r)) => l.checked_add(r).map(Object::U16),
                (Object::U32(l), Object::U32(r)) => l.checked_add(r).map(Object::U32),
                _ => None,
            },
            Binop::Sub => match (lhs, rhs) {
                (Object::U8(l), Object::U8(r)) => l.checked_sub(r).map(Object::U8),
                (Object::U16(l), Object::U16(r)) => l.checked_sub(r).map(Object::U16),
                (Object::U32(l), Object::U32(r)) => l.checked_sub(r).map(Object::U32),
                _ => None,
            },
            Binop::Mul => match (lhs, rhs) {
                (Object::U8(l), Object::U8(r)) => l.checked_mul(r).map(Object::U8),
                (Object::U16(l), Object::U16(r)) => l.checked_mul(r).map(Object::U16),
                (Object::U32(l), Object::U32(r)) => l.checked_mul(r).map(Object::U32),
                _ => None,
            },
            Binop::Div => match (lhs, rhs) {
                (Object::U8(l), Object::U8(r)) => l.checked_div(r).map(Object::U8),
                (Object::U16(l), Object::U16(r)) => l.checked_div(r).map(Object::U16),
                (Object::U32(l), Object::U32(r)) => l.checked_div(r).map(Object::U32),
                _ => None,
            },
            Binop::Rem => match (lhs, rhs) {
                (Object::U8(l), Object::U8(r)) => l.checked_rem(r).map(Object::U8),
                (Object::U16(l), Object::U16(r)) => l.checked_rem(r).map(Object::U16),
                (Object::U32(l), Object::U32(r)) => l.checked_rem(r).map(Object::U32),
                _ => None,
            },
            Binop::Shl => match (lhs, rhs) {
                (Object::U8(l), Object::U8(r)) => {
                    Some(Object::U8(l.checked_shl(r.into()).unwrap_or(0)))
                }
                (Object::U16(l), Object::U16(r)) => {
                    Some(Object::U16(l.checked_shl(r.into()).unwrap_or(0)))
                }
                (Object::U32(l), Object::U32(r)) => {
                    Some(Object::U32(l.checked_shl(r).unwrap_or(0)))
                }
                _ => None,
            },
            Binop::Shr => match (lhs, rhs) {
                (Object::U8(l), Object::U8(r)) => {
                    Some(Object::U8(l.checked_shr(r.into()).unwrap_or(0)))
                }
                (Object::U16(l), Object::U16(r)) => {
                    Some(Object::U16(l.checked_shr(r.into()).unwrap_or(0)))
                }
                (Object::U32(l), Object::U32(r)) => {
                    Some(Object::U32(l.checked_shr(r).unwrap_or(0)))
                }
                _ => None,
            },
            Binop::Eq => match (lhs, rhs) {
                (Object::U8(l), Object::U8(r)) => Some(ctx.bool_to_object(l == r)),
                (Object::U16(l), Object::U16(r)) => Some(ctx.bool_to_object(l == r)),
                (Object::U32(l), Object::U32(r)) => Some(ctx.bool_to_object(l == r)),
                (l, r) => bool_op(ctx, l, r, |l, r| l == r),
            },
            Binop::Neq => match (lhs, rhs) {
                (Object::U8(l), Object::U8(r)) => Some(ctx.bool_to_object(l != r)),
                (Object::U16(l), Object::U16(r)) => Some(ctx.bool_to_object(l != r)),
                (Object::U32(l), Object::U32(r)) => Some(ctx.bool_to_object(l != r)),
                (l, r) => bool_op(ctx, l, r, |l, r| l != r),
            },
            Binop::Gt => match (lhs, rhs) {
                (Object::U8(l), Object::U8(r)) => Some(ctx.bool_to_object(l > r)),
                (Object::U16(l), Object::U16(r)) => Some(ctx.bool_to_object(l > r)),
                (Object::U32(l), Object::U32(r)) => Some(ctx.bool_to_object(l > r)),
                (l, r) => bool_op(ctx, l, r, |l, r| l > r),
            },
            Binop::Gte => match (lhs, rhs) {
                (Object::U8(l), Object::U8(r)) => Some(ctx.bool_to_object(l >= r)),
                (Object::U16(l), Object::U16(r)) => Some(ctx.bool_to_object(l >= r)),
                (Object::U32(l), Object::U32(r)) => Some(ctx.bool_to_object(l >= r)),
                (l, r) => bool_op(ctx, l, r, |l, r| l >= r),
            },
            Binop::BitOr => match (lhs, rhs) {
                (Object::U8(l), Object::U8(r)) => Some(Object::U8(l | r)),
                (Object::U16(l), Object::U16(r)) => Some(Object::U16(l | r)),
                (Object::U32(l), Object::U32(r)) => Some(Object::U32(l | r)),
                (l, r) => bool_op(ctx, l, r, |l, r| l | r),
            },
            Binop::BitAnd => match (lhs, rhs) {
                (Object::U8(l), Object::U8(r)) => Some(Object::U8(l & r)),
                (Object::U16(l), Object::U16(r)) => Some(Object::U16(l & r)),
                (Object::U32(l), Object::U32(r)) => Some(Object::U32(l & r)),
                (l, r) => bool_op(ctx, l, r, |l, r| l & r),
            },
            Binop::BitXor => match (lhs, rhs) {
                (Object::U8(l), Object::U8(r)) => Some(Object::U8(l ^ r)),
                (Object::U16(l), Object::U16(r)) => Some(Object::U16(l ^ r)),
                (Object::U32(l), Object::U32(r)) => Some(Object::U32(l ^ r)),
                (l, r) => bool_op(ctx, l, r, |l, r| l ^ r),
            },
        }
    }

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
