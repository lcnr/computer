use std::convert::TryFrom;

use tindex::{TIndex, TSlice};

use shared_id::{FunctionId, FALSE_TYPE_ID, TRUE_TYPE_ID};

use mir::{binop::Binop, BlockId, Object, StepId, UnaryOperation};

use crate::{BoulderMirInterpreter, InterpretError};

impl<'a> BoulderMirInterpreter<'a> {
    fn to_bool(&self, b: bool) -> Object {
        if self.e2b {
            if b {
                Object::U8(self.mir.ctx.true_replacement)
            } else {
                Object::U8(self.mir.ctx.false_replacement)
            }
        } else {
            Object::Variant(
                if b { TRUE_TYPE_ID } else { FALSE_TYPE_ID },
                Box::new(Object::Unit),
            )
        }
    }

    pub fn execute_unary_operation(
        &mut self,
        steps: &TSlice<StepId, Object>,
        function: FunctionId,
        block: BlockId,
        step: StepId,
        op: UnaryOperation,
        expr: StepId,
    ) -> Result<Object, InterpretError> {
        #[cfg(feature = "profiler")]
        profile_scope!("execute_unary_operation");
        match op {
            UnaryOperation::Invert => match &steps[expr] {
                &Object::U8(x) => Ok(Object::U8(!x)),
                &Object::U16(x) => Ok(Object::U16(!x)),
                &Object::U32(x) => Ok(Object::U32(!x)),
                &Object::Variant(x, ref v) => {
                    if v.as_ref() == &Object::Unit && (x == TRUE_TYPE_ID || x == FALSE_TYPE_ID) {
                        Ok(self.to_bool(x == FALSE_TYPE_ID))
                    } else {
                        Err(InterpretError::InvalidOperation(function, block, step))
                    }
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            UnaryOperation::ToBytes => match &steps[expr] {
                &Object::U16(x) => Ok(Object::Struct(
                    x.to_le_bytes().iter().map(|&v| Object::U8(v)).collect(),
                )),
                &Object::U32(x) => Ok(Object::Struct(
                    x.to_le_bytes().iter().map(|&v| Object::U8(v)).collect(),
                )),
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            UnaryOperation::FromBytes => match &steps[expr] {
                &Object::Struct(ref content) => {
                    if let Ok(&[Object::U8(a), Object::U8(b)]) =
                        <&[mir::Object; 2]>::try_from(content.to_slice())
                    {
                        Ok(Object::U16(u16::from_le_bytes([a, b])))
                    } else if let Ok(
                        &[Object::U8(a), Object::U8(b), Object::U8(c), Object::U8(d)],
                    ) = <&[mir::Object; 4]>::try_from(content.to_slice())
                    {
                        Ok(Object::U32(u32::from_le_bytes([a, b, c, d])))
                    } else {
                        Err(InterpretError::InvalidOperation(function, block, step))
                    }
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            UnaryOperation::Debug => {
                println!(
                    "({}:{}:{}): {:?}",
                    function.as_index(),
                    block.as_index(),
                    step.as_index(),
                    &steps[expr]
                );
                Ok(Object::Unit)
            }
        }
    }

    pub fn execute_binop(
        &mut self,
        steps: &TSlice<StepId, Object>,
        (function, block, step): (FunctionId, BlockId, StepId),
        binop: Binop,
        l: StepId,
        r: StepId,
    ) -> Result<Object, InterpretError> {
        #[cfg(feature = "profiler")]
        profile_scope!("execute_binop");
        let invalid_args = InterpretError::InvalidBinopArguments(
            function,
            block,
            step,
            steps[l].clone(),
            steps[r].clone(),
        );

        match binop {
            Binop::Add => match (&steps[l], &steps[r]) {
                (&Object::U8(l), &Object::U8(r)) => {
                    l.checked_add(r).map(Object::U8).ok_or(invalid_args)
                }
                (&Object::U16(l), &Object::U16(r)) => {
                    l.checked_add(r).map(Object::U16).ok_or(invalid_args)
                }
                (&Object::U32(l), &Object::U32(r)) => {
                    l.checked_add(r).map(Object::U32).ok_or(invalid_args)
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Sub => match (&steps[l], &steps[r]) {
                (&Object::U8(l), &Object::U8(r)) => {
                    l.checked_sub(r).map(Object::U8).ok_or(invalid_args)
                }
                (&Object::U16(l), &Object::U16(r)) => {
                    l.checked_sub(r).map(Object::U16).ok_or(invalid_args)
                }
                (&Object::U32(l), &Object::U32(r)) => {
                    l.checked_sub(r).map(Object::U32).ok_or(invalid_args)
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Mul => match (&steps[l], &steps[r]) {
                (&Object::U8(l), &Object::U8(r)) => {
                    l.checked_mul(r).map(Object::U8).ok_or(invalid_args)
                }
                (&Object::U16(l), &Object::U16(r)) => {
                    l.checked_mul(r).map(Object::U16).ok_or(invalid_args)
                }
                (&Object::U32(l), &Object::U32(r)) => {
                    l.checked_mul(r).map(Object::U32).ok_or(invalid_args)
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Div => match (&steps[l], &steps[r]) {
                (&Object::U8(l), &Object::U8(r)) => {
                    l.checked_div(r).map(Object::U8).ok_or(invalid_args)
                }
                (&Object::U16(l), &Object::U16(r)) => {
                    l.checked_div(r).map(Object::U16).ok_or(invalid_args)
                }
                (&Object::U32(l), &Object::U32(r)) => {
                    l.checked_div(r).map(Object::U32).ok_or(invalid_args)
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Rem => match (&steps[l], &steps[r]) {
                (&Object::U8(l), &Object::U8(r)) => {
                    l.checked_rem(r).map(Object::U8).ok_or(invalid_args)
                }
                (&Object::U16(l), &Object::U16(r)) => {
                    l.checked_rem(r).map(Object::U16).ok_or(invalid_args)
                }
                (&Object::U32(l), &Object::U32(r)) => {
                    l.checked_rem(r).map(Object::U32).ok_or(invalid_args)
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Shl => match (&steps[l], &steps[r]) {
                (&Object::U8(l), &Object::U8(r)) => {
                    Ok(Object::U8(l.checked_shl(r.into()).unwrap_or(0)))
                }
                (&Object::U16(l), &Object::U16(r)) => {
                    Ok(Object::U16(l.checked_shl(r.into()).unwrap_or(0)))
                }
                (&Object::U32(l), &Object::U32(r)) => {
                    Ok(Object::U32(l.checked_shl(r).unwrap_or(0)))
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Shr => match (&steps[l], &steps[r]) {
                (&Object::U8(l), &Object::U8(r)) => {
                    Ok(Object::U8(l.checked_shr(r.into()).unwrap_or(0)))
                }
                (&Object::U16(l), &Object::U16(r)) => {
                    Ok(Object::U16(l.checked_shr(r.into()).unwrap_or(0)))
                }
                (&Object::U32(l), &Object::U32(r)) => {
                    Ok(Object::U32(l.checked_shr(r).unwrap_or(0)))
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Eq => match (&steps[l], &steps[r]) {
                (&Object::U8(l), &Object::U8(r)) => Ok(self.to_bool(l == r)),
                (&Object::U16(l), &Object::U16(r)) => Ok(self.to_bool(l == r)),
                (&Object::U32(l), &Object::U32(r)) => Ok(self.to_bool(l == r)),
                (&Object::Variant(l, ref v), &Object::Variant(r, ref u)) => {
                    if (v.as_ref() == &Object::Unit && u.as_ref() == &Object::Unit)
                        && (l == TRUE_TYPE_ID || l == FALSE_TYPE_ID)
                        && (r == TRUE_TYPE_ID || r == FALSE_TYPE_ID)
                    {
                        Ok(self.to_bool(l == r))
                    } else {
                        Err(InterpretError::InvalidOperation(function, block, step))
                    }
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Neq => match (&steps[l], &steps[r]) {
                (&Object::U8(l), &Object::U8(r)) => Ok(self.to_bool(l != r)),
                (&Object::U16(l), &Object::U16(r)) => Ok(self.to_bool(l != r)),
                (&Object::U32(l), &Object::U32(r)) => Ok(self.to_bool(l != r)),
                (&Object::Variant(l, ref v), &Object::Variant(r, ref u)) => {
                    if (v.as_ref() == &Object::Unit && u.as_ref() == &Object::Unit)
                        && (l == TRUE_TYPE_ID || l == FALSE_TYPE_ID)
                        && (r == TRUE_TYPE_ID || r == FALSE_TYPE_ID)
                    {
                        Ok(self.to_bool(l != r))
                    } else {
                        Err(InterpretError::InvalidOperation(function, block, step))
                    }
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Gt => match (&steps[l], &steps[r]) {
                (&Object::U8(l), &Object::U8(r)) => Ok(self.to_bool(l > r)),
                (&Object::U16(l), &Object::U16(r)) => Ok(self.to_bool(l > r)),
                (&Object::U32(l), &Object::U32(r)) => Ok(self.to_bool(l > r)),
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Gte => match (&steps[l], &steps[r]) {
                (&Object::U8(l), &Object::U8(r)) => Ok(self.to_bool(l >= r)),
                (&Object::U16(l), &Object::U16(r)) => Ok(self.to_bool(l >= r)),
                (&Object::U32(l), &Object::U32(r)) => Ok(self.to_bool(l >= r)),
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::BitOr => match (&steps[l], &steps[r]) {
                (&Object::U8(l), &Object::U8(r)) => Ok(Object::U8(l | r)),
                (&Object::U16(l), &Object::U16(r)) => Ok(Object::U16(l | r)),
                (&Object::U32(l), &Object::U32(r)) => Ok(Object::U32(l | r)),
                (&Object::Variant(l, ref v), &Object::Variant(r, ref u)) => {
                    if (v.as_ref() == &Object::Unit && u.as_ref() == &Object::Unit)
                        && (l == TRUE_TYPE_ID || l == FALSE_TYPE_ID)
                        && (r == TRUE_TYPE_ID || r == FALSE_TYPE_ID)
                    {
                        Ok(self.to_bool(l == TRUE_TYPE_ID || r == TRUE_TYPE_ID))
                    } else {
                        Err(InterpretError::InvalidOperation(function, block, step))
                    }
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::BitAnd => match (&steps[l], &steps[r]) {
                (&Object::U8(l), &Object::U8(r)) => Ok(Object::U8(l & r)),
                (&Object::U16(l), &Object::U16(r)) => Ok(Object::U16(l & r)),
                (&Object::U32(l), &Object::U32(r)) => Ok(Object::U32(l & r)),
                (&Object::Variant(l, ref v), &Object::Variant(r, ref u)) => {
                    if (v.as_ref() == &Object::Unit && u.as_ref() == &Object::Unit)
                        && (l == TRUE_TYPE_ID || l == FALSE_TYPE_ID)
                        && (r == TRUE_TYPE_ID || r == FALSE_TYPE_ID)
                    {
                        Ok(self.to_bool(l == TRUE_TYPE_ID && r == TRUE_TYPE_ID))
                    } else {
                        Err(InterpretError::InvalidOperation(function, block, step))
                    }
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::BitXor => match (&steps[l], &steps[r]) {
                (&Object::U8(l), &Object::U8(r)) => Ok(Object::U8(l ^ r)),
                (&Object::U16(l), &Object::U16(r)) => Ok(Object::U16(l ^ r)),
                (&Object::U32(l), &Object::U32(r)) => Ok(Object::U32(l ^ r)),
                (&Object::Variant(l, ref v), &Object::Variant(r, ref u)) => {
                    if (v.as_ref() == &Object::Unit && u.as_ref() == &Object::Unit)
                        && (l == TRUE_TYPE_ID || l == FALSE_TYPE_ID)
                        && (r == TRUE_TYPE_ID || r == FALSE_TYPE_ID)
                    {
                        Ok(self.to_bool((l == TRUE_TYPE_ID )^ (r == TRUE_TYPE_ID)))
                    } else {
                        Err(InterpretError::InvalidOperation(function, block, step))
                    }
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
        }
    }
}
