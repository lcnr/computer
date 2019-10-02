use tindex::TSlice;

use shared_id::{FunctionId, FALSE_TYPE_ID, TRUE_TYPE_ID};

use mir::{binop::Binop, BlockId, Object, StepId};

use crate::{BoulderMirInterpreter, InterpretError};

impl<'a> BoulderMirInterpreter<'a> {
    pub fn execute_binop(
        &mut self,
        steps: &TSlice<StepId, Object>,
        function: FunctionId,
        block: BlockId,
        step: StepId,
        binop: Binop,
        a: StepId,
        b: StepId,
    ) -> Result<Object, InterpretError> {
        fn to_bool(b: bool) -> Object {
            Object::Variant(
                if b { TRUE_TYPE_ID } else { FALSE_TYPE_ID },
                Box::new(Object::Unit),
            )
        }

        let invalid_args = InterpretError::InvalidBinopArguments(
            function,
            block,
            step,
            steps[a].clone(),
            steps[b].clone(),
        );

        match binop {
            Binop::Add => match (&steps[a], &steps[b]) {
                (&Object::U8(x), &Object::U8(y)) => {
                    x.checked_add(y).map(|r| Object::U8(r)).ok_or(invalid_args)
                }
                (&Object::U16(x), &Object::U16(y)) => {
                    x.checked_add(y).map(|r| Object::U16(r)).ok_or(invalid_args)
                }
                (&Object::U32(x), &Object::U32(y)) => {
                    x.checked_add(y).map(|r| Object::U32(r)).ok_or(invalid_args)
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Sub => match (&steps[a], &steps[b]) {
                (&Object::U8(x), &Object::U8(y)) => {
                    x.checked_sub(y).map(|r| Object::U8(r)).ok_or(invalid_args)
                }
                (&Object::U16(x), &Object::U16(y)) => {
                    x.checked_sub(y).map(|r| Object::U16(r)).ok_or(invalid_args)
                }
                (&Object::U32(x), &Object::U32(y)) => {
                    x.checked_sub(y).map(|r| Object::U32(r)).ok_or(invalid_args)
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Mul => match (&steps[a], &steps[b]) {
                (&Object::U8(x), &Object::U8(y)) => {
                    x.checked_mul(y).map(|r| Object::U8(r)).ok_or(invalid_args)
                }
                (&Object::U16(x), &Object::U16(y)) => {
                    x.checked_mul(y).map(|r| Object::U16(r)).ok_or(invalid_args)
                }
                (&Object::U32(x), &Object::U32(y)) => {
                    x.checked_mul(y).map(|r| Object::U32(r)).ok_or(invalid_args)
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Div => match (&steps[a], &steps[b]) {
                (&Object::U8(x), &Object::U8(y)) => {
                    x.checked_div(y).map(|r| Object::U8(r)).ok_or(invalid_args)
                }
                (&Object::U16(x), &Object::U16(y)) => {
                    x.checked_div(y).map(|r| Object::U16(r)).ok_or(invalid_args)
                }
                (&Object::U32(x), &Object::U32(y)) => {
                    x.checked_div(y).map(|r| Object::U32(r)).ok_or(invalid_args)
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Shl => match (&steps[a], &steps[b]) {
                (&Object::U8(x), &Object::U8(y)) => x
                    .checked_shl(y.into())
                    .map(|r| Object::U8(r))
                    .ok_or(invalid_args),
                (&Object::U16(x), &Object::U16(y)) => x
                    .checked_shl(y.into())
                    .map(|r| Object::U16(r))
                    .ok_or(invalid_args),
                (&Object::U32(x), &Object::U32(y)) => x
                    .checked_shl(y.into())
                    .map(|r| Object::U32(r))
                    .ok_or(invalid_args),
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Shr => match (&steps[a], &steps[b]) {
                (&Object::U8(x), &Object::U8(y)) => x
                    .checked_shr(y.into())
                    .map(|r| Object::U8(r))
                    .ok_or(invalid_args),
                (&Object::U16(x), &Object::U16(y)) => x
                    .checked_shr(y.into())
                    .map(|r| Object::U16(r))
                    .ok_or(invalid_args),
                (&Object::U32(x), &Object::U32(y)) => x
                    .checked_shr(y.into())
                    .map(|r| Object::U32(r))
                    .ok_or(invalid_args),
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Eq => match (&steps[a], &steps[b]) {
                (&Object::U8(x), &Object::U8(y)) => Ok(to_bool(x == y)),
                (&Object::U16(x), &Object::U16(y)) => Ok(to_bool(x == y)),
                (&Object::U32(x), &Object::U32(y)) => Ok(to_bool(x == y)),
                (&Object::Variant(x, ref v), &Object::Variant(y, ref u)) => {
                    if (v.as_ref() == &Object::Unit && u.as_ref() == &Object::Unit)
                        && (x == TRUE_TYPE_ID || x == FALSE_TYPE_ID)
                        && (y == TRUE_TYPE_ID || y == FALSE_TYPE_ID)
                    {
                        Ok(to_bool(x == y))
                    } else {
                        Err(InterpretError::InvalidOperation(function, block, step))
                    }
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Neq => match (&steps[a], &steps[b]) {
                (&Object::U8(x), &Object::U8(y)) => Ok(to_bool(x != y)),
                (&Object::U16(x), &Object::U16(y)) => Ok(to_bool(x != y)),
                (&Object::U32(x), &Object::U32(y)) => Ok(to_bool(x != y)),
                (&Object::Variant(x, ref v), &Object::Variant(y, ref u)) => {
                    if (v.as_ref() == &Object::Unit && u.as_ref() == &Object::Unit)
                        && (x == TRUE_TYPE_ID || x == FALSE_TYPE_ID)
                        && (y == TRUE_TYPE_ID || y == FALSE_TYPE_ID)
                    {
                        Ok(to_bool(x != y))
                    } else {
                        Err(InterpretError::InvalidOperation(function, block, step))
                    }
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Gt => match (&steps[a], &steps[b]) {
                (&Object::U8(x), &Object::U8(y)) => Ok(to_bool(x > y)),
                (&Object::U16(x), &Object::U16(y)) => Ok(to_bool(x > y)),
                (&Object::U32(x), &Object::U32(y)) => Ok(to_bool(x > y)),
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Gte => match (&steps[a], &steps[b]) {
                (&Object::U8(x), &Object::U8(y)) => Ok(to_bool(x >= y)),
                (&Object::U16(x), &Object::U16(y)) => Ok(to_bool(x >= y)),
                (&Object::U32(x), &Object::U32(y)) => Ok(to_bool(x >= y)),
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::BitOr => match (&steps[a], &steps[b]) {
                (&Object::U8(x), &Object::U8(y)) => Ok(Object::U8(x | y)),
                (&Object::U16(x), &Object::U16(y)) => Ok(Object::U16(x | y)),
                (&Object::U32(x), &Object::U32(y)) => Ok(Object::U32(x | y)),
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::BitAnd => match (&steps[a], &steps[b]) {
                (&Object::U8(x), &Object::U8(y)) => Ok(Object::U8(x & y)),
                (&Object::U16(x), &Object::U16(y)) => Ok(Object::U16(x & y)),
                (&Object::U32(x), &Object::U32(y)) => Ok(Object::U32(x & y)),
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
        }
    }
}
