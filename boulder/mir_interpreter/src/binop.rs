use std::convert::TryFrom;

use tindex::TSlice;

use shared_id::{FunctionId, FALSE_TYPE_ID, TRUE_TYPE_ID};

use mir::{binop::Binop, BlockId, Object, StepId, UnaryOperation};

use crate::{BoulderMirInterpreter, InterpretError};

fn to_bool(b: bool) -> Object {
    Object::Variant(
        if b { TRUE_TYPE_ID } else { FALSE_TYPE_ID },
        Box::new(Object::Unit),
    )
}

impl<'a> BoulderMirInterpreter<'a> {
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
                        Ok(to_bool(x == FALSE_TYPE_ID))
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
        }
    }

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
        #[cfg(feature = "profiler")]
        profile_scope!("execute_binop");
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
            Binop::Rem => match (&steps[a], &steps[b]) {
                (&Object::U8(x), &Object::U8(y)) => {
                    x.checked_rem(y).map(|r| Object::U8(r)).ok_or(invalid_args)
                }
                (&Object::U16(x), &Object::U16(y)) => {
                    x.checked_rem(y).map(|r| Object::U16(r)).ok_or(invalid_args)
                }
                (&Object::U32(x), &Object::U32(y)) => {
                    x.checked_rem(y).map(|r| Object::U32(r)).ok_or(invalid_args)
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Shl => match (&steps[a], &steps[b]) {
                (&Object::U8(x), &Object::U8(y)) => {
                    Ok(Object::U8(x.checked_shl(y.into()).unwrap_or(0)))
                }
                (&Object::U16(x), &Object::U16(y)) => {
                    Ok(Object::U16(x.checked_shl(y.into()).unwrap_or(0)))
                }
                (&Object::U32(x), &Object::U32(y)) => {
                    Ok(Object::U32(x.checked_shl(y.into()).unwrap_or(0)))
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::Shr => match (&steps[a], &steps[b]) {
                (&Object::U8(x), &Object::U8(y)) => {
                    Ok(Object::U8(x.checked_shr(y.into()).unwrap_or(0)))
                }
                (&Object::U16(x), &Object::U16(y)) => {
                    Ok(Object::U16(x.checked_shr(y.into()).unwrap_or(0)))
                }
                (&Object::U32(x), &Object::U32(y)) => {
                    Ok(Object::U32(x.checked_shr(y.into()).unwrap_or(0)))
                }
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
                (&Object::Variant(x, ref v), &Object::Variant(y, ref u)) => {
                    if (v.as_ref() == &Object::Unit && u.as_ref() == &Object::Unit)
                        && (x == TRUE_TYPE_ID || x == FALSE_TYPE_ID)
                        && (y == TRUE_TYPE_ID || y == FALSE_TYPE_ID)
                    {
                        Ok(to_bool(x == TRUE_TYPE_ID || y == TRUE_TYPE_ID))
                    } else {
                        Err(InterpretError::InvalidOperation(function, block, step))
                    }
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            Binop::BitAnd => match (&steps[a], &steps[b]) {
                (&Object::U8(x), &Object::U8(y)) => Ok(Object::U8(x & y)),
                (&Object::U16(x), &Object::U16(y)) => Ok(Object::U16(x & y)),
                (&Object::U32(x), &Object::U32(y)) => Ok(Object::U32(x & y)),
                (&Object::Variant(x, ref v), &Object::Variant(y, ref u)) => {
                    if (v.as_ref() == &Object::Unit && u.as_ref() == &Object::Unit)
                        && (x == TRUE_TYPE_ID || x == FALSE_TYPE_ID)
                        && (y == TRUE_TYPE_ID || y == FALSE_TYPE_ID)
                    {
                        Ok(to_bool(x == TRUE_TYPE_ID && y == TRUE_TYPE_ID))
                    } else {
                        Err(InterpretError::InvalidOperation(function, block, step))
                    }
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
        }
    }
}
