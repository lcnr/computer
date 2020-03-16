use std::convert::TryFrom;

use tindex::{TIndex, TSlice};

use shared_id::{BlockId, FunctionId, StepId, FALSE_TYPE_ID, TRUE_TYPE_ID};

use mir::{binop::Binop, Object, UnaryOperation};

use crate::{BoulderMirInterpreter, InterpretError};

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
            UnaryOperation::Invert => match steps[expr] {
                Object::U8(x) => Ok(Object::U8(!x)),
                Object::U16(x) => Ok(Object::U16(!x)),
                Object::U32(x) => Ok(Object::U32(!x)),
                Object::Variant(x, ref v) => {
                    if v.as_ref() == &Object::Unit && (x == TRUE_TYPE_ID || x == FALSE_TYPE_ID) {
                        Ok(self.mir.ctx.bool_to_object(x == FALSE_TYPE_ID))
                    } else {
                        Err(InterpretError::InvalidOperation(function, block, step))
                    }
                }
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            UnaryOperation::ToBytes => match steps[expr] {
                Object::U16(x) => Ok(Object::Struct(
                    x.to_le_bytes().iter().map(|&v| Object::U8(v)).collect(),
                )),
                Object::U32(x) => Ok(Object::Struct(
                    x.to_le_bytes().iter().map(|&v| Object::U8(v)).collect(),
                )),
                _ => Err(InterpretError::InvalidOperation(function, block, step)),
            },
            UnaryOperation::FromBytes => match steps[expr] {
                Object::Struct(ref content) => {
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
                    "debug ({}:{}:{}): {:?}",
                    function.as_index(),
                    block.as_index(),
                    step.as_index(),
                    &steps[expr]
                );
                Ok(Object::Unit)
            }
            UnaryOperation::BlackBox => Ok(steps[expr].clone()),
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

        binop
            .execute(&self.mir.ctx, steps[l].clone(), steps[r].clone())
            .ok_or(invalid_args)
    }
}
