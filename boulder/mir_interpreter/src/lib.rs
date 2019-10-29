#[cfg(feature = "profiler")]
#[macro_use]
extern crate thread_profiler;

use tindex::TVec;

use shared_id::{FunctionId, TypeId};

use mir::{Action, BlockId, Mir, Object, StepId, Terminator, Type};

mod binop;

#[derive(Debug, Clone)]
pub enum InterpretError {
    InvalidOperation(FunctionId, BlockId, StepId),
    InvalidUnaryOperationArguments(FunctionId, BlockId, StepId, Object),
    InvalidBinopArguments(FunctionId, BlockId, StepId, Object, Object),
    InvalidUnionAccess(FunctionId, BlockId, StepId, Object),
    InvalidReduce(FunctionId, BlockId, StepId, Object),
    UnresolvedMatch(FunctionId, BlockId, TypeId),
}

#[derive(Debug, Clone)]
pub struct BoulderMirInterpreter<'a> {
    mir: &'a Mir<'a>,
}

impl<'a> BoulderMirInterpreter<'a> {
    pub fn new(mir: &'a Mir<'a>) -> Self {
        Self { mir }
    }

    pub fn execute_function(
        &mut self,
        id: FunctionId,
        args: &[Object],
    ) -> Result<Object, InterpretError> {
        #[cfg(feature = "profiler")]
        profile_scope!("execute_function");
        let mut args_storage: Vec<_>;
        let mut args = args;
        let mut curr_block = BlockId::from(0);
        'outer: loop {
            #[cfg(feature = "profiler")]
            profile_scope!("execute_block");
            let mut steps = TVec::with_capacity(self.mir[id][curr_block].steps.len());
            for (step_id, step) in self.mir[id][curr_block].steps.iter().enumerate() {
                #[cfg(feature = "profiler")]
                profile_scope!("execute_step");
                let step_id = StepId::from(step_id);
                steps.push(match &step.action {
                    &Action::LoadInput(idx) => {
                        if let Some(obj) = args.get(idx) {
                            obj.clone()
                        } else {
                            return Err(InterpretError::InvalidOperation(id, curr_block, step_id));
                        }
                    }
                    &Action::LoadConstant(ref obj) => obj.clone(),
                    &Action::StructFieldAccess(step, field) => {
                        if let &Object::Struct(ref fields) = &steps[step] {
                            if let Some(field) = fields.get(field) {
                                field.clone()
                            } else {
                                return Err(InterpretError::InvalidOperation(
                                    id, curr_block, step_id,
                                ));
                            }
                        } else {
                            return Err(InterpretError::InvalidOperation(id, curr_block, step_id));
                        }
                    }
                    &Action::UnionFieldAccess(step, expected_id) => {
                        if let &Object::Field(actual_id, ref actual_field) = &steps[step] {
                            if expected_id != actual_id {
                                return Err(InterpretError::InvalidUnionAccess(
                                    id,
                                    curr_block,
                                    step_id,
                                    steps[step].clone(),
                                ));
                            } else {
                                actual_field.as_ref().clone()
                            }
                        } else {
                            return Err(InterpretError::InvalidOperation(id, curr_block, step_id));
                        }
                    }
                    &Action::Extend(target) => match &steps[target] {
                        obj @ Object::Variant(_, _) => obj.clone(),
                        obj => Object::Variant(
                            self.mir[id][curr_block].steps[target].ty,
                            Box::new(obj.clone()),
                        ),
                    },
                    &Action::Reduce(target) => match &steps[target] {
                        &Object::Variant(ty, ref content) => {
                            if let Type::Sum(cases) = &self.mir.types[step.ty] {
                                if cases.get(ty) {
                                    Object::Variant(ty, content.clone())
                                } else {
                                    return Err(InterpretError::InvalidReduce(
                                        id,
                                        curr_block,
                                        step_id,
                                        steps[target].clone(),
                                    ));
                                }
                            } else if step.ty == ty {
                                content.as_ref().clone()
                            } else {
                                return Err(InterpretError::InvalidReduce(
                                    id,
                                    curr_block,
                                    step_id,
                                    steps[target].clone(),
                                ));
                            }
                        }
                        _ => {
                            return Err(InterpretError::InvalidReduce(
                                id,
                                curr_block,
                                step_id,
                                steps[target].clone(),
                            ))
                        }
                    },
                    &Action::InitializeStruct(ref fields) => {
                        Object::Struct(fields.iter().map(|&f| steps[f].clone()).collect())
                    }
                    &Action::InitializeUnion(target, field) => {
                        Object::Field(field, Box::new(steps[target].clone()))
                    }
                    &Action::CallFunction(target_id, ref args) => {
                        let args: Vec<_> = args.iter().map(|&id| steps[id].clone()).collect();
                        self.execute_function(target_id, &args)?
                    }
                    &Action::UnaryOperation(op, expr) => {
                        self.execute_unary_operation(&steps, id, curr_block, step_id, op, expr)?
                    }
                    &Action::Binop(binop, a, b) => {
                        self.execute_binop(&steps, id, curr_block, step_id, binop, a, b)?
                    }
                });
            }

            match &self.mir[id][curr_block].terminator {
                &Terminator::Goto(None, ref ids) => return Ok(steps.remove(ids[0])),
                &Terminator::Goto(Some(block), ref input_steps) => {
                    args_storage = input_steps.iter().map(|&id| steps[id].clone()).collect();
                    args = &args_storage;
                    curr_block = block;
                }
                &Terminator::Match(expr, ref arms) => {
                    if let &Object::Variant(ty, ref obj) = &steps[expr] {
                        for &(arm_ty, block, ref arm_args) in arms.iter() {
                            if Type::is_subtype(ty, arm_ty, &self.mir.types) {
                                args_storage = arm_args
                                    .iter()
                                    .map(|&id| {
                                        id.map_or_else(
                                            || {
                                                if let &Type::Sum(_) = &self.mir.types[arm_ty] {
                                                    steps[expr].clone()
                                                } else {
                                                    obj.as_ref().clone()
                                                }
                                            },
                                            |id| steps[id].clone(),
                                        )
                                    })
                                    .collect();
                                args = &args_storage;
                                if let Some(block) = block {
                                    curr_block = block;

                                    continue 'outer;
                                } else {
                                    return Ok(args[0].clone());
                                }
                            }
                        }
                    } else {
                        let expr_ty = self.mir[id][curr_block].steps[expr].ty;
                        for &(arm_ty, block, ref arm_args) in arms.iter() {
                            if Type::is_subtype(expr_ty, arm_ty, &self.mir.types) {
                                args_storage = arm_args
                                    .iter()
                                    .map(|&id| {
                                        id.map_or_else(
                                            || steps[expr].clone(),
                                            |id| steps[id].clone(),
                                        )
                                    })
                                    .collect();
                                args = &args_storage;
                                if let Some(block) = block {
                                    curr_block = block;
                                    continue 'outer;
                                } else {
                                    return Ok(args[0].clone());
                                }
                            }
                        }
                    }

                    panic!("unexpected_match: {}:{:?}:{:?}", id, curr_block, expr);
                }
            }
        }
    }
}
