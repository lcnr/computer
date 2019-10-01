use tindex::TVec;

use shared_id::{FunctionId, TypeId};

use mir::{Action, BlockId, Mir, Object, StepId, Terminator, Type};

mod binop;

#[derive(Debug, Clone)]
pub enum InterpretError {
    InvalidOperation(FunctionId, BlockId, StepId),
    InvalidBinopArguments(FunctionId, BlockId, StepId, Object, Object),
    UnresolvedMatch(FunctionId, BlockId, TypeId),
}

#[derive(Debug, Clone)]
pub struct BoulderMirInterpreter<'a> {
    mir: &'a Mir,
}

impl<'a> BoulderMirInterpreter<'a> {
    pub fn new(mir: &'a Mir) -> Self {
        Self { mir }
    }

    pub fn execute_function(
        &mut self,
        id: FunctionId,
        args: &[Object],
    ) -> Result<Object, InterpretError> {
        let mut args_storage: Vec<_>;
        let mut args = args;
        let mut curr_block = BlockId::from(0);
        'outer: loop {
            let mut steps = TVec::with_capacity(self.mir[id][curr_block].steps.len());
            for (step_id, step) in self.mir[id][curr_block].steps.iter().enumerate() {
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
                    &Action::FieldAccess(step, field) => {
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
                    &Action::Extend(target) => match &steps[target] {
                        obj @ Object::Variant(_, _) => obj.clone(),
                        obj => Object::Variant(
                            self.mir[id][curr_block].steps[target].ty,
                            Box::new(obj.clone()),
                        ),
                    },
                    &Action::CallFunction(target_id, ref args) => {
                        let args: Vec<_> = args.iter().map(|&id| steps[id].clone()).collect();
                        self.execute_function(target_id, &args)?
                    }
                    &Action::Binop(binop, a, b) => {
                        self.execute_binop(&steps, id, curr_block, step_id, binop, a, b)?
                    }
                });
            }

            match &self.mir[id][curr_block].terminator {
                &Terminator::Return(id) => return Ok(steps.remove(id)),
                &Terminator::Goto(block, ref input_steps) => {
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
                                            || obj.as_ref().clone(),
                                            |id| steps[id].clone(),
                                        )
                                    })
                                    .collect();
                                args = &args_storage;
                                curr_block = block;
                                continue 'outer;
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
                                curr_block = block;
                                continue 'outer;
                            }
                        }
                    }

                    panic!("unexpected_match: {}:{:?}:{:?}", id, curr_block, expr);
                }
            }
        }
    }
}
