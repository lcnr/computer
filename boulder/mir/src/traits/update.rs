use tindex::TIndex;

use shared_id::{FunctionId, TypeId};

use crate::{Action, Block, Function, Step, StepId, Terminator, ctx::Context};

pub trait UpdateStepIds {
    fn update_step_ids(&mut self, f: &mut dyn FnMut(&mut StepId));

    /// shift all step ids for which the condition `self >= after` holds
    fn shift_step_ids(&mut self, after: StepId, by: isize) {
        self.update_step_ids(&mut |id| {
            if *id >= after {
                *id = StepId((id.0 as isize + by) as usize);
            }
        });
    }

    fn replace_step(&mut self, step: StepId, with: StepId) {
        self.update_step_ids(&mut |id| {
            if *id == step {
                *id = with;
            }
        })
    }
}

impl UpdateStepIds for Step {
    fn update_step_ids(&mut self, f: &mut dyn FnMut(&mut StepId)) {
        self.action.update_step_ids(f);
    }
}

impl UpdateStepIds for Action {
    fn update_step_ids(&mut self, f: &mut dyn FnMut(&mut StepId)) {
        match self {
            Action::UnaryOperation(_, id)
            | Action::Extend(id)
            | Action::Reduce(id)
            | Action::StructFieldAccess(id, _)
            | Action::UnionFieldAccess(id)
            | Action::InitializeUnion(id) => f(id),
            Action::Binop(_kind, a, b) => {
                f(a);
                f(b);
            }
            Action::InitializeStruct(fields) => {
                for field in fields {
                    f(field);
                }
            }
            Action::CallFunction(_, args) => {
                for arg in args {
                    f(arg);
                }
            }
            Action::LoadConstant(_) | Action::LoadInput(_) => (),
        }
    }
}
impl UpdateStepIds for Terminator {
    fn update_step_ids(&mut self, f: &mut dyn FnMut(&mut StepId)) {
        match self {
            Terminator::Goto(_, args) => {
                for arg in args {
                    f(arg);
                }
            }
            Terminator::Match(id, arms) => {
                f(id);
                for arm in arms {
                    for arg in arm.2.iter_mut() {
                        if let Some(arg) = arg.as_mut() {
                            f(arg);
                        }
                    }
                }
            }
        }
    }
}

impl UpdateStepIds for Block {
    fn update_step_ids(&mut self, f: &mut dyn FnMut(&mut StepId)) {
        for step in self.steps.iter_mut() {
            step.update_step_ids(f);
        }

        self.terminator.update_step_ids(f)
    }
}

pub trait UpdateFunctionIds {
    fn update_function_ids(&mut self, f: &mut dyn FnMut(&mut FunctionId));

    /// shift all step ids for which the condition `self >= after` holds
    fn shift_function_ids(&mut self, after: FunctionId, by: isize) {
        self.update_function_ids(&mut |id| {
            if *id >= after {
                *id = ((id.as_index() as isize + by) as usize).into();
            }
        });
    }

    fn replace_function(&mut self, ty: FunctionId, with: FunctionId) {
        self.update_function_ids(&mut |id| {
            if *id == ty {
                *id = with;
            }
        })
    }
}

impl UpdateFunctionIds for Context {
    fn update_function_ids(&mut self, f: &mut dyn FnMut(&mut FunctionId)) {
        f(&mut self.div32);
        f(&mut self.div16);
        f(&mut self.div8);
        f(&mut self.rem32);
        f(&mut self.rem16);
        f(&mut self.rem8);
        f(&mut self.mul32);
        f(&mut self.mul16);
        f(&mut self.mul8);
    }
}

impl<'a> UpdateFunctionIds for Function<'a> {
    fn update_function_ids(&mut self, f: &mut dyn FnMut(&mut FunctionId)) {
        for block in self.blocks.iter_mut() {
            block.update_function_ids(f);
        }
    }
}

impl UpdateFunctionIds for Block {
    fn update_function_ids(&mut self, f: &mut dyn FnMut(&mut FunctionId)) {
        for step in self.steps.iter_mut() {
            step.update_function_ids(f);
        }
    }
}

impl UpdateFunctionIds for Step {
    fn update_function_ids(&mut self, f: &mut dyn FnMut(&mut FunctionId)) {
        self.action.update_function_ids(f);
    }
}

impl UpdateFunctionIds for Action {
    fn update_function_ids(&mut self, f: &mut dyn FnMut(&mut FunctionId)) {
        match self {
            Action::CallFunction(id, _) => f(id),
            _ => (),
        }
    }
}

pub trait UpdateTypeIds {
    fn update_type_ids(&mut self, f: &mut dyn FnMut(&mut TypeId));

    /// shift all step ids for which the condition `self >= after` holds
    fn shift_type_ids(&mut self, after: TypeId, by: isize) {
        self.update_type_ids(&mut |id| {
            if *id >= after {
                *id = ((id.as_index() as isize + by) as usize).into();
            }
        });
    }

    fn replace_type(&mut self, ty: TypeId, with: TypeId) {
        self.update_type_ids(&mut |id| {
            if *id == ty {
                *id = with;
            }
        })
    }
}

impl UpdateTypeIds for Step {
    fn update_type_ids(&mut self, f: &mut dyn FnMut(&mut TypeId)) {
        f(&mut self.ty);
        self.action.update_type_ids(f);
    }
}

impl UpdateTypeIds for Action {
    fn update_type_ids(&mut self, _f: &mut dyn FnMut(&mut TypeId)) {
        match self {
            Action::UnaryOperation(_, _)
            | Action::Binop(_, _, _)
            | Action::Extend(_)
            | Action::Reduce(_)
            | Action::StructFieldAccess(_, _)
            | Action::UnionFieldAccess(_)
            | Action::InitializeUnion(_)
            | Action::InitializeStruct(_)
            | Action::CallFunction(_, _)
            | Action::LoadConstant(_)
            | Action::LoadInput(_) => (),
        }
    }
}

impl UpdateTypeIds for Terminator {
    fn update_type_ids(&mut self, f: &mut dyn FnMut(&mut TypeId)) {
        match self {
            Terminator::Goto(_, _) => (),
            Terminator::Match(_, arms) => {
                for &mut (ref mut ty, _, _) in arms.iter_mut() {
                    f(ty)
                }
            }
        }
    }
}

impl UpdateTypeIds for Block {
    fn update_type_ids(&mut self, f: &mut dyn FnMut(&mut TypeId)) {
        for step in self.steps.iter_mut() {
            step.update_type_ids(f);
        }

        self.terminator.update_type_ids(f)
    }
}

impl<'a> UpdateTypeIds for Function<'a> {
    fn update_type_ids(&mut self, f: &mut dyn FnMut(&mut TypeId)) {
        for block in self.blocks.iter_mut() {
            block.update_type_ids(f);
        }

        f(&mut self.ret);
    }
}
