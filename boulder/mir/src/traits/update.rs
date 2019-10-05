use crate::{Action, Block, Step, StepId, Terminator};

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

impl UpdateStepIds for () {
    fn update_step_ids(&mut self, _: &mut dyn FnMut(&mut StepId)) {}
}

impl UpdateStepIds for Step {
    fn update_step_ids(&mut self, f: &mut dyn FnMut(&mut StepId)) {
        self.action.update_step_ids(f);
    }
}

impl UpdateStepIds for Action {
    fn update_step_ids(&mut self, f: &mut dyn FnMut(&mut StepId)) {
        match self {
            Action::UnaryOperation(_, id) | Action::Extend(id) | Action::FieldAccess(id, _) => {
                f(id)
            }
            Action::Binop(_kind, a, b) => {
                f(a);
                f(b);
            }
            Action::InitializeStruct(_, fields) => {
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
            Terminator::Return(id) => f(id),
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
