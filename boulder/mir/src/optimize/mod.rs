use std::cmp::Ordering;

use super::*;

impl Mir {
    /// remove all steps after a step with an `Uninhabited` type
    /// this is always expected to run as the first optimization,
    /// as all other optimizations expect only the last step
    /// of a block to be a branch
    pub fn kill_uninhabited(&mut self) {
        let types = &mut self.types;

        for func in self.functions.iter_mut() {
            for block in func.content.iter_mut() {
                if let Some((pos, _)) = block
                    .content
                    .iter()
                    .enumerate()
                    .find(|(_, step)| types[step.ty.0] == Type::Uninhabited)
                {
                    block.content.truncate(pos + 1);
                }
            }
        }
    }

    /// remove all `Action::Extend` which do not change the type
    pub fn remove_noop_extend(&mut self) {
        for func in self.functions.iter_mut() {
            for block in func.content.iter_mut() {
                let mut i = StepId(0);
                while i.0 < block.content.len() {
                    if let Action::Extend(e) = block[i].action {
                        if block[e].ty == block[i].ty {
                            block.replace_step(i, e);
                        } else {
                            i.0 += 1;
                        }
                    } else {
                        i.0 += 1;
                    }
                }
            }
        }
    }
}

impl Action {
    pub fn update_step_ids<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut StepId),
    {
        match self {
            Action::Extend(id) | Action::Return(id) | Action::FieldAccess(id, _) => f(id),
            Action::Add(a, b)
            | Action::Sub(a, b)
            | Action::Mul(a, b)
            | Action::Div(a, b)
            | Action::Lt(a, b)
            | Action::BitOr(a, b) => {
                f(a);
                f(b);
            }
            Action::CallFunction(_, args) | Action::Goto(_, args) => {
                for arg in args {
                    f(arg);
                }
            }
            Action::Match(id, arms) => {
                f(id);
                for arm in arms {
                    for arg in arm.2.iter_mut() {
                        f(arg);
                    }
                }
            }
            Action::LoadConstant(_) | Action::LoadInput(_) => (),
        }
    }

    /// shift all step ids for which the condition `self >= after` holds
    pub fn shift_step_ids(&mut self, after: StepId, by: isize) {
        self.update_step_ids(|id| {
            if *id >= after {
                *id = StepId((id.0 as isize + by) as usize);
            }
        });
    }
}

impl Block {
    /// Removes a step from this block, this leads to undefined behavior if the step is still referenced.
    ///
    /// Consider `replace_step` if the step is still needed in some action.
    pub fn remove_step(&mut self, id: StepId) {
        self.content.remove(id.0);
        for c in self.content[id.0..].iter_mut() {
            c.action.shift_step_ids(id, -1);
        }
    }

    /// Remove `previous` from this block, updating all reference to this step to `new`
    pub fn replace_step(&mut self, previous: StepId, new: StepId) {
        self.content.remove(previous.0);
        for c in self.content[previous.0..].iter_mut() {
            c.action.update_step_ids(|id| {
                *id = match (*id).cmp(&previous) {
                    Ordering::Less => *id,
                    Ordering::Equal => {
                        if new > previous {
                            StepId(new.0 - 1)
                        } else {
                            new
                        }
                    }
                    Ordering::Greater => StepId(id.0 - 1),
                }
            })
        }
    }
}
