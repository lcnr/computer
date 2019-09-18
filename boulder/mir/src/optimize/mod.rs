use std::cmp::Ordering;

use super::*;

impl Mir {
    /// remove blocks where an input is unreachable
    pub fn kill_uninhabited(&mut self) {
        let types = &mut self.types;

        for func in self.functions.iter_mut() {
            let mut to_remove = Vec::new();
            for (i, block) in func.content.iter_mut().enumerate() {
                if block.input.iter().any(|i| types[i.0] == Type::Uninhabited) {
                    to_remove.push(BlockId(i));
                }
            }
            for i in to_remove.into_iter().rev() {
                func.remove_block(i);
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

    /// unify blocks if they are only found in sequence
    /// this simplifies future optimizations
    pub fn unify_blocks(&mut self) {
        // TODO
    }
}

impl Terminator {
    pub fn update_block_ids<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut BlockId),
    {
        match self {
            Terminator::Return(_) => (),
            Terminator::Goto(id, _) => f(id),
            Terminator::Match(_, arms) => {
                for arm in arms.iter_mut() {
                    f(&mut arm.1);
                }
            }
        }
    }

    /// shift all block ids for which the condition `self >= after` holds
    pub fn shift_block_ids(&mut self, after: BlockId, by: isize) {
        self.update_block_ids(|id| {
            if *id >= after {
                id.0 = (id.0 as isize + by) as usize;
            }
        });
    }

    pub fn update_step_ids<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut StepId),
    {
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

    /// shift all step ids for which the condition `self >= after` holds
    pub fn shift_step_ids(&mut self, after: StepId, by: isize) {
        self.update_step_ids(|id| {
            if *id >= after {
                *id = StepId((id.0 as isize + by) as usize);
            }
        });
    }
}

impl Action {
    pub fn update_step_ids<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut StepId),
    {
        match self {
            Action::Extend(id) | Action::FieldAccess(id, _) => f(id),
            Action::Add(a, b)
            | Action::Sub(a, b)
            | Action::Mul(a, b)
            | Action::Div(a, b)
            | Action::Lt(a, b)
            | Action::BitOr(a, b) => {
                f(a);
                f(b);
            }
            Action::CallFunction(_, args) => {
                for arg in args {
                    f(arg);
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

impl Function {
    pub fn remove_block(&mut self, id: BlockId) {
        self.content.remove(id.0);
        for block in self.content.iter_mut() {
            block.terminator.shift_block_ids(id, -1);
        }
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

        self.terminator.shift_step_ids(id, -1);
    }

    /// Remove `previous` from this block, updating all reference to this step to `new`
    pub fn replace_step(&mut self, previous: StepId, new: StepId) {
        let replacer = |id: &mut StepId| {
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
        };

        self.content.remove(previous.0);
        for c in self.content[previous.0..].iter_mut() {
            c.action.update_step_ids(replacer);
        }
        eprintln!("{:?}", &self.terminator);
        self.terminator.update_step_ids(replacer);
        eprintln!("{:?}", &self.terminator);
    }
}
