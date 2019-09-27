use std::cmp::Ordering;

use super::*;

impl<M: MirState> Mir<M> {
    /// remove blocks where an input is unreachable
    pub fn kill_uninhabited(&mut self) {
        let types = &mut self.types;

        for func in self.functions.iter_mut() {
            let mut to_remove = Vec::new();
            for (i, block) in func.blocks.iter_mut().enumerate() {
                if block.input.iter().any(|&input| types[input] == Type::Uninhabited) {
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
            for block in func.blocks.iter_mut() {
                let mut i = StepId(0);
                while i.0 < block.steps.len() {
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
}

impl<M: MirState> Function<M> {
    pub fn remove_block(&mut self, id: BlockId) {
        self.blocks.remove(id);
        for block in self.blocks.iter_mut() {
            block.terminator.shift_block_ids(id, -1);
        }
    }
}

impl<M: MirState> Block<M> {
    /// Removes a step from this block, this leads to undefined behavior if the step is still referenced.
    ///
    /// Consider `replace_step` if the step is still needed in some action.
    pub fn remove_step(&mut self, id: StepId) {
        self.steps.remove(id);
        for c in self.steps[id..].iter_mut() {
            c.action.shift_step_ids(id, -1);
        }

        self.terminator.shift_step_ids(id, -1);
    }

    /// Remove `previous` from this block, updating all reference to this step to `new`
    pub fn replace_step(&mut self, previous: StepId, new: StepId) {
        let mut replacer = |id: &mut StepId| {
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

        self.steps.remove(previous);
        for c in self.steps[previous..].iter_mut() {
            c.action.update_step_ids(&mut replacer);
        }
        self.terminator.update_step_ids(&mut replacer);
    }
}
