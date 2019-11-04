use std::{
    cmp::Ordering,
    mem,
    ops::{Bound, RangeBounds},
};

use tindex::{bitset::TBitSet, TVec};

use shared_id::{FunctionId, TypeId};

use crate::{
    traits::{UpdateFunctionIds, UpdateStepIds},
    Action, Block, BlockId, Function, Mir, Step, StepId, Terminator, Type,
};

mod optimize;
mod sum_types;
mod to_bytes;

fn get_or_insert_union(types: &mut TVec<TypeId, Type>, un: impl Iterator<Item = TypeId>) -> TypeId {
    let bitset = un.collect();
    if let Some(ty) = types
        .iter()
        .position(|ty| ty.is_union() && ty.expect_union() == &bitset)
    {
        TypeId::from(ty)
    } else {
        types.push(Type::Union(bitset))
    }
}

impl<'a> Mir<'a> {
    pub fn remove_function(&mut self, id: FunctionId) {
        self.functions.remove(id);
        self.ctx.shift_function_ids(id, -1);
        for func in self.functions.iter_mut() {
            func.shift_function_ids(id, -1);
        }
    }
}

impl<'a> Function<'a> {
    pub fn remove_block(&mut self, id: BlockId) {
        self.blocks.remove(id);
        for block in self.blocks.iter_mut() {
            block.terminator.shift_block_ids(id, -1);
        }
    }

    pub fn swap_blocks(&mut self, a: BlockId, b: BlockId) {
        for block in self.blocks.iter_mut() {
            block.terminator.update_block_ids(|id| {
                if *id == a {
                    *id = b;
                } else if *id == b {
                    *id = a;
                }
            })
        }
        self.blocks.swap(a, b);
    }

    pub fn split_block(&mut self, block: BlockId, after: StepId) -> BlockId {
        #[cfg(feature = "profiler")]
        profile_scope!("Function::split_block");
        let mut used = TBitSet::new();
        for step in self[block].steps[after..].iter().skip(1) {
            step.used_steps(&mut used);
        }
        self[block].terminator.used_steps(&mut used);
        let used = used.iter().filter(|&t| t <= after).collect::<Vec<_>>();

        let new = self.add_block();
        for &step in used.iter() {
            let ty = self[block][step].ty;
            self[new].add_input(ty);
        }

        let mut content = self[block].steps.split_off(StepId(after.0 + 1));
        self[new].steps.append(&mut content);
        let terminator = mem::replace(&mut self[block].terminator, Terminator::invalid());
        self.blocks[new].add_terminator(terminator);

        self[new].update_step_ids(&mut |id| {
            if *id > after {
                id.0 = id.0 + used.len() - (after.0 + 1);
            } else {
                id.0 = used.iter().position(|i| id == i).unwrap();
            }
        });

        self[block].terminator = Terminator::Goto(Some(new), used);

        new
    }

    pub fn remove_block_input(&mut self, id: BlockId, input: usize) {
        #[cfg(feature = "profiler")]
        profile_scope!("Function::remove_block_input");
        self[id].input.remove(input);
        for step in self[id].steps.index_iter().rev() {
            if let &mut Action::LoadInput(ref mut i) = &mut self[id][step].action {
                if *i == input {
                    self[id].remove_step(step);
                } else if *i > input {
                    *i -= 1;
                }
            }
        }
        for block in self.blocks.iter_mut() {
            match &mut block.terminator {
                &mut Terminator::Goto(target, ref mut steps) => {
                    if let Some(target) = target {
                        if target == id {
                            steps.remove(input);
                        }
                    }
                }
                &mut Terminator::Match(_, ref mut arms) => {
                    for &mut (_, target, ref mut steps) in arms.iter_mut() {
                        if let Some(target) = target {
                            if target == id {
                                steps.remove(input);
                            }
                        }
                    }
                }
            }
        }
    }
}

impl Block {
    /// Removes a step from this block, this leads to unspecified behavior if the step is still referenced.
    ///
    /// Consider `replace_step` if the step is still needed in some action.
    pub fn remove_step(&mut self, id: StepId) {
        self.steps.remove(id);
        for c in self.steps[id..].iter_mut() {
            c.action.shift_step_ids(id, -1);
        }

        self.terminator.shift_step_ids(id, -1);
    }

    /// replaces the step `old` with `steps`
    pub fn replace_step<I, P>(&mut self, old: StepId, steps: I, replacements: P) -> StepId
    where
        I: IntoIterator<Item = Step>,
        P: IntoIterator<Item = StepId>,
    {
        self.insert_steps(old..=old, steps, replacements)
    }

    /// Replaces all steps in `at` with the steps of `I`, returning the id of the next step.
    /// The steps in `steps` should start at `StepId(0)`.
    ///
    /// Steps which have the last element of `at` as a target now target the last element of `steps`.
    pub fn insert_steps<R, I, P>(&mut self, at: R, steps: I, replacements: P) -> StepId
    where
        R: RangeBounds<StepId>,
        I: IntoIterator<Item = Step>,
        P: IntoIterator<Item = StepId>,
    {
        let start = match at.start_bound() {
            Bound::Unbounded => 0,
            Bound::Excluded(i) => i.0 + 1,
            Bound::Included(i) => i.0,
        };

        let end_len = match at.end_bound() {
            Bound::Unbounded => 0,
            Bound::Excluded(i) => self.steps.len() - i.0,
            Bound::Included(i) => self.steps.len() - i.0 - 1,
        };

        let removed_len = self.steps.len() - end_len - start;

        self.steps.splice(at, steps);
        let inserted_end = self.steps.len() - end_len;
        let inserted_steps = &mut self.steps[StepId(start)..StepId(inserted_end)];
        let inserted_len = inserted_steps.len();
        for (new, old) in replacements.into_iter().zip(0..) {
            inserted_steps.iter_mut().for_each(|s| {
                s.replace_step(
                    StepId::replacement(old),
                    StepId(new.0 + std::usize::MAX / 4),
                )
            });
        }

        inserted_steps
            .iter_mut()
            .for_each(|s| s.shift_step_ids(StepId::from(0), start as isize));
        inserted_steps.iter_mut().for_each(|s| {
            s.shift_step_ids(
                StepId::from(std::usize::MAX / 4),
                -((std::usize::MAX / 4 + start) as isize),
            )
        });
        self.steps[StepId(inserted_end)..].iter_mut().for_each(|s| {
            s.shift_step_ids(
                StepId::from(start),
                inserted_len as isize - removed_len as isize,
            )
        });
        self.terminator.shift_step_ids(
            StepId::from(start),
            inserted_len as isize - removed_len as isize,
        );
        StepId(inserted_end)
    }

    /// Remove `previous` from this block, updating all reference to this step to `new`
    pub fn replace_step_with_existing(&mut self, previous: StepId, new: StepId) {
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
