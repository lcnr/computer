use std::{cmp::Ordering, mem};

use tindex::bitset::TBitSet;

use crate::{
    traits::UpdateStepIds, Action, Block, BlockId, Function, Mir, StepId, Terminator, Type,
};

impl Mir {
    /// remove blocks where an input is unreachable
    pub fn kill_uninhabited(&mut self) {
        let types = &mut self.types;

        for func in self.functions.iter_mut() {
            let mut to_remove = Vec::new();
            for (i, block) in func.blocks.iter_mut().enumerate() {
                if block
                    .input
                    .iter()
                    .any(|&input| types[input] == Type::Uninhabited)
                {
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
        for func in self.functions.iter_mut() {
            let mut used = TBitSet::new();
            used.add(BlockId::from(0));
            let mut allowed = TBitSet::new();
            for block in func.blocks.iter() {
                match &block.terminator {
                    &Terminator::Return(_) => (),
                    &Terminator::Goto(block, _) => {
                        allowed.set(block, !used.get(block));
                        used.add(block);
                    }
                    &Terminator::Match(_, ref arms) => {
                        for arm in arms.iter() {
                            allowed.set(arm.1, !used.get(arm.1));
                            used.add(arm.1);
                        }
                    }
                }
            }

            let mut to_remove = TBitSet::new();
            for i in 0..func.blocks.len() {
                let id = BlockId::from(i);
                if let &mut Terminator::Goto(block, ref mut goto_steps) = &mut func[id].terminator {
                    if allowed.get(block) {
                        let glue = mem::replace(goto_steps, Vec::new());
                        let mut removed = mem::replace(&mut func[block], Block::new());
                        removed.update_step_ids(&mut |step_id| {
                            if let Some(&target_id) = glue.get(step_id.0) {
                                *step_id = target_id;
                            } else {
                                step_id.0 = step_id.0 - glue.len() + func[id].steps.len();
                            }
                        });
                        func[id].terminator = removed.terminator;
                        func[id]
                            .steps
                            .extend_from_slice(&removed.steps[StepId::from(glue.len())..]);
                        to_remove.add(block);
                    }
                }
            }

            for i in to_remove.iter().rev() {
                func.remove_block(i);
            }
        }
    }

    pub fn remove_unused_steps(&mut self) {
        for func in self.functions.iter_mut() {
            let mut changing = true;
            while changing {
                changing = false;
                for block in (0..func.blocks.len()).map(BlockId::from) {
                    let mut used = TBitSet::new();
                    func[block].terminator.used_steps(&mut used);
                    let mut new_used = TBitSet::new();
                    while new_used != used {
                        for step in used.iter().rev() {
                            new_used.add(step);
                            func[block].steps[step].used_steps(&mut new_used);
                        }
                        mem::swap(&mut used, &mut new_used);
                    }

                    for step in (func[block].input.len()..func[block].steps.len())
                        .rev()
                        .map(StepId::from)
                    {
                        if !used.get(step) {
                            func[block].remove_step(step);
                        }
                    }

                    if block.0 != 0 {
                        for i in (0..func[block].input.len()).rev() {
                            if !used.get(StepId::from(i)) {
                                changing = true;
                                func.remove_block_input(block, i);
                            }
                        }
                    }
                }
            }
        }
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

impl Function {
    pub fn remove_block(&mut self, id: BlockId) {
        self.blocks.remove(id);
        for block in self.blocks.iter_mut() {
            block.terminator.shift_block_ids(id, -1);
        }
    }

    pub fn remove_block_input(&mut self, id: BlockId, input: usize) {
        self[id].input.remove(input);
        for step in (0..self[id].steps.len()).map(StepId::from).rev() {
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
                &mut Terminator::Return(_) => (),
                &mut Terminator::Goto(target, ref mut steps) => {
                    if target == id {
                        steps.remove(input);
                    }
                }
                &mut Terminator::Match(_, ref mut arms) => {
                    for &mut (_, block, ref mut steps) in arms.iter_mut() {
                        if block == id {
                            steps.remove(input);
                        }
                    }
                }
            }
        }
    }
}

impl Block {
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
