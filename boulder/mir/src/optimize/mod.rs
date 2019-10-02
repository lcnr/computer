use std::{cmp::Ordering, iter, mem};

use tindex::bitset::TBitSet;

use shared_id::TypeId;

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
    /// this optimization is required for the `BoulderMirInterpreter` to work.
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

    pub fn remove_unused_blocks(&mut self) {
        for func in self.functions.iter_mut() {
            let mut used: TBitSet<_> = iter::once(BlockId::from(0)).collect();
            let mut new_used = TBitSet::new();
            while used != new_used {
                for block in used.iter() {
                    new_used.add(block);
                    func[block].terminator.used_blocks(&mut new_used);
                }
                mem::swap(&mut used, &mut new_used);
            }

            for i in (0..func.blocks.len()).map(BlockId::from).rev() {
                if !used.get(i) {
                    func.remove_block(i);
                }
            }
        }
    }

    /// removes all steps and inputs of blocks which are not needed in the `terminator`
    /// of said block, this is done repeatedly until no input changed after one round
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

    /// removes all blocks which only steps are `LoadInput`
    pub fn remove_redirects(&mut self) {
        fn match_reduce(
            func: &mut Function,
            arms: &mut Vec<(TypeId, BlockId, Vec<Option<StepId>>)>,
            redirects: &TBitSet<BlockId>,
        ) -> bool {
            let mut changed = false;
            for arm in 0..arms.len() {
                let target = arms[arm].1;
                if redirects.get(target) {
                    if let &Terminator::Goto(target, ref steps) = &func[target].terminator {
                        changed = true;
                        arms[arm].1 = target;
                        arms[arm].2 = steps
                            .iter()
                            .map(|&step| {
                                if let Action::LoadInput(v) = func[target][step].action {
                                    arms[arm].2[v]
                                } else {
                                    unreachable!("redirect with unexpected action");
                                }
                            })
                            .collect();
                    }
                }
            }
            changed
        }

        for func in self.functions.iter_mut() {
            let mut redirects = TBitSet::new();
            for i in (0..func.blocks.len()).map(BlockId::from) {
                if func[i].steps.iter().all(|s| {
                    mem::discriminant(&s.action) == mem::discriminant(&Action::LoadInput(0))
                }) {
                    redirects.add(i);
                }
            }

            let mut changed = true;
            while changed {
                changed = false;
                for i in (1..func.blocks.len()).map(BlockId::from) {
                    match &mut func[i].terminator {
                        &mut Terminator::Goto(target, ref mut steps) => {
                            if redirects.get(target) {
                                changed = true;
                                let removed = mem::replace(steps, Vec::new());
                                let mut terminator = func[target].terminator.clone();
                                terminator.update_step_ids(&mut |id| {
                                    if let Action::LoadInput(v) = func[target][*id].action {
                                        *id = removed[v];
                                    } else {
                                        unreachable!("redirect with unexpected action");
                                    }
                                });
                                func[i].terminator = terminator;
                            }
                        }
                        &mut Terminator::Match(step, ref mut arms) => {
                            let mut arms = mem::replace(arms, Vec::new());
                            changed |= match_reduce(func, &mut arms, &redirects);
                            func[i].terminator = Terminator::Match(step, arms);
                        }
                        &mut Terminator::Return(_) => (),
                    }
                }
            }
        }
        self.remove_unused_blocks();
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

        self[block].terminator = Terminator::Goto(new, used);

        new
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
