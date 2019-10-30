use std::{iter, mem};

use tindex::bitset::TBitSet;

use shared_id::{FunctionId, TypeId};

use crate::{
    traits::UpdateStepIds, Action, Block, BlockId, Function, Mir, StepId, Terminator, Type,
};

impl<'a> Mir<'a> {
    /// remove blocks where an input is unreachable
    pub fn kill_uninhabited(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("Mir::kill_uninhabited");
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

    pub fn remove_unused_functions(&mut self, resolved_lang_items: bool) {
        let mut used = TBitSet::new();
        for (idx, _) in self
            .functions
            .iter()
            .enumerate()
            .filter(|(_, f)| f.ctx.export || f.ctx.is_test)
        {
            used.add(FunctionId::from(idx));
        }

        if !resolved_lang_items {
            used.add(self.ctx.mul32);
            used.add(self.ctx.mul16);
            used.add(self.ctx.mul8);
            used.add(self.ctx.div32);
            used.add(self.ctx.div16);
            used.add(self.ctx.div8);
            used.add(self.ctx.rem32);
            used.add(self.ctx.rem16);
            used.add(self.ctx.rem8);
        }

        let mut new_used = TBitSet::new();
        while used != new_used {
            for func in used.iter() {
                new_used.add(func);
                self.functions[func].used_functions(&mut new_used);
            }
            mem::swap(&mut used, &mut new_used);
        }

        for i in (0..self.functions.len()).map(FunctionId::from).rev() {
            if !used.get(i) {
                self.remove_function(i);
            }
        }
    }

    /// remove all `Action::Extend` which do not change the type
    /// this optimization is required for the `BoulderMirInterpreter` to work.
    pub fn remove_noop_extend(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("Mir::remove_noop_extend");
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
        #[cfg(feature = "profiler")]
        profile_scope!("Mir::unify_blocks");
        for func in self.functions.iter_mut() {
            let mut used = TBitSet::new();
            used.add(BlockId::from(0));
            let mut allowed = TBitSet::new();
            for block in func.blocks.iter() {
                match &block.terminator {
                    &Terminator::Goto(None, _) => (),
                    &Terminator::Goto(Some(block), _) => {
                        allowed.set(block, !used.get(block));
                        used.add(block);
                    }
                    &Terminator::Match(_, ref arms) => {
                        for arm in arms.iter() {
                            if let Some(target) = arm.1 {
                                allowed.set(target, !used.get(target));
                                used.add(target);
                            }
                        }
                    }
                }
            }

            let mut to_remove = TBitSet::new();
            for i in 0..func.blocks.len() {
                let id = BlockId::from(i);
                if let &mut Terminator::Goto(Some(block), ref mut goto_steps) =
                    &mut func[id].terminator
                {
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
        #[cfg(feature = "profiler")]
        profile_scope!("Mir::remove_unused_blocks");
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

            for i in func.blocks.index_iter().rev() {
                if !used.get(i) {
                    func.remove_block(i);
                }
            }
        }
    }

    /// removes all steps and inputs of blocks which are not needed in the `terminator`
    /// of said block, this is done repeatedly until no input changed after one round
    pub fn remove_unused_steps(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("Mir::remove_unused_steps");
        for func in self.functions.iter_mut() {
            let mut changing = true;
            while changing {
                changing = false;
                for block in func.blocks.index_iter() {
                    let mut used = TBitSet::new();
                    func[block].terminator.used_steps(&mut used);
                    let mut new_used = TBitSet::new();
                    while new_used != used {
                        for step in used.iter().rev() {
                            new_used.add(step);
                            let block = &mut func[block];
                            block.steps[step].used_steps(&mut new_used);
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
        #[cfg(feature = "profiler")]
        profile_scope!("Mir::remove_redirects");
        fn match_reduce(
            func: &mut Function,
            arms: &mut Vec<(TypeId, Option<BlockId>, Vec<Option<StepId>>)>,
            redirects: &TBitSet<BlockId>,
        ) -> bool {
            let mut changed = false;
            for arm in 0..arms.len() {
                if let Some(target) = arms[arm].1 {
                    if redirects.get(target) {
                        match &func[target].terminator {
                            &Terminator::Goto(next_block, ref steps) => {
                                changed = true;
                                arms[arm].1 = next_block;
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
                            &Terminator::Match(_, _) => (),
                        }
                    }
                }
            }
            changed
        }

        for func in self.functions.iter_mut() {
            let mut redirects = TBitSet::new();
            for i in (1..func.blocks.len()).map(BlockId::from) {
                if func[i].steps.iter().all(|s| {
                    mem::discriminant(&s.action) == mem::discriminant(&Action::LoadInput(0))
                }) {
                    redirects.add(i);
                }
            }

            let mut changed = true;
            while changed {
                changed = false;
                for i in func.blocks.index_iter() {
                    match &mut func[i].terminator {
                        &mut Terminator::Goto(Some(target), ref mut steps) => {
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
                        &mut Terminator::Goto(None, _) => (),
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
            Terminator::Goto(None, _) => (),
            Terminator::Goto(Some(id), _) => f(id),
            Terminator::Match(_, arms) => {
                arms.iter_mut()
                    .filter_map(|&mut (_, ref mut t, _)| t.as_mut())
                    .for_each(f);
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
