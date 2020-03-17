use std::{convert::identity, iter, mem};

use tindex::{tvec, TBitSet, TSlice, TVec};

use shared_id::{BlockId, FunctionId, InputId, LocationId, StepId};

use crate::{
    traits::{Reads, Update, Writes},
    Action, Arg, Block, Function, Lir, Terminator,
};

mod coloring;
mod const_prop;
mod inline;

impl<'a> Lir<'a> {
    /// Remove all moves.
    pub fn remove_moves(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("remove_moves");

        for function in self.functions.iter_mut() {
            for block in function.blocks.iter_mut() {
                block.uniquify();

                let mut replacements: TVec<LocationId, LocationId> =
                    (0..block.memory_len).map(LocationId).collect();

                block.steps = mem::take(&mut block.steps)
                    .into_iter()
                    .filter_map(|mut step| {
                        if let Action::Move(i, o) = step {
                            replacements[o] = replacements[i];
                            None
                        } else {
                            step.update(|s| replacements[s]);
                            Some(step)
                        }
                    })
                    .collect();
                
                block.terminator.update(|s: LocationId| replacements[s]);
            }
        }
    }

    /// Convert tail recursion into loops.
    pub fn loopify_tail_recursion(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("loopify_tail_recursion");

        for f in self.functions.index_iter() {
            for b in l!(self, f).blocks.index_iter() {
                if let Terminator::Goto(None, ref ret_args) = l!(self, f, b).terminator {
                    let last_step = l!(self, f, b).steps.last();
                    if let Some(Action::FunctionCall { id, args, ret }) = last_step {
                        let cond = *id == f && ret.iter().zip(ret_args.iter()).all(|(l, r)| {
                            matches!((l, r), (Some(l), Some(Arg::Location(r))) if l == r)
                        });

                        if cond {
                            l!(self, f, b).terminator =
                                Terminator::Goto(Some(BlockId(0)), args.clone());

                            l!(self, f, b).steps.pop();
                        }
                    }
                }
            }
        }
    }

    /// Removes `w_1` in the sequence `..., w_1, seq , w_2, ...`
    /// where `seq` does not read the given location.
    pub fn remove_dead_writes(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("remove_dead_writes");

        for func_id in self.functions.index_iter() {
            for block_id in self.functions[func_id].blocks.index_iter() {
                for input in self.functions[func_id].blocks[block_id]
                    .remove_dead_writes()
                    .iter()
                    .rev()
                {
                    self.functions[func_id].remove_input(block_id, input);
                    if block_id == BlockId(0) {
                        self.remove_function_input(func_id, input);
                    }
                }
            }
        }
    }

    /// Removes function which are unused and not exported
    pub fn remove_unused_functions(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("remove_unused_functions");

        let mut used = TBitSet::new();
        for (f, func) in self.functions.index_iter().zip(self.functions.iter()) {
            if func.ctx.test || func.ctx.export {
                let mut visited = TBitSet::new();
                visited.add(f);
                func.requires(&self, &mut visited, FunctionId::invalid());
                used.extend(visited)
            }
        }

        for i in self.functions.index_iter().rev() {
            if !used.get(i) {
                self.functions.remove(i);
                for func in self.functions.iter_mut() {
                    func.update(|f: FunctionId| if f > i { f - 1 } else { f });
                }
            }
        }
    }

    /// Removes blocks which can not be reached
    pub fn remove_unused_blocks(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("remove_unused_blocks");
        for func in self.functions.iter_mut() {
            let mut used: TBitSet<_> = iter::once(BlockId::from(0)).collect();
            let mut new_used = TBitSet::new();
            while used != new_used {
                for b in used.iter() {
                    new_used.add(b);
                    match func.blocks[b].terminator {
                        Terminator::Goto(target, _) => {
                            if let Some(target) = target {
                                new_used.add(target)
                            }
                        }
                        Terminator::Match(_, ref arms) => {
                            new_used.extend(arms.iter().filter_map(|arm| arm.target))
                        }
                    }
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

    pub fn merge_simple_blocks(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("merge_simple_blocks");

        fn merge(
            args: &TSlice<InputId, Option<Arg>>,
            inputs: &TSlice<InputId, LocationId>,
            mut term: Terminator,
        ) -> Terminator {
            if let Terminator::Match(ref mut expr, ref mut arms) = term {
                let pos = inputs.iter().position(|&l| l == *expr).unwrap();
                match args[InputId(pos)] {
                    None => panic!("undefined behavior"),
                    Some(Arg::Byte(v)) => {
                        let arm = arms.iter().find(|arm| arm.pat == v).expect("match undef");
                        term = Terminator::Goto(arm.target, arm.args.clone());
                    }
                    Some(Arg::Location(id)) => *expr = id,
                }
            }

            term.update(|v: Option<Arg>| {
                if let Some(Arg::Location(id)) = v {
                    let iter = args.iter().zip(inputs.iter());
                    for (&arg, &input) in iter {
                        if id == input {
                            return arg;
                        }
                    }

                    unreachable!("undefined arg");
                } else {
                    v
                }
            });

            term
        }

        for func in self.functions.iter_mut() {
            let mut to_merge = TBitSet::new();
            for (b, block) in func.blocks.index_iter().zip(func.blocks.iter()) {
                if block.steps.is_empty() {
                    to_merge.add(b);
                }
            }

            for b in func.blocks.index_iter() {
                match func.blocks[b].terminator {
                    Terminator::Goto(Some(target), ref args) => {
                        if to_merge.get(target) {
                            let target_block = &func.blocks[target];
                            let term = target_block.terminator.clone();
                            func.blocks[b].terminator = merge(args, &target_block.inputs, term);
                        }
                    }
                    Terminator::Goto(_, _) => {}
                    Terminator::Match(expr, ref arms) => {
                        let mut clone = arms.clone();
                        for arm in clone.iter_mut() {
                            if let Some(target) = arm.target {
                                if to_merge.get(target) {
                                    let target_block = &func.blocks[target];
                                    let term = target_block.terminator.clone();

                                    if let Terminator::Goto(to, args) =
                                        merge(&arm.args, &target_block.inputs, term)
                                    {
                                        arm.target = to;
                                        arm.args = args;
                                    }
                                }
                            }
                        }

                        func.blocks[b].terminator = Terminator::Match(expr, clone)
                    }
                }
            }
        }
    }

    /// Removes the given input from each function call, does not update the actual function
    fn remove_function_input(&mut self, f: FunctionId, input: InputId) {
        for func in self.functions.iter_mut() {
            for block in func.blocks.iter_mut() {
                for step in block.steps.iter_mut() {
                    if let Action::FunctionCall {
                        id, ref mut args, ..
                    } = *step
                    {
                        if f == id {
                            args.remove(input);
                        }
                    }
                }
            }
        }
    }
}

impl<'a> Function<'a> {
    /// Requires the given input to already be unused.
    pub fn remove_input(&mut self, b: BlockId, input: InputId) {
        self.blocks[b].inputs.remove(input);
        for block in self.blocks.iter_mut() {
            match block.terminator {
                Terminator::Goto(Some(target), ref mut args) if target == b => {
                    args.remove(input);
                }
                Terminator::Goto(_, _) => {}
                Terminator::Match(_, ref mut arms) => {
                    for arm in arms.iter_mut() {
                        if arm.target == Some(b) {
                            arm.args.remove(input);
                        }
                    }
                }
            }
        }
    }

    pub fn remove_block(&mut self, b: BlockId) {
        for block in self.blocks.iter_mut() {
            block
                .terminator
                .update(|block: BlockId| if block > b { block - 1 } else { block });
        }

        self.blocks.remove(b);
    }
}

impl Block {
    /// Give a unique location to each write.
    pub fn uniquify(&mut self) {
        let mut replacements: TVec<LocationId, LocationId> =
            (0..).map(LocationId).take(self.memory_len).collect();

        let mut repl = (self.memory_len..).map(LocationId);

        for step in self.steps.iter_mut().rev() {
            step.writes(|s| {
                let updated = replacements[s];
                replacements[s] = repl.next().unwrap();
                updated
            });
            step.reads(|s| replacements[s]);
        }

        for input in self.inputs.iter_mut().rev() {
            let updated = replacements[*input];
            replacements[*input] = repl.next().unwrap();
            *input = updated;
        }

        self.memory_len = repl.next().unwrap().0;
    }

    /// Removes all dead writes from self,
    /// and returns a bitset of unused inputs.
    pub fn remove_dead_writes(&mut self) -> TBitSet<InputId> {
        #[derive(Debug, Clone, Copy)]
        enum W {
            Input(InputId),
            Step(StepId),
            Return(StepId, usize),
        }

        // TODO: this can be used to remove inputs
        let mut last_writes = tvec![None; self.memory_len];
        let mut return_values = tvec![TBitSet::new(); self.steps.len()];
        let mut inputs = TBitSet::new();
        let mut steps = TBitSet::new();

        let mut add_to_remove = |elem| match elem {
            W::Input(v) => inputs.add(v),
            W::Step(id) => steps.add(id),
            W::Return(id, v) => return_values[id].add(v),
        };

        let arg_update = |last_writes: &mut TSlice<_, _>, args| {
            for &arg in args {
                if let Some(Arg::Location(id)) = arg {
                    last_writes[id] = None;
                }
            }
        };

        for id in self.inputs.index_iter() {
            if let Some(last) = last_writes[self.inputs[id]].replace(W::Input(id)) {
                add_to_remove(last);
            }
        }

        for step_id in self.steps.index_iter() {
            match self.steps[step_id] {
                Action::Invert(i, o) | Action::Move(i, o) | Action::BlackBox(i, o) => {
                    last_writes[i] = None;
                    if let Some(last) = last_writes[o].replace(W::Step(step_id)) {
                        add_to_remove(last);
                    }
                }
                Action::Debug(i) => last_writes[i] = None,
                Action::LoadConstant(_, o) => {
                    if let Some(last) = last_writes[o].replace(W::Step(step_id)) {
                        add_to_remove(last);
                    }
                }
                Action::Binop { l, r, out, .. } => {
                    if let Arg::Location(id) = l {
                        last_writes[id] = None;
                    }
                    if let Arg::Location(id) = r {
                        last_writes[id] = None;
                    }
                    if let Some(last) = last_writes[out].replace(W::Step(step_id)) {
                        add_to_remove(last);
                    }
                }
                Action::FunctionCall {
                    ref args, ref ret, ..
                } => {
                    arg_update(&mut last_writes, args);

                    for (i, v) in ret
                        .iter()
                        .enumerate()
                        .filter_map(|(i, r)| r.map(|r| (i, r)))
                    {
                        // TODO: FunctionCall should ret should be an Option
                        if let Some(last) = last_writes[v].replace(W::Return(step_id, i)) {
                            add_to_remove(last);
                        }
                    }
                }
            }
        }

        match self.terminator {
            Terminator::Goto(_, ref args) => arg_update(&mut last_writes, args),
            Terminator::Match(expr, ref arms) => {
                for arm in arms {
                    arg_update(&mut last_writes, &arm.args)
                }

                last_writes[expr] = None;
            }
        }

        for step in last_writes.iter().copied().filter_map(identity) {
            add_to_remove(step)
        }

        for step_id in return_values.index_iter() {
            for v in return_values[step_id].iter() {
                if let Action::FunctionCall { ref mut ret, .. } = self.steps[step_id] {
                    ret[v] = None;
                } else {
                    unreachable!()
                }
            }
        }

        for step in steps.iter().rev() {
            self.steps.remove(step);
        }

        inputs
    }
}
