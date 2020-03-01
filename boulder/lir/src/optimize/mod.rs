use std::convert::identity;

use tindex::{bitset::TBitSet, tvec, TSlice};

use graphc::{Coloring, Graph, NodeId};

use shared_id::{BlockId, FunctionId, InputId, LocationId, StepId};

use crate::{traits::UpdateLocation, Action, Arg, Block, Function, Lir, Terminator};

mod const_prop;

impl<'a> Lir<'a> {
    /// Minimizes the needed memory of each block without
    /// modifying step order or visible behavior.
    pub fn minimize_memory_usage(&mut self) {
        for function in self.functions.iter_mut() {
            for block in function.blocks.iter_mut() {
                let coloring = block.calculate_coloring();

                block.update_locations(|location| {
                    LocationId(coloring.nodes[NodeId::from(location.0)])
                });

                block.memory_len = coloring.k;
            }
        }
    }

    /// Remove all moves where `o == i`.
    pub fn remove_noop_moves(&mut self) {
        for function in self.functions.iter_mut() {
            for block in function.blocks.iter_mut() {
                block.steps.retain(|s| {
                    if let Action::Move(o, i) = s {
                        o != i
                    } else {
                        true
                    }
                });
            }
        }
    }

    /// Removes `w_1` in the sequence `..., w_1, seq , w_2, ...`
    /// where `seq` does not read the given location.
    pub fn remove_dead_writes(&mut self) {
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
    pub fn remove_input(&mut self, b: BlockId, input: InputId) {
        self.blocks[b].inputs.remove(input);
        for block in self.blocks.iter_mut() {
            match block.terminator {
                Terminator::Goto(Some(target), ref mut args) if target == b => {
                    args.remove(input);
                }
                Terminator::Goto(_, _) => (),
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
}

impl Block {
    /// removes all dead writes from self,
    /// Returns a bitset of unused inputs.
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
                Action::Invert(i, o) | Action::Move(i, o) => {
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

                    for i in 0..ret.len() {
                        if let Some(v) = ret[i] {
                            // TODO: FunctionCall should ret should be an Option
                            if let Some(last) = last_writes[v].replace(W::Return(step_id, i)) {
                                add_to_remove(last);
                            }
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

    pub fn calculate_coloring(&self) -> Coloring {
        let mut g = Graph::new();
        let mut alive = TBitSet::new();

        for _ in 0..self.memory_len {
            g.add_node();
        }

        let mut add_alive = |alive: &mut TBitSet<LocationId>, n: LocationId| {
            for other in alive.iter() {
                g.add_edge(NodeId::from(n.0), NodeId::from(other.0));
            }
            alive.add(n);
        };

        fn arg_alive<F>(
            mut f: F,
            alive: &mut TBitSet<LocationId>,
            args: &TSlice<InputId, Option<Arg>>,
        ) where
            F: FnMut(&mut TBitSet<LocationId>, LocationId),
        {
            for &arg in args.iter() {
                if let Some(Arg::Location(id)) = arg {
                    f(alive, id);
                }
            }
        }

        match self.terminator {
            Terminator::Goto(_, ref args) => {
                arg_alive(&mut add_alive, &mut alive, args);
            }
            Terminator::Match(expr, ref arms) => {
                for arm in arms {
                    arg_alive(&mut add_alive, &mut alive, &arm.args);
                }

                add_alive(&mut alive, expr);
            }
        }

        for step in self.steps.iter().rev() {
            match *step {
                Action::Invert(i, o) | Action::Move(i, o) => {
                    add_alive(&mut alive, o);
                    alive.remove(o);
                    add_alive(&mut alive, i);
                }
                Action::Debug(i) => add_alive(&mut alive, i),
                Action::LoadConstant(_, o) => {
                    add_alive(&mut alive, o);
                    alive.remove(o)
                }
                Action::Binop { l, r, out, .. } => {
                    add_alive(&mut alive, out);
                    alive.remove(out);
                    if let Arg::Location(id) = l {
                        add_alive(&mut alive, id);
                    }
                    if let Arg::Location(id) = r {
                        add_alive(&mut alive, id);
                    }
                }
                Action::FunctionCall {
                    ref args, ref ret, ..
                } => {
                    for &v in ret.iter().filter_map(Option::as_ref).rev() {
                        add_alive(&mut alive, v);
                        alive.remove(v);
                    }

                    arg_alive(&mut add_alive, &mut alive, args);
                }
            }
        }

        for &input in self.inputs.iter().rev() {
            add_alive(&mut alive, input);
            alive.remove(input)
        }

        g.minimal_coloring()
    }
}
