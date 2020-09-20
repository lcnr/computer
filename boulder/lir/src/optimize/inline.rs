use std::convert::identity;

use tindex::TBitSet;

use shared_id::{BlockId, FunctionId, LocationId, StepId};

use crate::{Action, Arg, Block, Lir, Terminator};

const MAX_INLINE_COST: usize = 500;

impl<'a> Lir<'a> {
    /// Testing stuff
    pub fn inline_all(&mut self) {
        for f in self.functions.index_iter() {
            for b in l!(self, f).blocks.index_iter().rev() {
                for s in l!(self, f, b).steps.index_iter().rev() {
                    // only inline functions if there are no temporaries
                    if matches!(l!(self, f, b, s), Action::FunctionCall { .. })
                        && l!(self, f, b).used_locations(s).element_count() == 0
                    {
                        self.inline_location(f, b, s);
                    }
                }
            }
        }
    }

    /// inlines single block functions
    pub fn inline_functions(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("inline_functions");

        let mut to_inline = TBitSet::new();
        for (f, function) in self.functions.index_iter().zip(self.functions.iter()) {
            if let Terminator::Goto(None, _) = function.blocks[BlockId(0)].terminator {
                to_inline.add(f);
            }
        }

        for f in self.functions.index_iter() {
            let mut b = BlockId(0);
            while b < l!(self, f).blocks.range_end() {
                let mut s = StepId(0);
                while s < l!(self, f, b).steps.range_end() {
                    if let Action::FunctionCall { id, .. } = l!(self, f, b, s) {
                        if self.should_inline(id, MAX_INLINE_COST).is_some() {
                            self.inline_location(f, b, s);
                        }
                    }

                    s = s + 1;
                }

                b = b + 1;
            }
        }

        self.remove_moves();
    }

    /// Inlines a given function call.
    ///
    /// # Examples
    ///
    /// ```plain
    /// A:
    ///     a;
    ///     f();
    ///     b;
    ///     goto B;
    /// B:
    ///     ...
    /// ```
    ///
    /// gets converted into
    ///
    /// ```plain
    /// A:
    ///     a;
    ///     goto D;
    /// B:
    ///     ...
    /// C:  
    ///     b;
    ///     goto B;
    /// D:  
    ///     content of f()
    /// ```
    pub fn inline_location(&mut self, f: FunctionId, b: BlockId, s: StepId) {
        let mut block = l!(self, f, b).clone();
        let needed_locations = block.used_locations(s);
        let rest = block.steps.split_off(s + 1);
        if let Action::FunctionCall { id, mut args, ret } = block.steps.pop().unwrap() {
            let block_iter = l!(self, id).blocks.index_iter();

            let continue_id = {
                let inputs = ret
                    .iter()
                    .copied()
                    .filter_map(identity)
                    .chain(needed_locations.iter())
                    .collect();
                let memory_len = l!(self, f, b).memory_len;
                let terminator = l!(self, f, b).terminator.clone();
                let steps = rest;

                l!(self, f).blocks.push(Block {
                    inputs,
                    memory_len,
                    steps,
                    terminator,
                })
            };

            let function_start = l!(self, f).blocks.range_end();
            args.extend(needed_locations.iter().map(Arg::Location).map(Some));
            block.terminator = Terminator::Goto(Some(function_start), args);

            for other_b in block_iter {
                let mut block = l!(self, id, other_b).clone();
                let block_start = block.memory_len;
                for i in (0..needed_locations.element_count()).map(LocationId) {
                    block.inputs.push(i + block_start);
                    block.memory_len += 1;
                }

                let temporaries_args = (block_start..block.memory_len)
                    .map(LocationId)
                    .map(Arg::Location)
                    .map(Some);
                match block.terminator {
                    Terminator::Goto(ref mut target, ref mut args) => {
                        if let Some(target) = target {
                            *target = *target + function_start;
                        } else {
                            *target = Some(continue_id);
                            *args = args
                                .iter()
                                .enumerate()
                                .filter(|&(i, _)| ret[i].is_some())
                                .map(|(_, &arg)| arg)
                                .collect();
                        }

                        args.extend(temporaries_args);
                    }
                    Terminator::Match(_, ref mut arms) => {
                        for arm in arms.iter_mut() {
                            if let Some(ref mut target) = arm.target {
                                *target = *target + function_start;
                            } else {
                                arm.args = arm
                                    .args
                                    .iter()
                                    .enumerate()
                                    .filter(|&(i, _)| ret[i].is_some())
                                    .map(|(_, &arg)| arg)
                                    .collect();
                                arm.target = Some(continue_id);
                            }

                            arm.args.extend(temporaries_args.clone());
                        }
                    }
                }
                l!(self, f).blocks.push(block);
            }

            l!(self, f, b) = block;
        } else {
            unreachable!("tried to inline an ordinary step");
        }
    }

    fn should_inline(&self, f: FunctionId, allowed_cost: usize) -> Option<usize> {
        let mut current_cost = 0;
        for block in l!(self, f).blocks.iter() {
            current_cost += 25;

            for step in block.steps.iter() {
                current_cost += match *step {
                    Action::BlackBox(..)
                    | Action::Move(..)
                    | Action::Debug(..)
                    | Action::LoadConstant(..) => 0,
                    Action::Invert(..) => 5,
                    Action::Binop { .. } => 20,
                    Action::FunctionCall { id, .. } => {
                        self.should_inline(id, allowed_cost.checked_sub(current_cost)?)?
                    }
                };
            }

            current_cost += match block.terminator {
                Terminator::Goto(..) => 0,
                Terminator::Match(_, ref arms) => arms.len() * 10,
            };
        }

        allowed_cost.checked_sub(current_cost)
    }
}
