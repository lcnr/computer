use std::convert::identity;

use tindex::TBitSet;

use shared_id::{BlockId, FunctionId, LocationId, StepId};

use crate::{traits::Update, Action, Arg, Block, Lir, Terminator};

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
            for b in l!(self, f).blocks.index_iter() {
                let mut s = StepId(0);
                'steps: while s < l!(self, f, b).steps.range_end() {
                    if let Action::FunctionCall {
                        id,
                        ref args,
                        ref ret,
                    } = l!(self, f, b, s)
                    {
                        if to_inline.get(id) {
                            let memory_offset = l!(self, f, b).memory_len;
                            let other = &l!(self, id, BlockId(0));
                            let mut actions = Vec::new();
                            for (&arg, &input) in args.iter().zip(other.inputs.iter()) {
                                match arg {
                                    None => (),
                                    Some(Arg::Byte(v)) => {
                                        actions.push(Action::LoadConstant(v, input + memory_offset))
                                    }
                                    Some(Arg::Location(id)) => {
                                        actions.push(Action::Move(id, input + memory_offset))
                                    }
                                }
                            }

                            for mut step in other.steps.iter().cloned() {
                                step.update(|l| l + memory_offset);
                                actions.push(step);
                            }

                            if let Terminator::Goto(None, ref args) = other.terminator {
                                for (&ret, &arg) in ret.iter().zip(args.iter()) {
                                    match (ret, arg) {
                                        (None, _) | (_, None) => (),
                                        (Some(ret), Some(Arg::Byte(v))) => {
                                            actions.push(Action::LoadConstant(v, ret))
                                        }
                                        (Some(ret), Some(Arg::Location(id))) => {
                                            actions.push(Action::Move(id + memory_offset, ret))
                                        }
                                    }
                                }
                            } else {
                                unreachable!("unexpected terminator");
                            }

                            l!(self, f, b).memory_len += other.memory_len;
                            l!(self, f, b).steps.splice(s..s + 1, actions.into_iter());

                            // do not increment `s`.
                            continue 'steps;
                        }
                    }

                    s = s + 1;
                }
            }
        }
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
}
