use tindex::TBitSet;

use shared_id::{BlockId, StepId};

use crate::{traits::Update, Action, Arg, Lir, Terminator};

impl<'a> Lir<'a> {
    /// inlines single block functions
    pub fn inline_functions(&mut self) {
        let mut to_inline = TBitSet::new();
        for (f, function) in self.functions.index_iter().zip(self.functions.iter()) {
            if let Terminator::Goto(None, _) = function.blocks[BlockId(0)].terminator {
                to_inline.add(f);
            }
        }

        for f in self.functions.index_iter() {
            for b in f!(self, f).blocks.index_iter() {
                let mut s = StepId(0);
                'steps: while s < b!(self, f, b).steps.range_end() {
                    if let Action::FunctionCall {
                        id,
                        ref args,
                        ref ret,
                    } = s!(self, f, b, s)
                    {
                        if to_inline.get(id) {
                            let memory_offset = b!(self, f, b).memory_len;
                            let other = &b!(self, id, BlockId(0));
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

                            b!(self, f, b).memory_len += other.memory_len;
                            b!(self, f, b).steps.splice(s..s + 1, actions.into_iter());

                            // do not increment `s`.
                            continue 'steps;
                        }
                    }

                    s = s + 1;
                }
            }
        }
    }
}
