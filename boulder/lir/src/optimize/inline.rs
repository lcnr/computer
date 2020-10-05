use crate::traits::Update;
use crate::{Action, Arg, Block, Lir, Terminator};
use shared_id::{BlockId, FunctionId, LocationId, StepId};
use std::mem;

const MAX_INLINE_COST: usize = 100;

impl<'a> Lir<'a> {
    /// inlines single block functions
    pub fn inline_functions(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("inline_functions");
        for f in self.functions.index_iter() {
            let mut b = BlockId(0);
            while b < l!(self, f).blocks.range_end() {
                let mut s = StepId(0);
                while s < l!(self, f, b).steps.range_end() {
                    if let Action::FunctionCall { id, .. } = l!(self, f, b, s) {
                        let function_inline_cost = if l!(self, id).ctx.inline {
                            MAX_INLINE_COST * 10
                        } else {
                            MAX_INLINE_COST
                        };
                        if self.should_inline(id, function_inline_cost).is_some() {
                            self.inline_location(f, b, s);
                        }
                    }

                    s = s + 1;
                }

                b = b + 1;
            }
        }
    }

    /// Inlines a given function call.
    pub fn inline_location(&mut self, f: FunctionId, b: BlockId, s: StepId) {
        let mut block = l!(self, f, b).clone();
        let rest = block.steps.split_off(s + 1);
        if let Action::FunctionCall { id, args, ret } = block.steps.pop().unwrap() {
            let block_count = l!(self, f).blocks.len();
            let inlinee_block_count = l!(self, id).blocks.len();
            let memory_len = l!(self, f).memory_len;

            for (i, &arg) in args.iter().enumerate() {
                match arg {
                    Some(arg) => drop(
                        block
                            .steps
                            .push(Action::Move(arg, LocationId(memory_len + i))),
                    ),
                    None => (),
                }
            }

            let terminator = mem::replace(
                &mut block.terminator,
                Terminator::Goto(Some(BlockId(block_count))),
            );
            for blk in l!(self, id).blocks.index_iter() {
                let mut block = l!(self, id, blk).clone();
                block.update(|id| {
                    if let Some(target) = id {
                        Some(BlockId(target.0 + block_count))
                    } else {
                        Some(BlockId(block_count + inlinee_block_count))
                    }
                });
                block.update(|id: LocationId| id + memory_len);
                l!(self, f).blocks.push(block);
            }

            l!(self, f, b) = block;
            l!(self, f).blocks.push(Block {
                steps: ret
                    .iter()
                    .enumerate()
                    .filter_map(|(i, ret)| {
                        ret.map(|ret| Action::Move(Arg::Location(LocationId(memory_len + i)), ret))
                    })
                    .chain(rest)
                    .collect(),
                terminator,
            });
            l!(self, f).memory_len = l!(self, f).memory_len + l!(self, id).memory_len;
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
                    Action::BlackBox(..) | Action::Move(..) | Action::Debug(..) => 2,
                    Action::Invert(..) => 5,
                    Action::Binop { .. } => 20,
                    Action::FunctionCall { id, .. } => {
                        self.should_inline(id, allowed_cost.checked_sub(current_cost)?)?
                    }
                    Action::Noop => 0,
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
