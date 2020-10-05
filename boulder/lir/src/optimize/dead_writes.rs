use tindex::{TBitSet, TVec};

use shared_id::LocationId;

use crate::reachability::Reachability;
use crate::traits::{Reads, Writes};
use crate::{Action, Block, Function, Lir};

impl<'a> Lir<'a> {
    pub fn dead_write_elim(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("dead_write_elim");
        for func in self.functions.iter_mut() {
            func.dead_write_elim();
        }
    }
}

impl<'a> Function<'a> {
    pub fn dead_write_elim(&mut self) {
        for blk in self.blocks.index_iter() {
            let loc = self.terminator_reachability(blk);
            self.blocks[blk].dead_write_elim(loc);
            self.blocks[blk].remove_noops();
        }
    }
}

impl Block {
    pub fn dead_write_elim(&mut self, mut loc: TVec<LocationId, Reachability>) {
        self.terminator.reads(|id| {
            loc[id] = Reachability::Access;
        });

        for step in self.steps.iter_mut().rev() {
            let mut unused = TBitSet::new();
            step.writes(|id| {
                match loc[id] {
                    Reachability::Ignore | Reachability::Overwrite => unused.add(id),
                    Reachability::Access => (),
                }
                loc[id] = Reachability::Overwrite;
                id
            });

            if !unused.is_empty() {
                match *step {
                    Action::Invert(_, o)
                    | Action::BlackBox(_, o)
                    | Action::Move(_, o)
                    | Action::Binop {
                        op: _,
                        l: _,
                        r: _,
                        out: o,
                    } => {
                        assert!(unused.get(o));
                        *step = Action::Noop;
                    }
                    Action::Debug(_) | Action::Noop => unreachable!(),
                    Action::FunctionCall {
                        id: _,
                        args: _,
                        ref mut ret,
                    } => {
                        for ret in ret.iter_mut() {
                            if let Some(out) = *ret {
                                if unused.get(out) {
                                    *ret = None;
                                }
                            }
                        }

                        if ret.iter().all(|r| r.is_none()) {
                            *step = Action::Noop;
                        }
                    }
                }
            }

            step.reads(|id| {
                loc[id] = Reachability::Access;
                id
            });
        }
    }
}
