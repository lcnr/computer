use crate::traits::{Reads, Writes};
use crate::{Action, Arg, Block, Function, Lir, Terminator};
use shared_id::{BlockId, LocationId};
use tindex::{tvec, TVec};

#[derive(Clone)]
struct Origins(Vec<LocationId>);

impl Origins {
    fn is_refinement(&self, other: &Self) -> bool {
        self.0.iter().all(|l| other.0.contains(l))
    }

    fn merge(&mut self, other: &Self) {
        self.0.retain(|l| other.0.contains(l))
    }

    fn clear(&mut self) {
        self.0.clear()
    }

    fn remove(&mut self, v: LocationId) {
        self.0.retain(|&l| l != v);
    }

    fn receive(&mut self, v: Self, l: LocationId) {
        self.0.clear();
        *self = v;
        self.0.push(l);
    }
}

impl<'a> Lir<'a> {
    /// Changes
    /// ```plain
    /// move @n @m
    /// # ...
    /// add @m @o
    /// ```
    /// to
    /// ```plain
    /// move @n @m
    /// # ...
    /// add @n @o
    /// ```
    pub fn trace_moves(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("trace_moves");
        for func in self.functions.iter_mut() {
            func.trace_moves()
        }
    }
}

impl<'a> Function<'a> {
    fn trace_moves(&mut self) {
        let initial_origins = self.initial_origins();
        for (blk, origins) in self.blocks.index_iter().zip(initial_origins).rev() {
            if let Some(origins) = origins {
                self.blocks[blk].trace_moves(origins);
            } else {
                self.remove_block(blk)
            }
        }
    }

    fn initial_origins(&self) -> TVec<BlockId, Option<TVec<LocationId, Origins>>> {
        let mut initial_origins = tvec![None; self.blocks.len()];

        let current: TVec<LocationId, Origins> = tvec![Origins(Vec::new()); self.memory_len];
        let mut to_visit = vec![(current, BlockId(0))];
        while let Some((mut current, blk)) = to_visit.pop() {
            match initial_origins[blk] {
                None => initial_origins[blk] = Some(current.clone()),
                Some(ref mut initial_origins) => {
                    if initial_origins
                        .iter()
                        .zip(current.iter())
                        .all(|(init, curr)| init.is_refinement(curr))
                    {
                        // Fixpoint has been reached.
                        continue;
                    }

                    for (init, curr) in initial_origins.iter_mut().zip(current.iter_mut()) {
                        init.merge(curr);
                        *curr = init.clone();
                    }
                }
            }

            for step in self.blocks[blk].steps.iter() {
                match *step {
                    Action::Invert(_, out)
                    | Action::BlackBox(_, out)
                    | Action::Move(Arg::Byte(_), out)
                    | Action::Binop { out, .. } => {
                        current.iter_mut().for_each(|o| o.remove(out));
                        current[out].clear();
                    }
                    Action::Move(Arg::Location(i), out) => {
                        current.iter_mut().for_each(|o| o.remove(out));
                        let clone = current[i].clone();
                        current[out].receive(clone, i);
                    }
                    Action::Debug(_) => (),
                    Action::FunctionCall {
                        id: _,
                        args: _,
                        ref ret,
                    } => {
                        for &ret in ret {
                            if let Some(out) = ret {
                                current.iter_mut().for_each(|o| o.remove(out));
                                current[out].clear();
                            }
                        }
                    }
                    Action::Noop => (),
                }
            }

            match self.blocks[blk].terminator {
                Terminator::Goto(Some(target)) => to_visit.push((current, target)),
                Terminator::Goto(None) => (),
                Terminator::Match(_, ref arms) => {
                    for arm in arms.iter() {
                        if let Some(target) = arm.target {
                            to_visit.push((current.clone(), target));
                        }
                    }
                }
            }
        }

        initial_origins
    }
}

impl Block {
    fn trace_moves(&mut self, mut origins: TVec<LocationId, Origins>) {
        for step in self.steps.iter_mut() {
            step.reads(|id| origins[id].0.first().copied().unwrap_or(id));
            match *step {
                Action::Move(Arg::Location(i), out) => {
                    origins.iter_mut().for_each(|o| o.remove(out));
                    let clone = origins[i].clone();
                    origins[out].receive(clone, i);
                }
                ref mut other => {
                    other.writes(|id| {
                        origins.iter_mut().for_each(|o| o.remove(id));
                        origins[id].clear();
                        id
                    });
                }
            }
        }

        self.terminator.reads(|id| {
            origins[id].0.first().copied().unwrap_or(id);
        })
    }
}
