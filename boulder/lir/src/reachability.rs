use crate::traits::{Reads, Writes};
use crate::{Block, Function, Terminator};
use shared_id::{BlockId, LocationId};
use std::cmp;
use tindex::{tvec, TBitSet, TVec};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Reachability {
    Overwrite,
    Ignore,
    Access,
}

impl Reachability {
    pub fn read(&mut self) {
        *self = match self {
            Reachability::Ignore | Reachability::Access => Reachability::Access,
            Reachability::Overwrite => Reachability::Overwrite,
        };
    }

    pub fn write(&mut self) {
        *self = match self {
            Reachability::Ignore | Reachability::Overwrite => Reachability::Overwrite,
            Reachability::Access => Reachability::Access,
        };
    }

    pub fn append(&mut self, other: Reachability) {
        match other {
            Reachability::Ignore => (),
            Reachability::Access => self.read(),
            Reachability::Overwrite => self.write(),
        }
    }

    pub fn merge(&mut self, other: Reachability) {
        *self = cmp::max(*self, other);
    }
}

impl<'a> Function<'a> {
    /// Locations reachable after the terminator of `block`.
    pub fn terminator_reachability(&self, block: BlockId) -> TVec<LocationId, Reachability> {
        let mut visited = TBitSet::new();
        terminator(self, block, &mut visited)
    }

    pub fn block_reachability(&self, block: BlockId) -> TVec<LocationId, Reachability> {
        recurse(self, block, &mut TBitSet::new())
    }
}

fn terminator<'a>(
    func: &Function<'a>,
    block: BlockId,
    visited: &mut TBitSet<BlockId>,
) -> TVec<LocationId, Reachability> {
    let mut reach = tvec![Reachability::Ignore; func.memory_len];
    match func.blocks[block].terminator {
        Terminator::Goto(Some(target)) => {
            if !visited.get(target) {
                visited.add(target);
                reach
                    .iter_mut()
                    .zip(recurse(func, target, visited))
                    .for_each(|(r, rec)| r.append(rec));
                visited.remove(target);
            }
        }
        Terminator::Goto(None) => {
            reach
                .iter_mut()
                .take(func.return_len)
                .for_each(|r| r.read());
        }
        // The match target is checked locally.
        Terminator::Match(_, ref arms) => {
            let mut match_reach: TVec<LocationId, Reachability> =
                tvec![Reachability::Ignore; func.memory_len];
            for arm in arms.iter() {
                if let Some(target) = arm.target {
                    if !visited.get(target) {
                        visited.add(target);
                        match_reach
                            .iter_mut()
                            .zip(recurse(func, target, visited))
                            .for_each(|(m, v)| m.merge(v));
                        visited.remove(target);
                    }
                } else {
                    match_reach
                        .iter_mut()
                        .take(func.return_len)
                        .for_each(|m| m.merge(Reachability::Access));
                }
            }

            reach
                .iter_mut()
                .zip(match_reach)
                .for_each(|(r, m)| r.append(m))
        }
    }

    reach
}

fn recurse<'a>(
    func: &Function<'a>,
    block: BlockId,
    visited: &mut TBitSet<BlockId>,
) -> TVec<LocationId, Reachability> {
    let mut v = func.blocks[block].self_reachability(func.memory_len);
    v.iter_mut()
        .zip(terminator(func, block, visited))
        .for_each(|(v, t)| v.append(t));
    v
}

impl Block {
    pub fn self_reachability(&self, memory_len: usize) -> TVec<LocationId, Reachability> {
        let mut reach = tvec![Reachability::Ignore; memory_len];
        for step in self.steps.iter() {
            step.reads(|id| {
                reach[id].read();
            });
            step.writes(|id| {
                reach[id].write();
            });
        }

        self.terminator.reads(|id| {
            reach[id].read();
        });

        reach
    }
}
