use crate::{traits::Update, Action, Arg, Block, Function, Lir, MatchArm, Terminator};
use shared_id::{BlockId, FunctionId};
use std::{iter, mem};
use tindex::{tvec, TBitSet, TVec};

mod coloring;
mod const_prop;
mod dead_writes;
mod inline;
mod match_branches;
mod simplify_comparision_branch;
mod trace_moves;

impl<'a> Lir<'a> {
    /// Remove all identity moves.
    pub fn remove_identity_moves(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("remove_identity_moves");

        for function in self.functions.iter_mut() {
            for block in function.blocks.iter_mut() {
                for step in block.steps.iter_mut() {
                    match step {
                        Action::Move(Arg::Location(a), b) if a == b => {
                            *step = Action::Noop;
                        }
                        _ => (),
                    }
                }

                block.remove_noops();
            }
        }
    }

    pub fn trivial_matches(&mut self) {
        for function in self.functions.iter_mut() {
            for block in function.blocks.iter_mut() {
                if let Terminator::Match(_, ref arms) = block.terminator {
                    if arms
                        .split_first()
                        .filter(|(fst, rst)| rst.iter().all(|arm| arm.target == fst.target))
                        .is_some()
                    {
                        block.terminator = Terminator::Goto(arms[0].target);
                    }
                }
            }
        }
    }

    pub fn merge_trivial_redirects(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("merge_trivial_redirects");

        for function in self.functions.iter_mut() {
            let trivial_redirects: TVec<BlockId, Option<Terminator>> = function
                .blocks
                .iter()
                .map(|block| {
                    if block.steps.is_empty() {
                        Some(block.terminator.clone())
                    } else {
                        None
                    }
                })
                .collect();

            for block in function.blocks.iter_mut() {
                match block.terminator {
                    Terminator::Goto(Some(target)) => {
                        if let Some(ref term) = trivial_redirects[target] {
                            block.terminator = term.clone();
                        }
                    }
                    Terminator::Goto(None) => (),
                    Terminator::Match(pat, ref mut arms) => {
                        let mut iter = mem::take(arms).into_iter().peekable();
                        while let Some(arm) = iter.next() {
                            match arm.target.and_then(|t| trivial_redirects[t].as_ref()) {
                                Some(&Terminator::Goto(target)) => {
                                    arms.push(MatchArm {
                                        pat: arm.pat,
                                        target,
                                    });
                                }
                                Some(&Terminator::Match(re_pat, ref re_arms))
                                    if false && pat == re_pat && iter.peek().is_none() =>
                                {
                                    arms.extend(re_arms.iter().copied())
                                }
                                _ => arms.push(arm),
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn merge_adjacent_blocks(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("merge_adjacent_blocks");
        #[derive(Copy, Clone, PartialEq, Eq)]
        enum Use {
            No,
            Single,
            Multi,
        }

        for func in self.functions.iter_mut() {
            let mut target_blocks = tvec![Use::No; func.blocks.len()];
            for block in func.blocks.iter_mut() {
                block.update(|b: BlockId| {
                    target_blocks[b] = match target_blocks[b] {
                        Use::No => Use::Single,
                        Use::Single | Use::Multi => Use::Multi,
                    };
                    b
                });
            }

            for blk in func.blocks.index_iter() {
                match func.blocks[blk].terminator {
                    Terminator::Goto(Some(t)) if target_blocks[t] == Use::Single => {
                        func.blocks[blk].terminator = func.blocks[t].terminator.clone();
                        let steps = func.blocks[t].steps.clone();
                        func.blocks[blk].steps.extend(steps);
                    }
                    _ => (),
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
                        Terminator::Goto(target) => {
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
}

impl<'a> Function<'a> {
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
    fn remove_noops(&mut self) {
        self.steps = mem::take(&mut self.steps)
            .into_iter()
            .filter(|s| *s != Action::Noop)
            .collect();
    }
}

fn arm_for(arms: &[MatchArm], pat: u8) -> MatchArm {
    let (last, rest) = arms.split_last().unwrap();
    *rest.iter().find(|arm| arm.pat == pat).unwrap_or(last)
}
