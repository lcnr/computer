use crate::traits::Writes;
use crate::{Action, Arg, Binop, BoolOp, Function, Lir, Terminator};

impl<'a> Lir<'a> {
    pub fn simplify_match_branches(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("simplify_match_branches");

        for func in self.functions.iter_mut() {
            func.simplify_match_branches();
        }
    }
}

impl<'a> Function<'a> {
    pub fn simplify_match_branches(&mut self) {
        'blocks: for blk in self.blocks.index_iter() {
            let (pat, value, vt, ot) = match self.blocks[blk].terminator {
                Terminator::Goto(_) => continue,
                Terminator::Match(pat, ref arms) => match arms.as_slice() {
                    [fst, snd] => {
                        if let (Some(vt), Some(ot)) = (fst.target, snd.target) {
                            (pat, fst.pat, vt, ot)
                        } else {
                            continue;
                        }
                    }
                    _ => continue,
                },
            };

            if self.blocks[vt].terminator != self.blocks[ot].terminator
                || self.blocks[vt].steps.len() != self.blocks[ot].steps.len()
            {
                continue;
            }

            let mut steps = Vec::new();
            let mut can_use_pat = true;
            for (vs, os) in self.blocks[vt]
                .steps
                .iter()
                .zip(self.blocks[ot].steps.iter())
            {
                match (vs, os) {
                    _ if vs == os => steps.push(vs.clone()),
                    (&Action::Move(Arg::Byte(vv), lv), &Action::Move(Arg::Byte(vo), lo))
                        if lv == lo && can_use_pat =>
                    {
                        assert_ne!(vv, vo);
                        steps.push(Action::Binop {
                            op: Binop::Logic(BoolOp::Eq, vv, vo),
                            l: Arg::Byte(value),
                            r: Arg::Location(pat),
                            out: lv,
                        });
                    }
                    _ => continue 'blocks,
                }

                vs.writes(|id| {
                    if id == pat {
                        can_use_pat = false;
                    }
                });
            }
            self.blocks[blk].steps.extend(steps);
            self.blocks[blk].terminator = self.blocks[vt].terminator.clone();
        }

        self.remove_unused_blocks();
    }
}
