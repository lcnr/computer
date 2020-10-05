use crate::traits::Writes;
use crate::{Action, Arg, Binop, BoolOp, Function, Lir, MatchArm, Terminator};
use tindex::TBitSet;

impl<'a> Lir<'a> {
    pub fn simplify_comparision_branch(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("simplify_comparision_branch");

        for func in self.functions.iter_mut() {
            func.simplify_comparision_branch();
        }
    }
}

impl<'a> Function<'a> {
    fn simplify_comparision_branch(&mut self) {
        'blocks: for block in self.blocks.iter_mut() {
            let (pat, arms) = match block.terminator {
                Terminator::Goto(_) => continue 'blocks,
                Terminator::Match(pat, ref arms) => (pat, arms),
            };

            let mut overwritten = TBitSet::new();
            for step in block.steps.iter().rev() {
                let mut wrote_to_pat = false;
                step.writes(|id| {
                    if id == pat {
                        wrote_to_pat = true;
                    } else {
                        overwritten.add(id)
                    }
                });

                if wrote_to_pat {
                    match *step {
                        Action::Binop {
                            op: Binop::Logic(BoolOp::Eq, t, f),
                            l: Arg::Location(loc),
                            r: Arg::Byte(v),
                            out: _,
                        } => {
                            if !overwritten.get(loc) {
                                let t = super::arm_for(arms, t);
                                let f = super::arm_for(arms, f);
                                if t == f {
                                    panic!("unexpected bool match");
                                }
                                block.terminator = Terminator::Match(
                                    loc,
                                    vec![
                                        MatchArm {
                                            pat: v,
                                            target: t.target,
                                        },
                                        MatchArm {
                                            pat: 177,
                                            target: f.target,
                                        },
                                    ],
                                );
                            }
                        }
                        _ => (),
                    }
                    continue 'blocks;
                }
            }
        }
    }
}
