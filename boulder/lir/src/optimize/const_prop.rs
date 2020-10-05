use crate::{Action, Arg, Binop, Block, BoolOp, Function, Lir, Terminator};
use shared_id::{BlockId, LocationId, StepId};
use std::iter;
use tindex::{tvec, TVec};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum State {
    Undefined,
    Known(u8),
    Unknown,
}

impl State {
    fn is_refinement(self, other: Self) -> bool {
        match (self, other) {
            (State::Undefined, State::Undefined) => true,
            (State::Undefined, State::Known(_)) | (State::Undefined, State::Unknown) => false,
            (State::Known(_), State::Undefined) => true,
            (State::Known(l), State::Known(r)) => {
                if l == r {
                    true
                } else {
                    false
                }
            }
            (State::Known(_), State::Unknown) => false,
            (State::Unknown, _) => true,
        }
    }

    fn merge(self, other: Self) -> Self {
        match (self, other) {
            (State::Undefined, State::Undefined) => State::Undefined,
            (State::Undefined, State::Known(v)) | (State::Known(v), State::Undefined) => {
                State::Known(v)
            }
            (State::Known(l), State::Known(r)) => {
                if l == r {
                    State::Known(l)
                } else {
                    State::Unknown
                }
            }
            (State::Unknown, _) | (_, State::Unknown) => State::Unknown,
        }
    }
}

impl<'a> Lir<'a> {
    pub fn const_propagate(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("const_propagate");

        for func in self.functions.iter_mut() {
            func.const_propagate();
        }
    }
}

impl<'a> Function<'a> {
    pub fn const_propagate(&mut self) {
        let mut initial_states = self.initial_states();
        for blk in self.blocks.index_iter() {
            if let Some(initial_states) = initial_states[blk].take() {
                self.blocks[blk].const_propagate(initial_states);
            }
        }

        self.remove_unused_blocks();
    }

    fn initial_states(&self) -> TVec<BlockId, Option<TVec<LocationId, State>>> {
        let mut initial_states = tvec![None; self.blocks.len()];

        let current: TVec<LocationId, State> = iter::repeat(State::Unknown)
            .take(self.input_len)
            .chain(iter::repeat(State::Undefined).take(self.memory_len - self.input_len))
            .collect();
        let mut to_visit = vec![(current, BlockId(0))];
        while let Some((mut current, blk)) = to_visit.pop() {
            match initial_states[blk] {
                None => initial_states[blk] = Some(current.clone()),
                Some(ref mut initial_states) => {
                    if initial_states
                        .iter()
                        .zip(current.iter())
                        .all(|(init, &curr)| init.is_refinement(curr))
                    {
                        // Fixpoint has been reached.
                        continue;
                    }

                    for (init, curr) in initial_states.iter_mut().zip(current.iter_mut()) {
                        *init = init.merge(*curr);
                        *curr = *init;
                    }
                }
            }

            for step in self.blocks[blk].steps.iter() {
                match *step {
                    Action::Invert(i, o) => match current[i] {
                        State::Undefined => unreachable!(),
                        State::Known(v) => current[o] = State::Known(!v),
                        State::Unknown => current[o] = State::Unknown,
                    },
                    Action::BlackBox(_, o) => current[o] = State::Unknown,
                    Action::Move(Arg::Location(i), o) => match current[i] {
                        State::Undefined => current[o] = State::Undefined,
                        State::Known(v) => current[o] = State::Known(v),
                        State::Unknown => current[o] = State::Unknown,
                    },
                    Action::Move(Arg::Byte(v), o) => current[o] = State::Known(v),
                    Action::Debug(_) => (),
                    Action::Binop { op, l, r, out } => {
                        current[out] = State::Unknown;
                        let v = |l| match l {
                            Arg::Byte(v) => Some(v),
                            Arg::Location(id) => match current[id] {
                                State::Undefined => unreachable!(),
                                State::Known(v) => Some(v),
                                State::Unknown => None,
                            },
                        };

                        match (v(l), v(r)) {
                            (Some(l), Some(r)) => {
                                let v = match op {
                                    Binop::Add => l.checked_add(r),
                                    Binop::Sub => l.checked_sub(r),
                                    Binop::Shl => l.checked_shl(r.into()).or(Some(0)),
                                    Binop::Shr => l.checked_shr(r.into()).or(Some(0)),
                                    Binop::Logic(bool_op, tru, fals) => {
                                        if match bool_op {
                                            BoolOp::Eq => l == r,
                                            BoolOp::Gt => l > r,
                                            BoolOp::Gte => l >= r,
                                        } {
                                            Some(tru)
                                        } else {
                                            Some(fals)
                                        }
                                    }
                                    Binop::BitOr => Some(l | r),
                                    Binop::BitAnd => Some(l & r),
                                    Binop::BitXor => Some(l ^ r),
                                }
                                .expect("undefined op");
                                current[out] = State::Known(v);
                            }
                            (Some(_), None) | (None, Some(_)) | (None, None) => (),
                        }
                    }
                    Action::FunctionCall {
                        id: _,
                        args: _,
                        ref ret,
                    } => {
                        for &ret in ret {
                            if let Some(out) = ret {
                                current[out] = State::Unknown;
                            }
                        }
                    }
                    Action::Noop => (),
                }
            }

            match self.blocks[blk].terminator {
                Terminator::Goto(Some(target)) => to_visit.push((current, target)),
                Terminator::Goto(None) => (),
                Terminator::Match(val, ref arms) => match current[val] {
                    State::Undefined => unreachable!(),
                    State::Known(v) => {
                        if let Some(target) = arms
                            .iter()
                            .find(|arm| arm.pat == v)
                            .unwrap_or_else(|| arms.last().unwrap())
                            .target
                        {
                            to_visit.push((current, target))
                        }
                    }
                    State::Unknown => {
                        let (last, rst) = arms.split_last().unwrap();

                        for arm in rst.iter() {
                            if let Some(target) = arm.target {
                                let mut clone = current.clone();
                                clone[val] = State::Known(arm.pat);
                                to_visit.push((clone, target));
                            }
                        }

                        if let Some(target) = last.target {
                            to_visit.push((current, target));
                        }
                    }
                },
            }
        }

        initial_states
    }
}

impl Block {
    pub fn const_propagate(&mut self, mut loc: TVec<LocationId, State>) {
        let mut step = StepId(0);
        while step < self.steps.range_end() {
            match self.steps[step] {
                Action::Invert(i, o) => match loc[i] {
                    State::Undefined => unreachable!(),
                    State::Known(v) => {
                        self.steps[step] = Action::Move(Arg::Byte(!v), o);
                        loc[o] = State::Known(!v);
                    }
                    State::Unknown => loc[o] = State::Unknown,
                },
                Action::BlackBox(_, o) => loc[o] = State::Unknown,
                Action::Move(Arg::Location(i), o) => {
                    if let State::Known(v) = loc[i] {
                        self.steps[step] = Action::Move(Arg::Byte(v), o);
                        loc[o] = State::Known(v);
                    } else {
                        loc[o] = State::Unknown;
                    }
                }
                Action::Move(Arg::Byte(v), o) => loc[o] = State::Known(v),
                Action::Debug(_) => (),
                Action::Binop { op, l, r, out } => {
                    loc[out] = State::Unknown;
                    let v = |l| match l {
                        Arg::Byte(v) => Some(v),
                        Arg::Location(id) => match loc[id] {
                            State::Undefined => unreachable!(),
                            State::Known(v) => Some(v),
                            State::Unknown => None,
                        },
                    };

                    match (v(l), v(r)) {
                        (Some(l), Some(r)) => {
                            let v = match op {
                                Binop::Add => l.checked_add(r),
                                Binop::Sub => l.checked_sub(r),
                                Binop::Shl => l.checked_shl(r.into()).or(Some(0)),
                                Binop::Shr => l.checked_shr(r.into()).or(Some(0)),
                                Binop::Logic(bool_op, tru, fals) => {
                                    if match bool_op {
                                        BoolOp::Eq => l == r,
                                        BoolOp::Gt => l > r,
                                        BoolOp::Gte => l >= r,
                                    } {
                                        Some(tru)
                                    } else {
                                        Some(fals)
                                    }
                                }
                                Binop::BitOr => Some(l | r),
                                Binop::BitAnd => Some(l & r),
                                Binop::BitXor => Some(l ^ r),
                            }
                            .expect("undefined op");
                            self.steps[step] = Action::Move(Arg::Byte(v), out);
                            loc[out] = State::Known(v);
                        }
                        (Some(l), None) => {
                            self.steps[step] = Action::Binop {
                                op,
                                l: Arg::Byte(l),
                                r,
                                out,
                            };

                            match op {
                                Binop::Logic(BoolOp::Gt, tru, fals) => match l {
                                    0 => {
                                        self.steps[step] = Action::Move(Arg::Byte(fals), out);
                                        loc[out] = State::Known(fals);
                                    }
                                    255 => {
                                        self.steps[step] = Action::Binop {
                                            op: Binop::Logic(BoolOp::Eq, fals, tru),
                                            r,
                                            l: Arg::Byte(255),
                                            out,
                                        }
                                    }
                                    _ => (),
                                },
                                Binop::Logic(BoolOp::Gte, tru, fals) => match l {
                                    0 => {
                                        self.steps[step] = Action::Binop {
                                            op: Binop::Logic(BoolOp::Eq, tru, fals),
                                            r,
                                            l: Arg::Byte(0),
                                            out,
                                        }
                                    }
                                    255 => {
                                        self.steps[step] = Action::Move(Arg::Byte(tru), out);
                                        loc[out] = State::Known(tru);
                                    }
                                    _ => (),
                                },
                                Binop::BitAnd => match l {
                                    0 => {
                                        self.steps[step] = Action::Move(Arg::Byte(0), out);
                                        loc[out] = State::Known(0);
                                    }
                                    255 => self.steps[step] = Action::Move(r, out),
                                    _ => (),
                                },
                                Binop::Add | Binop::BitOr | Binop::BitXor if l == 0 => {
                                    self.steps[step] = Action::Move(r, out);
                                }
                                Binop::Shl | Binop::Shr if l == 0 => {
                                    self.steps[step] = Action::Move(Arg::Byte(0), out);
                                    loc[out] = State::Known(0);
                                }
                                _ => (),
                            }
                        }
                        (None, Some(r)) => {
                            self.steps[step] = Action::Binop {
                                op,
                                l,
                                r: Arg::Byte(r),
                                out,
                            };

                            match op {
                                Binop::Logic(BoolOp::Gt, tru, fals) => match r {
                                    0 => {
                                        self.steps[step] = Action::Binop {
                                            op: Binop::Logic(BoolOp::Eq, fals, tru),
                                            l,
                                            r: Arg::Byte(0),
                                            out,
                                        }
                                    }
                                    255 => {
                                        self.steps[step] = Action::Move(Arg::Byte(fals), out);
                                        loc[out] = State::Known(fals);
                                    }
                                    _ => (),
                                },
                                Binop::Logic(BoolOp::Gte, tru, fals) => match r {
                                    0 => {
                                        self.steps[step] = Action::Move(Arg::Byte(tru), out);
                                        loc[out] = State::Known(tru);
                                    }
                                    255 => {
                                        self.steps[step] = Action::Binop {
                                            op: Binop::Logic(BoolOp::Eq, tru, fals),
                                            l,
                                            r: Arg::Byte(255),
                                            out,
                                        }
                                    }
                                    _ => (),
                                },
                                Binop::BitAnd => match r {
                                    0 => {
                                        self.steps[step] = Action::Move(Arg::Byte(0), out);
                                        loc[out] = State::Known(0);
                                    }
                                    255 => self.steps[step] = Action::Move(l, out),
                                    _ => (),
                                },
                                Binop::Add
                                | Binop::Sub
                                | Binop::Shl
                                | Binop::Shr
                                | Binop::BitOr
                                | Binop::BitXor
                                    if r == 0 =>
                                {
                                    self.steps[step] = Action::Move(l, out);
                                }
                                _ => (),
                            }
                        }
                        (None, None) => (),
                    }
                }
                Action::FunctionCall {
                    id: _,
                    ref mut args,
                    ref ret,
                } => {
                    *args = args
                        .iter()
                        .map(|&a| {
                            if let Some(Arg::Location(id)) = a {
                                match loc[id] {
                                    State::Undefined => None,
                                    State::Known(v) => Some(Arg::Byte(v)),
                                    State::Unknown => Some(Arg::Location(id)),
                                }
                            } else {
                                a
                            }
                        })
                        .collect();

                    for &ret in ret {
                        if let Some(out) = ret {
                            loc[out] = State::Unknown;
                        }
                    }
                }
                Action::Noop => (),
            }
            step = step + 1;
        }

        match self.terminator {
            Terminator::Goto(_) => (),
            Terminator::Match(l, ref arms) => {
                if let State::Known(v) = loc[l] {
                    let arm = arms.iter().find(|arm| arm.pat == v).unwrap();
                    self.terminator = Terminator::Goto(arm.target);
                }
            }
        }
    }
}
