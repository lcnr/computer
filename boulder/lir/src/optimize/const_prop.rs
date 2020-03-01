// TODO: consider unifying `const_prop` with `lir_interpreter`.
use std::{convert::identity, iter};

use tindex::{TSlice, TVec};

use shared_id::{BlockId, FunctionId, InputId, LocationId};

use crate::{Action, Arg, Binop, Lir, Memory};

impl<'a> Lir<'a> {
    pub fn const_propagate(&mut self) {
        for f in self.functions.index_iter() {
            for b in self.functions[f].blocks.index_iter() {
                let inputs: TVec<_, _> = self.functions[f].blocks[b]
                    .inputs
                    .iter()
                    .map(|_| Memory::Unknown)
                    .collect();
                self.propagate_block(f, b, &inputs);
            }
        }
    }

    fn propagate_block<'b>(&mut self, f: FunctionId, b: BlockId, inputs: &TSlice<InputId, Memory>) {
        macro_rules! b {
            () => {
                self.functions[f].blocks[b]
            };
        }

        let mut memory: TVec<LocationId, Memory> = iter::repeat(Memory::Undefined)
            .take(b!().memory_len)
            .collect();
        for (&target, &value) in b!().inputs.iter().zip(inputs.iter()) {
            memory[target] = value;
        }

        let steps = b!()
            .steps
            .iter()
            .filter_map(|s| s.const_propagate(&self, &mut memory))
            .collect();

        b!().steps = steps;
    }
}

impl Action {
    fn const_propagate(&self, lir: &Lir, mem: &mut TSlice<LocationId, Memory>) -> Option<Action> {
        match *self {
            Action::Invert(i, o) => match mem[i] {
                Memory::Undefined => {
                    mem[o] = Memory::Undefined;
                    None
                }
                Memory::Unknown => {
                    mem[o] = Memory::Unknown;
                    Some(self.clone())
                }
                Memory::Byte(v) => {
                    mem[o] = Memory::Byte(!v);
                    Some(Action::LoadConstant(!v, o))
                }
            },
            Action::Move(i, o) => match mem[i] {
                Memory::Undefined => {
                    mem[o] = Memory::Undefined;
                    None
                }
                Memory::Unknown => {
                    mem[o] = Memory::Unknown;
                    Some(self.clone())
                }
                Memory::Byte(v) => {
                    mem[o] = Memory::Byte(v);
                    Some(Action::LoadConstant(v, o))
                }
            },
            Action::Debug(_) => Some(self.clone()),
            Action::LoadConstant(v, o) => {
                mem[o] = Memory::Byte(v);
                Some(self.clone())
            }
            Action::Binop { op, l, r, out } => {
                let v = |r| match r {
                    Arg::Byte(v) => Memory::Byte(v),
                    Arg::Location(id) => mem[id],
                };

                match (v(l), v(r)) {
                    (Memory::Undefined, _) | (_, Memory::Undefined) => {
                        mem[out] = Memory::Undefined;
                        None
                    }
                    (Memory::Byte(l), Memory::Byte(r)) => {
                        let to_bool = |b| {
                            if b {
                                Some(lir.ctx.true_replacement)
                            } else {
                                Some(lir.ctx.false_replacement)
                            }
                        };

                        let v = match op {
                            Binop::Add => l.checked_add(r),
                            Binop::Sub => l.checked_sub(r),
                            Binop::Shl => l.checked_shl(r.into()).or(Some(0)),
                            Binop::Shr => l.checked_shr(r.into()).or(Some(0)),
                            Binop::Eq => to_bool(l == r),
                            Binop::Neq => to_bool(l != r),
                            Binop::Gt => to_bool(l > r),
                            Binop::Gte => to_bool(l >= r),
                            Binop::BitOr => Some(l | r),
                            Binop::BitAnd => Some(l & r),
                            Binop::BitXor => Some(l ^ r),
                        };

                        if let Some(v) = v {
                            mem[out] = Memory::Byte(v);
                            Some(Action::LoadConstant(v, out))
                        } else {
                            mem[out] = Memory::Undefined;
                            None
                        }
                    }
                    (Memory::Byte(l), Memory::Unknown) => {
                        mem[out] = Memory::Unknown;
                        Some(Action::Binop {
                            op,
                            l: Arg::Byte(l),
                            r,
                            out,
                        })
                    }
                    (Memory::Unknown, Memory::Byte(r)) => {
                        mem[out] = Memory::Unknown;
                        Some(Action::Binop {
                            op,
                            l,
                            r: Arg::Byte(r),
                            out,
                        })
                    }
                    (Memory::Unknown, Memory::Unknown) => {
                        mem[out] = Memory::Unknown;
                        Some(self.clone())
                    }
                }
            }
            Action::FunctionCall { id, ref args, ref ret } => {
                for r in ret.iter().copied().filter_map(identity) {
                    mem[r] = Memory::Unknown;
                }

                let args = args.iter().map(|&a| {
                    if let Some(Arg::Location(id)) = a {
                        match mem[id] {
                            Memory::Unknown => Some(Arg::Location(id)),
                            Memory::Byte(v) => Some(Arg::Byte(v)),
                            Memory::Undefined => None,
                        }
                    } else {
                        a
                    }
                }).collect();

                Some(Action::FunctionCall {
                    id,
                    args,
                    ret: ret.clone(),
                })
            }
        }
    }
}
