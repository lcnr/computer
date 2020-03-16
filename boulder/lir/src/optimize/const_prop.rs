// TODO: consider unifying `const_prop` with `lir_interpreter`.
use std::{convert::identity, iter};

use tindex::{tvec, TSlice, TVec};

use shared_id::{BlockId, FunctionId, InputId, LocationId, StepId};

use crate::{Action, Arg, Binop, Lir, Memory, Terminator};

impl<'a> Lir<'a> {
    pub fn const_propagate(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("const_propagate");

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

    fn propagate_block(&mut self, f: FunctionId, b: BlockId, inputs: &TSlice<InputId, Memory>) {
        let mut memory: TVec<LocationId, Memory> = iter::repeat(Memory::Undefined)
            .take(l!(self, f, b).memory_len)
            .collect();
        for (&target, &value) in l!(self, f, b).inputs.iter().zip(inputs.iter()) {
            memory[target] = value;
        }

        let steps = l!(self, f, b)
            .steps
            .iter()
            .filter_map(|s| s.const_propagate(&self, &mut memory))
            .collect();

        l!(self, f, b).steps = steps;

        match l!(self, f, b).terminator {
            Terminator::Goto(_, ref mut args) => {
                propagate_args(&memory, args);
            }
            Terminator::Match(expr, ref mut arms) => match memory[expr] {
                Memory::Undefined => panic!("match on undefined: \n{}", self),
                Memory::Byte(v) => {
                    for arm in arms.iter_mut() {
                        if arm.pat == v {
                            propagate_args(&memory, &mut arm.args);
                            l!(self, f, b).terminator =
                                Terminator::Goto(arm.target, arm.args.clone());
                            return;
                        }
                    }

                    panic!("match on invalid data");
                }
                Memory::Unknown => {
                    for arm in arms.iter_mut() {
                        memory[expr] = Memory::Byte(arm.pat);
                        propagate_args(&memory, &mut arm.args);
                    }
                }
            },
        }
    }

    pub fn propagate_block_arguments(&mut self) {
        #[derive(Debug, Clone, Copy)]
        pub enum Input {
            None,
            Unique(Option<u8>),
            Multiple,
        }

        for func in self.functions.iter_mut() {
            let mut inputs: TVec<BlockId, TVec<InputId, Input>> = func
                .blocks
                .iter()
                .map(|b| tvec![Input::None; b.inputs.len()])
                .collect();

            let mut update_inputs = |target: BlockId, args: &TSlice<InputId, Option<Arg>>| {
                let target_inputs = &mut inputs[target];
                for (target, &arg) in target_inputs.iter_mut().zip(args.iter()) {
                    match (*target, arg) {
                        (_, None) | (Input::Multiple, _) => {}
                        (Input::None, Some(Arg::Location(_))) => *target = Input::Unique(None),
                        (Input::None, Some(Arg::Byte(v))) => *target = Input::Unique(Some(v)),
                        (Input::Unique(Some(a)), Some(Arg::Byte(b))) if a == b => {}
                        (Input::Unique(_), Some(_)) => *target = Input::Multiple,
                    }
                }
            };

            for block in func.blocks.iter() {
                match block.terminator {
                    Terminator::Goto(Some(target), ref args) => update_inputs(target, args),
                    Terminator::Goto(_, _) => {}
                    Terminator::Match(_, ref arms) => {
                        for arm in arms.iter() {
                            if let Some(target) = arm.target {
                                update_inputs(target, &arm.args);
                            }
                        }
                    }
                }
            }

            // skip the first block as its function arguments are currently not checked
            for b in func.blocks.index_iter().skip(1) {
                for (i, &input) in inputs[b].index_iter().zip(inputs[b].iter()).rev() {
                    match input {
                        Input::None => {
                            func.remove_input(b, i);
                        }
                        Input::Unique(Some(v)) => {
                            let block = &mut func.blocks[b];
                            let l = block.inputs[i];
                            block.steps.insert(StepId(0), Action::LoadConstant(v, l));
                            func.remove_input(b, i);
                        }
                        Input::Unique(None) | Input::Multiple => {}
                    }
                }
            }
        }
    }
}

fn propagate_args(mem: &TSlice<LocationId, Memory>, args: &mut TSlice<InputId, Option<Arg>>) {
    for arg in args.iter_mut() {
        if let Some(Arg::Location(l)) = *arg {
            match mem[l] {
                Memory::Byte(v) => *arg = Some(Arg::Byte(v)),
                Memory::Undefined => *arg = None,
                Memory::Unknown => {}
            }
        }
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
            Action::BlackBox(_, o) => {
                mem[o] = Memory::Unknown;
                Some(self.clone())
            }
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
            Action::FunctionCall {
                id,
                ref args,
                ref ret,
            } => {
                let args = args
                    .iter()
                    .map(|&a| {
                        if let Some(Arg::Location(id)) = a {
                            match mem[id] {
                                Memory::Unknown => Some(Arg::Location(id)),
                                Memory::Byte(v) => Some(Arg::Byte(v)),
                                Memory::Undefined => None,
                            }
                        } else {
                            a
                        }
                    })
                    .collect();

                for r in ret.iter().copied().filter_map(identity) {
                    mem[r] = Memory::Unknown;
                }

                Some(Action::FunctionCall {
                    id,
                    args,
                    ret: ret.clone(),
                })
            }
        }
    }
}
