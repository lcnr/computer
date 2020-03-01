// TODO: consider unifying `const_prop` with `lir_interpreter`.
use std::{convert::identity, iter};

use tindex::{TSlice, TVec};

use shared_id::{BlockId, FunctionId, InputId, LocationId};

use crate::{Action, Arg, Lir, Memory};

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
    fn const_propagate(&self, _lir: &Lir, mem: &mut TSlice<LocationId, Memory>) -> Option<Action> {
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
            Action::Debug(i) => Some(self.clone()),
            Action::LoadConstant(v, o) => {
                mem[o] = Memory::Byte(v);
                Some(self.clone())
            }
            Action::Binop { out, .. } => {
                /* let v = |r| match r {
                    Arg::Byte(v) => Memory::Byte(v),
                    Arg::Location(id) => mem[id],
                };

                match (v(l), v(r)) {
                    (Memory::Undefined, _) | (_, Memory::Undefined) => {
                        mem[out] = Memory::Undefined;
                        None
                    }
                    (Memory::Byte(a), Memory::Byte(b)) => {
                        mem[o] = match op {
                            Binop::Add
                        }
                    }
                } */
                mem[out] = Memory::Unknown;
                Some(self.clone())
            }
            Action::FunctionCall { ref ret, .. } => {
                for r in ret.iter().copied().filter_map(identity) {
                    mem[r] = Memory::Unknown;
                }
                Some(self.clone())
            }
        }
    }
}
