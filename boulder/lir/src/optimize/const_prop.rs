// TODO: consider unifying `const_prop` with `lir_interpreter`.
use std::iter;

use tindex::{TSlice, TVec};

use shared_id::{BlockId, FunctionId, InputId, LocationId};

use crate::{Lir, Action, Block};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Memory {
    Byte(u8),
    Undefined,
    /// e.g. inputs
    Unknown,
}

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
            () => { self.functions[f].blocks[b] };
        }

        let mut memory: TVec<LocationId, Memory> = iter::repeat(Memory::Undefined).take(b!().memory_len).collect();
        for (&target, &value) in b!().inputs.iter().zip(inputs.iter()) {
            memory[target] = value;
        }

        for s in b!().steps.index_iter() {
            b!().steps[s].const_propagate(&memory);
        }
    }
}

impl Action {
    fn const_propagate(&mut self, _mem: &TSlice<LocationId, Memory>) {

    }
}
