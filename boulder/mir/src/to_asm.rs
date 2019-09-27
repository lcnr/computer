use std::fmt::Write;

use tindex::TSlice;

use crate::{traits::InitialMirState, Function, Mir, Type, TypeId};

impl Mir<InitialMirState> {
    pub fn to_asm(self) -> String {
        let mut asm = String::new();
        for func in self.functions {
            asm.push_str(&func.to_asm(&self.types));
        }
        asm
    }
}

impl Function<InitialMirState> {
    pub fn to_asm(self, types: &TSlice<TypeId, Type>) -> String {
        let mut asm = format!("{}:\n", self.name);

        for (id, block) in self.blocks.iter().enumerate() {
            write!(asm, ".block{}:", id).unwrap();
            for step in block.steps.iter() {}
        }

        dbg!(asm)
    }
}
