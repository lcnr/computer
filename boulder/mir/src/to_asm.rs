use crate::{Mir, InitialMirState, Type, Function};

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
    pub fn to_asm(self, types: &[Type]) -> String {
        unimplemented!()
    }
}