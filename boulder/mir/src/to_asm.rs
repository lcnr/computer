use crate::{Mir, InitialMirState};

impl Mir<InitialMirState> {
    pub fn to_asm(self) -> String {
        "hello".to_string()
    }
}