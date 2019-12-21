use tindex::TSlice;

use shared_id::TypeId;

use crate::{Function, Mir, Type};

impl<'a> Mir<'a> {
    /// stop using structs. Replace each struct with its individual fields
    /// until no struct types are used.
    ///
    /// TODO: UNIONS!?!?!?!
    pub fn flatten_structs(&mut self) {
        for func in self.functions.iter_mut() {
            func.flatten_structs(&self.types);
        }
    }
}

impl<'a> Function<'a> {
    pub fn flatten_structs(&mut self, types: &TSlice<TypeId, Type>) {
        let _ = types;
        unimplemented!();
    }
}
