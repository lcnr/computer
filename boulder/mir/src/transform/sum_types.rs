use crate::Mir;

impl<'a> Mir<'a> {
    /// split all sum types into a tagged union and a tag
    pub fn reduce_sum_types(&mut self) {
        for ty in self.types.index_iter() {}
    }
}
