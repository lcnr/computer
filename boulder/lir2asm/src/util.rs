use arrayvec::ArrayVec;

const CHUNK_SIZE: usize = 3;

pub struct Chunks<I> {
    iter: I,
}

impl<'a, I> Chunks<I> {
    pub fn new(iter: I) -> Self {
        Chunks { iter }
    }
}

impl<'a, I: Iterator> Iterator for Chunks<I> {
    type Item = ArrayVec<[I::Item; 3]>;

    fn next(&mut self) -> Option<Self::Item> {
        let iter: ArrayVec<_> = self.iter.by_ref().take(CHUNK_SIZE).collect();
        if iter.is_empty() {
            None
        } else {
            Some(iter)
        }
    }
}
