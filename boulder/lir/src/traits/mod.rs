use shared_id::LocationId;

mod reads;
mod writes;

pub trait UpdateLocation<'a, F>
where
    F: FnMut(LocationId) -> LocationId,
{
    fn update_locations(&'a mut self, f: F);
}

/// # Warning
///
/// This implementation depends on the fact that all current
/// implementations of `Reads` and `Writes` first read and
/// then write.
///
/// Implementing `Reads` and `Writes` for `Block` would therefore
/// be incorrect.
impl<'a, F, T: 'a> UpdateLocation<'a, F> for T
where
    F: FnMut(LocationId) -> LocationId,
    for<'b, 'c> &'b mut T: Reads<&'c mut F> + Writes<&'c mut F>,
{
    fn update_locations(&'a mut self, mut f: F) {
        let f = &mut f;
        self.reads(f);
        self.writes(f);
    }
}

pub trait Reads<F> {
    fn reads(self, f: F);
}

pub trait Writes<F> {
    fn writes(self, f: F);
}
