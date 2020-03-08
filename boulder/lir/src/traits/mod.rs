use shared_id::{BlockId, FunctionId, LocationId};

mod reads;
mod writes;

use crate::{Action, Arg, Function, Terminator};

pub trait Update<F, V>
where
    F: FnMut(V) -> V,
{
    fn update(&mut self, f: F);
}

impl<'a, F> Update<F, FunctionId> for Function<'a>
where
    F: FnMut(FunctionId) -> FunctionId,
{
    fn update(&mut self, mut f: F) {
        for block in self.blocks.iter_mut() {
            for step in block.steps.iter_mut() {
                if let Action::FunctionCall { id, .. } = step {
                    *id = f(*id);
                }
            }
        }
    }
}

impl<F> Update<F, Option<Arg>> for Terminator
where
    F: FnMut(Option<Arg>) -> Option<Arg>,
{
    fn update(&mut self, mut f: F) {
        match self {
            Terminator::Goto(_, args) => {
                for arg in args.iter_mut() {
                    *arg = f(*arg);
                }
            }
            Terminator::Match(_, ref mut arms) => {
                for arm in arms.iter_mut() {
                    for arg in arm.args.iter_mut() {
                        *arg = f(*arg);
                    }
                }
            }
        }
    }
}

impl<F> Update<F, BlockId> for Terminator
where
    F: FnMut(BlockId) -> BlockId,
{
    fn update(&mut self, mut f: F) {
        match self {
            Terminator::Goto(Some(target), _) => *target = f(*target),
            Terminator::Goto(_, _) => (),
            Terminator::Match(_, ref mut arms) => {
                for arm in arms.iter_mut() {
                    if let Some(ref mut target) = arm.target {
                        *target = f(*target)
                    }
                }
            }
        }
    }
}

/// # Warning
///
/// This implementation depends on the fact that all current
/// implementations of `Reads` and `Writes` first read and
/// then write.
///
/// Implementing `Reads` and `Writes` for `Block` would therefore
/// be incorrect.
impl<F, T> Update<F, LocationId> for T
where
    F: FnMut(LocationId) -> LocationId,
    for<'b, 'c> &'b mut T: Reads<&'c mut F> + Writes<&'c mut F>,
{
    fn update(&mut self, mut f: F) {
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