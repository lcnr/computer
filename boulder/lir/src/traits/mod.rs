use shared_id::{BlockId, FunctionId, LocationId};

mod reads;
mod writes;

use crate::{Action, Arg, Block, Function, Terminator};

/// A trait used to indicate that all reads happen before the first write.
pub trait OrderedRW {}

impl OrderedRW for Terminator {}
impl OrderedRW for Action {}

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
    fn update(&mut self, _: F) {
        match self {
            Terminator::Goto(_) => {}
            Terminator::Match(_, _) => {}
        }
    }
}

impl<F> Update<F, BlockId> for Block
where
    F: FnMut(BlockId) -> BlockId,
{
    fn update(&mut self, f: F) {
        self.terminator.update(f);
    }
}

impl<F> Update<F, Option<BlockId>> for Block
where
    F: FnMut(Option<BlockId>) -> Option<BlockId>,
{
    fn update(&mut self, f: F) {
        self.terminator.update(f);
    }
}

impl<F> Update<F, BlockId> for Terminator
where
    F: FnMut(BlockId) -> BlockId,
{
    fn update(&mut self, mut f: F) {
        match self {
            Terminator::Goto(Some(target)) => *target = f(*target),
            Terminator::Goto(None) => {}
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

impl<F> Update<F, Option<BlockId>> for Terminator
where
    F: FnMut(Option<BlockId>) -> Option<BlockId>,
{
    fn update(&mut self, mut f: F) {
        match self {
            Terminator::Goto(target) => *target = f(*target),
            Terminator::Match(_, ref mut arms) => {
                for arm in arms.iter_mut() {
                    arm.target = f(arm.target)
                }
            }
        }
    }
}

impl<F, T> Update<F, LocationId> for T
where
    F: FnMut(LocationId) -> LocationId,
    for<'b, 'c> &'b mut T: Reads<&'c mut F> + Writes<&'c mut F>,
    T: OrderedRW,
{
    fn update(&mut self, mut f: F) {
        let f = &mut f;
        self.reads(f);
        self.writes(f);
    }
}

impl<F> Update<F, LocationId> for Block
where
    F: FnMut(LocationId) -> LocationId,
{
    fn update(&mut self, mut f: F) {
        for step in self.steps.iter_mut() {
            step.update(&mut f);
        }

        self.terminator.update(f);
    }
}

pub trait Reads<F> {
    fn reads(self, f: F);
}

pub trait Writes<F> {
    fn writes(self, f: F);
}
