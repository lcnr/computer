use shared_id::LocationId;

use crate::{traits::Writes, Action, Terminator};

impl<'a, F> Writes<F> for &'a Action
where
    F: FnMut(LocationId),
{
    fn writes(self, mut f: F) {
        match self {
            Action::Invert(_, o)
            | Action::Move(_, o)
            | Action::Binop { out: o, .. }
            | Action::LoadConstant(_, o) => {
                f(*o);
            }
            Action::Debug(_) => {}
            Action::FunctionCall { ret, .. } => {
                for v in ret.iter().filter_map(Option::as_ref) {
                    f(*v);
                }
            }
        }
    }
}

impl<'a, F> Writes<F> for &'a mut Action
where
    F: FnMut(LocationId) -> LocationId,
{
    fn writes(self, mut f: F) {
        match self {
            Action::Invert(_, o)
            | Action::Move(_, o)
            | Action::Binop { out: o, .. }
            | Action::LoadConstant(_, o) => {
                *o = f(*o);
            }
            Action::Debug(_) => {}
            Action::FunctionCall { ret, .. } => {
                for v in ret.iter_mut().filter_map(Option::as_mut) {
                    *v = f(*v);
                }
            }
        }
    }
}

impl<'a, F> Writes<F> for &'a Terminator
where
    F: FnMut(LocationId),
{
    fn writes(self, _: F) {}
}

impl<'a, F> Writes<F> for &'a mut Terminator
where
    F: FnMut(LocationId) -> LocationId,
{
    fn writes(self, _: F) {}
}
