use tindex::TSlice;

use shared_id::{InputId, LocationId};

use crate::{traits::Reads, Action, Arg, Terminator};

fn arg_locations<F>(mut f: F, args: &TSlice<InputId, Option<Arg>>)
where
    F: FnMut(LocationId),
{
    for arg in args.iter() {
        if let Some(Arg::Location(id)) = arg {
            f(*id);
        }
    }
}

fn arg_locations_mut<F>(mut f: F, args: &mut TSlice<InputId, Option<Arg>>)
where
    F: FnMut(LocationId) -> LocationId,
{
    for arg in args.iter_mut() {
        if let Some(Arg::Location(id)) = arg {
            *id = f(*id);
        }
    }
}

impl<'a, F> Reads<F> for &'a Action
where
    F: FnMut(LocationId),
{
    fn reads(self, mut f: F) {
        match self {
            Action::Invert(i, _) | Action::Move(i, _) => {
                f(*i);
            }
            Action::Debug(i) => f(*i),
            Action::LoadConstant(_, _) => {}
            Action::Binop { l, r, .. } => {
                if let Arg::Location(id) = l {
                    f(*id);
                }
                if let Arg::Location(id) = r {
                    f(*id);
                }
            }
            Action::FunctionCall { args, .. } => {
                arg_locations(&mut f, args);
            }
        }
    }
}

impl<'a, F> Reads<F> for &'a mut Action
where
    F: FnMut(LocationId) -> LocationId,
{
    fn reads(self, mut f: F) {
        match self {
            Action::Invert(i, _) | Action::Move(i, _) => {
                *i = f(*i);
            }
            Action::Debug(i) => *i = f(*i),
            Action::LoadConstant(_, _) => {}
            Action::Binop { l, r, .. } => {
                if let Arg::Location(id) = l {
                    *id = f(*id);
                }
                if let Arg::Location(id) = r {
                    *id = f(*id);
                }
            }
            Action::FunctionCall { args, .. } => {
                arg_locations_mut(&mut f, args);
            }
        }
    }
}

impl<'a, F> Reads<F> for &'a Terminator
where
    F: FnMut(LocationId),
{
    fn reads(self, mut f: F) {
        match self {
            Terminator::Goto(_, args) => {
                arg_locations(f, args);
            }
            Terminator::Match(expr, arms) => {
                f(*expr);

                for arm in arms.iter() {
                    arg_locations(&mut f, &arm.args);
                }
            }
        }
    }
}

impl<'a, F> Reads<F> for &'a mut Terminator
where
    F: FnMut(LocationId) -> LocationId,
{
    fn reads(self, mut f: F) {
        match self {
            Terminator::Goto(_, args) => {
                arg_locations_mut(f, args);
            }
            Terminator::Match(expr, arms) => {
                *expr = f(*expr);

                for arm in arms.iter_mut() {
                    arg_locations_mut(&mut f, &mut arm.args);
                }
            }
        }
    }
}
