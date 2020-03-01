use tindex::TSlice;

use shared_id::{InputId, LocationId};

use crate::{Action, Arg, Block, Terminator};

pub trait UpdateLocation<F>
where
    F: FnMut(LocationId) -> LocationId,
{
    fn update_locations(&mut self, f: F);
}

impl<F> UpdateLocation<F> for Block
where
    F: FnMut(LocationId) -> LocationId,
{
    fn update_locations(&mut self, mut f: F) {
        for input in self.inputs.iter_mut() {
            *input = f(*input);
        }

        for step in self.steps.iter_mut() {
            step.update_locations(&mut f);
        }

        self.terminator.update_locations(f);
    }
}

impl<F> UpdateLocation<F> for Action
where
    F: FnMut(LocationId) -> LocationId,
{
    fn update_locations(&mut self, mut f: F) {
        match self {
            Action::Invert(i, o) | Action::Move(i, o) => {
                *i = f(*i);
                *o = f(*o);
            }
            Action::Debug(i) => *i = f(*i),
            Action::LoadConstant(_, o) => *o = f(*o),
            Action::Binop { l, r, out, .. } => {
                if let Arg::Location(id) = l {
                    *id = f(*id);
                }
                if let Arg::Location(id) = r {
                    *id = f(*id);
                }
                *out = f(*out);
            }
            Action::FunctionCall { args, ret, .. } => {
                arg_locations(&mut f, args);

                for v in ret.iter_mut().filter_map(Option::as_mut) {
                    *v = f(*v);
                }
            }
        }
    }
}

impl<F> UpdateLocation<F> for Terminator
where
    F: FnMut(LocationId) -> LocationId,
{
    fn update_locations(&mut self, mut f: F) {
        match self {
            Terminator::Goto(_, args) => {
                arg_locations(f, args);
            }
            Terminator::Match(expr, arms) => {
                *expr = f(*expr);

                for arm in arms.iter_mut() {
                    arg_locations(&mut f, &mut arm.args);
                }
            }
        }
    }
}

fn arg_locations<F>(mut f: F, args: &mut TSlice<InputId, Option<Arg>>)
where
    F: FnMut(LocationId) -> LocationId,
{
    for arg in args.iter_mut() {
        if let Some(Arg::Location(id)) = arg {
            *id = f(*id);
        }
    }
}
