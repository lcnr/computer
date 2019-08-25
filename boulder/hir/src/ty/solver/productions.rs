use diagnostics::CompileError;

use crate::ty::{
    self,
    solver::{Context, EntityState, TypeSolver},
    TypeId, Kind
};

use solver::{Entity, Production, SolvedEntity};

#[derive(Debug)]
pub struct FieldAccess {
    field_name: Box<str>,
    field_types: Vec<(TypeId, TypeId)>,
}

impl FieldAccess {
    pub fn new(field_name: Box<str>, field_types: Vec<(TypeId, TypeId)>) -> Self {
        Self {
            field_name,
            field_types,
        }
    }
}

impl<'a, 'b> Production<Context<'a, 'b>, EntityState, CompileError> for FieldAccess {
    fn resolve(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: SolvedEntity<EntityState>,
        target: Entity<EntityState>,
    ) -> Result<(), CompileError> {
        if let Some(value) = self.field_types.iter().find_map(|&(o, f)| if o == origin.value {
            Some(f)
        } else {
            None
        }) {
            match target.state {
                state @ EntityState::Unbound => {
                    *state = EntityState::Bound(vec![value]);
                    Ok(())
                }
                EntityState::Bound(ref mut v) => {
                    if v.contains(&value) {
                        *v = vec![value];
                        Ok(())
                    } else {
                        let found_str = TypeSolver::ty_error_str(ctx.types, target.state);
                        let expected_str = &ctx.types[value.0].name.item;
                        CompileError::new(
                            ctx.meta.get(&target.id).unwrap(),
                            format_args!(
                                "Mismatched types: found {}, expected `{}`",
                                found_str, expected_str
                            ),
                        )
                    }
                }
            }
        } else {
            let allowed_objects = EntityState::Bound(self.field_types.iter().map(|&(o, _)| o).collect());
            let expected_str = TypeSolver::ty_error_str(ctx.types, &allowed_objects);
            let found_str = &ctx.types[origin.value.0].name.item;
            CompileError::build(
                ctx.meta.get(&target.id).unwrap(),
                format_args!(
                    "Mismatched types: found `{}`, expected {}",
                    found_str, expected_str
                ),
            ).with_help(format_args!("no field `{}` on type `{}`", self.field_name, found_str)).build()
        }
    }

    fn resolve_backwards(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: Entity<EntityState>,
        target: SolvedEntity<EntityState>,
    ) -> Result<(), CompileError> {
        let values: Vec<_> = self.field_types.iter().filter_map(|&(o, f)| if f == target.value {
            Some(o)
        } else {
            None
        }).collect();

        if !values.is_empty() {
            match origin.state {
                state @ EntityState::Unbound => {
                    *state = EntityState::Bound(values);
                    Ok(())
                }
                EntityState::Bound(ref mut v) => {
                    let allowed_values: Vec<_> =
                        v.iter().copied().filter(|t| values.contains(&t)).collect();
                    if !allowed_values.is_empty() {
                        *v = allowed_values;
                        Ok(())
                    } else {
                        let allowed_objects = EntityState::Bound(self.field_types.iter().map(|&(o, _)| o).collect());
                        let found_str = TypeSolver::ty_error_str(ctx.types, origin.state);
                        let expected_str = TypeSolver::ty_error_str(ctx.types, &allowed_objects);
                        CompileError::new(
                            ctx.meta.get(&target.id).unwrap(),
                            format_args!(
                                "Mismatched types: found {}, expected {}",
                                found_str, expected_str
                            ),
                        )
                    }
                }
            }
        } else {
            let allowed_targets = EntityState::Bound(self.field_types.iter().map(|&(_, f)| f).collect());
            let expected_str = TypeSolver::ty_error_str(ctx.types, &allowed_targets);
            let found_str = &ctx.types[target.value.0].name.item;
            CompileError::build(
                ctx.meta.get(&target.id).unwrap(),
                format_args!(
                    "Mismatched types: found `{}`, expected {}",
                    found_str, expected_str
                ),
            )
            .with_location(&ctx.meta.get(&origin.id).unwrap())
            .with_help(format_args!("No struct has a field `{}` with type `{}`", self.field_name, found_str))
            .build()
        }
    }
}

#[derive(Debug)]
pub struct Equality;

impl<'a, 'b> Production<Context<'a, 'b>, EntityState, CompileError> for Equality {
    fn resolve(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: SolvedEntity<EntityState>,
        target: Entity<EntityState>,
    ) -> Result<(), CompileError> {
        match target.state {
            state @ EntityState::Unbound => {
                *state = EntityState::Bound(vec![origin.value]);
                Ok(())
            }
            EntityState::Bound(ref mut v) => {
                let allowed_values: Vec<_> =
                    v.iter().copied().filter(|&t| t == origin.value).collect();
                if !allowed_values.is_empty() {
                    *v = allowed_values;
                    Ok(())
                } else {
                    let found_str = TypeSolver::ty_error_str(ctx.types, target.state);
                    let expected_str = &ctx.types[origin.value.0].name.item;
                    CompileError::build(
                        ctx.meta.get(&target.id).unwrap(),
                        format_args!(
                            "Mismatched types: found {}, expected `{}`",
                            found_str, expected_str
                        ),
                    )
                    .with_location(&ctx.meta.get(&origin.id).unwrap())
                    .build()
                }
            }
        }
    }

    fn resolve_backwards(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: Entity<EntityState>,
        target: SolvedEntity<EntityState>,
    ) -> Result<(), CompileError> {
        match origin.state {
            state @ EntityState::Unbound => {
                *state = EntityState::Bound(vec![target.value]);
                Ok(())
            }
            EntityState::Bound(ref mut v) => {
                let allowed_values: Vec<_> =
                    v.iter().copied().filter(|&t| t == target.value).collect();
                if !allowed_values.is_empty() {
                    *v = allowed_values;
                    Ok(())
                } else {
                    let expected_str = TypeSolver::ty_error_str(ctx.types, origin.state);
                    let found_str = &ctx.types[target.value.0].name.item;
                    CompileError::build(
                        ctx.meta.get(&target.id).unwrap(),
                        format_args!(
                            "Mismatched types: found `{}`, expected {}",
                            found_str, expected_str
                        ),
                    )
                    .with_location(&ctx.meta.get(&origin.id).unwrap())
                    .build()
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct Extension;

impl<'a, 'b> Production<Context<'a, 'b>, EntityState, CompileError> for Extension {
    fn resolve(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: SolvedEntity<EntityState>,
        target: Entity<EntityState>,
    ) -> Result<(), CompileError> {
        match target.state {
            EntityState::Unbound => Ok(()),
            EntityState::Bound(v) => {
                let allowed_values: Vec<_> = v
                    .iter()
                    .copied()
                    .filter(|&t| ty::is_subtype(origin.value, t, ctx.types))
                    .collect();
                if !allowed_values.is_empty() {
                    *v = allowed_values;
                    Ok(())
                } else {
                    let expected_str = TypeSolver::ty_error_str(ctx.types, target.state);
                    let found_str = &ctx.types[origin.value.0].name.item;
                    CompileError::build(
                        ctx.meta.get(&target.id).unwrap(),
                        format_args!(
                            "Mismatched types: found `{}`, expected {}",
                            found_str, expected_str
                        ),
                    )
                    .with_location(&ctx.meta.get(&origin.id).unwrap())
                    .build()
                }
            }
        }
    }

    fn resolve_backwards(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: Entity<EntityState>,
        target: SolvedEntity<EntityState>,
    ) -> Result<(), CompileError> {
        match origin.state {
            state @ EntityState::Unbound => {
                match ctx.types[target.value.0].kind {
                    Kind::Sum(_) => Ok(()),
                    _ => {
                        *state = EntityState::Bound(vec![target.value]);
                        Ok(())
                    }
                }
            },
            EntityState::Bound(v) => {
                let allowed_values: Vec<_> = v
                    .iter()
                    .copied()
                    .filter(|&t| ty::is_subtype(t, target.value, ctx.types))
                    .collect();
                if !allowed_values.is_empty() {
                    *v = allowed_values;
                    Ok(())
                } else {
                    let expected_str = TypeSolver::ty_error_str(ctx.types, origin.state);
                    let found_str = &ctx.types[target.value.0].name.item;
                    CompileError::build(
                        ctx.meta.get(&target.id).unwrap(),
                        format_args!(
                            "Mismatched types: found `{}`, expected {}",
                            found_str, expected_str
                        ),
                    )
                    .with_location(&ctx.meta.get(&origin.id).unwrap())
                    .build()
                }
            }
        }
    }
}
