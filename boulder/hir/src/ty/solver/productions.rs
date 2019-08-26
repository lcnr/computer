use diagnostics::CompileError;

use crate::ty::{
    self,
    solver::{Context, EntityState, TypeSolver},
    Kind, TypeId,
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
        if let Some(value) =
            self.field_types
                .iter()
                .find_map(|&(o, f)| if o == origin.value { Some(f) } else { None })
        {
            match target.state {
                state @ EntityState::Unbound => {
                    *state = EntityState::Bound(vec![value]);
                    Ok(())
                }
                EntityState::Subtype(restriction) => {
                    if match &ctx.types[value.0].kind {
                        Kind::Sum(e) => e.iter().all(|t| restriction.contains(t)),
                        _ => restriction.contains(&value),
                    } {
                        *target.state = EntityState::Bound(vec![value]);
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
            let allowed_objects =
                EntityState::Bound(self.field_types.iter().map(|&(o, _)| o).collect());
            let expected_str = TypeSolver::ty_error_str(ctx.types, &allowed_objects);
            let found_str = &ctx.types[origin.value.0].name.item;
            CompileError::build(
                ctx.meta.get(&target.id).unwrap(),
                format_args!(
                    "Mismatched types: found `{}`, expected {}",
                    found_str, expected_str
                ),
            )
            .with_help(format_args!(
                "no field `{}` on type `{}`",
                self.field_name, found_str
            ))
            .build()
        }
    }

    fn resolve_backwards(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: Entity<EntityState>,
        target: SolvedEntity<EntityState>,
    ) -> Result<(), CompileError> {
        let values: Vec<_> = self
            .field_types
            .iter()
            .filter_map(|&(o, f)| if f == target.value { Some(o) } else { None })
            .collect();

        if !values.is_empty() {
            match origin.state {
                state @ EntityState::Unbound => {
                    *state = EntityState::Bound(values);
                    Ok(())
                }
                EntityState::Subtype(_) => Ok(()),
                EntityState::Bound(ref mut v) => {
                    let allowed_values: Vec<_> =
                        v.iter().copied().filter(|t| values.contains(&t)).collect();
                    if !allowed_values.is_empty() {
                        *v = allowed_values;
                        Ok(())
                    } else {
                        let allowed_objects =
                            EntityState::Bound(self.field_types.iter().map(|&(o, _)| o).collect());
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
            let allowed_targets =
                EntityState::Bound(self.field_types.iter().map(|&(_, f)| f).collect());
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
            .with_help(format_args!(
                "No struct has a field `{}` with type `{}`",
                self.field_name, found_str
            ))
            .build()
        }
    }
}

#[derive(Debug)]
pub struct Equality;

impl Equality {
    fn commutative_resolve<'a, 'b>(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        solved: SolvedEntity<EntityState>,
        unsolved: Entity<EntityState>,
        flip_error: bool,
    ) -> Result<(), CompileError> {
        match unsolved.state {
            state @ EntityState::Unbound => {
                *state = EntityState::Bound(vec![solved.value]);
                Ok(())
            }
            EntityState::Subtype(restriction) => match &ctx.types[solved.value.0].kind {
                Kind::Sum(v) => {
                    if v.iter().all(|t| restriction.contains(t)) {
                        *unsolved.state = EntityState::Bound(vec![solved.value]);
                        Ok(())
                    } else {
                        let unsolved_str = TypeSolver::ty_error_str(ctx.types, unsolved.state);
                        let solved_str = &ctx.types[solved.value.0].name.item;

                        let (found_str, expected_str) = if flip_error {
                            (solved_str.as_ref(), unsolved_str.as_ref())
                        } else {
                            (unsolved_str.as_ref(), solved_str.as_ref())
                        };

                        CompileError::build(
                            ctx.meta.get(&solved.id).unwrap(),
                            format_args!(
                                "Mismatched types: found `{}`, expected {}",
                                found_str, expected_str
                            ),
                        )
                        .with_location(&ctx.meta.get(&unsolved.id).unwrap())
                        .build()
                    }
                }
                _ => {
                    if restriction.contains(&solved.value) {
                        *unsolved.state = EntityState::Bound(vec![solved.value]);
                        Ok(())
                    } else {
                        let unsolved_str = TypeSolver::ty_error_str(ctx.types, unsolved.state);
                        let solved_str = &ctx.types[solved.value.0].name.item;

                        let (found_str, expected_str) = if flip_error {
                            (solved_str.as_ref(), unsolved_str.as_ref())
                        } else {
                            (unsolved_str.as_ref(), solved_str.as_ref())
                        };
                        CompileError::build(
                            ctx.meta.get(&solved.id).unwrap(),
                            format_args!(
                                "Mismatched types: found `{}`, expected {}",
                                found_str, expected_str
                            ),
                        )
                        .with_location(&ctx.meta.get(&unsolved.id).unwrap())
                        .build()
                    }
                }
            },
            EntityState::Bound(ref mut v) => {
                let allowed_values: Vec<_> =
                    v.iter().copied().filter(|&t| t == solved.value).collect();
                if !allowed_values.is_empty() {
                    *v = allowed_values;
                    Ok(())
                } else {
                    let unsolved_str = TypeSolver::ty_error_str(ctx.types, unsolved.state);
                    let solved_str = &ctx.types[solved.value.0].name.item;

                    let (found_str, expected_str) = if flip_error {
                        (solved_str.as_ref(), unsolved_str.as_ref())
                    } else {
                        (unsolved_str.as_ref(), solved_str.as_ref())
                    };
                    CompileError::build(
                        ctx.meta.get(&solved.id).unwrap(),
                        format_args!(
                            "Mismatched types: found `{}`, expected {}",
                            found_str, expected_str
                        ),
                    )
                    .with_location(&ctx.meta.get(&unsolved.id).unwrap())
                    .build()
                }
            }
        }
    }
}

impl<'a, 'b> Production<Context<'a, 'b>, EntityState, CompileError> for Equality {
    fn resolve(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: SolvedEntity<EntityState>,
        target: Entity<EntityState>,
    ) -> Result<(), CompileError> {
        self.commutative_resolve(ctx, origin, target, false)
    }

    fn resolve_backwards(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: Entity<EntityState>,
        target: SolvedEntity<EntityState>,
    ) -> Result<(), CompileError> {
        self.commutative_resolve(ctx, target, origin, true)
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
            EntityState::Unbound | EntityState::Subtype(_) => Ok(()),
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
            state @ EntityState::Unbound => match &ctx.types[target.value.0].kind {
                Kind::Sum(v) => {
                    *state = EntityState::Subtype(v.clone());
                    Ok(())
                }
                _ => {
                    *state = EntityState::Bound(vec![target.value]);
                    Ok(())
                }
            },
            EntityState::Subtype(restriction) => match &ctx.types[target.value.0].kind {
                Kind::Sum(v) => {
                    let updated: Vec<TypeId> = restriction
                        .iter()
                        .filter(|r| v.contains(r))
                        .copied()
                        .collect();
                    if !updated.is_empty() {
                        if updated.len() == 1 {
                            *origin.state = EntityState::Bound(updated);
                        } else {
                            *restriction = updated;
                        }
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
                _ => {
                    if restriction.contains(&target.value) {
                        *origin.state = EntityState::Bound(vec![target.value]);
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
