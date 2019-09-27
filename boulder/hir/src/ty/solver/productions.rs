use diagnostics::CompileError;

use crate::ty::{
    self,
    solver::{Context, EntityState, TypeSolver},
    TypeId,
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
            if target.state.try_bind(vec![value], ctx.types) {
                Ok(())
            } else {
                let expected_str =
                    TypeSolver::ty_error_str(ctx.types, &EntityState::Bound(vec![value]));
                let found_str = TypeSolver::ty_error_str(ctx.types, &target.state);
                CompileError::build(
                    ctx.meta.get(&target.id).unwrap(),
                    format_args!(
                        "Mismatched types: found `{}`, expected {}",
                        found_str, expected_str
                    ),
                )
                .with_location(ctx.meta.get(&origin.id).unwrap())
                .build()
            }
        } else {
            let allowed_objects =
                EntityState::Bound(self.field_types.iter().map(|&(o, _)| o).collect());
            let expected_str = TypeSolver::ty_error_str(ctx.types, &allowed_objects);
            let found_str = &ctx.types[origin.value].name.item;
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
            if origin.state.try_bind(values, ctx.types) {
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
        } else {
            let allowed_targets =
                EntityState::Bound(self.field_types.iter().map(|&(_, f)| f).collect());
            let expected_str = TypeSolver::ty_error_str(ctx.types, &allowed_targets);
            let found_str = &ctx.types[target.value].name.item;
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
        if unsolved.state.try_bind(vec![solved.value], ctx.types) {
            Ok(())
        } else {
            let unsolved_str = TypeSolver::ty_error_str(ctx.types, unsolved.state);
            let solved_str = format!("`{}`", &ctx.types[solved.value].name.item);

            let (found_str, expected_str) = if flip_error {
                (solved_str, unsolved_str)
            } else {
                (unsolved_str, solved_str)
            };
            CompileError::build(
                ctx.meta.get(&solved.id).unwrap(),
                format_args!(
                    "Mismatched types: found {}, expected {}",
                    found_str, expected_str
                ),
            )
            .with_location(&ctx.meta.get(&unsolved.id).unwrap())
            .build()
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
        if target.state.try_subtype(
            ty::subtypes(origin.value, ctx.types),
            ctx.types,
            ctx.type_lookup,
        ) {
            Ok(())
        } else {
            let expected_str = TypeSolver::ty_error_str(ctx.types, target.state);
            let found_str = &ctx.types[origin.value].name.item;
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

    fn resolve_backwards(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: Entity<EntityState>,
        target: SolvedEntity<EntityState>,
    ) -> Result<(), CompileError> {
        if origin.state.try_supertype(
            ty::subtypes(target.value, ctx.types),
            ctx.types,
            ctx.type_lookup,
        ) {
            Ok(())
        } else {
            let expected_str = TypeSolver::ty_error_str(ctx.types, origin.state);
            let found_str = &ctx.types[target.value].name.item;
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
