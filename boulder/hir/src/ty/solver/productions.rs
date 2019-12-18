use std::iter;

use shared_id::{TypeId, U16_BYTES_TYPE_ID, U16_TYPE_ID, U32_BYTES_TYPE_ID, U32_TYPE_ID};

use solver::{Entity, Production, SolvedEntity};

use diagnostics::CompileError;

use crate::ty::{
    self,
    solver::{Context, EntityState, TypeSolver},
};

#[derive(Debug)]
pub struct FieldAccess<'a> {
    field_name: &'a str,
    field_types: Vec<(TypeId, TypeId)>,
}

impl<'a> FieldAccess<'a> {
    pub fn new(field_name: &'a str, field_types: Vec<(TypeId, TypeId)>) -> Self {
        Self {
            field_name,
            field_types,
        }
    }
}

impl<'a, 'b> Production<Context<'a, 'b>, EntityState, CompileError> for FieldAccess<'a> {
    fn resolve(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: SolvedEntity<EntityState>,
        target: Entity<EntityState>,
    ) -> Result<(), CompileError> {
        #[cfg(feature = "profiler")]
        profile_scope!("FieldAccess::new");
        if let Some(value) =
            self.field_types
                .iter()
                .find_map(|&(o, f)| if o == origin.value { Some(f) } else { None })
        {
            if target
                .state
                .try_bind(iter::once(value).collect(), ctx.types)
            {
                Ok(())
            } else {
                let expected_str = TypeSolver::ty_error_str(
                    ctx.types,
                    &EntityState::Bound(iter::once(value).collect()),
                );
                let found_str = TypeSolver::ty_error_str(ctx.types, &target.state);
                CompileError::build(
                    &ctx.meta[target.id],
                    format_args!(
                        "Mismatched types: found `{}`, expected {}",
                        found_str, expected_str
                    ),
                )
                .with_location(&ctx.meta[origin.id])
                .build()
            }
        } else {
            let allowed_objects =
                EntityState::Bound(self.field_types.iter().map(|&(o, _)| o).collect());
            let expected_str = TypeSolver::ty_error_str(ctx.types, &allowed_objects);
            let found_str = &ctx.types[origin.value].name.item;
            CompileError::build(
                &ctx.meta[target.id],
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
        _ctx: &mut Context<'a, 'b>,
        _origin: Entity<EntityState>,
        _target: SolvedEntity<EntityState>,
    ) -> Result<(), CompileError> {
        Ok(())
    }
}

#[derive(Debug)]
pub struct ToBytes;

impl<'a, 'b> Production<Context<'a, 'b>, EntityState, CompileError> for ToBytes {
    fn resolve(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: SolvedEntity<EntityState>,
        target: Entity<EntityState>,
    ) -> Result<(), CompileError> {
        #[cfg(feature = "profiler")]
        profile_scope!("ToBytes::resolve");
        match origin.value {
            U16_TYPE_ID => {
                if target
                    .state
                    .try_bind(iter::once(U16_BYTES_TYPE_ID).collect(), ctx.types)
                {
                    Ok(())
                } else {
                    let found_str = TypeSolver::ty_error_str(ctx.types, target.state);
                    CompileError::build(
                        &ctx.meta[target.id],
                        format_args!(
                            "Mismatched types: found `{}`, expected `u16Bytes`",
                            found_str,
                        ),
                    )
                    .with_location(&ctx.meta[origin.id])
                    .build()
                }
            }
            U32_TYPE_ID => {
                if target
                    .state
                    .try_bind(iter::once(U32_BYTES_TYPE_ID).collect(), ctx.types)
                {
                    Ok(())
                } else {
                    let found_str = TypeSolver::ty_error_str(ctx.types, target.state);
                    CompileError::build(
                        &ctx.meta[target.id],
                        format_args!(
                            "Mismatched types: found `{}`, expected `u32Bytes`",
                            found_str
                        ),
                    )
                    .with_location(&ctx.meta[origin.id])
                    .build()
                }
            }
            _ => CompileError::build(
                &ctx.meta[target.id],
                format_args!(
                    "Mismatched types: found `{}`, expected `u16` or `u32`",
                    ctx.types[origin.value].name.item
                ),
            )
            .with_location(&ctx.meta[origin.id])
            .build(),
        }
    }

    fn resolve_backwards(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: Entity<EntityState>,
        target: SolvedEntity<EntityState>,
    ) -> Result<(), CompileError> {
        #[cfg(feature = "profiler")]
        profile_scope!("ToBytes::resolve_backwards");
        match target.value {
            U16_BYTES_TYPE_ID => {
                if origin
                    .state
                    .try_bind(iter::once(U16_TYPE_ID).collect(), ctx.types)
                {
                    Ok(())
                } else {
                    let found_str = TypeSolver::ty_error_str(ctx.types, origin.state);
                    CompileError::build(
                        &ctx.meta[origin.id],
                        format_args!("Mismatched types: found `{}`, expected `u16`", found_str,),
                    )
                    .with_location(&ctx.meta[target.id])
                    .build()
                }
            }
            U32_BYTES_TYPE_ID => {
                if origin
                    .state
                    .try_bind(iter::once(U32_TYPE_ID).collect(), ctx.types)
                {
                    Ok(())
                } else {
                    let found_str = TypeSolver::ty_error_str(ctx.types, origin.state);
                    CompileError::build(
                        &ctx.meta[origin.id],
                        format_args!("Mismatched types: found `{}`, expected `u32`", found_str),
                    )
                    .with_location(&ctx.meta[target.id])
                    .build()
                }
            }
            _ => CompileError::build(
                &ctx.meta[origin.id],
                format_args!(
                    "Mismatched types: found `{}`, expected `u16Bytes` or `u32Bytes`",
                    ctx.types[target.value].name.item
                ),
            )
            .with_location(&ctx.meta[target.id])
            .build(),
        }
    }
}

#[derive(Debug)]
pub struct FromBytes;

impl<'a, 'b> Production<Context<'a, 'b>, EntityState, CompileError> for FromBytes {
    fn resolve(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: SolvedEntity<EntityState>,
        target: Entity<EntityState>,
    ) -> Result<(), CompileError> {
        #[cfg(feature = "profiler")]
        profile_scope!("FromBytes::resolve");
        match origin.value {
            U16_BYTES_TYPE_ID => {
                if target
                    .state
                    .try_bind(iter::once(U16_TYPE_ID).collect(), ctx.types)
                {
                    Ok(())
                } else {
                    let found_str = TypeSolver::ty_error_str(ctx.types, target.state);
                    CompileError::build(
                        &ctx.meta[target.id],
                        format_args!(
                            "Mismatched types: found `{}`, expected `u16Bytes`",
                            found_str,
                        ),
                    )
                    .with_location(&ctx.meta[origin.id])
                    .build()
                }
            }
            U32_BYTES_TYPE_ID => {
                if target
                    .state
                    .try_bind(iter::once(U32_TYPE_ID).collect(), ctx.types)
                {
                    Ok(())
                } else {
                    let found_str = TypeSolver::ty_error_str(ctx.types, target.state);
                    CompileError::build(
                        &ctx.meta[target.id],
                        format_args!(
                            "Mismatched types: found `{}`, expected `u32Bytes`",
                            found_str
                        ),
                    )
                    .with_location(&ctx.meta[origin.id])
                    .build()
                }
            }
            _ => CompileError::build(
                &ctx.meta[target.id],
                format_args!(
                    "Mismatched types: found `{}`, expected `u16` or `u32`",
                    ctx.types[origin.value].name.item
                ),
            )
            .with_location(&ctx.meta[origin.id])
            .build(),
        }
    }

    fn resolve_backwards(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: Entity<EntityState>,
        target: SolvedEntity<EntityState>,
    ) -> Result<(), CompileError> {
        #[cfg(feature = "profiler")]
        profile_scope!("FromBytes::resolve_backwards");
        match target.value {
            U16_TYPE_ID => {
                if origin
                    .state
                    .try_bind(iter::once(U16_BYTES_TYPE_ID).collect(), ctx.types)
                {
                    Ok(())
                } else {
                    let found_str = TypeSolver::ty_error_str(ctx.types, origin.state);
                    CompileError::build(
                        &ctx.meta[origin.id],
                        format_args!("Mismatched types: found `{}`, expected `u16`", found_str,),
                    )
                    .with_location(&ctx.meta[target.id])
                    .build()
                }
            }
            U32_TYPE_ID => {
                if origin
                    .state
                    .try_bind(iter::once(U32_BYTES_TYPE_ID).collect(), ctx.types)
                {
                    Ok(())
                } else {
                    let found_str = TypeSolver::ty_error_str(ctx.types, origin.state);
                    CompileError::build(
                        &ctx.meta[origin.id],
                        format_args!("Mismatched types: found `{}`, expected `u32`", found_str),
                    )
                    .with_location(&ctx.meta[target.id])
                    .build()
                }
            }
            _ => CompileError::build(
                &ctx.meta[origin.id],
                format_args!(
                    "Mismatched types: found `{}`, expected `u16Bytes` or `u32Bytes`",
                    ctx.types[target.value].name.item
                ),
            )
            .with_location(&ctx.meta[target.id])
            .build(),
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
        #[cfg(feature = "profiler")]
        profile_scope!("Equality::commutative_resolve");
        if unsolved
            .state
            .try_bind(iter::once(solved.value).collect(), ctx.types)
        {
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
                &ctx.meta[solved.id],
                format_args!(
                    "Mismatched types: found {}, expected {}",
                    found_str, expected_str
                ),
            )
            .with_location(&ctx.meta[unsolved.id])
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
        #[cfg(feature = "profiler")]
        profile_scope!("Extension::resolve");
        if target.state.try_subtype(
            ty::subtypes(origin.value, ctx.types),
            ctx.types,
            ctx.modules,
        ) {
            Ok(())
        } else {
            let expected_str = TypeSolver::ty_error_str(ctx.types, target.state);
            let found_str = &ctx.types[origin.value].name.item;
            CompileError::build(
                &ctx.meta[target.id],
                format_args!(
                    "Mismatched types: found `{}`, expected {}",
                    found_str, expected_str
                ),
            )
            .with_location(&ctx.meta[origin.id])
            .build()
        }
    }

    fn resolve_backwards(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: Entity<EntityState>,
        target: SolvedEntity<EntityState>,
    ) -> Result<(), CompileError> {
        #[cfg(feature = "profiler")]
        profile_scope!("Extension::resolve_backwards");
        if origin.state.try_supertype(
            ty::subtypes(target.value, ctx.types),
            ctx.types,
            ctx.modules,
        ) {
            Ok(())
        } else {
            let expected_str = TypeSolver::ty_error_str(ctx.types, origin.state);
            let found_str = &ctx.types[target.value].name.item;
            CompileError::build(
                &ctx.meta[target.id],
                format_args!(
                    "Mismatched types: found `{}`, expected {}",
                    found_str, expected_str
                ),
            )
            .with_location(&ctx.meta[origin.id])
            .build()
        }
    }
}
