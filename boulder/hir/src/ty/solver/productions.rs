use diagnostics::CompileError;

use crate::ty::{
    self,
    solver::{Context, TypeSolver},
    TypeId,
};

use solver::{Entity, Production};

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

impl<'a, 'b> Production<Context<'a, 'b>, TypeId, CompileError> for FieldAccess {
    fn resolve(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        object: Entity<TypeId>,
        field: Entity<TypeId>,
    ) -> Result<(), CompileError> {
        let object_ty = object.content[0];
        if let Some(pos) = self.field_types.iter().position(|&(o, _)| object_ty == o) {
            let ty = self.field_types[pos].1;
            
            if field.content.contains(&ty) {
                field.content.clear();
                field.content.push(ty);
                Ok(())
            } else {
                let found_str = TypeSolver::ty_error_str(ctx.types, &field.content);
                let expected_str = TypeSolver::ty_error_str(ctx.types, &[ty]);
                CompileError::new(
                    ctx.meta.get(&field.id).unwrap(),
                    format_args!(
                        "Mismatched types: found {}, expected {}",
                        found_str, expected_str
                    ),
                )
            }
        } else {
            CompileError::new(
                ctx.meta.get(&field.id).unwrap(),
                format_args!(
                    "No field `{}` on type `{}`",
                    self.field_name, ctx.types[object_ty.0].name.item,
                ),
            )
        }
    }

    fn resolve_backwards(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        object: Entity<TypeId>,
        field: Entity<TypeId>,
    ) -> Result<(), CompileError> {
        let field_ty = field.content[0];
        if ctx.bounds.contains_key(&field.id) {
            let possible_structs: Vec<_> = self
                .field_types
                .iter()
                .filter(|&&(_, f)| f == field_ty)
                .map(|&(o, _)| o)
                .collect();
            if !possible_structs.is_empty() {
                let object_types: Vec<_> = possible_structs
                    .into_iter()
                    .filter(|t| object.content.contains(t))
                    .collect();
                if !object_types.is_empty() {
                    *object.content = object_types;
                    Ok(())
                } else {
                    let found_str = TypeSolver::ty_error_str(ctx.types, object.content);
                    let expected_str = TypeSolver::ty_error_str(ctx.types, &object_types);
                    CompileError::new(
                        ctx.meta.get(&field.id).unwrap(),
                        format_args!(
                            "Mismatched types: found {}, expected {}",
                            found_str, expected_str
                        ),
                    )
                }
            } else {
                CompileError::new(
                    ctx.meta.get(&field.id).unwrap(),
                    format_args!(
                        "No field `{}` with type `{}`",
                        self.field_name, ctx.types[field_ty.0].name.item,
                    ),
                )
            }
        } else {
            Ok(())
        }
    }
}

#[derive(Debug)]
pub struct Equality;

impl<'a, 'b> Production<Context<'a, 'b>, TypeId, CompileError> for Equality {
    fn resolve(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: Entity<TypeId>,
        target: Entity<TypeId>,
    ) -> Result<(), CompileError> {
        let origin_ty = origin.content[0];
        if ctx.bounds.contains_key(&target.id) || ctx.bounds.contains_key(&origin.id) {
            ctx.bounds.insert(target.id, vec![origin_ty]);
            ctx.bounds.insert(origin.id, vec![origin_ty]);

            // target must exactly match origin
            if target.content.contains(&origin_ty) {
                target.content.clear();
                target.content.push(origin_ty);
                Ok(())
            } else {
                let found_str = TypeSolver::ty_error_str(ctx.types, target.content);
                let expected_str = TypeSolver::ty_error_str(ctx.types, &[origin_ty]);
                CompileError::build(
                    ctx.meta.get(&target.id).unwrap(),
                    format_args!(
                        "Mismatched types: found {}, expected {}",
                        found_str, expected_str
                    ),
                )
                .with_location(&ctx.meta.get(&origin.id).unwrap())
                .build()
            }
        } else {
            let types = target
                .content
                .iter()
                .map(|t| ty::build_sum_ty(ctx.types, ctx.type_lookup, &[origin_ty, *t]))
                .collect::<Result<_, _>>()?;

            target.content.clone_from(&types);
            *origin.content = types;
            Ok(())
        }
    }

    fn resolve_backwards(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: Entity<TypeId>,
        target: Entity<TypeId>,
    ) -> Result<(), CompileError> {
        let target_ty = target.content[0];
        if ctx.bounds.contains_key(&target.id) || ctx.bounds.contains_key(&origin.id) {
            ctx.bounds.insert(target.id, vec![target_ty]);
            ctx.bounds.insert(origin.id, vec![target_ty]);

            // target must exactly match origin
            if origin.content.contains(&target_ty) {
                origin.content.clear();
                origin.content.push(target_ty);
                Ok(())
            } else {
                let found_str = TypeSolver::ty_error_str(ctx.types, &[target_ty]);
                let expected_str = TypeSolver::ty_error_str(ctx.types, origin.content);
                CompileError::build(
                    ctx.meta.get(&target.id).unwrap(),
                    format_args!(
                        "Mismatched types: found {}, expected {}",
                        found_str, expected_str
                    ),
                )
                .with_location(&ctx.meta.get(&origin.id).unwrap())
                .build()
            }
        } else {
            let types = origin
                .content
                .iter()
                .map(|t| ty::build_sum_ty(ctx.types, ctx.type_lookup, &[target_ty, *t]))
                .collect::<Result<_, _>>()?;

            origin.content.clone_from(&types);
            *target.content = types;

            Ok(())
        }
    }
}

#[derive(Debug)]
pub struct Extension;

impl<'a, 'b> Production<Context<'a, 'b>, TypeId, CompileError> for Extension {
    fn resolve(
        &mut self,
        _ctx: &mut Context<'a, 'b>,
        _origin: Entity<TypeId>,
        _target: Entity<TypeId>,
    ) -> Result<(), CompileError> {
        Ok(())
    }

    fn resolve_backwards(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: Entity<TypeId>,
        target: Entity<TypeId>,
    ) -> Result<(), CompileError> {
        let target_ty = target.content[0];
        if ctx.bounds.contains_key(&target.id) {
            // target must exactly match origin
            let bound = ctx.add_bound(origin.id, ty::subtypes(target_ty, ctx.types));
            let possible_origins = origin
                .content
                .iter()
                .filter(|t| bound.contains(t))
                .copied()
                .collect::<Vec<_>>();

            if !possible_origins.is_empty() {
                *origin.content = possible_origins;
                Ok(())
            } else {
                let found_str = TypeSolver::ty_error_str(ctx.types, target.content);
                let expected_str = TypeSolver::ty_error_str(ctx.types, origin.content);
                CompileError::build(
                    ctx.meta.get(&target.id).unwrap(),
                    format_args!(
                        "Mismatched types: found {}, expected {}",
                        found_str, expected_str
                    ),
                )
                .with_location(&ctx.meta.get(&origin.id).unwrap())
                .build()
            }
        } else {
            Ok(())
        }
    }
}
