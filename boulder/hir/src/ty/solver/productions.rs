use std::collections::HashMap;

use diagnostics::{CompileError, Meta};

use crate::ty::{
    solver::{Context, TypeSolver},
    Type, TypeId,
};

use solver::{Entity, EntityId, Production};

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
    }
}

pub struct Extension;

impl<'a, 'b> Production<Context<'a, 'b>, TypeId, CompileError> for Extension {
    fn resolve(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: Entity<TypeId>,
        target: Entity<TypeId>,
    ) -> Result<(), CompileError> {
        let origin_ty = origin.content[0];
        if ctx.bound.contains(&target.id) {
            // target must exactly match origin
            if target.content.contains(&origin_ty) {
                target.content.clear();
                target.content.push(origin_ty);
                Ok(())
            } else {
                let found_str = TypeSolver::ty_error_str(ctx.types, target.content);
                let expected_str = TypeSolver::ty_error_str(ctx.types, &[origin_ty]);
                CompileError::new(
                    ctx.meta.get(&target.id).unwrap(),
                    format_args!(
                        "Mismatched types: found {}, expected {}",
                        found_str, expected_str
                    ),
                )
            }
        } else {
            unimplemented!()
        }
    }

    fn resolve_backwards(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: Entity<TypeId>,
        target: Entity<TypeId>,
    ) -> Result<(), CompileError> {
        unimplemented!()
    }
}
