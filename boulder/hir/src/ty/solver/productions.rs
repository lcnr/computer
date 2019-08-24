use std::collections::HashMap;

use diagnostics::{CompileError, Meta};

use crate::ty::{
    solver::{Context, TypeSolver},
    Type, TypeId,
};

use solver::{Entity, EntityId, Production, ResolvedEntity};

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
        object: ResolvedEntity<TypeId>,
        field: Entity<TypeId>,
    ) -> Result<(), CompileError> {
        if let Some(pos) = self.field_types.iter().position(|(o, _)| object.ty == o) {
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
                    self.field_name, ctx.types[object.ty.0].name.item,
                ),
            )
        }
    }

    fn resolve_backwards(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        object: Entity<TypeId>,
        field: ResolvedEntity<TypeId>,
    ) -> Result<(), CompileError> {
        let possible_structs: Vec<_> = self
            .field_types
            .iter()
            .filter(|&(_, f)| f == field.ty)
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
                    self.field_name, ctx.types[field.ty.0].name.item,
                ),
            )
        }
    }
}

pub struct Extention;

impl<'a, 'b> Production<Context<'a, 'b>, TypeId, CompileError> for Extention {
    fn resolve(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: ResolvedEntity<TypeId>,
        target: Entity<TypeId>,
    ) -> Result<(), CompileError> {
        unimplemented!()
    }

    fn resolve_backwards(
        &mut self,
        ctx: &mut Context<'a, 'b>,
        origin: Entity<TypeId>,
        target: ResolvedEntity<TypeId>,
    ) -> Result<(), CompileError> {
        unimplemented!()
    }
}
