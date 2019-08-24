use std::collections::HashMap;

use diagnostics::{CompileError, Meta};

use crate::ty::{solver::{TypeSolver}, Type, TypeId};

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

impl<'a, 'b> Production<(&'b mut Vec<Type<'a, TypeId>>,
            HashMap<EntityId, Meta<'a, ()>>), TypeId, CompileError> for FieldAccess {
    fn resolve(
        &mut self,
        (types, ref meta): &mut (
            &'b mut Vec<Type<'a, TypeId>>,
            HashMap<EntityId, Meta<'a, ()>>,
        ),
        Entity {
            id: _obj_id,
            content: obj,
        }: Entity<TypeId>,
        Entity {
            id: field_id,
            content: field,
        }: Entity<TypeId>,
    ) -> Result<(), CompileError> {
        if obj.len() == 1 {
            let obj = obj[0];
            if let Some(pos) = self.field_types.iter().position(|&(o, _)| obj == o) {
                let ty = self.field_types[pos].1;
                if field.contains(&ty) {
                    field.clear();
                    field.push(ty);
                    Ok(())
                } else {
                    let found_str = TypeSolver::ty_error_str(types, &field);
                    let expected_str = TypeSolver::ty_error_str(types, &[ty]);
                    CompileError::new(
                        meta.get(&field_id).unwrap(),
                        format_args!(
                            "Mismatched types: found {}, expected {}",
                            found_str, expected_str
                        ),
                    )
                }
            } else {
                CompileError::new(
                    meta.get(&field_id).unwrap(),
                    format_args!(
                        "No field `{}` on type `{}`",
                        self.field_name, types[obj.0].name.item,
                    ),
                )
            }
        } else {
            let field = field[0];
            let possible_structs: Vec<_> = self
                .field_types
                .iter()
                .filter(|&&(_, f)| f == field)
                .map(|&(o, _)| o)
                .collect();
            if !possible_structs.is_empty() {
                let object: Vec<_> = possible_structs
                    .into_iter()
                    .filter(|t| obj.contains(t))
                    .collect();
                if !object.is_empty() {
                    *obj = object;
                    Ok(())
                } else {
                    let found_str = TypeSolver::ty_error_str(types, obj);
                    let expected_str = TypeSolver::ty_error_str(types, &object);
                    CompileError::new(
                        meta.get(&field_id).unwrap(),
                        format_args!(
                            "Mismatched types: found {}, expected {}",
                            found_str, expected_str
                        ),
                    )
                }
            } else {
                CompileError::new(
                    meta.get(&field_id).unwrap(),
                    format_args!(
                        "No field `{}` with type `{}`",
                        self.field_name, types[field.0].name.item,
                    ),
                )
            }
        }
    }
}
