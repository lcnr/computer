use std::collections::HashMap;

use diagnostics::{CompileError, Meta};

use solver::{
    ApplyRuleError, ConstraintSolver, EntityId, ExpectedFound, ProductionId, Rule, Solution,
    SolveError,
};

use crate::ty::{Type, TypeId};

pub struct TypeSolver<'a, 'b> {
    solver: ConstraintSolver<TypeId>,
    empty: TypeId,
    all: Vec<TypeId>,
    integers: Vec<TypeId>,
    fields: HashMap<Box<str>, ProductionId>,
    fields_rev: HashMap<ProductionId, Box<str>>,
    meta: HashMap<EntityId, Meta<'a, ()>>,
    types: &'b mut Vec<Type<'a, TypeId>>,
}

impl<'a, 'b> TypeSolver<'a, 'b> {
    pub fn new(types: &'b mut Vec<Type<'a, TypeId>>) -> Self {
        let mut solver = ConstraintSolver::new();

        let all = (0..types.len()).map(|t| TypeId(t)).collect();
        let empty = TypeId(types.iter().position(|t| &*t.name.item == "Empty").unwrap());
        let integers = types
            .iter()
            .enumerate()
            .filter_map(|(i, ty)| {
                if ["u8", "u16", "u32"].contains(&&*ty.name.item) {
                    Some(TypeId(i))
                } else {
                    None
                }
            })
            .collect();

        let mut fields = HashMap::new();
        let mut fields_rev = HashMap::new();
        for (i, ty) in types.iter().enumerate() {
            for field in ty.fields() {
                let id = fields.entry(field.name.item.clone()).or_insert_with(|| {
                    let id = solver.add_production(HashMap::new());
                    fields_rev.insert(id, field.name.item.clone());
                    id
                });
                solver.extend_production(*id, TypeId(i), field.ty.item);
            }
        }

        Self {
            solver,
            empty,
            all,
            integers,
            fields,
            fields_rev,
            meta: HashMap::new(),
            types,
        }
    }

    fn ty_error_str(types: &[Type<'a, TypeId>], expected: &[TypeId]) -> String {
        match expected {
            [] => unreachable!("expected no types"),
            [one] => format!("`{}`", types[one.0].name.item),
            [one, two] => format!(
                "either `{}` or `{}`",
                types[one.0].name.item, types[two.0].name.item
            ),
            [one, two, three] => format!(
                "one of `{}`, `{}` or `{}`",
                types[one.0].name.item, types[two.0].name.item, types[three.0].name.item
            ),
            _ => format!(
                "one of `{}`, `{}` or {} more",
                types[expected[0].0].name.item,
                types[expected[1].0].name.item,
                expected.len() - 2
            ),
        }
    }

    fn add_entity(&mut self, types: Vec<TypeId>, meta: Meta<'a, ()>) -> EntityId {
        let id = self.solver.add_entity(types);
        self.meta.insert(id, meta);
        id
    }

    pub fn add_integer(&mut self, meta: Meta<'a, ()>) -> EntityId {
        self.add_entity(self.integers.clone(), meta)
    }

    pub fn add_empty(&mut self, meta: Meta<'a, ()>) -> EntityId {
        self.add_entity(vec![self.empty], meta)
    }

    pub fn add_unconstrained(&mut self, meta: Meta<'a, ()>) -> EntityId {
        self.add_entity(self.all.clone(), meta)
    }

    pub fn add_typed(&mut self, ty: TypeId, meta: Meta<'a, ()>) -> EntityId {
        self.add_entity(vec![ty], meta)
    }

    pub fn add_equality(&mut self, a: EntityId, b: EntityId) -> Result<(), CompileError> {
        if let Err(ExpectedFound { expected, found }) = self.solver.add_equality(a, b) {
            CompileError::build(
                self.meta.get(&found.0).unwrap(),
                format_args!(
                    "Mismatched types: found {}, expected {}",
                    Self::ty_error_str(self.types, &found.1),
                    Self::ty_error_str(self.types, &expected.1),
                ),
            )
            .with_location(self.meta.get(&expected.0).unwrap())
            .build()
        } else {
            Ok(())
        }
    }

    pub fn types(&mut self) -> &mut Vec<Type<'a, TypeId>> {
        self.types
    }

    pub fn add_field_access(
        &mut self,
        obj: EntityId,
        field: EntityId,
        name: &Meta<'_, Box<str>>,
    ) -> Result<(), CompileError> {
        if let Some(&e) = self.fields.get(&name.item) {
            self.solver.add_rule(obj, Rule::Production(e, field));
            Ok(())
        } else {
            CompileError::new(
                &name,
                format_args!(
                    "Access of unknown field `{}`, no viable struct found",
                    name.item
                ),
            )
        }
    }

    pub fn solve(self) -> Result<Solution<TypeId>, CompileError> {
        match self.solver.solve() {
            Ok(solution) => Ok(solution),
            Err(SolveError::UnsolvedEntities(entities)) => {
                let types = self.types;
                let meta = self.meta;
                let ((start_id, start_types), rest) = entities.split_first().unwrap();

                let msg = if rest.is_empty() {
                    "Value with multiple possible types found:"
                } else {
                    "Values with multiple possible types found:"
                };
                rest.into_iter()
                    .fold(
                        CompileError::build(meta.get(&start_id).unwrap(), msg).with_help(
                            format_args!("could be {}", Self::ty_error_str(types, start_types)),
                        ),
                        |err, (id, ty)| {
                            err.with_location(meta.get(&id).unwrap())
                                .with_help(format_args!(
                                    "could be {}",
                                    Self::ty_error_str(types, &ty)
                                ))
                        },
                    )
                    .build()
            }
            Err(SolveError::ApplyRuleError(ApplyRuleError::InvalidProduction(
                prod,
                _obj,
                obj_ty,
                target,
            ))) => CompileError::new(
                self.meta.get(&target).unwrap(),
                format_args!(
                    "No field `{}` on type `{}`",
                    self.fields_rev.get(&prod).unwrap(),
                    self.types[obj_ty.0].name.item
                ),
            ),
            Err(SolveError::ApplyRuleError(ApplyRuleError::InvalidProductionResult(
                _prod,
                target,
                ty,
                expected,
            ))) => {
                let expected_str = Self::ty_error_str(self.types, &expected);
                CompileError::new(
                    self.meta.get(&target).unwrap(),
                    format_args!(
                        "Mismatched types: found `{}`, expected {}",
                        self.types[ty.0].name.item, expected_str
                    ),
                )
            }
        }
    }
}
