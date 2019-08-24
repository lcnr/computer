use std::collections::{HashMap, HashSet};

use diagnostics::{CompileError, Meta};

use solver::{ConstraintSolver, EntityId, ExpectedFound, ProductionId, Solution, SolveError};

mod productions;

use crate::ty::{Type, TypeId};

use productions::*;

struct Context<'a, 'b> {
    types: &'b mut Vec<Type<'a, TypeId>>,
    meta: HashMap<EntityId, Meta<'a, ()>>,
    bound: HashSet<EntityId>,
}

pub struct TypeSolver<'a, 'b> {
    solver: ConstraintSolver<Context<'a, 'b>, TypeId, CompileError>,
    empty: TypeId,
    all: Vec<TypeId>,
    integers: Vec<TypeId>,
    fields: HashMap<Box<str>, ProductionId>,
}

impl<'a, 'b> TypeSolver<'a, 'b> {
    pub fn new(types: &'b mut Vec<Type<'a, TypeId>>) -> Self {
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

        let mut fields = HashMap::<Box<str>, Vec<(TypeId, TypeId)>>::new();
        for (i, ty) in types.iter().enumerate() {
            for field in ty.fields() {
                fields
                    .entry(field.name.item.clone())
                    .or_default()
                    .push((TypeId(i), field.ty.item))
            }
        }

        let mut solver = ConstraintSolver::new(Context {
            types,
            meta: HashMap::new(),
            bound: HashSet::new(),
        });
        let fields = fields
            .into_iter()
            .map(|(name, field_types)| {
                let field_name = name.clone();
                let field_access = FieldAccess::new(field_name, field_types);

                let id = solver.define_production(Box::new(field_access));
                (name, id)
            })
            .collect();

        Self {
            solver,
            empty,
            all,
            integers,
            fields,
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
        let ctx = &mut self.solver.context();
        ctx.meta.insert(id, meta);
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
            let ctx = self.solver.context();
            CompileError::build(
                ctx.meta.get(&found.0).unwrap(),
                format_args!(
                    "Mismatched types: found {}, expected {}",
                    Self::ty_error_str(ctx.types, &found.1),
                    Self::ty_error_str(ctx.types, &expected.1),
                ),
            )
            .with_location(ctx.meta.get(&expected.0).unwrap())
            .build()
        } else {
            Ok(())
        }
    }

    pub fn types(&mut self) -> &mut Vec<Type<'a, TypeId>> {
        self.solver.context().types
    }

    pub fn add_field_access(
        &mut self,
        obj: EntityId,
        field: EntityId,
        name: &Meta<'_, Box<str>>,
    ) -> Result<(), CompileError> {
        if let Some(&e) = self.fields.get(&name.item) {
            self.solver.add_production(e, obj, field);
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

    pub fn solve(mut self) -> Result<Solution<TypeId>, CompileError> {
        match self.solver.solve() {
            Ok(solution) => Ok(solution),
            Err(SolveError::UnsolvedEntities(entities)) => {
                let ctx = self.solver.context();
                let ((start_id, start_types), rest) = entities.split_first().unwrap();

                let msg = if rest.is_empty() {
                    "Value with multiple possible types found:"
                } else {
                    "Values with multiple possible types found:"
                };
                rest.into_iter()
                    .fold(
                        CompileError::build(ctx.meta.get(&start_id).unwrap(), msg).with_help(
                            format_args!("could be {}", Self::ty_error_str(ctx.types, start_types)),
                        ),
                        |err, (id, ty)| {
                            err.with_location(ctx.meta.get(&id).unwrap())
                                .with_help(format_args!(
                                    "could be {}",
                                    Self::ty_error_str(ctx.types, &ty)
                                ))
                        },
                    )
                    .build()
            }
            Err(SolveError::ProductionError(c)) => Err(c),
        }
    }
}
