use std::collections::{HashMap, HashSet};

use diagnostics::{CompileError, Meta};

use solver::{ConstraintSolver, EntityId, ProductionId, Solution, SolveError};

mod productions;

use crate::ty::{Type, TypeId};

use productions::*;

#[derive(Debug)]
struct Context<'a, 'b> {
    types: &'b mut Vec<Type<'a, TypeId>>,
    type_lookup: &'b mut HashMap<Box<str>, TypeId>,
    meta: HashMap<EntityId, Meta<'a, ()>>,
    bounds: HashMap<EntityId, Vec<TypeId>>,
}

#[derive(Debug)]
pub struct TypeSolver<'a, 'b> {
    solver: ConstraintSolver<Context<'a, 'b>, TypeId, CompileError>,
    empty: TypeId,
    all: Vec<TypeId>,
    integers: Vec<TypeId>,
    fields: HashMap<Box<str>, (ProductionId, Vec<TypeId>)>,
    equality: ProductionId,
    extension: ProductionId,
}

impl<'a, 'b> TypeSolver<'a, 'b> {
    pub fn new(
        types: &'b mut Vec<Type<'a, TypeId>>,
        type_lookup: &'b mut HashMap<Box<str>, TypeId>,
    ) -> Self {
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
            type_lookup,
            meta: HashMap::new(),
            bounds: HashMap::new(),
        });

        let fields = fields
            .into_iter()
            .map(|(name, field_types)| {
                let field_name = name.clone();
                let obj_types = field_types.iter().map(|(o, _)| o.clone()).collect();
                let field_access = FieldAccess::new(field_name, field_types);

                let id = solver.define_production(Box::new(field_access));
                (name, (id, obj_types))
            })
            .collect();

        let equality = solver.define_production(Box::new(Equality));
        let extension = solver.define_production(Box::new(Extension));

        Self {
            solver,
            empty,
            all,
            integers,
            fields,
            equality,
            extension,
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

    fn add_bound(&mut self, id: EntityId, bound: Vec<TypeId>) {
        self.solver
            .context()
            .bounds
            .entry(id)
            .and_modify(|ty| {
                *ty = std::mem::replace(ty, Vec::new())
                    .into_iter()
                    .filter(|t| bound.contains(t))
                    .collect();
            })
            .or_insert_with(|| self.integers.clone());
    }

    pub fn add_integer(&mut self, meta: Meta<'a, ()>) -> EntityId {
        let id = self.add_entity(self.integers.clone(), meta);
        self.add_bound(id, self.integers.clone());
        id
    }

    pub fn add_empty(&mut self, meta: Meta<'a, ()>) -> EntityId {
        self.add_entity(vec![self.empty], meta)
    }

    pub fn add_unconstrained(&mut self, meta: Meta<'a, ()>) -> EntityId {
        self.add_entity(self.all.clone(), meta)
    }

    pub fn add_typed(&mut self, ty: TypeId, meta: Meta<'a, ()>) -> EntityId {
        let id = self.add_entity(vec![ty], meta);
        self.add_bound(id, vec![ty]);
        id
    }

    pub fn add_equality(&mut self, origin: EntityId, target: EntityId) {
        self.solver.add_production(self.equality, origin, target);
    }

    pub fn add_extension(&mut self, origin: EntityId, target: EntityId) {
        self.solver.add_production(self.extension, origin, target);
    }

    pub fn add_field_access(
        &mut self,
        obj: EntityId,
        field: EntityId,
        name: &Meta<'_, Box<str>>,
    ) -> Result<(), CompileError> {
        if let Some(&(id, ref bounds)) = self.fields.get(&name.item) {
            self.solver.add_production(id, obj, field);
            self.add_bound(id, bound.clone());
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

    pub fn types(&mut self) -> &mut Vec<Type<'a, TypeId>> {
        self.solver.context().types
    }

    pub fn type_lookup(&mut self) -> &mut HashMap<Box<str>, TypeId> {
        self.solver.context().type_lookup
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
