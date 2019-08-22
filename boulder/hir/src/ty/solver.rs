use std::collections::HashMap;

use diagnostics::{CompileError, Meta};

use solver::{ConstraintSolver, EntityId, ExpectedFound, ProductionId, Rule, Solution, SolveError};

use crate::ty::{Type, TypeId};

pub struct TypeSolver<'a, 'b> {
    solver: ConstraintSolver<TypeId>,
    empty: TypeId,
    all: Vec<TypeId>,
    integers: Vec<TypeId>,
    fields: HashMap<Box<str>, ProductionId>,
    meta: HashMap<EntityId, Meta<'a, ()>>,
    types: &'b [Type<'a, TypeId>],
}

impl<'a, 'b> TypeSolver<'a, 'b> {
    pub fn new(types: &'b [Type<'a, TypeId>]) -> Self {
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
        for (i, ty) in types.iter().enumerate() {
            for field in ty.fields() {
                let id = fields
                    .entry(field.name.item.clone())
                    .or_insert_with(|| solver.add_production(HashMap::new()));
                solver.extend_production(*id, TypeId(i), vec![field.ty.item]);
            }
        }

        Self {
            solver,
            empty,
            all,
            integers,
            fields,
            meta: HashMap::new(),
            types
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
            let expected_meta = self.meta.get(&found.0).unwrap();
            let found_meta = self.meta.get(&expected.0).unwrap();

            match (&*expected.1, &*found.1) {
                ([expected], [found]) => {
                    CompileError::build(found_meta, format_args!(
                        "Mismatched types: expected `{}`, found `{}`",
                        self.types[expected.0].name.item, self.types[found.0].name.item
                    )).with_location(expected_meta)
                    .build()
                },
                _ => unimplemented!(),
            }
            
        } else {
            Ok(())
        }
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
                format_args!("Access of unknown field `{}`, no viable struct found", name.item),
            )
        }
    }

    pub fn solve(self) -> Result<Solution<TypeId>, SolveError<TypeId>> {
        self.solver.solve()
    }
}
