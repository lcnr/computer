use std::{collections::{HashMap}};

use diagnostics::{Meta, CompileError};

use solver::{ConstraintSolver, EntityId, ProductionId, Solution, SolveError, Rule};


use crate::ty::{Type, TypeId};

pub struct TypeSolver {
    solver: ConstraintSolver<TypeId>,
    empty: TypeId,
    all: Vec<TypeId>,
    integers: Vec<TypeId>,
    fields: HashMap<Box<str>, ProductionId>,
}

impl TypeSolver {
    pub fn new(types: &[Type<TypeId>]) -> Self {
        let mut solver = ConstraintSolver::new();

        let all = (0..types.len()).map(|t| TypeId(t)).collect();
        let empty = TypeId(types.iter().position(|t| &*t.name.item == "Empty").unwrap());
        let integers = types.iter().enumerate().filter_map(|(i, ty)| if ["u8", "u16", "u32"].contains(&&*ty.name.item) {
            Some(TypeId(i))
        } else { 
            None
        }).collect();

        let mut fields = HashMap::new();
        for (i, ty) in types.iter().enumerate() {
            for field in ty.fields() {
                let id = fields.entry(field.name.item.clone()).or_insert_with(|| solver.add_production(HashMap::new()));
                solver.extend_production(*id, TypeId(i), vec![field.ty.item]);
            }
        }

        Self {
            solver,
            empty,
            all,
            integers,
            fields,
        }
    }

    pub fn add_integer(&mut self) -> EntityId {
        self.solver.add_entity(self.integers.clone())
    }

    pub fn add_empty(&mut self) -> EntityId {
        self.solver.add_entity(vec![self.empty])
    }

    pub fn add_unconstrained(&mut self) -> EntityId {
        self.solver.add_entity(self.all.clone())
    }

    pub fn add_equality(&mut self, a: EntityId, b: EntityId) {
        self.solver.add_equality(a, b);
    }

    pub fn add_typed(&mut self, ty: TypeId) -> EntityId {
        self.solver.add_entity(vec![ty])
    }

    pub fn add_field_access(&mut self, obj: EntityId, field: EntityId, name: &Meta<'_, Box<str>>) -> Result<(), CompileError> {
        if let Some(&e) = self.fields.get(&name.item) {
            self.solver.add_rule(obj, Rule::Production(e, field));
            Ok(())
        } else {
            CompileError::new(&name, format_args!("Type error, no known struct has a field `{}`", name.item))
        }
    }

    pub fn solve(self) -> Result<Solution<TypeId>, SolveError<TypeId>> {
        self.solver.solve()
    }
}