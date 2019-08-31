use std::collections::HashMap;

use diagnostics::{CompileError, Meta};

use solver::{ConstraintSolver, EntityId, ProductionId, Solution, SolveError};

mod productions;

use crate::ty::{self, Kind, Type, TypeId};

use productions::*;

#[derive(Debug)]
pub struct Context<'a, 'b> {
    pub types: &'b mut Vec<Type<'a, TypeId>>,
    pub type_lookup: &'b mut HashMap<Box<str>, TypeId>,
    pub meta: HashMap<EntityId, Meta<'a, ()>>,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
enum EntityState {
    Unbound,
    Restricted {
        /// at most a combination of the listed types
        supertypes: Vec<TypeId>,
        /// contains at least these subtypes
        subtypes: Vec<TypeId>,
    },
    Bound(Vec<TypeId>),
}

impl EntityState {
    fn simplify_restriction(
        supertypes: Vec<TypeId>,
        subtypes: Vec<TypeId>,
        types: &mut Vec<Type<TypeId>>,
        lookup: &mut HashMap<Box<str>, TypeId>,
    ) -> Self {
        if supertypes.len() == 1 {
            EntityState::Bound(supertypes)
        } else if supertypes == subtypes {
            EntityState::Bound(vec![ty::build_sum_ty(types, lookup, &supertypes)])
        } else {
            EntityState::Restricted {
                supertypes,
                subtypes,
            }
        }
    }

    pub fn try_subtype(
        &mut self,
        mut restriction: Vec<TypeId>,
        types: &mut Vec<Type<TypeId>>,
        lookup: &mut HashMap<Box<str>, TypeId>,
    ) -> bool {
        match &self {
            EntityState::Unbound => {
                *self = EntityState::Restricted {
                    supertypes: Vec::new(),
                    subtypes: restriction,
                };
                true
            }
            EntityState::Restricted {
                supertypes,
                subtypes,
            } => {
                if supertypes.is_empty() || !restriction.iter().any(|r| !supertypes.contains(r)) {
                    restriction.extend(subtypes.iter().copied());
                    restriction.sort();
                    restriction.dedup();
                    *self =
                        Self::simplify_restriction(supertypes.to_vec(), restriction, types, lookup);
                    true
                } else {
                    false
                }
            }
            EntityState::Bound(v) => {
                let allowed_types = v
                    .iter()
                    .copied()
                    .filter(|&v| {
                        let subtypes = ty::subtypes(v, types);
                        restriction.iter().all(|ty| subtypes.contains(ty))
                    })
                    .collect::<Vec<_>>();

                if !allowed_types.is_empty() {
                    *self = EntityState::Bound(allowed_types);
                    true
                } else {
                    false
                }
            }
        }
    }

    pub fn try_supertype(
        &mut self,
        restriction: Vec<TypeId>,
        types: &mut Vec<Type<TypeId>>,
        lookup: &mut HashMap<Box<str>, TypeId>,
    ) -> bool {
        match &self {
            EntityState::Unbound => {
                *self = EntityState::Restricted {
                    supertypes: restriction,
                    subtypes: Vec::new(),
                };
                true
            }
            EntityState::Restricted {
                supertypes,
                subtypes,
            } => {
                let mut allowed_types = supertypes
                    .iter()
                    .copied()
                    .filter(|ty| restriction.contains(ty))
                    .collect::<Vec<_>>();

                allowed_types.sort();
                allowed_types.dedup();
                if !allowed_types.is_empty() && subtypes.iter().all(|ty| allowed_types.contains(ty))
                {
                    *self =
                        Self::simplify_restriction(allowed_types, subtypes.to_vec(), types, lookup);
                    true
                } else {
                    false
                }
            }
            EntityState::Bound(v) => {
                let allowed_types = v
                    .iter()
                    .copied()
                    .filter(|&v| {
                        ty::subtypes(v, types)
                            .iter()
                            .all(|ty| restriction.contains(ty))
                    })
                    .collect::<Vec<_>>();

                if !allowed_types.is_empty() {
                    *self = EntityState::Bound(allowed_types);
                    true
                } else {
                    false
                }
            }
        }
    }

    pub fn try_bind(&mut self, ty: Vec<TypeId>, types: &[Type<TypeId>]) -> bool {
        match self {
            state @ EntityState::Unbound => {
                *state = EntityState::Bound(ty);
                true
            }
            EntityState::Restricted {
                supertypes,
                subtypes,
            } => {
                let mut allowed_types = Vec::new();
                for ty in ty {
                    let e;
                    let e = match &types[ty.0].kind {
                        Kind::Sum(e) => e,
                        _ => {
                            e = vec![ty];
                            &e
                        }
                    };

                    let is_valid = if !supertypes.is_empty() {
                        e.iter().all(|t| supertypes.contains(t))
                    } else {
                        true
                    } && subtypes.iter().fold(true, |s, t| s && e.contains(t));

                    if is_valid {
                        allowed_types.push(ty);
                    }
                }

                if !allowed_types.is_empty() {
                    *self = EntityState::Bound(allowed_types);
                    true
                } else {
                    false
                }
            }
            EntityState::Bound(ref mut v) => {
                let allowed_types = v
                    .iter()
                    .copied()
                    .filter(|t| ty.contains(t))
                    .collect::<Vec<_>>();
                if !allowed_types.is_empty() {
                    *self = EntityState::Bound(allowed_types);
                    true
                } else {
                    false
                }
            }
        }
    }
}

impl solver::EntityState for EntityState {
    type Result = TypeId;

    fn solved(&self) -> bool {
        match self {
            EntityState::Unbound | EntityState::Restricted { .. } => false,
            EntityState::Bound(v) => v.len() == 1,
        }
    }

    fn solution(&self) -> TypeId {
        if let EntityState::Bound(ref v) = self {
            v.last().copied().unwrap()
        } else {
            unreachable!("{:?}", self);
        }
    }
}

#[derive(Debug)]
pub struct TypeSolver<'a, 'b> {
    solver: ConstraintSolver<Context<'a, 'b>, EntityState, CompileError>,
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

    fn ty_error_str(types: &[Type<'a, TypeId>], expected: &EntityState) -> String {
        match expected {
            EntityState::Unbound => "any type".into(),
            EntityState::Restricted {
                supertypes,
                subtypes,
            } => {
                let sup = match supertypes.as_slice() {
                    [] => String::new(),
                    [one] => format!("`{}`", types[one.0].name.item),
                    [one, two] => format!(
                        "a combination of `{}` or `{}`",
                        types[one.0].name.item, types[two.0].name.item
                    ),
                    [one, two, three] => format!(
                        "a combination of `{}`, `{}` or `{}`",
                        types[one.0].name.item, types[two.0].name.item, types[three.0].name.item
                    ),
                    _ => format!(
                        "a combination of `{}`, `{}` or {} more",
                        types[supertypes[0].0].name.item,
                        types[supertypes[1].0].name.item,
                        supertypes.len() - 2
                    ),
                };

                let sub = match subtypes.as_slice() {
                    [] => String::new(),
                    [one] => format!("containing `{}`", types[one.0].name.item),
                    [one, two] => format!(
                        "containing `{}` or `{}`",
                        types[one.0].name.item, types[two.0].name.item
                    ),
                    [one, two, three] => format!(
                        "containing `{}`, `{}` or `{}`",
                        types[one.0].name.item, types[two.0].name.item, types[three.0].name.item
                    ),
                    _ => format!(
                        "containing `{}`, `{}` or {} more",
                        types[subtypes[0].0].name.item,
                        types[subtypes[1].0].name.item,
                        subtypes.len() - 2
                    ),
                };

                match (!sup.is_empty(), !sub.is_empty()) {
                    (true, true) => format!("{}, {}", sup, sub),
                    (true, false) => format!("{}", sup),
                    (false, true) => format!("a type {}", sub),
                    (false, false) => unreachable!("empty restriction"),
                }
            }
            EntityState::Bound(v) => match v.as_slice() {
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
                    types[v[0].0].name.item,
                    types[v[1].0].name.item,
                    v.len() - 2
                ),
            },
        }
    }

    fn add_entity(&mut self, state: EntityState, meta: Meta<'a, ()>) -> EntityId {
        let id = self.solver.add_entity(state);
        self.solver.context().meta.insert(id, meta);
        id
    }

    pub fn add_integer(&mut self, meta: Meta<'a, ()>) -> EntityId {
        let id = self.add_entity(EntityState::Bound(self.integers.clone()), meta);
        id
    }

    pub fn add_empty(&mut self, meta: Meta<'a, ()>) -> EntityId {
        self.add_typed(self.empty, meta)
    }

    pub fn add_unbound(&mut self, meta: Meta<'a, ()>) -> EntityId {
        self.add_entity(EntityState::Unbound, meta)
    }

    pub fn add_typed(&mut self, ty: TypeId, meta: Meta<'a, ()>) -> EntityId {
        let id = self.add_entity(EntityState::Bound(vec![ty]), meta);
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
        if let Some(&(id, _)) = self.fields.get(&name.item) {
            self.solver.add_production(id, obj, field);
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

    pub fn ctx(&mut self) -> &mut Context<'a, 'b> {
        self.solver.context()
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
