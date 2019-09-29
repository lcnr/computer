use std::{collections::HashMap, iter};

use tindex::{bitset::TBitSet, TSlice, TVec};

use shared_id::{TypeId, EMPTY_TYPE_ID};

use diagnostics::{CompileError, Meta};

use solver::{ConstraintSolver, EntityId, ProductionId, SolveError};

mod productions;

use crate::ty::{self, Kind, Type};

use productions::{Equality, Extension, FieldAccess};

#[derive(Debug)]
pub struct Context<'a, 'b> {
    pub types: &'b mut TVec<TypeId, Type<'a, TypeId>>,
    pub type_lookup: &'b mut HashMap<Box<str>, TypeId>,
    pub meta: TVec<EntityId, Meta<'a, ()>>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum EntityState {
    Unbound,
    Restricted {
        /// at most a combination of the listed types
        supertypes: TBitSet<TypeId>,
        /// contains at least these subtypes
        subtypes: TBitSet<TypeId>,
    },
    Bound(TBitSet<TypeId>),
}

impl EntityState {
    fn simplify_restriction(
        supertypes: TBitSet<TypeId>,
        subtypes: TBitSet<TypeId>,
        types: &mut TVec<TypeId, Type<TypeId>>,
        lookup: &mut HashMap<Box<str>, TypeId>,
    ) -> Self {
        if supertypes.element_count() == 1 {
            EntityState::Bound(supertypes)
        } else if supertypes == subtypes {
            EntityState::Bound(iter::once(ty::build_sum_ty(types, lookup, &supertypes)).collect())
        } else {
            EntityState::Restricted {
                supertypes,
                subtypes,
            }
        }
    }

    pub fn try_subtype(
        &mut self,
        mut restriction: TBitSet<TypeId>,
        types: &mut TVec<TypeId, Type<TypeId>>,
        lookup: &mut HashMap<Box<str>, TypeId>,
    ) -> bool {
        match &self {
            EntityState::Unbound => {
                *self = EntityState::Restricted {
                    supertypes: TBitSet::new(),
                    subtypes: restriction,
                };
                true
            }
            EntityState::Restricted {
                supertypes,
                subtypes,
            } => {
                if supertypes.is_empty() || !restriction.iter().any(|r| !supertypes.get(r)) {
                    restriction.extend(subtypes.iter());
                    *self =
                        Self::simplify_restriction(supertypes.clone(), restriction, types, lookup);
                    true
                } else {
                    false
                }
            }
            EntityState::Bound(v) => {
                let allowed_types: TBitSet<_> = v
                    .iter()
                    .filter(|&v| {
                        let subtypes = ty::subtypes(v, types);
                        restriction.iter().all(|ty| subtypes.get(ty))
                    })
                    .collect();

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
        restriction: TBitSet<TypeId>,
        types: &mut TVec<TypeId, Type<TypeId>>,
        lookup: &mut HashMap<Box<str>, TypeId>,
    ) -> bool {
        match &self {
            EntityState::Unbound => {
                *self = EntityState::Restricted {
                    supertypes: restriction,
                    subtypes: TBitSet::new(),
                };
                true
            }
            EntityState::Restricted {
                supertypes,
                subtypes,
            } => {
                let allowed_types = if supertypes.is_empty() {
                    restriction
                } else {
                    supertypes
                        .iter()
                        .filter(|&ty| restriction.get(ty))
                        .collect()
                };

                if !allowed_types.is_empty() && subtypes.iter().all(|ty| allowed_types.get(ty)) {
                    *self =
                        Self::simplify_restriction(allowed_types, subtypes.clone(), types, lookup);
                    true
                } else {
                    false
                }
            }
            EntityState::Bound(v) => {
                let allowed_types: TBitSet<_> = v
                    .iter()
                    .filter(|&v| ty::subtypes(v, types).iter().all(|ty| restriction.get(ty)))
                    .collect();

                if !allowed_types.is_empty() {
                    *self = EntityState::Bound(allowed_types);
                    true
                } else {
                    false
                }
            }
        }
    }

    pub fn try_bind(&mut self, ty: TBitSet<TypeId>, types: &TSlice<TypeId, Type<TypeId>>) -> bool {
        match self {
            state @ EntityState::Unbound => {
                *state = EntityState::Bound(ty);
                true
            }
            EntityState::Restricted {
                supertypes,
                subtypes,
            } => {
                let mut allowed_types = TBitSet::new();
                for ty in ty {
                    let e;
                    let e = match &types[ty].kind {
                        Kind::Sum(e) => e,
                        _ => {
                            e = iter::once(ty).collect();
                            &e
                        }
                    };

                    let is_valid = if !supertypes.is_empty() {
                        e.iter().all(|t| supertypes.get(t))
                    } else {
                        true
                    } && subtypes.iter().fold(true, |s, t| s && e.get(t));

                    if is_valid {
                        allowed_types.add(ty);
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
                let allowed_types: TBitSet<_> = v.iter().filter(|&t| ty.get(t)).collect();
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
            EntityState::Bound(v) => v.iter().skip(1).next().is_none(),
        }
    }

    fn solution(&self) -> TypeId {
        if let EntityState::Bound(ref v) = self {
            v.iter().next().unwrap()
        } else {
            unreachable!("{:?}", self);
        }
    }
}

#[derive(Debug)]
pub struct TypeSolver<'a, 'b> {
    solver: ConstraintSolver<Context<'a, 'b>, EntityState, CompileError>,
    empty: TypeId,
    integers: TBitSet<TypeId>,
    fields: HashMap<Box<str>, (ProductionId, Vec<TypeId>)>,
    equality: ProductionId,
    extension: ProductionId,
}

impl<'a, 'b> TypeSolver<'a, 'b> {
    pub fn new(
        types: &'b mut TVec<TypeId, Type<'a, TypeId>>,
        type_lookup: &'b mut HashMap<Box<str>, TypeId>,
    ) -> Self {
        let empty = EMPTY_TYPE_ID;
        let integers = types
            .iter()
            .enumerate()
            .filter_map(|(i, ty)| {
                if ["u8", "u16", "u32"].contains(&&*ty.name.item) {
                    Some(TypeId::from(i))
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
                    .push((TypeId::from(i), field.ty.item))
            }
        }

        let mut solver = ConstraintSolver::new(Context {
            types,
            type_lookup,
            meta: TVec::new(),
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
            integers,
            fields,
            equality,
            extension,
        }
    }

    fn ty_error_str(types: &TSlice<TypeId, Type<'a, TypeId>>, expected: &EntityState) -> String {
        match expected {
            EntityState::Unbound => "any type".into(),
            EntityState::Restricted {
                supertypes,
                subtypes,
            } => {
                let supertypes = supertypes.iter().collect::<Box<[_]>>();
                let sup = match supertypes.as_ref() {
                    &[] => String::new(),
                    &[one] => format!("`{}`", types[one].name.item),
                    &[one, two] => format!(
                        "a combination of `{}` or `{}`",
                        types[one].name.item, types[two].name.item
                    ),
                    &[one, two, three] => format!(
                        "a combination of `{}`, `{}` or `{}`",
                        types[one].name.item, types[two].name.item, types[three].name.item
                    ),
                    _ => format!(
                        "a combination of `{}`, `{}` or {} more",
                        types[supertypes[0]].name.item,
                        types[supertypes[1]].name.item,
                        supertypes.len() - 2
                    ),
                };

                let subtypes = subtypes.iter().collect::<Box<[_]>>();
                let sub = match subtypes.as_ref() {
                    &[] => String::new(),
                    &[one] => format!("containing `{}`", types[one].name.item),
                    &[one, two] => format!(
                        "containing `{}` or `{}`",
                        types[one].name.item, types[two].name.item
                    ),
                    &[one, two, three] => format!(
                        "containing `{}`, `{}` or `{}`",
                        types[one].name.item, types[two].name.item, types[three].name.item
                    ),
                    _ => format!(
                        "containing `{}`, `{}` or {} more",
                        types[subtypes[0]].name.item,
                        types[subtypes[1]].name.item,
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
            EntityState::Bound(v) => {
                let v = v.iter().collect::<Box<[_]>>();
                match v.as_ref() {
                    &[] => unreachable!("expected no types"),
                    &[one] => format!("`{}`", types[one].name.item),
                    &[one, two] => format!(
                        "either `{}` or `{}`",
                        types[one].name.item, types[two].name.item
                    ),
                    &[one, two, three] => format!(
                        "one of `{}`, `{}` or `{}`",
                        types[one].name.item, types[two].name.item, types[three].name.item
                    ),
                    _ => format!(
                        "one of `{}`, `{}` or {} more",
                        types[v[0]].name.item,
                        types[v[1]].name.item,
                        v.len() - 2
                    ),
                }
            }
        }
    }

    pub fn integers(&self) -> &TBitSet<TypeId> {
        &self.integers
    }

    fn add_entity(&mut self, state: EntityState, meta: Meta<'a, ()>) -> EntityId {
        let id = self.solver.add_entity(state);
        assert_eq!(self.solver.context().meta.push(meta), id);
        id
    }

    pub fn add_integer(&mut self, meta: Meta<'a, ()>) -> EntityId {
        self.add_entity(EntityState::Bound(self.integers.clone()), meta)
    }

    pub fn add_empty(&mut self, meta: Meta<'a, ()>) -> EntityId {
        self.add_typed(self.empty, meta)
    }

    pub fn add_bound(&mut self, types: TBitSet<TypeId>, meta: Meta<'a, ()>) -> EntityId {
        self.add_entity(EntityState::Bound(types), meta)
    }

    pub fn add_unbound(&mut self, meta: Meta<'a, ()>) -> EntityId {
        self.add_entity(EntityState::Unbound, meta)
    }

    pub fn add_typed(&mut self, ty: TypeId, meta: Meta<'a, ()>) -> EntityId {
        let id = self.add_entity(EntityState::Bound(iter::once(ty).collect()), meta);
        id
    }

    pub fn override_entity(&mut self, id: EntityId, ty: TypeId, meta: Meta<'a, ()>) {
        self.solver
            .override_entity_state(id, EntityState::Bound(iter::once(ty).collect()));
        self.solver.context().meta[id] = meta;
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

    pub fn types(&mut self) -> &mut TVec<TypeId, Type<'a, TypeId>> {
        self.solver.context().types
    }

    pub fn type_lookup(&mut self) -> &mut HashMap<Box<str>, TypeId> {
        self.solver.context().type_lookup
    }

    pub fn solve(mut self) -> Result<TVec<EntityId, TypeId>, CompileError> {
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
                        CompileError::build(&ctx.meta[*start_id], msg).with_help(format_args!(
                            "could be {}",
                            Self::ty_error_str(ctx.types, start_types)
                        )),
                        |err, (id, ty)| {
                            err.with_location(&ctx.meta[*id]).with_help(format_args!(
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
