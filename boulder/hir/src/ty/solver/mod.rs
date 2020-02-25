use std::{collections::HashMap, iter};

use tindex::{bitset::TBitSet, TSlice, TVec};

use shared_id::{TypeId, EMPTY_TYPE_ID};

use diagnostics::{CompileError, Meta};

use solver::{ConstraintSolver, EntityId, ProductionId, SolveError};

mod productions;

use crate::{
    module::Module,
    ty::{self, Kind, Type},
};

use productions::{Equality, Extension, FieldAccess, FromBytes, ToBytes};

#[derive(Debug)]
pub struct Context<'a, 'b> {
    pub types: &'b mut TVec<TypeId, Type<'a, TypeId>>,
    pub meta: TVec<EntityId, Meta<'a, ()>>,
    pub modules: &'b mut Module<'a>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum EntityState {
    Unbound,
    Restricted {
        /// at most a combination of the listed types
        upper_limit: TBitSet<TypeId>,
        /// contains at least these lower_limit
        lower_limit: TBitSet<TypeId>,
    },
    Bound(TBitSet<TypeId>),
}

impl EntityState {
    fn simplify_restriction(
        upper_limit: TBitSet<TypeId>,
        lower_limit: TBitSet<TypeId>,
        types: &mut TVec<TypeId, Type<TypeId>>,
        modules: &mut Module<'_>,
    ) -> Self {
        if upper_limit.element_count() == 1 {
            EntityState::Bound(upper_limit)
        } else if upper_limit == lower_limit {
            EntityState::Bound(iter::once(ty::build_sum_ty(&upper_limit, types, modules)).collect())
        } else {
            EntityState::Restricted {
                upper_limit,
                lower_limit,
            }
        }
    }

    pub fn try_subtype(
        &mut self,
        mut restriction: TBitSet<TypeId>,
        types: &mut TVec<TypeId, Type<TypeId>>,
        modules: &mut Module<'_>,
    ) -> bool {
        #[cfg(feature = "profiler")]
        profile_scope!("try_subtype");
        match &self {
            EntityState::Unbound => {
                *self = EntityState::Restricted {
                    upper_limit: TBitSet::new(),
                    lower_limit: restriction,
                };
                true
            }
            EntityState::Restricted {
                upper_limit,
                lower_limit,
            } => {
                if upper_limit.is_empty() || !restriction.iter().any(|r| !upper_limit.get(r)) {
                    restriction.extend(lower_limit.iter());
                    *self = Self::simplify_restriction(
                        upper_limit.clone(),
                        restriction,
                        types,
                        modules,
                    );
                    true
                } else {
                    false
                }
            }
            EntityState::Bound(v) => {
                let allowed_types: TBitSet<_> = v
                    .iter()
                    .filter(|&v| {
                        let lower_limit = ty::subtypes(v, types);
                        restriction.iter().all(|ty| lower_limit.get(ty))
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
        modules: &mut Module<'_>,
    ) -> bool {
        #[cfg(feature = "profiler")]
        profile_scope!("try_supertype");
        match &self {
            EntityState::Unbound => {
                *self = EntityState::Restricted {
                    upper_limit: restriction,
                    lower_limit: TBitSet::new(),
                };
                true
            }
            EntityState::Restricted {
                upper_limit,
                lower_limit,
            } => {
                let allowed_types = if upper_limit.is_empty() {
                    restriction
                } else {
                    upper_limit
                        .iter()
                        .filter(|&ty| restriction.get(ty))
                        .collect()
                };

                if !allowed_types.is_empty() && lower_limit.iter().all(|ty| allowed_types.get(ty)) {
                    *self = Self::simplify_restriction(
                        allowed_types,
                        lower_limit.clone(),
                        types,
                        modules,
                    );
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
        #[cfg(feature = "profiler")]
        profile_scope!("try_bind");
        match self {
            state @ EntityState::Unbound => {
                *state = EntityState::Bound(ty);
                true
            }
            EntityState::Restricted {
                upper_limit,
                lower_limit,
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

                    let is_valid = if !upper_limit.is_empty() {
                        e.iter().all(|t| upper_limit.get(t))
                    } else {
                        true
                    } && lower_limit.iter().all(|t| e.get(t));

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
            EntityState::Bound(v) => v.iter().nth(1).is_none(),
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
    solver: ConstraintSolver<'a, Context<'a, 'b>, EntityState, CompileError>,
    empty: TypeId,
    integers: TBitSet<TypeId>,
    fields: HashMap<&'a str, (ProductionId, Vec<TypeId>)>,
    equality: ProductionId,
    extension: ProductionId,
    to_bytes: ProductionId,
    from_bytes: ProductionId,
}

impl<'a, 'b> TypeSolver<'a, 'b> {
    pub fn new(types: &'b mut TVec<TypeId, Type<'a, TypeId>>, modules: &'b mut Module<'a>) -> Self {
        #[cfg(feature = "profiler")]
        profile_scope!("TypeSolver::new");
        let empty = EMPTY_TYPE_ID;
        let integers = types
            .index_iter()
            .filter(|&i| ["u8", "u16", "u32"].contains(&&*types[i].name.item))
            .collect();

        let mut fields = HashMap::<&str, Vec<(TypeId, TypeId)>>::new();
        for (i, ty) in types.index_iter().zip(types.iter()) {
            let ty_fields: &TSlice<_, _> = if let Kind::Struct(v) | Kind::Union(v) = &ty.kind {
                v
            } else {
                (&[] as &[_]).into()
            };

            for field in ty_fields.iter() {
                fields
                    .entry(field.name.item)
                    .or_default()
                    .push((i, field.ty.item))
            }
        }

        let mut solver = ConstraintSolver::new(Context {
            types,
            modules,
            meta: TVec::new(),
        });

        let fields = fields
            .into_iter()
            .map(|(name, field_types)| {
                let obj_types = field_types.iter().map(|&(o, _)| o).collect();
                let field_access = FieldAccess::new(name, field_types);

                let id = solver.define_production(Box::new(field_access));
                (name, (id, obj_types))
            })
            .collect();

        let equality = solver.define_production(Box::new(Equality));
        let extension = solver.define_production(Box::new(Extension));
        let to_bytes = solver.define_production(Box::new(ToBytes));
        let from_bytes = solver.define_production(Box::new(FromBytes));

        Self {
            solver,
            empty,
            integers,
            fields,
            equality,
            extension,
            to_bytes,
            from_bytes,
        }
    }

    fn ty_error_str(types: &TSlice<TypeId, Type<'a, TypeId>>, expected: &EntityState) -> String {
        match expected {
            EntityState::Unbound => "any type".into(),
            EntityState::Restricted {
                upper_limit,
                lower_limit,
            } => {
                let upper_limit = upper_limit.iter().collect::<Box<[_]>>();
                let sup = match upper_limit.as_ref() {
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
                        types[upper_limit[0]].name.item,
                        types[upper_limit[1]].name.item,
                        upper_limit.len() - 2
                    ),
                };

                let lower_limit = lower_limit.iter().collect::<Box<[_]>>();
                let sub = match lower_limit.as_ref() {
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
                        types[lower_limit[0]].name.item,
                        types[lower_limit[1]].name.item,
                        lower_limit.len() - 2
                    ),
                };

                match (sup.is_empty(), sub.is_empty()) {
                    (false, false) => format!("{}, {}", sup, sub),
                    (false, true) => sup,
                    (true, false) => format!("a type {}", sub),
                    (true, true) => unreachable!("empty restriction"),
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
        self.add_entity(EntityState::Bound(iter::once(ty).collect()), meta)
    }

    pub fn override_entity(&mut self, id: EntityId, ty: TypeId, meta: Meta<'a, ()>) {
        self.solver
            .override_entity_state(id, EntityState::Bound(iter::once(ty).collect()));
        self.solver.context().meta[id] = meta;
    }

    pub fn add_equality(&mut self, origin: EntityId, target: EntityId) {
        self.solver.add_production(self.equality, origin, target);
    }

    pub fn add_to_bytes(&mut self, origin: EntityId, target: EntityId) {
        self.solver.add_production(self.to_bytes, origin, target);
    }

    pub fn add_from_bytes(&mut self, origin: EntityId, target: EntityId) {
        self.solver.add_production(self.from_bytes, origin, target);
    }

    pub fn add_extension(&mut self, origin: EntityId, target: EntityId) {
        self.solver.add_production(self.extension, origin, target);
    }

    pub fn add_field_access(
        &mut self,
        obj: EntityId,
        field: EntityId,
        name: &Meta<'a, &'a str>,
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

    pub fn modules(&mut self) -> &mut Module<'a> {
        self.solver.context().modules
    }

    pub fn solve(mut self) -> Result<TVec<EntityId, TypeId>, CompileError> {
        #[cfg(feature = "profiler")]
        profile_scope!("TypeSolver::solver");
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
                rest.iter()
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
