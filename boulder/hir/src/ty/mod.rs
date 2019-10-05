use std::{collections::HashMap, iter};

use tindex::{bitset::TBitSet, TSlice, TVec};

use shared_id::{FieldId, TypeId};

use diagnostics::{CompileError, Meta};

use crate::UnresolvedType;

pub mod solver;

#[derive(Debug, Clone)]
pub struct Type<'a, T> {
    pub name: Meta<'a, Box<str>>,
    pub attributes: Vec<Meta<'a, Box<str>>>,
    pub kind: Kind<'a, T>,
}

impl<'a> Type<'a, UnresolvedType<'a>> {
    pub fn resolve(
        self,
        types: &mut TVec<TypeId, Type<'a, TypeId>>,
        lookup: &mut HashMap<Box<str>, TypeId>,
    ) -> Result<(), CompileError> {
        let id = *lookup.get(&self.name.item).unwrap();

        let kind = match self.kind {
            Kind::Unit => Kind::Unit,
            Kind::Uninhabited => Kind::Uninhabited,
            Kind::U8 => Kind::U8,
            Kind::U16 => Kind::U16,
            Kind::U32 => Kind::U32,
            Kind::Struct(mut fields) => {
                fields.sort_by(|a, b| a.name.item.cmp(&b.name.item));
                for window in fields.windows(2) {
                    if window[0].name.item == window[1].name.item {
                        CompileError::build(
                            &window[1].name,
                            format_args!(
                                "Identifier `{}` is bound more than once in this parameter list",
                                &window[1].name.item,
                            ),
                        )
                        .with_location(&window[0].name)
                        .build()?;
                    }
                }

                Kind::Struct(
                    fields
                        .into_iter()
                        .map(|m| {
                            Ok(Field {
                                name: m.name,
                                ty: resolve(m.ty, types, lookup)?,
                            })
                        })
                        .collect::<Result<_, _>>()?,
                )
            }
            Kind::Sum(v) => Kind::Sum(v),
        };
        types[id].kind = kind;
        Ok(())
    }
}

pub fn resolve<'a>(
    unresolved: Meta<'a, UnresolvedType<'a>>,
    types: &mut TVec<TypeId, Type<'a, TypeId>>,
    lookup: &mut HashMap<Box<str>, TypeId>,
) -> Result<Meta<'a, TypeId>, CompileError> {
    Ok(match &unresolved.item {
        UnresolvedType::Sum(cases) => {
            let type_ids = cases
                .into_iter()
                .map(|c| {
                    if let Some(&i) = lookup.get(&c.item) {
                        Ok(i)
                    } else {
                        CompileError::new(
                            &c,
                            format_args!("Cannot find type `{}` in this scope", c.item),
                        )
                    }
                })
                .collect::<Result<TBitSet<_>, _>>()?;

            unresolved.replace(build_sum_ty(types, lookup, &type_ids))
        }
        UnresolvedType::Named(ref name) => {
            if let Some(&i) = lookup.get(&*name) {
                unresolved.replace(i)
            } else {
                CompileError::new(
                    &unresolved,
                    format_args!("Cannot find type `{}` in this scope", name),
                )?
            }
        }
        UnresolvedType::Integer => unimplemented!("unresolvable type: {:?}", unresolved),
    })
}

pub fn build_sum_ty<'a>(
    types: &mut TVec<TypeId, Type<'a, TypeId>>,
    lookup: &mut HashMap<Box<str>, TypeId>,
    cases: &TBitSet<TypeId>,
) -> TypeId {
    let mut values = TBitSet::new();
    let visited = &mut TBitSet::new();
    for case in cases.iter() {
        values.extend(flatten_sum_ty(types, case, visited));
    }

    if values.element_count() == 1 {
        values.iter().next().unwrap()
    } else {
        let mut iter = values.iter();

        let first = iter.next().expect("trying to build an empty sum type");
        let (type_name, resolved_type_name) = iter.fold(
            (format!("{}", first), format!("{}", types[first].name.item)),
            |(s, r), ty| {
                (
                    format!("{} | {}", s, ty),
                    format!("{} | {}", r, types[ty].name.item),
                )
            },
        );

        *lookup.entry(type_name.into()).or_insert_with(|| {
            types.push(Type {
                name: Meta::fake(resolved_type_name.into()),
                attributes: Vec::new(),
                kind: Kind::Sum(values),
            })
        })
    }
}

/// returns all possible types, removing duplicates
pub fn flatten_sum_ty(
    types: &TSlice<TypeId, Type<TypeId>>,
    ty: TypeId,
    visited: &mut TBitSet<TypeId>,
) -> TBitSet<TypeId> {
    if !visited.get(ty) {
        visited.add(ty);
        if let Kind::Sum(cases) = &types[ty].kind {
            cases
                .iter()
                .flat_map(|t| flatten_sum_ty(types, t, visited))
                .collect()
        } else {
            iter::once(ty).collect()
        }
    } else {
        TBitSet::new()
    }
}

pub fn check_recursive_ty(types: &TSlice<TypeId, Type<TypeId>>) -> Result<(), CompileError> {
    let mut result = Ok(());
    for (id, t) in types.iter().enumerate() {
        if t.contains(id.into(), types, &mut TBitSet::new()) {
            result = CompileError::new(
                &t.name,
                format_args!("Recursive type `{}` has infinite size", t.name.item),
            );
        }
    }

    result
}

pub fn subtypes(ty: TypeId, types: &TSlice<TypeId, Type<TypeId>>) -> TBitSet<TypeId> {
    if let Kind::Sum(t) = &types[ty].kind {
        t.clone()
    } else {
        iter::once(ty).collect()
    }
}

pub fn is_subtype(ty: TypeId, of: TypeId, types: &TSlice<TypeId, Type<TypeId>>) -> bool {
    if ty == of {
        true
    } else {
        match &types[of].kind {
            Kind::Sum(options) => {
                if let Kind::Sum(t) = &types[ty].kind {
                    t.iter().all(|ty| options.get(ty))
                } else {
                    options.get(ty)
                }
            }
            _ => false,
        }
    }
}

impl<'a> Type<'a, TypeId> {
    // check if this type contains `ty`
    pub fn contains(
        &self,
        ty: TypeId,
        types: &TSlice<TypeId, Type<'a, TypeId>>,
        visited: &mut TBitSet<TypeId>,
    ) -> bool {
        match &self.kind {
            Kind::Unit | Kind::Uninhabited | Kind::U8 | Kind::U16 | Kind::U32 => false,
            Kind::Struct(fields) => {
                for field in fields {
                    let field_id = field.ty.item;
                    if field_id == ty {
                        return true;
                    }

                    if !visited.get(field_id) {
                        visited.add(field_id);
                        if types[field_id].contains(ty, types, visited) {
                            return true;
                        }
                    }
                }
                false
            }
            Kind::Sum(cases) => {
                for case in cases.iter() {
                    if case == ty {
                        return true;
                    }

                    if !visited.get(case) {
                        visited.add(case);
                        if types[case].contains(ty, types, visited) {
                            return true;
                        }
                    }
                }
                false
            }
        }
    }

    pub fn fields(&self) -> &[Field<'a, TypeId>] {
        if let Kind::Struct(v) = &self.kind {
            &v
        } else {
            &[]
        }
    }

    pub fn get_field(&self, name: &Box<str>) -> Option<FieldId> {
        if let Kind::Struct(v) = &self.kind {
            Some(
                v.binary_search_by(|probe| probe.name.cmp(name))
                    .ok()?
                    .into(),
            )
        } else {
            None
        }
    }

    pub fn to_mir(self) -> mir::Type {
        match self.kind {
            Kind::Unit => mir::Type::Unit,
            Kind::Uninhabited => mir::Type::Uninhabited,
            Kind::U8 => mir::Type::U8,
            Kind::U16 => mir::Type::U16,
            Kind::U32 => mir::Type::U32,
            Kind::Struct(fields) => {
                mir::Type::Struct(fields.into_iter().map(|m| m.ty.item).collect())
            }
            Kind::Sum(cases) => mir::Type::Sum(cases.iter().collect()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Field<'a, T> {
    pub name: Meta<'a, Box<str>>,
    pub ty: Meta<'a, T>,
}

#[derive(Debug, Clone)]
pub enum Kind<'a, T> {
    Uninhabited,
    Unit,
    U8,
    U16,
    U32,
    Struct(Vec<Field<'a, T>>),
    Sum(TBitSet<TypeId>),
}
