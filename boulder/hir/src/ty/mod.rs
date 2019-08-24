use super::*;

pub mod solver;

use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(pub usize);

impl TypeId {
    pub fn to_mir(self) -> mir::TypeId {
        mir::TypeId(self.0)
    }
}

pub const EMPTY_ID: TypeId = TypeId(0);
pub const NEVER_ID: TypeId = TypeId(1);

#[derive(Debug, Clone)]
pub struct Type<'a, T> {
    pub name: Meta<'a, Box<str>>,
    pub kind: Kind<'a, T>,
}

impl<'a> Type<'a, Box<str>> {
    pub fn resolve(
        self,
        lookup: &HashMap<Box<str>, TypeId>,
    ) -> Result<Type<'a, TypeId>, CompileError> {
        Ok(Type {
            name: self.name,
            kind: match self.kind {
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
                            ).with_location(&window[0].name).build()?;
                        }
                    }
                    Kind::Struct(
                        fields
                            .into_iter()
                            .map(|m| {
                                if let Some(&id) = lookup.get(&m.ty.item) {
                                    Ok(Field {
                                        name: m.name,
                                        ty: m.ty.replace(id),
                                    })
                                } else {
                                    CompileError::new(
                                        &m.ty,
                                        format_args!(
                                            "Cannot find type `{}` in this scope",
                                            &m.ty.item
                                        ),
                                    )?
                                }
                            })
                            .collect::<Result<_, _>>()?,
                    )
                }
                Kind::Sum(cases) => unimplemented!("{:?}", cases),
            },
        })
    }
}

pub fn resolve<'a>(
    unresolved: Meta<'a, UnresolvedType<'a>>,
    types: &mut Vec<Type<'a, TypeId>>,
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
                .collect::<Result<Vec<_>, _>>()?;

            unresolved.replace(build_sum_ty(types, lookup, &type_ids)?)
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
        _ => unimplemented!("unresolvable type: {:?}", unresolved),
    })
}

pub fn build_sum_ty<'a>(
    types: &mut Vec<Type<'a, TypeId>>,
    lookup: &mut HashMap<Box<str>, TypeId>,
    cases: &[TypeId],
) -> Result<TypeId, CompileError> {
    let mut values = Vec::with_capacity(cases.len());
    let visited = &mut HashSet::new();
    for &case in cases {
        values.append(&mut flatten_sum_ty(types, case, visited));
        values.sort();
        values.dedup();
    }

    if values.len() == 1 {
        Ok(values[0])
    } else {
        let (first, rest) = values
            .split_first()
            .expect("trying to build an empty sum type");
        let (type_name, resolved_type_name) = rest.iter().fold(
            (
                format!("{}", first.0),
                format!("{}", types[first.0].name.item),
            ),
            |(s, r), ty| {
                (
                    format!("{} | {}", s, ty.0),
                    format!("{} | {}", r, types[ty.0].name.item),
                )
            },
        );

        Ok(*lookup.entry(type_name.into()).or_insert_with(|| {
            let id = TypeId(types.len());
            types.push(Type {
                name: Meta::fake(resolved_type_name.into()),
                kind: Kind::Sum(values),
            });
            id
        }))
    }
}

/// returns all possible types, removing duplicates
pub fn flatten_sum_ty(
    types: &[Type<TypeId>],
    ty: TypeId,
    visited: &mut HashSet<TypeId>,
) -> Vec<TypeId> {
    if visited.insert(ty) {
        if let Kind::Sum(cases) = &types[ty.0].kind {
            let mut cases = cases
                .iter()
                .flat_map(|&t| flatten_sum_ty(types, t, visited))
                .collect::<Vec<TypeId>>();
            cases.sort();
            cases.dedup();
            cases
        } else {
            vec![ty]
        }
    } else {
        Vec::new()
    }
}

pub fn check_recursive_ty(types: &[Type<TypeId>]) -> Result<(), CompileError> {
    let mut result = Ok(());
    for (id, t) in types.iter().enumerate() {
        if t.contains(TypeId(id), types, &mut HashSet::new()) {
            result = CompileError::new(
                &t.name,
                format_args!("Recursive type `{}` has infinite size", t.name.item),
            );
        }
    }

    result
}

pub fn is_subtype(ty: TypeId, of: TypeId, types: &[Type<TypeId>]) -> bool {
    if ty == of {
        true
    } else {
        match &types[of.0].kind {
            Kind::Sum(options) => {
                if let Kind::Sum(t) = &types[of.0].kind {
                    t.iter().all(|ty| options.contains(ty))
                } else {
                    options.contains(&ty)
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
        types: &[Type<'a, TypeId>],
        visited: &mut HashSet<TypeId>,
    ) -> bool {
        match &self.kind {
            Kind::Unit | Kind::Uninhabited | Kind::U8 | Kind::U16 | Kind::U32 => false,
            Kind::Struct(fields) => {
                for field in fields {
                    let field_id = field.ty.item;
                    if field_id == ty {
                        return true;
                    }

                    if visited.insert(field_id) {
                        if types[field_id.0].contains(ty, types, visited) {
                            return true;
                        }
                    }
                }
                false
            }
            Kind::Sum(cases) => {
                for &case in cases {
                    if case == ty {
                        return true;
                    }

                    if visited.insert(case) {
                        if types[case.0].contains(ty, types, visited) {
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
            Some(FieldId(
                v.binary_search_by(|probe| probe.name.cmp(name)).ok()?,
            ))
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
                mir::Type::Struct(fields.into_iter().map(|m| m.ty.item.to_mir()).collect())
            }
            Kind::Sum(cases) => mir::Type::Sum(cases.into_iter().map(|t| t.to_mir()).collect()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldId(pub usize);

impl FieldId {
    pub fn to_mir(self) -> mir::FieldId {
        mir::FieldId(self.0)
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
    Sum(Vec<TypeId>),
}
