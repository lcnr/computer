use super::*;

use std::{collections::HashSet, mem};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

impl TypeId {
    pub fn to_mir(self) -> mir::TypeId {
        mir::TypeId(self.0)
    }
}

pub const EMPTY_ID: TypeId = TypeId(0);
pub const NEVER_ID: TypeId = TypeId(1);
pub const INTEGER_GROUP_ID: GroupId = GroupId(0);

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
                Kind::Struct(mut members) => {
                    members.sort_by(|a, b| a.name.item.cmp(&b.name.item));
                    for window in members.windows(2) {
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
                        members
                            .into_iter()
                            .map(|m| {
                                if let Some(&id) = lookup.get(&m.ty.item) {
                                    Ok(Member {
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
            },
        })
    }
}

impl<'a> Type<'a, TypeId> {
    pub fn to_mir(self) -> mir::Type {
        match self.kind {
            Kind::Unit => mir::Type::Unit,
            Kind::Uninhabited => mir::Type::Uninhabited,
            Kind::U8 => mir::Type::U8,
            Kind::U16 => mir::Type::U16,
            Kind::U32 => mir::Type::U32,
            Kind::Struct(members) => {
                mir::Type::Struct(members.into_iter().map(|m| m.ty.item.to_mir()).collect())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Member<'a, T> {
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
    Struct(Vec<Member<'a, T>>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GroupId(usize);

#[derive(Debug, Clone)]
pub struct Group {
    name: Box<str>,
    members: HashSet<TypeId>,
}

impl Group {
    pub fn new(name: Box<str>) -> Self {
        Self {
            name,
            members: HashSet::new(),
        }
    }

    pub fn with_member(mut self, member: TypeId) -> Self {
        self.add_member(member);
        self
    }

    pub fn add_member(&mut self, member: TypeId) {
        assert!(
            self.members.insert(member),
            "Internal compiler error: type already in group: {:?}",
            member
        );
    }
}

#[derive(Debug, Clone, Copy)]
pub struct EntityId(pub usize);

#[derive(Debug, Clone)]
pub struct Entity<'a> {
    origin: Meta<'a, ()>,
    state: State,
    groups: Vec<GroupId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum State {
    Solved(TypeId),
    Open,
}

#[derive(Debug, Clone)]
struct Equality(EntityId, EntityId);

#[derive(Debug, Clone)]
struct Membership(EntityId, GroupId);

#[derive(Debug, Clone)]
pub struct Constraints<'a> {
    equalities: Vec<Equality>,
    groups: Vec<Group>,
    entities: Vec<Entity<'a>>,
}

impl<'a> Constraints<'a> {
    pub fn new() -> Self {
        Self {
            equalities: Vec::new(),
            groups: Vec::new(),
            entities: Vec::new(),
        }
    }

    pub fn add_group(&mut self, group: Group) -> GroupId {
        let id = GroupId(self.groups.len());
        self.groups.push(group);
        id
    }

    pub fn add_equality(&mut self, a: EntityId, b: EntityId) {
        self.equalities.push(Equality(a, b));
    }

    pub fn add_membership(&mut self, e: EntityId, g: GroupId) {
        let groups = &mut self.entities[e.0].groups;
        if !groups.contains(&g) {
            groups.push(g);
        }
    }

    pub fn add_entity(&mut self, meta: Meta<'a, ()>, state: State) -> EntityId {
        let id = EntityId(self.entities.len());
        self.entities.push(Entity {
            origin: meta.simplify(),
            state,
            groups: Vec::new(),
        });
        id
    }

    pub fn entity_count(&self) -> usize {
        self.entities.len()
    }

    pub fn convert_variable_id(&self, var: Meta<'a, VariableId>) -> EntityId {
        EntityId(var.item.0)
    }

    pub fn get_state(&self, entity: EntityId) -> State {
        self.entities[entity.0].state
    }

    pub fn set_state(&mut self, entity: EntityId, state: State) -> Result<(), GroupId> {
        let entity = &mut self.entities[entity.0];
        match state {
            State::Solved(ty) => {
                entity.state = state;
                let groups = &self.groups;
                entity
                    .groups
                    .iter()
                    .find(|g| !groups[g.0].members.contains(&ty))
                    .map_or(Ok(()), |&g| Err(g))
            }
            State::Open => {
                entity.state = state;
                Ok(())
            }
        }
    }

    fn step(&mut self, types: &[Type<TypeId>]) -> Result<bool, CompileError> {
        let mut modified = false;

        let equalities = mem::replace(&mut self.equalities, Vec::new());
        for &Equality(a, b) in equalities.iter() {
            match (self.get_state(a), self.get_state(b)) {
                (State::Open, State::Open) => (),
                (State::Solved(ty), State::Open) => {
                    self.set_state(b, State::Solved(ty)).or_else(|g| {
                        CompileError::build(
                            &self.entities[b.0].origin,
                            format_args!(
                                "Mismatched types, expected `{}`, found group `{}`",
                                types[ty.0].name.item, self.groups[g.0].name
                            ),
                        )
                        .with_location(&self.entities[a.0].origin)
                        .build()
                    })?;
                    modified = true;
                }
                (State::Open, State::Solved(ty)) => {
                    self.set_state(a, State::Solved(ty)).or_else(|g| {
                        CompileError::build(
                            &self.entities[b.0].origin,
                            format_args!(
                                "Mismatched types, expected group `{}`, found type `{}`",
                                self.groups[g.0].name, types[ty.0].name.item,
                            ),
                        )
                        .with_location(&self.entities[a.0].origin)
                        .build()
                    })?;
                    modified = true;
                }
                (State::Solved(a_ty), State::Solved(b_ty)) => {
                    if a_ty != b_ty {
                        CompileError::build(
                            &self.entities[b.0].origin,
                            format_args!(
                                "Mismatched types, expected `{}`, found `{}`",
                                types[a_ty.0].name.item, types[b_ty.0].name.item
                            ),
                        )
                        .with_location(&self.entities[a.0].origin)
                        .build()?;
                    }
                }
            }
        }
        mem::replace(&mut self.equalities, equalities);

        Ok(modified)
    }

    pub fn solve(&mut self, types: &[Type<TypeId>]) -> Result<Vec<TypeId>, CompileError> {
        while self.step(types)? {}

        if let Some(e) = self.entities.iter().find(|e| e.state == State::Open) {
            CompileError::new(
                &e.origin,
                "Type check was unable to come to a satisfying result",
            )?
        }

        Ok(self
            .entities
            .iter()
            .map(|e| {
                if let State::Solved(ty) = e.state {
                    ty
                } else {
                    unreachable!("Variable with open state after solve");
                }
            })
            .collect())
    }
}
