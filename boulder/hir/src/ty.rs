use super::*;

use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

impl TypeId {
    pub fn to_mir(self) -> mir::TypeId {
        mir::TypeId(self.0)
    }
}

pub const EMPTY_ID: TypeId = TypeId(0);
pub const INTEGER_GROUP_ID: GroupId = GroupId(0);

#[derive(Debug, Clone)]
pub struct Type {
    pub name: Box<str>,
    pub kind: Kind,
}

impl Type {
    pub fn to_mir(self) -> mir::Type {
        match self.kind {
            Kind::Empty => mir::Type::Empty,
            Kind::U8 => mir::Type::U8,
            Kind::U16 => mir::Type::U16,
            Kind::U32 => mir::Type::U32,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
    Empty,
    U8,
    U16,
    U32,
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
pub struct EntityId(usize);

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

    pub fn set_state(&mut self, entity: EntityId, state: State) {
        self.entities[entity.0].state = state;
    }

    fn step(&mut self, types: &[Type]) -> Result<bool, CompileError> {
        let mut modified = false;
        for &Equality(a, b) in self.equalities.iter() {
            match (self.get_state(a), self.get_state(b)) {
                (State::Open, State::Open) => (),
                (State::Solved(ty), State::Open) => {
                    for g in self.entities[b.0].groups.iter() {
                        if !self.groups[g.0].members.contains(&ty) {
                            CompileError::build(
                                &self.entities[b.0].origin,
                                format_args!(
                                    "Mismatched types, expected `{}`, found group `{}`",
                                    types[ty.0].name, self.groups[g.0].name
                                ),
                            )
                            .with_location(&self.entities[a.0].origin)
                            .build()?;
                        }
                    }
                    self.entities[b.0].state = State::Solved(ty);
                    modified = true;
                }
                (State::Open, State::Solved(ty)) => {
                    for g in self.entities[a.0].groups.iter() {
                        if !self.groups[g.0].members.contains(&ty) {
                            CompileError::build(
                                &self.entities[b.0].origin,
                                format_args!(
                                    "Mismatched types, expected group `{}`, found type `{}`",
                                    self.groups[g.0].name, types[ty.0].name,
                                ),
                            )
                            .with_location(&self.entities[a.0].origin)
                            .build()?;
                        }
                    }
                    self.entities[a.0].state = State::Solved(ty);
                    modified = true;
                }
                (State::Solved(a_ty), State::Solved(b_ty)) => {
                    if a_ty != b_ty {
                        CompileError::build(
                            &self.entities[b.0].origin,
                            format_args!(
                                "Mismatched types, expected `{}`, found `{}`",
                                types[a_ty.0].name, types[b_ty.0].name
                            ),
                        )
                        .with_location(&self.entities[a.0].origin)
                        .build()?;
                    }
                }
            }
        }

        Ok(modified)
    }

    pub fn solve(&mut self, types: &[Type]) -> Result<Vec<TypeId>, CompileError> {
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
