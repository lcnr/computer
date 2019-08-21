use std::{collections::{HashSet, HashMap}, mem};

use diagnostics::{CompileError, Meta};

use crate::{
    function::VariableId,
    ty::{Type, TypeId},
};

pub const INTEGER_GROUP_ID: GroupId = GroupId(0);

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EntityId(pub usize);

pub struct Field(Box<str>, Vec<EntityId>);

#[derive(Debug, Clone)]
pub struct Entity<'a> {
    origin: Meta<'a, ()>,
    state: State,
    groups: Vec<GroupId>,
    fields: Vec<FieldId>,
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
struct FieldAccess(EntityId, EntityId, Box<str>);

#[derive(Debug, Clone, Copy)]
pub struct FieldId(usize);

#[derive(Debug, Clone)]
pub enum ConstraintError {
    GroupError(GroupId),
    FieldError(FieldId),
}

#[derive(Debug, Clone)]
pub struct Constraints<'a, 'b: 'a> {
    equalities: Vec<Equality>,
    fields: Vec<FieldAccess>,
    groups: Vec<Group>,
    entities: Vec<Entity<'b>>,
    types: &'a [Type<'b, TypeId>],
}

impl<'a, 'b: 'a> Constraints<'a, 'b> {
    pub fn new(types: &'a [Type<'b, TypeId>]) -> Self {
        Self {
            equalities: Vec::new(),
            fields: Vec::new(),
            groups: Vec::new(),
            entities: Vec::new(),
            types,
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

    pub fn add_field_access(&mut self, obj: EntityId, field: EntityId, name: Box<str>) {
        let id = FieldId(self.fields.len());
        self.fields.push(FieldAccess(obj, field, name));
        self.entities[obj.0].fields.push(id);
        self.entities[field.0].fields.push(id);
    }

    pub fn add_membership(&mut self, e: EntityId, g: GroupId) {
        let groups = &mut self.entities[e.0].groups;
        if !groups.contains(&g) {
            groups.push(g);
        }
    }

    pub fn add_entity(&mut self, meta: Meta<'b, ()>, state: State) -> EntityId {
        let id = EntityId(self.entities.len());
        self.entities.push(Entity {
            origin: meta.simplify(),
            state,
            groups: Vec::new(),
            fields: Vec::new(),
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

    pub fn set_ty(&mut self, entity: EntityId, ty: TypeId) -> Result<(), GroupId> {
        let entity = &mut self.entities[entity.0];
        entity.state = State::Solved(ty);
        let groups = &self.groups;
        entity
            .groups
            .iter()
            .find(|g| !groups[g.0].members.contains(&ty))
            .map_or(Ok(()), |&g| Err(g))
    }

     fn step(&mut self) -> Result<bool, CompileError> {
        let mut modified = false;

        let equalities = mem::replace(&mut self.equalities, Vec::new());
        for &Equality(a, b) in equalities.iter() {
            match (self.get_state(a), self.get_state(b)) {
                (State::Open, State::Open) => (),
                (State::Solved(ty), State::Open) => {
                    self.set_ty(b, ty).or_else(|g| {
                        CompileError::build(
                            &self.entities[b.0].origin,
                            format_args!(
                                "Mismatched types, expected `{}`, found group `{}`",
                                self.types[ty.0].name.item, self.groups[g.0].name
                            ),
                        )
                        .with_location(&self.entities[a.0].origin)
                        .build()
                    })?;
                    modified = true;
                }
                (State::Open, State::Solved(ty)) => {
                    self.set_ty(a, ty).or_else(|g| {
                        CompileError::build(
                            &self.entities[b.0].origin,
                            format_args!(
                                "Mismatched types, expected group `{}`, found type `{}`",
                                self.groups[g.0].name, self.types[ty.0].name.item,
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
                                self.types[a_ty.0].name.item, self.types[b_ty.0].name.item
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

    pub fn solve(&mut self) -> Result<Vec<TypeId>, CompileError> {
        while self.step()? {}

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
