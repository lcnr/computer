use diagnostics::{CompileError, Meta};

use std::collections::HashMap;

use crate::{expression::Expression, ty, Type, TypeId, UnresolvedType, UnresolvedVariable};

#[derive(Debug, Clone, Copy)]
pub struct VariableId(pub usize);

#[derive(Debug, Clone)]
pub struct Variable<'a, T> {
    pub name: Meta<'a, Box<str>>,
    pub ty: Meta<'a, T>,
}

#[derive(Debug, Clone)]
pub struct Function<'a, V, T, N> {
    pub name: Meta<'a, Box<str>>,
    pub arguments: Vec<VariableId>,
    pub variables: Vec<Variable<'a, T>>,
    pub ret: Meta<'a, T>,
    pub body: Expression<'a, V, N>,
}

impl<'a> Function<'a, UnresolvedVariable<'a>, UnresolvedType, ()> {
    pub fn new(name: Meta<'a, Box<str>>) -> Self {
        Self {
            name,
            arguments: Vec::new(),
            ret: Meta::<()>::default().replace(UnresolvedType::Unknown),
            variables: Vec::new(),
            body: Expression::Block((), Meta::default(), Vec::new()),
        }
    }

    pub fn add_variable(
        &mut self,
        name: Meta<'a, Box<str>>,
        ty: Meta<'a, UnresolvedType>,
    ) -> VariableId {
        let id = VariableId(self.variables.len());
        self.variables.push(Variable { name, ty });
        id
    }

    pub fn add_argument(
        &mut self,
        name: Meta<'a, Box<str>>,
        ty: Meta<'a, UnresolvedType>,
    ) -> Result<(), CompileError> {
        if self.variables.iter().any(|v| v.name.item == name.item) {
            CompileError::new(
                &name,
                format_args!(
                    "Identifier `{}` is bound more than once in this parameter list",
                    name.item
                ),
            )?
        }
        let id = self.add_variable(name, ty);
        self.arguments.push(id);
        Ok(())
    }

    pub fn set_return(&mut self, ret: Meta<'a, Box<str>>) {
        self.ret = ret.map(|r| UnresolvedType::Named(r));
    }

    pub fn set_body(&mut self, body: Expression<'a, UnresolvedVariable<'a>, ()>) {
        self.body = body;
    }

    pub fn resolve_variables(
        mut self,
    ) -> Result<Function<'a, Meta<'a, VariableId>, UnresolvedType, ()>, CompileError> {
        let mut variable_lookup = Vec::new();
        variable_lookup.push(
            self.variables
                .iter()
                .enumerate()
                .map(|(i, v)| (v.name.item.clone(), VariableId(i)))
                .collect(),
        );

        let body = self
            .body
            .resolve_variables(&mut self.variables, &mut variable_lookup)?;

        Ok(Function {
            name: self.name,
            arguments: self.arguments,
            ret: self.ret,
            variables: self.variables,
            body,
        })
    }
}

impl<'a> Function<'a, Meta<'a, VariableId>, UnresolvedType, ()> {
    pub fn resolve_types(
        self,
        types: &[Type],
    ) -> Result<Function<'a, Meta<'a, VariableId>, TypeId, TypeId>, CompileError> {
        let mut constraints = ty::Constraints::new();
        let integers = constraints.add_group(
            types
                .iter()
                .enumerate()
                .filter(|(_, t)| [ty::Kind::U8, ty::Kind::U16, ty::Kind::U32].contains(&t.kind))
                .fold(ty::Group::new("Integers".into()), |g, (i, _)| {
                    g.with_member(TypeId(i))
                }),
        );

        assert_eq!(integers, ty::INTEGER_GROUP_ID);

        let lookup = types
            .iter()
            .enumerate()
            .map(|(i, t)| (&t.name, i))
            .collect::<HashMap<_, _>>();

        // constraints must not contain any entities right now,
        // as we want `VariableId`s to be equal to `EntityId`s
        assert_eq!(constraints.entity_count(), 0);
        for variable in self.variables.iter() {
            let id = constraints.add_entity(variable.name.simplify(), ty::State::Open);
            match variable.ty.item {
                UnresolvedType::Integer => constraints.add_membership(id, integers),
                UnresolvedType::Named(ref name) => {
                    if let Some(&i) = lookup.get(&name) {
                        constraints.set_state(id, ty::State::Solved(TypeId(i)));
                    } else {
                        CompileError::new(
                            &variable.ty,
                            format_args!("Cannot find type `{}` in this scope", name),
                        )?;
                    }
                }
                UnresolvedType::Unknown => (),
            }
        }

        let id = constraints.add_entity(self.ret.simplify(), ty::State::Open);
        match &self.ret.item {
            UnresolvedType::Integer => constraints.add_membership(id, integers),
            UnresolvedType::Named(ref name) => {
                if let Some(&i) = lookup.get(&name) {
                    constraints.set_state(id, ty::State::Solved(TypeId(i)));
                } else {
                    CompileError::new(
                        &self.ret,
                        format_args!("Cannot find type `{}` in this scope", name),
                    )?;
                }
            }
            UnresolvedType::Unknown => (),
        }
        let body = self.body.type_constraints(&mut constraints);
        constraints.add_equality(id, body);

        let entities = constraints.solve(types)?;
        /*Ok(Function {
            name: self.name,
            arguments: self.arguments,
            ret: self.ret.replace(entities[self.variables.len()]),
            variables: self
                .variables
                .into_iter()
                .zip(entities)
                .map(|(v, t)| Variable {
                    name: v.name,
                    ty: v.ty.replace(t),
                })
                .collect(),
            body: self.body,
        });*/
        unimplemented!()
    }
}

impl<'a> Function<'a, Meta<'a, VariableId>, TypeId, TypeId> {
    pub fn to_mir(self, types: &[mir::Type]) -> Result<mir::Function, CompileError> {
        let mut func = mir::Function::new();
        let mut start = mir::Block::new();

        let mut variables: Vec<Option<mir::StepId>> =
            std::iter::repeat(None).take(self.variables.len()).collect();
        for (i, arg) in self.arguments.iter().enumerate() {
            let id = start.add_input(self.variables[arg.0].ty.to_mir());
            variables[i] = Some(id);
        }

        func.content.push(start);

        Ok(dbg!(func))
    }
}
