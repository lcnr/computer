use boulder_core::{CompileError, Meta};

use std::collections::HashMap;

use crate::{expression::Expression, Context, Type, UnresolvedType};

#[derive(Debug, Clone, Copy)]
pub struct VariableId(pub usize);

#[derive(Debug, Clone)]
pub struct Variable<'a, T> {
    pub name: Meta<'a, Box<str>>,
    pub ty: Meta<'a, T>,
}

#[derive(Debug, Clone)]
pub struct Function<'a, V, T> {
    pub name: Meta<'a, Box<str>>,
    pub arguments: Vec<VariableId>,
    pub variables: Vec<Variable<'a, T>>,
    pub ret: Meta<'a, T>,
    pub body: Expression<'a, V, T>,
}

impl<'a> Function<'a, Box<str>, UnresolvedType> {
    pub fn new(name: Meta<'a, Box<str>>) -> Self {
        Self {
            name,
            arguments: Vec::new(),
            ret: Meta::<()>::default().replace(UnresolvedType::Unknown),
            variables: Vec::new(),
            body: Expression::Block(Meta::default(), Vec::new()),
        }
    }

    fn add_variable(
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
        if let Some(arg) = self.variables.iter().find(|v| v.name.item == name.item) {
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

    pub fn set_body(&mut self, body: Expression<'a, Box<str>, UnresolvedType>) {
        self.body = body;
    }

    pub fn build(mut self) -> Result<Function<'a, VariableId, UnresolvedType>, CompileError> {
        let mut variable_lookup = Vec::new();
        variable_lookup.push(self.variables.iter().enumerate().map(|(i, v)| (v.name.item.clone(), VariableId(i))).collect());

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

    //pub fn type_ck(&self, ctx: &mut Vec<Context>) -> Result<(), CompileError> {
    //    unimplemented!()
    //}
}
