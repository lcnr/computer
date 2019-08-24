use diagnostics::{CompileError, Meta};

use std::collections::HashMap;

use crate::{
    expression::Expression, ty, ty::solver::TypeSolver, IdentifierState, ResolvedIdentifiers,
    ResolvedTypes, Type, TypeId, TypeState, UnresolvedIdentifiers, UnresolvedType, UnresolvedTypes,
};

#[derive(Debug, Clone, Copy)]
pub struct VariableId(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct FunctionId(pub usize);

impl FunctionId {
    pub fn to_mir(self) -> mir::FunctionId {
        mir::FunctionId(self.0)
    }
}

#[derive(Debug, Clone)]
pub struct Variable<'a, T> {
    pub name: Meta<'a, Box<str>>,
    pub ty: Meta<'a, T>,
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition<'a, T> {
    pub name: Meta<'a, ()>,
    pub ty: Meta<'a, T>,
    pub args: Vec<Meta<'a, T>>,
}

#[derive(Debug, Clone)]
pub struct Function<'a, V: IdentifierState, N: TypeState, T> {
    pub name: Meta<'a, Box<str>>,
    pub arguments: Vec<VariableId>,
    pub variables: Vec<Variable<'a, T>>,
    pub ret: Meta<'a, T>,
    pub body: Expression<'a, V, N>,
}

impl<'a> Function<'a, UnresolvedIdentifiers<'a>, UnresolvedTypes<'a>, Option<UnresolvedType<'a>>> {
    pub fn new(name: Meta<'a, Box<str>>) -> Self {
        Self {
            name,
            arguments: Vec::new(),
            ret: Meta::fake(Some(UnresolvedType::Named("Empty".into()))),
            variables: Vec::new(),
            body: Expression::Block((), Meta::default(), Vec::new()),
        }
    }

    pub fn add_variable(
        &mut self,
        name: Meta<'a, Box<str>>,
        ty: Meta<'a, Option<UnresolvedType<'a>>>,
    ) -> VariableId {
        let id = VariableId(self.variables.len());
        self.variables.push(Variable { name, ty });
        id
    }

    pub fn add_argument(
        &mut self,
        name: Meta<'a, Box<str>>,
        ty: Meta<'a, Option<UnresolvedType<'a>>>,
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

    pub fn set_return(&mut self, ret: Meta<'a, UnresolvedType<'a>>) {
        self.ret = ret.map(|r| Some(r));
    }

    pub fn set_body(
        &mut self,
        body: Expression<'a, UnresolvedIdentifiers<'a>, UnresolvedTypes<'a>>,
    ) {
        self.body = body;
    }

    pub fn resolve_identifiers(
        mut self,
        function_lookup: &HashMap<Box<str>, Meta<'a, FunctionId>>,
    ) -> Result<
        Function<'a, ResolvedIdentifiers<'a>, UnresolvedTypes<'a>, Option<UnresolvedType<'a>>>,
        CompileError,
    > {
        let mut variable_lookup = Vec::new();
        variable_lookup.push(
            self.variables
                .iter()
                .enumerate()
                .map(|(i, v)| (v.name.item.clone(), VariableId(i)))
                .collect(),
        );

        let body = self.body.resolve_identifiers(
            &mut self.variables,
            &mut variable_lookup,
            function_lookup,
        )?;

        Ok(Function {
            name: self.name,
            arguments: self.arguments,
            ret: self.ret,
            variables: self.variables,
            body,
        })
    }
}

impl<'a> Function<'a, ResolvedIdentifiers<'a>, UnresolvedTypes<'a>, Option<UnresolvedType<'a>>> {
    pub fn definition(
        &self,
        types: &mut Vec<Type<'a, TypeId>>,
        type_lookup: &mut HashMap<Box<str>, TypeId>,
    ) -> Result<FunctionDefinition<'a, TypeId>, CompileError> {
        Ok(FunctionDefinition {
            name: self.name.simplify(),
            ty: ty::resolve(self.ret.clone().map(|t| t.unwrap()), types, type_lookup)?,
            args: self
                .arguments
                .iter()
                .map(|arg| {
                    let variable = &self.variables[arg.0];
                    ty::resolve(variable.ty.clone().map(|t| t.unwrap()), types, type_lookup)
                })
                .collect::<Result<Vec<Meta<'a, TypeId>>, CompileError>>()?,
        })
    }

    pub fn resolve_expr_types<'b>(
        self,
        function_lookup: &[FunctionDefinition<'a, TypeId>],
        types: &mut Vec<Type<'a, TypeId>>,
        type_lookup: &mut HashMap<Box<str>, TypeId>,
    ) -> Result<Function<'a, ResolvedIdentifiers<'a>, ResolvedTypes<'a>, TypeId>, CompileError>
    {
        let ret_ty = ty::resolve(self.ret.clone().map(|t| t.unwrap()), types, type_lookup).unwrap();

        let mut solver = TypeSolver::new(types, type_lookup);
        // constraints must not contain any entities right now,
        // as we want `VariableId`s to be equal to `EntityId`s
        let variables = self
            .variables
            .iter()
            .map(|variable| {
                Ok(match variable.ty.item {
                    Some(UnresolvedType::Sum(_)) => unimplemented!(),
                    Some(UnresolvedType::Integer) => solver.add_integer(variable.ty.simplify()),
                    Some(UnresolvedType::Named(ref name)) => {
                        if let Some(&i) = solver.type_lookup().get(name) {
                            solver.add_typed(i, variable.ty.simplify())
                        } else {
                            CompileError::new(
                                &variable.ty,
                                format_args!("Cannot find type `{}` in this scope", name),
                            )?
                        }
                    }
                    None => solver.add_unconstrained(variable.name.simplify()),
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        let ret = solver.add_typed(ret_ty.item, ret_ty.simplify());

        let body = self
            .body
            .type_constraints(function_lookup, &variables, &mut solver)?;

        solver.add_extension(body.id(), ret);
        let solution = solver.solve()?;

        let body = body.insert_types(types, &solution);
        Ok(Function {
            name: self.name,
            arguments: self.arguments,
            ret: self.ret.replace(solution[ret]),
            variables: self
                .variables
                .into_iter()
                .zip(variables)
                .map(|(v, id)| Variable {
                    name: v.name,
                    ty: v.ty.replace(solution[id]),
                })
                .collect(),
            body: body,
        })
    }
}

impl<'a> Function<'a, ResolvedIdentifiers<'a>, ResolvedTypes<'a>, TypeId> {
    pub fn to_mir(self, types: &[mir::Type]) -> Result<mir::Function, CompileError> {
        let mut func = mir::Function::new();
        let mut start = mir::Block::new();

        let mut variables: Vec<Option<mir::StepId>> =
            std::iter::repeat(None).take(self.variables.len()).collect();
        for (i, arg) in self.arguments.iter().enumerate() {
            let id = start.add_input(self.variables[arg.0].ty.to_mir());
            variables[i] = Some(id);
        }

        let id = func.add_block(start);

        let ret = self.body.to_mir(types, &mut variables, id, &mut func)?;
        func.block(id).add_step(mir::Step::new(
            ty::NEVER_ID.to_mir(),
            mir::Action::Return(ret),
        ));

        Ok(func)
    }
}
