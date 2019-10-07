use tindex::{TSlice, TVec};

use shared_id::{FunctionId, TypeId};

use diagnostics::{CompileError, Meta};

use crate::{
    expr::{Expression, ResolveIdentifiersContext, ToMirContext, TypeConstraintsContext},
    module::Module,
    traits::{
        IdentifierState, ResolvedIdentifiers, ResolvedTypes, TypeState, UnresolvedIdentifiers,
        UnresolvedTypes,
    },
    ty::{self, solver::TypeSolver},
    Attribute, Type, UnresolvedType,
};

mod index;

#[derive(Debug, Clone, Copy)]
pub struct VariableId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ScopeId(usize);

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
    pub at: Vec<Box<str>>,
    pub attributes: Vec<Attribute<'a>>,
    pub arguments: Vec<VariableId>,
    pub variables: TVec<VariableId, Variable<'a, T>>,
    pub ret: Meta<'a, T>,
    pub body: Expression<'a, V, N>,
}

impl<'a> Function<'a, UnresolvedIdentifiers<'a>, UnresolvedTypes<'a>, Option<UnresolvedType<'a>>> {
    pub fn new(name: Meta<'a, Box<str>>, at: Vec<Box<str>>) -> Self {
        let ret_meta = name.simplify();
        Self {
            name,
            at,
            attributes: Vec::new(),
            arguments: Vec::new(),
            ret: ret_meta.replace(Some(UnresolvedType::Named("Empty".into()))),
            variables: TVec::new(),
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
        mut body: Expression<'a, UnresolvedIdentifiers<'a>, UnresolvedTypes<'a>>,
    ) {
        if let Expression::Block(a, name, b) = body {
            body = Expression::Block(
                a,
                name.map(|e| {
                    assert!(e.is_none());
                    Some("fn".into())
                }),
                b,
            );
        } else {
            unimplemented!("function with a non block as body type: {:?}", body);
        }
        self.body = body;
    }

    pub fn resolve_identifiers(
        mut self,
        types: &mut TVec<TypeId, Type<'a, TypeId>>,
        modules: &mut Module<'a>,
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

        let mut scope_lookup = TVec::new();

        let body = self
            .body
            .resolve_identifiers(&mut ResolveIdentifiersContext {
                at: &self.at,
                variables: &mut self.variables,
                variable_lookup: &mut variable_lookup,
                scope_lookup: &mut scope_lookup,
                types,
                modules,
            })?;

        Ok(Function {
            name: self.name,
            at: self.at,
            attributes: self.attributes,
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
        types: &mut TVec<TypeId, Type<'a, TypeId>>,
        modules: &mut Module,
    ) -> Result<FunctionDefinition<'a, TypeId>, CompileError> {
        Ok(FunctionDefinition {
            name: self.name.simplify(),
            ty: ty::resolve(
                &self.at,
                self.ret.clone().map(|t| t.unwrap()),
                types,
                modules,
            )?,
            args: self
                .arguments
                .iter()
                .map(|&arg| {
                    let variable = &self.variables[arg];
                    ty::resolve(
                        &self.at,
                        variable.ty.clone().map(|t| t.unwrap()),
                        types,
                        modules,
                    )
                })
                .collect::<Result<Vec<Meta<'a, TypeId>>, CompileError>>()?,
        })
    }

    pub fn resolve_expr_types<'b>(
        self,
        function_definitions: &TSlice<FunctionId, FunctionDefinition<'a, TypeId>>,
        types: &mut TVec<TypeId, Type<'a, TypeId>>,
        modules: &mut Module<'a>,
    ) -> Result<Function<'a, ResolvedIdentifiers<'a>, ResolvedTypes<'a>, TypeId>, CompileError>
    {
        let ret_ty = ty::resolve(
            &self.at,
            self.ret.clone().map(|t| t.unwrap()),
            types,
            modules,
        )
        .unwrap();

        let mut solver = TypeSolver::new(types, modules);
        // constraints must not contain any entities right now,
        // as we want `VariableId`s to be equal to `EntityId`s
        let variables = self
            .variables
            .iter()
            .map(|variable| {
                Ok(match &variable.ty.item {
                    Some(unresolved @ UnresolvedType::Sum(_))
                    | Some(unresolved @ UnresolvedType::Named(_)) => {
                        let ctx = solver.ctx();
                        let ty = ty::resolve(
                            &self.at,
                            variable.ty.clone().replace(unresolved.clone()),
                            ctx.types,
                            ctx.modules,
                        )?;
                        solver.add_typed(ty.item, ty.simplify())
                    }
                    Some(UnresolvedType::Integer) => solver.add_integer(variable.ty.simplify()),
                    None => solver.add_unbound(variable.name.simplify()),
                })
            })
            .collect::<Result<TVec<_, _>, _>>()?;

        let body = self.body.type_constraints(&mut TypeConstraintsContext {
            at: &self.at,
            functions: function_definitions,
            variables: &variables,
            scopes: &mut TVec::new(),
            solver: &mut solver,
        })?;
        let body_id = body.id();
        solver.override_entity(body_id, ret_ty.item, ret_ty.simplify());
        let solution = solver.solve()?;

        let body = body.insert_types(types, &solution)?;
        Ok(Function {
            name: self.name,
            at: self.at,
            attributes: self.attributes,
            arguments: self.arguments,
            ret: self.ret.replace(solution[body_id]),
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
    pub fn definition(&self) -> FunctionDefinition<'a, TypeId> {
        FunctionDefinition {
            name: self.name.simplify(),
            ty: self.ret.clone(),
            args: self
                .arguments
                .iter()
                .map(|&arg| self.variables[arg].ty.clone())
                .collect(),
        }
    }

    pub fn to_mir(
        self,
        types: &TSlice<TypeId, mir::Type>,
        function_definitions: &TSlice<FunctionId, FunctionDefinition<'a, TypeId>>,
    ) -> Result<mir::Function, CompileError> {
        let mut func = mir::Function::new(
            self.name.item,
            self.attributes
                .into_iter()
                .map(|v| v.name.item.into())
                .collect(), // TODO: mir attributes
            self.ret.item,
        );
        let mut id = func.add_block();
        let start = &mut func[id];

        let mut variables: TVec<VariableId, Option<mir::StepId>> =
            std::iter::repeat(None).take(self.variables.len()).collect();
        for (i, &arg) in self.arguments.iter().enumerate() {
            let i = VariableId(i);
            let id = start.add_input(self.variables[arg].ty.item);
            variables[i] = Some(id);
        }

        let variable_types: TVec<VariableId, TypeId> =
            self.variables.iter().map(|v| v.ty.item).collect();

        let ret = self.body.to_mir(&mut ToMirContext {
            types,
            variable_types: &variable_types,
            function_definitions,
            var_lookup: &mut variables,
            scopes: &mut TVec::new(),
            temporaries: &mut Vec::new(),
            curr: &mut id,
            func: &mut func,
        })?;
        func[id].add_terminator(mir::Terminator::Return(ret));

        Ok(func)
    }
}
