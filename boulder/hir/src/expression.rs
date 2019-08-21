use crate::*;

use crate::ty::EntityId;

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Expression<'a, V: IdentifierState, N> {
    Block(N, Meta<'a, ()>, Vec<Expression<'a, V, N>>),
    Variable(N, V::Variable),
    Lit(N, Meta<'a, Literal>),
    Binop(
        N,
        Meta<'a, Binop>,
        Box<Expression<'a, V, N>>,
        Box<Expression<'a, V, N>>,
    ),
    Statement(N, Meta<'a, ()>, Box<Expression<'a, V, N>>),
    Assignment(N, V::Variable, Box<Expression<'a, V, N>>),
    FunctionCall(N, V::Function, Vec<Expression<'a, V, N>>),
}

impl<'a> Expression<'a, UnresolvedIdentifiers<'a>, ()> {
    pub fn resolve_identifiers(
        self,
        variables: &mut Vec<function::Variable<'a, UnresolvedType>>,
        variable_lookup: &mut Vec<Vec<(Box<str>, VariableId)>>,
        function_lookup: &HashMap<Box<str>, Meta<'a, FunctionId>>,
    ) -> Result<Expression<'a, ResolvedIdentifiers<'a>, ()>, CompileError> {
        fn get_id<'b>(
            name: Meta<'b, Box<str>>,
            variable_lookup: &mut Vec<Vec<(Box<str>, VariableId)>>,
        ) -> Result<Meta<'b, VariableId>, CompileError> {
            for scope in variable_lookup.iter().rev() {
                for (var, id) in scope.iter().rev() {
                    if var == &name.item {
                        return Ok(name.replace(id.clone()));
                    }
                }
            }

            CompileError::new(
                &name,
                format_args!("Cannot find value `{}` in this scope", name.item),
            )
        }

        Ok(match self {
            Expression::Block((), meta, expressions) => {
                variable_lookup.push(Vec::new());
                let mut new = Vec::new();
                for expr in expressions {
                    new.push(expr.resolve_identifiers(
                        variables,
                        variable_lookup,
                        function_lookup,
                    )?);
                }
                variable_lookup.pop();
                Expression::Block((), meta, new)
            }
            Expression::Variable((), var) => match var {
                UnresolvedVariable::Existing(name) => {
                    Expression::Variable((), get_id(name, variable_lookup)?)
                }
                UnresolvedVariable::New(name, type_name) => {
                    let id = VariableId(variables.len());
                    let meta = name.simplify();
                    variable_lookup
                        .last_mut()
                        .unwrap()
                        .push((name.item.clone(), id));
                    variables.push(function::Variable {
                        name,
                        ty: type_name.map_or_else(
                            || meta.clone().replace(UnresolvedType::Unknown),
                            |name| name.map(|n| UnresolvedType::Named(n)),
                        ),
                    });
                    Expression::Variable((), meta.replace(id))
                }
            },
            Expression::Lit((), lit) => Expression::Lit((), lit),
            Expression::Binop((), op, rhs, lhs) => Expression::Binop(
                (),
                op,
                Box::new(rhs.resolve_identifiers(variables, variable_lookup, function_lookup)?),
                Box::new(lhs.resolve_identifiers(variables, variable_lookup, function_lookup)?),
            ),
            Expression::Assignment((), var, expr) => {
                let expr = expr.resolve_identifiers(variables, variable_lookup, function_lookup)?;

                let id = match var {
                    UnresolvedVariable::Existing(name) => get_id(name, variable_lookup)?,
                    UnresolvedVariable::New(name, type_name) => {
                        let id = VariableId(variables.len());
                        let meta = name.simplify();
                        variable_lookup
                            .last_mut()
                            .unwrap()
                            .push((name.item.clone(), id));
                        variables.push(function::Variable {
                            name,
                            ty: type_name.map_or_else(
                                || meta.clone().replace(UnresolvedType::Unknown),
                                |name| name.map(|n| UnresolvedType::Named(n)),
                            ),
                        });
                        meta.replace(id)
                    }
                };

                Expression::Assignment((), id, Box::new(expr))
            }
            Expression::Statement((), meta, expr) => Expression::Statement(
                (),
                meta,
                Box::new(expr.resolve_identifiers(variables, variable_lookup, function_lookup)?),
            ),
            Expression::FunctionCall((), name, args) => {
                let mut new = Vec::new();
                for expr in args {
                    variable_lookup.push(Vec::new());
                    new.push(expr.resolve_identifiers(
                        variables,
                        variable_lookup,
                        function_lookup,
                    )?);
                    variable_lookup.pop();
                }

                if let Some(id) = function_lookup.get(&name.item) {
                    Expression::FunctionCall((), name.replace(id.item), new)
                } else {
                    CompileError::new(
                        &name,
                        format_args!("Cannot find function `{}` in this scope", name.item),
                    )?
                }
            }
        })
    }
}

impl<'a> Expression<'a, ResolvedIdentifiers<'a>, ()> {
    pub fn type_constraints(
        self,
        functions: &[FunctionDefinition<'a, TypeId>],
        constraints: &mut ty::Constraints<'a>,
    ) -> Result<Expression<'a, ResolvedIdentifiers<'a>, EntityId>, CompileError> {
        Ok(match self {
            Expression::Block((), meta, v) => {
                let (id, content) = if v.is_empty() {
                    (
                        constraints.add_entity(meta.simplify(), ty::State::Solved(ty::EMPTY_ID)),
                        Vec::new(),
                    )
                } else {
                    let content = v
                        .into_iter()
                        .map(|expr| expr.type_constraints(functions, constraints))
                        .collect::<Result<Vec<_>, CompileError>>()?;
                    let id = content.last().unwrap().id();
                    (id, content)
                };
                Expression::Block(id, meta, content)
            }
            Expression::Variable((), var) => {
                Expression::Variable(constraints.convert_variable_id(var.clone()), var)
            }
            Expression::Lit((), lit) => match &lit.item {
                Literal::Integer(_) => {
                    let id = constraints.add_entity(lit.simplify(), ty::State::Open);
                    constraints.add_membership(id, ty::INTEGER_GROUP_ID);
                    Expression::Lit(id, lit)
                }
            },
            Expression::Binop((), op, a, b) => match op.item {
                Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => {
                    let a = a.type_constraints(functions, constraints)?;
                    constraints.add_membership(a.id(), ty::INTEGER_GROUP_ID);
                    let b = b.type_constraints(functions, constraints)?;
                    constraints.add_membership(b.id(), ty::INTEGER_GROUP_ID);
                    constraints.add_equality(a.id(), b.id());
                    Expression::Binop(a.id(), op, Box::new(a), Box::new(b))
                }
            },
            Expression::Statement((), meta, expr) => {
                let span = expr.span().simplify().append(meta.clone());
                let expr = expr.type_constraints(functions, constraints)?;
                Expression::Statement(
                    constraints.add_entity(span, ty::State::Solved(ty::EMPTY_ID)),
                    meta,
                    Box::new(expr),
                )
            }
            Expression::Assignment((), var, expr) => {
                let span = var.span().simplify().append(expr.span());
                let expr = expr.type_constraints(functions, constraints)?;
                let var_id = constraints.convert_variable_id(var.clone());
                constraints.add_equality(expr.id(), var_id);
                Expression::Assignment(
                    constraints.add_entity(span, ty::State::Solved(ty::EMPTY_ID)),
                    var,
                    Box::new(expr),
                )
            }
            Expression::FunctionCall((), name, args) => {
                let definition = &functions[name.0];
                let args = args
                    .into_iter()
                    .map(|expr| expr.type_constraints(functions, constraints))
                    .collect::<Result<Vec<_>, CompileError>>()?;

                if args.len() != definition.args.len() {
                    let location = args
                        .last()
                        .map_or(name.simplify(), |arg| name.simplify().append(arg.span()))
                        .extend_right(')');
                    if definition.args.len() == 1 {
                        CompileError::build(
                            &definition.name,
                            format_args!(
                                "This function takes 1 parameter but received {}",
                                args.len()
                            ),
                        )
                        .with_location(&location)
                        .build()?
                    } else {
                        CompileError::build(
                            &definition.name,
                            format_args!(
                                "This function takes {} parameters but received {}",
                                definition.args.len(),
                                args.len()
                            ),
                        )
                        .with_location(&location)
                        .build()?
                    }
                }

                for (actual, expected) in args.iter().zip(&definition.args) {
                    let expected = constraints
                        .add_entity(expected.simplify(), ty::State::Solved(expected.item));
                    constraints.add_equality(expected, actual.id());
                }

                let ret =
                    constraints.add_entity(name.simplify(), ty::State::Solved(definition.ty.item));
                Expression::FunctionCall(ret, name, args)
            }
        })
    }
}

impl<'a> Expression<'a, ResolvedIdentifiers<'a>, EntityId> {
    pub fn id(&self) -> EntityId {
        match self {
            &Expression::Block(id, _, _)
            | &Expression::Variable(id, _)
            | &Expression::Lit(id, _)
            | &Expression::Binop(id, _, _, _)
            | &Expression::Statement(id, _, _)
            | &Expression::Assignment(id, _, _)
            | &Expression::FunctionCall(id, _, _) => id,
        }
    }

    pub fn insert_types(self, types: &[TypeId]) -> Expression<'a, ResolvedIdentifiers<'a>, TypeId> {
        match self {
            Expression::Block(id, meta, v) => {
                let ty = types[id.0];
                let content = v
                    .into_iter()
                    .map(|expr| expr.insert_types(types))
                    .collect::<Vec<_>>();
                Expression::Block(ty, meta, content)
            }
            Expression::Variable(id, var) => Expression::Variable(types[id.0], var),
            Expression::Lit(id, lit) => match &lit.item {
                Literal::Integer(_) => Expression::Lit(types[id.0], lit),
            },
            Expression::Binop(id, op, a, b) => match op.item {
                Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => {
                    let a = a.insert_types(types);
                    let b = b.insert_types(types);
                    Expression::Binop(types[id.0], op, Box::new(a), Box::new(b))
                }
            },
            Expression::Statement(id, meta, expr) => {
                let expr = expr.insert_types(types);
                Expression::Statement(types[id.0], meta, Box::new(expr))
            }
            Expression::Assignment(id, var, expr) => {
                let expr = expr.insert_types(types);
                Expression::Assignment(types[id.0], var, Box::new(expr))
            }
            Expression::FunctionCall(id, name, args) => {
                let args = args
                    .into_iter()
                    .map(|expr| expr.insert_types(types))
                    .collect::<Vec<_>>();
                Expression::FunctionCall(types[id.0], name, args)
            }
        }
    }
}

impl<'a> Expression<'a, ResolvedIdentifiers<'a>, TypeId> {
    pub fn to_mir(
        self,
        types: &[mir::Type],
        var_lookup: &mut [Option<mir::StepId>],
        curr: mir::BlockId,
        func: &mut mir::Function,
    ) -> Result<mir::StepId, CompileError> {
        use mir::{Action, Object, Step};
        match self {
            Expression::Block(ty, _, mut v) => {
                // we do not start a new block for an expression block
                // as only control flow requires new blocks
                Ok(if let Some(last) = v.pop() {
                    for e in v {
                        e.to_mir(types, var_lookup, curr, func)?;
                    }

                    last.to_mir(types, var_lookup, curr, func)?
                } else {
                    assert_eq!(ty, ty::EMPTY_ID);
                    func.block(curr)
                        .add_step(Step::new(ty.to_mir(), Action::LoadConstant(Object::Unit)))
                })
            }
            Expression::Variable(ty, var) => {
                if let Some(step) = var_lookup[var.0] {
                    assert_eq!(ty.to_mir(), func.block(curr).get_step(step).ty);
                    Ok(step)
                } else {
                    let span = var.span_str();
                    CompileError::new(
                        &var,
                        format_args!("Use of possibly uninitialized variable: `{}`", span),
                    )
                }
            }
            Expression::Lit(ty, lit) => {
                let type_id = ty.to_mir();
                let ty = &types[type_id.0];
                let obj = match &lit.item {
                    &Literal::Integer(i) => match ty {
                        mir::Type::U8 => u8::try_from(i).map(|i| Object::U8(i)),
                        mir::Type::U16 => u16::try_from(i).map(|i| Object::U16(i)),
                        mir::Type::U32 => u32::try_from(i).map(|i| Object::U32(i)),
                        _ => unreachable!("Unknown literal type: `{:?}`", lit),
                    }
                    .or_else(|_| {
                        CompileError::new(&lit, format_args!("Literal out of range for `{}`", ty))
                    }),
                }?;
                Ok(func
                    .block(curr)
                    .add_step(Step::new(type_id, Action::LoadConstant(obj))))
            }
            Expression::Binop(ty, op, a, b) => {
                let a = a.to_mir(types, var_lookup, curr, func)?;
                let b = b.to_mir(types, var_lookup, curr, func)?;
                Ok(func.block(curr).add_step(Step::new(
                    ty.to_mir(),
                    match op.item {
                        Binop::Add => Action::Add(a, b),
                        Binop::Sub => Action::Sub(a, b),
                        Binop::Mul => Action::Mul(a, b),
                        Binop::Div => Action::Div(a, b),
                    },
                )))
            }
            Expression::Statement(ty, _meta, expr) => {
                assert_eq!(ty, ty::EMPTY_ID);
                expr.to_mir(types, var_lookup, curr, func)?;
                Ok(func
                    .block(curr)
                    .add_step(Step::new(ty.to_mir(), Action::LoadConstant(Object::Unit))))
            }
            Expression::Assignment(ty, var, expr) => {
                let expr = expr.to_mir(types, var_lookup, curr, func)?;
                var_lookup[var.0] = Some(expr);
                assert_eq!(ty, ty::EMPTY_ID);
                Ok(func
                    .block(curr)
                    .add_step(Step::new(ty.to_mir(), Action::LoadConstant(Object::Unit))))
            }
            Expression::FunctionCall(ty, id, args) => {
                let args = args
                    .into_iter()
                    .map(|arg| arg.to_mir(types, var_lookup, curr, func))
                    .collect::<Result<Vec<_>, CompileError>>()?;
                Ok(func.block(curr).add_step(Step::new(
                    ty.to_mir(),
                    Action::CallFunction(id.item.to_mir(), args),
                )))
            }
        }
    }
}

impl<'a, T: IdentifierState, N> diagnostics::Span<'a> for Expression<'a, T, N>
where
    T::Variable: diagnostics::Span<'a>,
    T::Function: diagnostics::Span<'a>,
{
    fn span(&self) -> Meta<'a, ()> {
        match self {
            Expression::Block(_, meta, _) => meta.simplify(),
            Expression::Variable(_, var) => var.span(),
            Expression::Lit(_, lit) => lit.simplify(),
            Expression::Binop(_, _op, a, b) => a.span().append(b.span()),
            Expression::Statement(_, meta, expr) => expr.span().append(meta.clone()),
            Expression::Assignment(_, var, expr) => var.span().simplify().append(expr.span()),
            Expression::FunctionCall(_, name, _args) => name.span(),
        }
    }
}
