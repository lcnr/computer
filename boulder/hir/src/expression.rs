use crate::*;

use crate::ty::solver::TypeSolver;

use std::{collections::HashMap, convert::identity};

#[derive(Debug, Clone)]
pub struct MatchArm<'a, V: IdentifierState, N: TypeState> {
    pub pattern: V::Variable,
    pub expr: Expression<'a, V, N>,
}

#[derive(Debug, Clone)]
pub enum Expression<'a, V: IdentifierState, N: TypeState> {
    Block(N::Type, Meta<'a, ()>, Vec<Expression<'a, V, N>>),
    Variable(N::Type, V::Variable),
    Lit(N::Type, Meta<'a, Literal>),
    Binop(
        N::Type,
        Meta<'a, Binop>,
        Box<Expression<'a, V, N>>,
        Box<Expression<'a, V, N>>,
    ),
    Statement(N::Type, Box<Expression<'a, V, N>>),
    Assignment(N::Type, V::Variable, Box<Expression<'a, V, N>>),
    FunctionCall(N::Type, V::Function, Vec<Expression<'a, V, N>>),
    FieldAccess(N::Type, Box<Expression<'a, V, N>>, N::Field),
    Match(
        N::Type,
        Meta<'a, ()>,
        Box<Expression<'a, V, N>>,
        Vec<MatchArm<'a, V, N>>,
    ),
    TypeRestriction(Box<Expression<'a, V, N>>, N::Restriction),
}

impl<'a> Expression<'a, UnresolvedIdentifiers<'a>, UnresolvedTypes<'a>> {
    pub fn resolve_identifiers(
        self,
        variables: &mut Vec<function::Variable<'a, Option<UnresolvedType<'a>>>>,
        variable_lookup: &mut Vec<Vec<(Box<str>, VariableId)>>,
        function_lookup: &HashMap<Box<str>, Meta<'a, FunctionId>>,
    ) -> Result<Expression<'a, ResolvedIdentifiers<'a>, UnresolvedTypes<'a>>, CompileError> {
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
                        ty: type_name,
                    });
                    Expression::Block((), meta.simplify(), Vec::new())
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
                    UnresolvedVariable::New(name, ty) => {
                        let id = VariableId(variables.len());
                        let meta = name.simplify();
                        variable_lookup
                            .last_mut()
                            .unwrap()
                            .push((name.item.clone(), id));
                        variables.push(function::Variable { name, ty });
                        meta.replace(id)
                    }
                };

                Expression::Assignment((), id, Box::new(expr))
            }
            Expression::Statement((), expr) => Expression::Statement(
                (),
                Box::new(expr.resolve_identifiers(variables, variable_lookup, function_lookup)?),
            ),
            Expression::FunctionCall((), name, args) => {
                let mut new = Vec::new();
                for expr in args {
                    new.push(expr.resolve_identifiers(
                        variables,
                        variable_lookup,
                        function_lookup,
                    )?);
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
            Expression::FieldAccess((), obj, field) => Expression::FieldAccess(
                (),
                Box::new(obj.resolve_identifiers(variables, variable_lookup, function_lookup)?),
                field,
            ),
            Expression::Match((), meta, value, match_arms) => {
                let value = Box::new(value.resolve_identifiers(
                    variables,
                    variable_lookup,
                    function_lookup,
                )?);

                let mut new = Vec::new();
                for arm in match_arms {
                    variable_lookup.push(Vec::new());
                    let pattern = match arm.pattern {
                        UnresolvedVariable::New(name, ty) => {
                            let id = VariableId(variables.len());
                            let meta = name.simplify();
                            variable_lookup
                                .last_mut()
                                .unwrap()
                                .push((name.item.clone(), id));
                            variables.push(function::Variable { name, ty });
                            meta.replace(id)
                        }
                        err @ UnresolvedVariable::Existing(_) => {
                            unreachable!("invalid match pattern: {:?}", err)
                        }
                    };
                    new.push(MatchArm {
                        pattern,
                        expr: arm.expr.resolve_identifiers(
                            variables,
                            variable_lookup,
                            function_lookup,
                        )?,
                    });
                    variable_lookup.pop();
                }
                Expression::Match((), meta, value, new)
            }
            Expression::TypeRestriction(expr, ty) => Expression::TypeRestriction(
                Box::new(expr.resolve_identifiers(variables, variable_lookup, function_lookup)?),
                ty,
            ),
        })
    }
}

impl<'a> Expression<'a, ResolvedIdentifiers<'a>, UnresolvedTypes<'a>> {
    pub fn type_constraints<'b>(
        self,
        functions: &[FunctionDefinition<'a, TypeId>],
        variables: &[solver::EntityId],
        solver: &mut TypeSolver<'a, 'b>,
    ) -> Result<Expression<'a, ResolvedIdentifiers<'a>, ResolvingTypes<'a>>, CompileError> {
        Ok(match self {
            Expression::Block((), meta, v) => {
                let (id, content) = if v.is_empty() {
                    (solver.add_empty(meta.simplify()), Vec::new())
                } else {
                    let content = v
                        .into_iter()
                        .map(|expr| expr.type_constraints(functions, variables, solver))
                        .collect::<Result<Vec<_>, CompileError>>()?;
                    let id = content.last().unwrap().id();
                    (id, content)
                };
                Expression::Block(id, meta, content)
            }
            Expression::Variable((), var) => {
                let id = solver.add_unbound(var.simplify());
                solver.add_equality(variables[var.0], id);
                Expression::Variable(id, var)
            }
            Expression::Lit((), lit) => match &lit.item {
                Literal::Integer(_) => {
                    let id = solver.add_integer(lit.simplify());
                    Expression::Lit(id, lit)
                }
            },
            Expression::Binop((), op, a, b) => {
                let a = a.type_constraints(functions, variables, solver)?;
                let b = b.type_constraints(functions, variables, solver)?;
                match op.item {
                    Binop::Add | Binop::Sub | Binop::Mul | Binop::Div | Binop::BitOr => {
                        let integer = solver.add_integer(op.simplify());
                        solver.add_equality(a.id(), b.id());
                        solver.add_equality(a.id(), integer);
                        Expression::Binop(a.id(), op, Box::new(a), Box::new(b))
                    }
                    Binop::Lt => {
                        let integer = solver.add_integer(op.simplify());
                        solver.add_equality(a.id(), b.id());
                        solver.add_equality(a.id(), integer);
                        let id = solver.add_typed(ty::BOOL_ID, op.simplify());
                        Expression::Binop(id, op, Box::new(a), Box::new(b))
                    }
                }
            }
            Expression::Statement((), expr) => {
                let span = expr.span().extend_right(';');
                let expr = expr.type_constraints(functions, variables, solver)?;
                Expression::Statement(solver.add_empty(span), Box::new(expr))
            }
            Expression::Assignment((), var, expr) => {
                let span = var.span().simplify().append(expr.span());
                let expr = expr.type_constraints(functions, variables, solver)?;
                let var_id = variables[var.0];
                solver.add_extension(expr.id(), var_id);
                Expression::Assignment(solver.add_empty(span), var, Box::new(expr))
            }
            Expression::FunctionCall((), name, args) => {
                let definition = &functions[name.0];
                let args = args
                    .into_iter()
                    .map(|expr| expr.type_constraints(functions, variables, solver))
                    .collect::<Result<Vec<_>, CompileError>>()?;

                if args.len() != definition.args.len() {
                    let location = args
                        .last()
                        .map_or(name.simplify(), |arg| name.simplify().append(arg.span()))
                        .extend_right(')');
                    let parameters_str = if definition.args.len() == 1 {
                        "parameter"
                    } else {
                        "parameters"
                    };
                    CompileError::build(
                        &definition.name,
                        format_args!(
                            "This function takes {} {} but received {}",
                            definition.args.len(),
                            parameters_str,
                            args.len()
                        ),
                    )
                    .with_location(&location)
                    .build()?
                }

                for (actual, expected) in args.iter().zip(&definition.args) {
                    let expected = solver.add_typed(expected.item, expected.simplify());
                    solver.add_extension(actual.id(), expected);
                }

                let ret = solver.add_typed(definition.ty.item, args.last().map_or(name.simplify(), |a| name.simplify().append(a.span())).extend_right(')'));
                Expression::FunctionCall(ret, name, args)
            }
            Expression::FieldAccess((), obj, field) => {
                let obj = obj.type_constraints(functions, variables, solver)?;
                let access = solver.add_unbound(field.simplify());
                solver.add_field_access(obj.id(), access, &field)?;
                Expression::FieldAccess(access, Box::new(obj), field)
            }
            Expression::Match((), meta, value, match_arms) => {
                let value = value.type_constraints(functions, variables, solver)?;
                let result = solver.add_unbound(meta.clone());
                let match_arms = match_arms
                    .into_iter()
                    .map(|arm| {
                        let id = variables[arm.pattern.item.0];
                        solver.add_extension(id, value.id());
                        let expr = arm.expr.type_constraints(functions, variables, solver)?;
                        solver.add_extension(expr.id(), result);
                        Ok(MatchArm {
                            pattern: arm.pattern,
                            expr,
                        })
                    })
                    .collect::<Result<_, _>>()?;
                Expression::Match(result, meta, Box::new(value), match_arms)
            }
            Expression::TypeRestriction(expr, ty) => {
                let mut expr = expr.type_constraints(functions, variables, solver)?;
                let meta = ty.simplify();
                let expected = if let UnresolvedType::Integer = &ty.item {
                    solver.add_integer(meta)
                } else {
                    let ctx = solver.ctx();

                    let ty = ty::resolve(ty, ctx.types, ctx.type_lookup)?;
                    solver.add_typed(ty.item, meta)
                };
                solver.add_equality(expr.id(), expected);
                *expr.id_mut() = expected;
                expr
            }
        })
    }
}

impl<'a> Expression<'a, ResolvedIdentifiers<'a>, ResolvingTypes<'a>> {
    pub fn id(&self) -> solver::EntityId {
        match self {
            &Expression::Block(id, _, _)
            | &Expression::Variable(id, _)
            | &Expression::Lit(id, _)
            | &Expression::Binop(id, _, _, _)
            | &Expression::Statement(id, _)
            | &Expression::Assignment(id, _, _)
            | &Expression::FunctionCall(id, _, _)
            | &Expression::FieldAccess(id, _, _)
            | &Expression::Match(id, _, _, _) => id,
            &Expression::TypeRestriction(_, ()) => {
                unreachable!("type restriction after type check")
            }
        }
    }

    pub fn id_mut(&mut self) -> &mut solver::EntityId {
        match self {
            Expression::Block(id, _, _)
            | Expression::Variable(id, _)
            | Expression::Lit(id, _)
            | Expression::Binop(id, _, _, _)
            | Expression::Statement(id, _)
            | Expression::Assignment(id, _, _)
            | Expression::FunctionCall(id, _, _)
            | Expression::FieldAccess(id, _, _)
            | Expression::Match(id, _, _, _) => id,
            Expression::TypeRestriction(_, ()) => unreachable!("type restriction after type check"),
        }
    }

    pub fn insert_types(
        self,
        types: &[Type<'a, TypeId>],
        type_result: &solver::Solution<TypeId>,
    ) -> Expression<'a, ResolvedIdentifiers<'a>, ResolvedTypes<'a>> {
        match self {
            Expression::Block(id, meta, v) => {
                let ty = type_result[id];
                let content = v
                    .into_iter()
                    .map(|expr| expr.insert_types(types, type_result))
                    .collect::<Vec<_>>();
                Expression::Block(ty, meta, content)
            }
            Expression::Variable(id, var) => Expression::Variable(type_result[id], var),
            Expression::Lit(id, lit) => match &lit.item {
                Literal::Integer(_) => Expression::Lit(type_result[id], lit),
            },
            Expression::Binop(id, op, a, b) => {
                let a = a.insert_types(types, type_result);
                let b = b.insert_types(types, type_result);
                Expression::Binop(type_result[id], op, Box::new(a), Box::new(b))
            }
            Expression::Statement(id, expr) => {
                let expr = expr.insert_types(types, type_result);
                Expression::Statement(type_result[id], Box::new(expr))
            }
            Expression::Assignment(id, var, expr) => {
                let expr = expr.insert_types(types, type_result);
                Expression::Assignment(type_result[id], var, Box::new(expr))
            }
            Expression::FunctionCall(id, name, args) => {
                let args = args
                    .into_iter()
                    .map(|expr| expr.insert_types(types, type_result))
                    .collect::<Vec<_>>();
                Expression::FunctionCall(type_result[id], name, args)
            }
            Expression::FieldAccess(id, obj, field) => {
                let obj_ty = type_result[obj.id()];
                Expression::FieldAccess(
                    type_result[id],
                    Box::new(obj.insert_types(types, type_result)),
                    field.map(|field| {
                        types[obj_ty.0]
                            .get_field(&field)
                            .expect("type check: invalid field")
                    }),
                )
            }
            Expression::Match(id, meta, expr, match_arms) => {
                let match_arms = match_arms
                    .into_iter()
                    .map(|arm| MatchArm {
                        pattern: arm.pattern,
                        expr: arm.expr.insert_types(types, type_result),
                    })
                    .collect();
                Expression::Match(
                    type_result[id],
                    meta,
                    Box::new(expr.insert_types(types, type_result)),
                    match_arms,
                )
            }
            Expression::TypeRestriction(expr, _) => expr.insert_types(types, type_result),
        }
    }
}

impl<'a> Expression<'a, ResolvedIdentifiers<'a>, ResolvedTypes<'a>> {
    pub fn to_mir(
        self,
        types: &[mir::Type],
        variable_types: &[TypeId],
        var_lookup: &mut Vec<Option<mir::StepId>>,
        curr: &mut mir::BlockId,
        func: &mut mir::Function,
    ) -> Result<mir::StepId, CompileError> {
        use mir::{Action, Object, Step};
        match self {
            Expression::Block(ty, _, mut v) => {
                // we do not start a new block for an expression block
                // as only control flow requires new blocks
                Ok(if let Some(last) = v.pop() {
                    for e in v {
                        e.to_mir(types, variable_types, var_lookup, curr, func)?;
                    }

                    last.to_mir(types, variable_types, var_lookup, curr, func)?
                } else {
                    assert_eq!(ty, ty::EMPTY_ID);
                    func.block(*curr)
                        .add_step(Step::new(ty.to_mir(), Action::LoadConstant(Object::Unit)))
                })
            }
            Expression::Variable(ty, var) => {
                if let Some(step) = var_lookup[var.0] {
                    assert_eq!(ty.to_mir(), func.block(*curr).get_step(step).ty);
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
                        ty => unreachable!("Unknown literal type: `{:?}`, {:?}", lit, ty),
                    }
                    .or_else(|_| {
                        CompileError::new(&lit, format_args!("Literal out of range for `{}`", ty))
                    }),
                }?;
                Ok(func
                    .block(*curr)
                    .add_step(Step::new(type_id, Action::LoadConstant(obj))))
            }
            Expression::Binop(ty, op, a, b) => {
                let a = a.to_mir(types, variable_types, var_lookup, curr, func)?;
                let b = b.to_mir(types, variable_types, var_lookup, curr, func)?;
                Ok(func.block(*curr).add_step(Step::new(
                    ty.to_mir(),
                    match op.item {
                        Binop::Add => Action::Add(a, b),
                        Binop::Sub => Action::Sub(a, b),
                        Binop::Mul => Action::Mul(a, b),
                        Binop::Div => Action::Div(a, b),
                        Binop::BitOr => Action::BitOr(a, b),
                        Binop::Lt => Action::Lt(a, b),
                    },
                )))
            }
            Expression::Statement(ty, expr) => {
                assert_eq!(ty, ty::EMPTY_ID);
                expr.to_mir(types, variable_types, var_lookup, curr, func)?;
                Ok(func
                    .block(*curr)
                    .add_step(Step::new(ty.to_mir(), Action::LoadConstant(Object::Unit))))
            }
            Expression::Assignment(ty, var, expr) => {
                let expr = expr.to_mir(types, variable_types, var_lookup, curr, func)?;
                var_lookup[var.0] = Some(expr);
                assert_eq!(ty, ty::EMPTY_ID);
                Ok(func
                    .block(*curr)
                    .add_step(Step::new(ty.to_mir(), Action::LoadConstant(Object::Unit))))
            }
            Expression::FunctionCall(ty, id, args) => {
                let args = args
                    .into_iter()
                    .map(|arg| arg.to_mir(types, variable_types, var_lookup, curr, func))
                    .collect::<Result<Vec<_>, CompileError>>()?;
                Ok(func.block(*curr).add_step(Step::new(
                    ty.to_mir(),
                    Action::CallFunction(id.item.to_mir(), args),
                )))
            }
            Expression::FieldAccess(ty, obj, field) => {
                let obj = obj.to_mir(types, variable_types, var_lookup, curr, func)?;
                Ok(func.block(*curr).add_step(Step::new(
                    ty.to_mir(),
                    Action::FieldAccess(obj, field.to_mir()),
                )))
            }
            Expression::Match(ty_id, _, value, match_arms) => {
                let old_block = *curr;

                let value = value.to_mir(types, variable_types, var_lookup, curr, func)?;

                let mut arms = Vec::new();
                let match_step = mir::StepId(func.block(*curr).content.len());
                let mut arms_data = Vec::new();
                for arm in match_arms {
                    let mut available_variables = var_lookup.to_vec();

                    let mut id = to_mir::initialized_mir_block(
                        variable_types,
                        &mut available_variables,
                        func,
                    );
                    let block = func.block(id);
                    available_variables[arm.pattern.0] =
                        Some(block.add_input(variable_types[arm.pattern.0].to_mir()));

                    arms.push((variable_types[arm.pattern.0].to_mir(), id, {
                        let mut args: Vec<mir::StepId> =
                            var_lookup.iter().copied().filter_map(identity).collect();
                        args.push(match_step);
                        args
                    }));

                    let expr_id = arm.expr.to_mir(
                        types,
                        variable_types,
                        &mut available_variables,
                        &mut id,
                        func,
                    )?;

                    arms_data.push((id, expr_id, available_variables));
                }

                let mut initialized_variables = if let Some((first, rest)) = arms_data.split_first()
                {
                    rest.iter().fold(first.2.to_vec(), |mut lookup, arm| {
                        lookup
                            .iter_mut()
                            .zip(&arm.2)
                            .for_each(|(v, arm)| *v = v.and(*arm));
                        lookup
                    })
                } else {
                    var_lookup.to_vec()
                };

                let id =
                    to_mir::initialized_mir_block(variable_types, &mut initialized_variables, func);
                let step = func.block(id).add_input(ty_id.to_mir());

                arms_data.iter().for_each(|&(block, step, ref vars)| {
                    let block = func.block(block);
                    let mut steps: Vec<mir::StepId> = vars
                        .iter()
                        .copied()
                        .zip(initialized_variables.iter().copied())
                        .filter_map(|(v, needed)| needed.and(v))
                        .collect();
                    steps.push(step);
                    block.add_step(Step::new(
                        ty::NEVER_ID.to_mir(),
                        mir::Action::Goto(id, steps),
                    ));
                });

                func.block(old_block).add_step(Step::new(
                    ty::NEVER_ID.to_mir(),
                    mir::Action::Match(value, arms),
                ));
                *curr = id;
                *var_lookup = initialized_variables;
                Ok(step)
            }
            Expression::TypeRestriction(_, ()) => unreachable!("type restriction after type check"),
        }
    }
}

impl<'a, T: IdentifierState, N: TypeState> diagnostics::Span<'a> for Expression<'a, T, N>
where
    T::Variable: diagnostics::Span<'a>,
    T::Function: diagnostics::Span<'a>,
    N::Field: diagnostics::Span<'a>,
{
    fn span(&self) -> Meta<'a, ()> {
        match self {
            Expression::Block(_, meta, _) => meta.simplify(),
            Expression::Variable(_, var) => var.span(),
            Expression::Lit(_, lit) => lit.simplify(),
            Expression::Binop(_, _op, a, b) => a.span().append(b.span()),
            Expression::Statement(_, expr) => expr.span().extend_right(';'),
            Expression::Assignment(_, var, expr) => var.span().simplify().append(expr.span()),
            Expression::FunctionCall(_, name, _args) => name.span(),
            Expression::FieldAccess(_, _, field) => field.span().extend_left('.'),
            Expression::Match(_, meta, _, _) => meta.clone(),
            Expression::TypeRestriction(expr, _) => expr.span(),
        }
    }
}
