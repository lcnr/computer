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
    Block(N::Type, V::Scope, Vec<Expression<'a, V, N>>),
    Variable(N::Type, V::Variable),
    Lit(N::Type, Meta<'a, Literal<V>>),
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
    Loop(N::Type, V::Scope, Box<Expression<'a, V, N>>),
    Break(N::Type, V::Scope, Box<Expression<'a, V, N>>),
    TypeRestriction(Box<Expression<'a, V, N>>, N::Restriction),
}

pub struct ResolveIdentifiersContext<'a, 'b> {
    pub variables: &'b mut Vec<function::Variable<'a, Option<UnresolvedType<'a>>>>,
    pub variable_lookup: &'b mut Vec<Vec<(Box<str>, VariableId)>>,
    pub function_lookup: &'b HashMap<Box<str>, Meta<'a, FunctionId>>,
    pub scope_lookup: &'b mut Vec<Option<Box<str>>>,
    pub types: &'b mut Vec<Type<'a, TypeId>>,
    pub type_lookup: &'b mut HashMap<Box<str>, TypeId>,
}

impl<'a> Expression<'a, UnresolvedIdentifiers<'a>, UnresolvedTypes<'a>> {
    pub fn resolve_identifiers(
        self,
        ctx: &mut ResolveIdentifiersContext<'a, '_>,
    ) -> Result<Expression<'a, ResolvedIdentifiers<'a>, UnresolvedTypes<'a>>, CompileError> {
        fn get_id<'b>(
            name: Meta<'b, Box<str>>,
            variable_lookup: &mut Vec<Vec<(Box<str>, VariableId)>>,
        ) -> Option<Meta<'b, VariableId>> {
            for scope in variable_lookup.iter().rev() {
                for (var, id) in scope.iter().rev() {
                    if var == &name.item {
                        return Some(name.replace(id.clone()));
                    }
                }
            }
            None
        }

        Ok(match self {
            Expression::Block((), scope, expressions) => {
                ctx.variable_lookup.push(Vec::new());
                let mut new = Vec::new();
                let scope_id = ScopeId(ctx.scope_lookup.len());
                ctx.scope_lookup.push(scope.item.clone());
                for expr in expressions {
                    new.push(expr.resolve_identifiers(ctx)?);
                }
                ctx.variable_lookup.pop();
                Expression::Block((), scope.replace(scope_id), new)
            }
            Expression::Variable((), var) => match var {
                UnresolvedVariable::Existing(name) => {
                    if let Some(id) = get_id(name.clone(), ctx.variable_lookup) {
                        Expression::Variable((), id)
                    } else if let Some(&ty) = ctx.type_lookup.get(&name.item) {
                        if let ty::Kind::Unit = ctx.types[ty.0].kind {
                            Expression::Lit((), name.replace(Literal::Unit(ty)))
                        } else {
                            unimplemented!()
                        }
                    } else {
                        CompileError::new(
                            &name,
                            format_args!("Cannot find value `{}` in this scope", name.item),
                        )?
                    }
                }
                UnresolvedVariable::New(name, type_name) => {
                    let id = VariableId(ctx.variables.len());
                    let lit = name.replace(Literal::Unit(ty::EMPTY_ID));
                    ctx.variable_lookup
                        .last_mut()
                        .unwrap()
                        .push((name.item.clone(), id));
                    ctx.variables.push(function::Variable {
                        name,
                        ty: type_name,
                    });
                    Expression::Lit((), lit)
                }
            },
            Expression::Lit((), lit) => {
                let meta = lit.simplify();
                let lit = match lit.item {
                    Literal::Integer(v) => meta.replace(Literal::Integer(v)),
                    Literal::Unit(_) => unreachable!("Typed literal during identifier resolution"),
                };
                Expression::Lit((), lit)
            }
            Expression::Binop((), op, rhs, lhs) => Expression::Binop(
                (),
                op,
                Box::new(rhs.resolve_identifiers(ctx)?),
                Box::new(lhs.resolve_identifiers(ctx)?),
            ),
            Expression::Assignment((), var, expr) => {
                let expr = expr.resolve_identifiers(ctx)?;

                let id = match var {
                    UnresolvedVariable::Existing(name) => get_id(name.clone(), ctx.variable_lookup)
                        .map_or_else(
                            || {
                                CompileError::new(
                                    &name,
                                    format_args!("Cannot find value `{}` in this scope", name.item),
                                )
                            },
                            |v| Ok(v),
                        )?,
                    UnresolvedVariable::New(name, ty) => {
                        let id = VariableId(ctx.variables.len());
                        let meta = name.simplify();
                        ctx.variable_lookup
                            .last_mut()
                            .unwrap()
                            .push((name.item.clone(), id));
                        ctx.variables.push(function::Variable { name, ty });
                        meta.replace(id)
                    }
                };

                Expression::Assignment((), id, Box::new(expr))
            }
            Expression::Statement((), expr) => {
                Expression::Statement((), Box::new(expr.resolve_identifiers(ctx)?))
            }
            Expression::FunctionCall((), name, args) => {
                let mut new = Vec::new();
                for expr in args {
                    new.push(expr.resolve_identifiers(ctx)?);
                }

                if let Some(id) = ctx.function_lookup.get(&name.item) {
                    Expression::FunctionCall((), name.replace(id.item), new)
                } else {
                    CompileError::new(
                        &name,
                        format_args!("Cannot find function `{}` in this scope", name.item),
                    )?
                }
            }
            Expression::FieldAccess((), obj, field) => {
                Expression::FieldAccess((), Box::new(obj.resolve_identifiers(ctx)?), field)
            }
            Expression::Match((), meta, value, match_arms) => {
                let value = Box::new(value.resolve_identifiers(ctx)?);

                let mut new = Vec::new();
                for arm in match_arms {
                    ctx.variable_lookup.push(Vec::new());
                    let pattern = match arm.pattern {
                        UnresolvedVariable::New(name, ty) => {
                            let id = VariableId(ctx.variables.len());
                            let meta = name.simplify();
                            ctx.variable_lookup
                                .last_mut()
                                .unwrap()
                                .push((name.item.clone(), id));
                            ctx.variables.push(function::Variable { name, ty });
                            meta.replace(id)
                        }
                        err @ UnresolvedVariable::Existing(_) => {
                            unreachable!("invalid match pattern: {:?}", err)
                        }
                    };
                    new.push(MatchArm {
                        pattern,
                        expr: arm.expr.resolve_identifiers(ctx)?,
                    });
                    ctx.variable_lookup.pop();
                }
                Expression::Match((), meta, value, new)
            }
            Expression::Loop((), _, _) => unimplemented!("loop"),
            Expression::Break((), scope, expr) => {
                if let Some(ref s) = &scope.item {
                    for (i, id) in ctx
                        .scope_lookup
                        .iter()
                        .enumerate()
                        .filter_map(|(i, v)| v.as_ref().map(|v| (ScopeId(i), v)))
                    {
                        if s == id {
                            return Ok(Expression::Break(
                                (),
                                scope.replace(i),
                                Box::new(expr.resolve_identifiers(ctx)?),
                            ));
                        }
                    }

                    CompileError::new(
                        &scope,
                        format_args!("Cannot find scope `{}` in this scope", s),
                    )?
                } else {
                    Expression::Break(
                        (),
                        scope.replace(ScopeId(ctx.scope_lookup.len() - 1)),
                        Box::new(expr.resolve_identifiers(ctx)?),
                    )
                }
            }
            Expression::TypeRestriction(expr, ty) => {
                Expression::TypeRestriction(Box::new(expr.resolve_identifiers(ctx)?), ty)
            }
        })
    }
}

pub struct TypeConstraintsContext<'a, 'b, 'c> {
    pub functions: &'c [FunctionDefinition<'a, TypeId>],
    pub variables: &'c [solver::EntityId],
    pub scopes: &'c mut Vec<solver::EntityId>,
    pub solver: &'c mut TypeSolver<'a, 'b>,
}

impl<'a> Expression<'a, ResolvedIdentifiers<'a>, UnresolvedTypes<'a>> {
    pub fn type_constraints<'b, 'c>(
        self,
        ctx: &mut TypeConstraintsContext<'a, 'b, 'c>,
    ) -> Result<Expression<'a, ResolvedIdentifiers<'a>, ResolvingTypes<'a>>, CompileError> {
        Ok(match self {
            Expression::Block((), meta, v) => {
                let (id, content) = if v.is_empty() {
                    (ctx.solver.add_empty(meta.simplify()), Vec::new())
                } else {
                    let id = ctx.solver.add_unbound(meta.simplify());
                    ctx.scopes.push(id);
                    let content = v
                        .into_iter()
                        .map(|expr| expr.type_constraints(ctx))
                        .collect::<Result<Vec<_>, CompileError>>()?;

                    let expr_id = content.last().unwrap().id();
                    ctx.solver.add_equality(id, expr_id);
                    ctx.scopes.pop();
                    (expr_id, content)
                };
                Expression::Block(id, meta, content)
            }
            Expression::Variable((), var) => {
                let id = ctx.solver.add_unbound(var.simplify());
                ctx.solver.add_equality(ctx.variables[var.0], id);
                Expression::Variable(id, var)
            }
            Expression::Lit((), lit) => {
                let meta = lit.simplify();
                match lit.item {
                    Literal::Integer(v) => {
                        let id = ctx.solver.add_integer(meta.clone());
                        Expression::Lit(id, meta.replace(Literal::Integer(v)))
                    }
                    Literal::Unit(t) => {
                        let id = ctx.solver.add_typed(t, meta.simplify());
                        Expression::Lit(id, meta.replace(Literal::Unit(t)))
                    }
                }
            }
            Expression::Binop((), op, a, b) => {
                let a = a.type_constraints(ctx)?;
                let b = b.type_constraints(ctx)?;
                match op.item {
                    Binop::Add | Binop::Sub | Binop::Mul | Binop::Div | Binop::BitOr => {
                        let integer = ctx.solver.add_integer(op.simplify());
                        ctx.solver.add_equality(a.id(), b.id());
                        ctx.solver.add_equality(a.id(), integer);
                        Expression::Binop(a.id(), op, Box::new(a), Box::new(b))
                    }
                    Binop::Lt => {
                        let integer = ctx.solver.add_integer(op.simplify());
                        ctx.solver.add_equality(a.id(), b.id());
                        ctx.solver.add_equality(a.id(), integer);
                        let id = ctx.solver.add_typed(ty::BOOL_ID, op.simplify());
                        Expression::Binop(id, op, Box::new(a), Box::new(b))
                    }
                }
            }
            Expression::Statement((), expr) => {
                let span = expr.span().extend_right(';');
                let expr = expr.type_constraints(ctx)?;
                Expression::Statement(ctx.solver.add_empty(span), Box::new(expr))
            }
            Expression::Assignment((), var, expr) => {
                let span = var.span().simplify().append(expr.span());
                let expr = expr.type_constraints(ctx)?;
                let var_id = ctx.variables[var.0];
                ctx.solver.add_extension(expr.id(), var_id);
                Expression::Assignment(ctx.solver.add_empty(span), var, Box::new(expr))
            }
            Expression::FunctionCall((), name, args) => {
                let definition = &ctx.functions[name.0];
                let args = args
                    .into_iter()
                    .map(|expr| expr.type_constraints(ctx))
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
                    let expected = ctx.solver.add_typed(expected.item, expected.simplify());
                    ctx.solver.add_extension(actual.id(), expected);
                }

                let ret = ctx.solver.add_typed(
                    definition.ty.item,
                    args.last()
                        .map_or(name.simplify(), |a| name.simplify().append(a.span()))
                        .extend_right(')'),
                );
                Expression::FunctionCall(ret, name, args)
            }
            Expression::FieldAccess((), obj, field) => {
                let obj = obj.type_constraints(ctx)?;
                let access = ctx.solver.add_unbound(field.simplify());
                ctx.solver.add_field_access(obj.id(), access, &field)?;
                Expression::FieldAccess(access, Box::new(obj), field)
            }
            Expression::Match((), meta, value, match_arms) => {
                let value = value.type_constraints(ctx)?;
                let result = ctx.solver.add_unbound(meta.clone());
                let match_arms = match_arms
                    .into_iter()
                    .map(|arm| {
                        let id = ctx.variables[arm.pattern.item.0];
                        ctx.solver.add_extension(id, value.id());
                        let expr = arm.expr.type_constraints(ctx)?;
                        ctx.solver.add_extension(expr.id(), result);
                        Ok(MatchArm {
                            pattern: arm.pattern,
                            expr,
                        })
                    })
                    .collect::<Result<_, _>>()?;
                Expression::Match(result, meta, Box::new(value), match_arms)
            }
            Expression::Loop((), _id, _body) => unimplemented!("loop"),
            Expression::Break((), scope_id, expr) => {
                let expr = expr.type_constraints(ctx)?;
                ctx.solver
                    .add_equality(ctx.scopes[scope_id.item.0], expr.id());
                Expression::Break(
                    ctx.solver.add_typed(ty::NEVER_ID, scope_id.simplify()),
                    scope_id,
                    Box::new(expr),
                )
            }
            Expression::TypeRestriction(expr, ty) => {
                let mut expr = expr.type_constraints(ctx)?;
                let meta = ty.simplify();
                let expected = if let UnresolvedType::Integer = &ty.item {
                    ctx.solver.add_integer(meta)
                } else {
                    let solver_ctx = ctx.solver.ctx();

                    let ty = ty::resolve(ty, solver_ctx.types, solver_ctx.type_lookup)?;
                    ctx.solver.add_typed(ty.item, meta)
                };
                ctx.solver.add_extension(expr.id(), expected);
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
            | &Expression::Match(id, _, _, _)
            | &Expression::Loop(id, _, _)
            | &Expression::Break(id, _, _) => id,
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
            | Expression::Match(id, _, _, _)
            | Expression::Loop(id, _, _)
            | Expression::Break(id, _, _) => id,
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
            Expression::Lit(id, lit) => Expression::Lit(type_result[id], lit),
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
            Expression::Loop(_id, _scope, _body) => unimplemented!("loop"),
            Expression::Break(id, scope_id, expr) => Expression::Break(
                type_result[id],
                scope_id,
                Box::new(expr.insert_types(types, type_result)),
            ),
            Expression::TypeRestriction(expr, _) => expr.insert_types(types, type_result),
        }
    }
}

pub struct ToMirContext<'a> {
    pub types: &'a [mir::Type],
    pub variable_types: &'a [TypeId],
    pub var_lookup: &'a mut Vec<Option<mir::StepId>>,
    pub scopes: &'a mut Vec<mir::BlockId>,
    pub curr: &'a mut mir::BlockId,
    pub func: &'a mut mir::Function,
}

impl<'a> Expression<'a, ResolvedIdentifiers<'a>, ResolvedTypes<'a>> {
    pub fn to_mir<'b>(self, ctx: &mut ToMirContext<'b>) -> Result<mir::StepId, CompileError> {
        use mir::{Action, Object, Step};
        match self {
            Expression::Block(ty, _, mut v) => {
                // we do not start a new block for an expression block
                // as only control flow requires new blocks
                Ok(if let Some(last) = v.pop() {
                    let end = ctx.func.add_block();
                    ctx.scopes.push(end);
                    for e in v {
                        e.to_mir(ctx)?;
                    }

                    let last_id = last.to_mir(ctx)?;
                    ctx.scopes.pop();

                    to_mir::initialized_mir_block(
                        end,
                        ctx.variable_types,
                        ctx.var_lookup,
                        ctx.func,
                    );
                    let step = ctx.func.block(end).add_input(ty.to_mir());
                    let mut steps: Vec<mir::StepId> = ctx
                        .var_lookup
                        .iter()
                        .copied()
                        .filter_map(identity)
                        .collect();
                    steps.push(last_id);
                    ctx.func.block(*ctx.curr).add_step(Step::new(
                        ty::NEVER_ID.to_mir(),
                        mir::Action::Goto(end, steps),
                    ));
                    *ctx.curr = end;
                    step
                } else {
                    assert_eq!(ty, ty::EMPTY_ID);
                    ctx.func
                        .block(*ctx.curr)
                        .add_step(Step::new(ty.to_mir(), Action::LoadConstant(Object::Unit)))
                })
            }
            Expression::Variable(ty, var) => {
                if let Some(step) = ctx.var_lookup[var.0] {
                    assert_eq!(ty.to_mir(), ctx.func.block(*ctx.curr).get_step(step).ty);
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
                let ty = &ctx.types[type_id.0];
                let obj = match &lit.item {
                    &Literal::Integer(i) => match ty {
                        mir::Type::U8 => u8::try_from(i).map(|i| Object::U8(i)),
                        mir::Type::U16 => u16::try_from(i).map(|i| Object::U16(i)),
                        mir::Type::U32 => u32::try_from(i).map(|i| Object::U32(i)),
                        ty => unreachable!("Unknown literal type: `{:?}`, {:?}", lit, ty),
                    }
                    .or_else(|_| {
                        CompileError::new(&lit, format_args!("Literal out of range for `{}`", ty))
                    })?,
                    &Literal::Unit(_) => mir::Object::Unit,
                };
                Ok(ctx
                    .func
                    .block(*ctx.curr)
                    .add_step(Step::new(type_id, Action::LoadConstant(obj))))
            }
            Expression::Binop(ty, op, a, b) => {
                let a = a.to_mir(ctx)?;
                let b = b.to_mir(ctx)?;
                Ok(ctx.func.block(*ctx.curr).add_step(Step::new(
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
                expr.to_mir(ctx)?;
                Ok(ctx
                    .func
                    .block(*ctx.curr)
                    .add_step(Step::new(ty.to_mir(), Action::LoadConstant(Object::Unit))))
            }
            Expression::Assignment(ty, var, expr) => {
                let expr = expr.to_mir(ctx)?;
                ctx.var_lookup[var.0] = Some(expr);
                assert_eq!(ty, ty::EMPTY_ID);
                Ok(ctx
                    .func
                    .block(*ctx.curr)
                    .add_step(Step::new(ty.to_mir(), Action::LoadConstant(Object::Unit))))
            }
            Expression::FunctionCall(ty, id, args) => {
                let args = args
                    .into_iter()
                    .map(|arg| arg.to_mir(ctx))
                    .collect::<Result<Vec<_>, CompileError>>()?;
                Ok(ctx.func.block(*ctx.curr).add_step(Step::new(
                    ty.to_mir(),
                    Action::CallFunction(id.item.to_mir(), args),
                )))
            }
            Expression::FieldAccess(ty, obj, field) => {
                let obj = obj.to_mir(ctx)?;
                Ok(ctx.func.block(*ctx.curr).add_step(Step::new(
                    ty.to_mir(),
                    Action::FieldAccess(obj, field.to_mir()),
                )))
            }
            Expression::Match(ty_id, _, value, match_arms) => {
                let old_block = *ctx.curr;

                let value = value.to_mir(ctx)?;

                let mut arms = Vec::new();
                let match_step = mir::StepId(ctx.func.block(*ctx.curr).content.len());
                let mut arms_data = Vec::new();
                for arm in match_arms {
                    let mut available_variables = ctx.var_lookup.to_vec();

                    let mut id = ctx.func.add_block();
                    to_mir::initialized_mir_block(
                        id,
                        ctx.variable_types,
                        &mut available_variables,
                        ctx.func,
                    );
                    let block = ctx.func.block(id);
                    available_variables[arm.pattern.0] =
                        Some(block.add_input(ctx.variable_types[arm.pattern.0].to_mir()));

                    arms.push((ctx.variable_types[arm.pattern.0].to_mir(), id, {
                        let mut args: Vec<mir::StepId> = ctx
                            .var_lookup
                            .iter()
                            .copied()
                            .filter_map(identity)
                            .collect();
                        args.push(match_step);
                        args
                    }));

                    let expr_id = arm.expr.to_mir(&mut ToMirContext {
                        types: ctx.types,
                        variable_types: ctx.variable_types,
                        var_lookup: &mut available_variables,
                        scopes: ctx.scopes,
                        curr: &mut id,
                        func: ctx.func,
                    })?;

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
                    ctx.var_lookup.to_vec()
                };

                let id = ctx.func.add_block();
                to_mir::initialized_mir_block(
                    id,
                    ctx.variable_types,
                    &mut initialized_variables,
                    ctx.func,
                );
                let step = ctx.func.block(id).add_input(ty_id.to_mir());

                arms_data.iter().for_each(|&(block, step, ref vars)| {
                    let block = ctx.func.block(block);
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

                ctx.func.block(old_block).add_step(Step::new(
                    ty::NEVER_ID.to_mir(),
                    mir::Action::Match(value, arms),
                ));
                *ctx.curr = id;
                *ctx.var_lookup = initialized_variables;
                Ok(step)
            }
            Expression::Loop(_ty, _scope, _body) => unimplemented!("loop"),
            Expression::Break(ty, scope_id, expr) => {
                let step = expr.to_mir(ctx)?;
                let mut steps: Vec<mir::StepId> = ctx
                    .var_lookup
                    .iter()
                    .copied()
                    .filter_map(identity)
                    .collect();
                steps.push(step);
                Ok(ctx.func.block(*ctx.curr).add_step(Step::new(
                    ty.to_mir(),
                    mir::Action::Goto(ctx.scopes[scope_id.item.0], steps),
                )))
            }
            Expression::TypeRestriction(_, ()) => unreachable!("type restriction after type check"),
        }
    }
}

impl<'a, T: IdentifierState, N: TypeState> diagnostics::Span<'a> for Expression<'a, T, N>
where
    T::Variable: diagnostics::Span<'a>,
    T::Function: diagnostics::Span<'a>,
    T::Scope: diagnostics::Span<'a>,
    N::Field: diagnostics::Span<'a>,
{
    fn span(&self) -> Meta<'a, ()> {
        match self {
            Expression::Block(_, scope, _) => scope.span().simplify(),
            Expression::Variable(_, var) => var.span(),
            Expression::Lit(_, lit) => lit.simplify(),
            Expression::Binop(_, _op, a, b) => a.span().append(b.span()),
            Expression::Statement(_, expr) => expr.span().extend_right(';'),
            Expression::Assignment(_, var, expr) => var.span().simplify().append(expr.span()),
            Expression::FunctionCall(_, name, _args) => name.span(),
            Expression::FieldAccess(_, _, field) => field.span().extend_left('.'),
            Expression::Match(_, meta, _, _) => meta.clone(),
            Expression::Loop(_, scope, _) => scope.span().simplify(),
            Expression::Break(_, scope, _) => scope.span().simplify(),
            Expression::TypeRestriction(expr, _) => expr.span(),
        }
    }
}
