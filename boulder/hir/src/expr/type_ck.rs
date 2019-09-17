use diagnostics::{CompileError, Span};

use crate::{
    expr::{Expression, MatchArm},
    func::FunctionDefinition,
    ty::{self, solver::TypeSolver, Type, TypeId},
    Binop, Literal, ResolvedIdentifiers, ResolvedTypes, ResolvingTypes, UnresolvedType,
    UnresolvedTypes,
};

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
            Expression::Loop((), meta, v) => {
                if v.is_empty() {
                    CompileError::new(
                        &meta,
                        "Provably endless loop, consider adding a `break` statement",
                    )?
                }

                let id = ctx.solver.add_unbound(meta.simplify());
                ctx.scopes.push(id);
                let content = v
                    .into_iter()
                    .map(|expr| expr.type_constraints(ctx))
                    .collect::<Result<Vec<_>, CompileError>>()?;

                ctx.scopes.pop();
                Expression::Loop(id, meta, content)
            }
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
            Expression::Loop(id, meta, content) => {
                let ty = type_result[id];
                let content = content
                    .into_iter()
                    .map(|expr| expr.insert_types(types, type_result))
                    .collect::<Vec<_>>();
                Expression::Loop(ty, meta, content)
            }
            Expression::Break(id, scope_id, expr) => Expression::Break(
                type_result[id],
                scope_id,
                Box::new(expr.insert_types(types, type_result)),
            ),
            Expression::TypeRestriction(expr, _) => expr.insert_types(types, type_result),
        }
    }
}
