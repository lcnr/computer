use tindex::{TSlice, TVec};

use shared_id::{TypeId, BOOL_TYPE_ID, EMPTY_TYPE_ID, NEVER_TYPE_ID};

use diagnostics::{CompileError, Span};

use crate::{
    expr::{Binop, Expression, MatchArm, UnaryOperation},
    func::{FunctionDefinition, ScopeId, VariableId},
    traits::{ResolvedIdentifiers, ResolvedTypes, ResolvingTypes, UnresolvedTypes},
    ty::{self, solver::TypeSolver, Type},
    FunctionId, Literal, Pattern, UnresolvedType,
};

pub struct TypeConstraintsContext<'a, 'b, 'c> {
    pub functions: &'c TSlice<FunctionId, FunctionDefinition<'a, TypeId>>,
    pub variables: &'c TSlice<VariableId, solver::EntityId>,
    pub scopes: &'c mut TVec<ScopeId, (solver::EntityId, usize)>,
    pub solver: &'c mut TypeSolver<'a, 'b>,
}

impl<'a> Expression<'a, ResolvedIdentifiers<'a>, UnresolvedTypes<'a>> {
    pub fn type_constraints<'b, 'c>(
        self,
        ctx: &mut TypeConstraintsContext<'a, 'b, 'c>,
    ) -> Result<Expression<'a, ResolvedIdentifiers<'a>, ResolvingTypes<'a>>, CompileError> {
        Ok(match self {
            Expression::Block((), meta, v) => {
                let id = ctx.solver.add_unbound(meta.simplify());
                ctx.scopes.push((id, 0));
                let content = v
                    .into_iter()
                    .map(|expr| expr.type_constraints(ctx))
                    .collect::<Result<Vec<_>, CompileError>>()?;

                let expr_id = if let Some(last) = content.last() {
                    last.id()
                } else {
                    ctx.solver.add_typed(EMPTY_TYPE_ID, meta.simplify())
                };

                let (_, count) = ctx.scopes.pop().unwrap();
                if count == 0 && meta.item != ScopeId::from(0) {
                    ctx.solver.add_equality(expr_id, id);
                } else {
                    ctx.solver.add_extension(expr_id, id);
                }

                Expression::Block(id, meta, content)
            }
            Expression::Variable((), var) => {
                let id = ctx.solver.add_unbound(var.simplify());
                ctx.solver.add_equality(ctx.variables[var.item], id);
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
            Expression::UnaryOperation((), op, expr) => {
                let expr = expr.type_constraints(ctx)?;
                match op.item {
                    UnaryOperation::Invert => {
                        let mut possible_types = ctx.solver.integers().clone();
                        possible_types.add(BOOL_TYPE_ID);
                        let v = ctx.solver.add_bound(possible_types, op.simplify());
                        ctx.solver.add_extension(expr.id(), v);
                        Expression::UnaryOperation(v, op, Box::new(expr))
                    }
                }
            }
            Expression::Binop((), op, a, b) => {
                let a = a.type_constraints(ctx)?;
                let b = b.type_constraints(ctx)?;
                match op.item {
                    Binop::Add | Binop::Sub | Binop::Mul | Binop::Div | Binop::Shl | Binop::Shr => {
                        let integer = ctx.solver.add_integer(op.simplify());
                        ctx.solver.add_equality(a.id(), b.id());
                        ctx.solver.add_equality(a.id(), integer);
                        Expression::Binop(a.id(), op, Box::new(a), Box::new(b))
                    }
                    Binop::BitOr | Binop::BitAnd => {
                        let mut possible_types = ctx.solver.integers().clone();
                        possible_types.add(BOOL_TYPE_ID);
                        let v = ctx.solver.add_bound(possible_types, op.simplify());
                        ctx.solver.add_extension(a.id(), v);
                        ctx.solver.add_extension(b.id(), v);
                        Expression::Binop(v, op, Box::new(a), Box::new(b))
                    }
                    Binop::Eq | Binop::Neq => {
                        let mut possible_types = ctx.solver.integers().clone();
                        possible_types.add(BOOL_TYPE_ID);
                        let v = ctx.solver.add_bound(possible_types, op.simplify());
                        ctx.solver.add_extension(a.id(), v);
                        ctx.solver.add_extension(b.id(), v);
                        let v = ctx.solver.add_typed(BOOL_TYPE_ID, op.simplify());
                        Expression::Binop(v, op, Box::new(a), Box::new(b))
                    }
                    Binop::Gte | Binop::Gt | Binop::Lt | Binop::Lte => {
                        let integer = ctx.solver.add_integer(op.simplify());
                        ctx.solver.add_equality(a.id(), b.id());
                        ctx.solver.add_equality(a.id(), integer);
                        let id = ctx.solver.add_typed(BOOL_TYPE_ID, op.simplify());
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
                let var_id = ctx.variables[var.item];
                ctx.solver.add_extension(expr.id(), var_id);
                Expression::Assignment(ctx.solver.add_empty(span), var, Box::new(expr))
            }
            Expression::InitializeStruct((), kind, fields) => {
                let res = ctx.solver.add_typed(kind.item, kind.simplify());
                let fields = fields
                    .into_iter()
                    .map(|(name, expr)| {
                        let expr = expr.type_constraints(ctx)?;
                        let extended = ctx.solver.add_unbound(name.simplify());
                        ctx.solver.add_extension(expr.id(), extended);
                        ctx.solver.add_field_access(res, extended, &name)?;
                        Ok((name, expr))
                    })
                    .collect::<Result<Vec<_>, CompileError>>()?;
                Expression::InitializeStruct(res, kind, fields)
            }
            Expression::FunctionCall((), name, args) => {
                let definition = &ctx.functions[name.item];
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
                        let id = match &arm.pattern {
                            Pattern::Named(named) => ctx.variables[named.item],
                            Pattern::Underscore(ty) => ctx.solver.add_typed(ty.item, ty.simplify()),
                        };

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
                ctx.scopes.push((id, 0));
                let content = v
                    .into_iter()
                    .map(|expr| expr.type_constraints(ctx))
                    .collect::<Result<Vec<_>, CompileError>>()?;

                if ctx.scopes.pop().unwrap().1 == 0 {
                    ctx.solver
                        .override_entity(id, NEVER_TYPE_ID, meta.simplify());
                };
                Expression::Loop(id, meta, content)
            }
            Expression::Break((), scope_id, expr) => {
                let expr = expr.type_constraints(ctx)?;
                ctx.solver
                    .add_extension(expr.id(), ctx.scopes[scope_id.item].0);
                ctx.scopes[scope_id.item].1 += 1;

                Expression::Break(
                    ctx.solver.add_typed(NEVER_TYPE_ID, scope_id.simplify()),
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
            | &Expression::UnaryOperation(id, _, _)
            | &Expression::Binop(id, _, _, _)
            | &Expression::Statement(id, _)
            | &Expression::Assignment(id, _, _)
            | &Expression::InitializeStruct(id, _, _)
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
            | Expression::UnaryOperation(id, _, _)
            | Expression::Binop(id, _, _, _)
            | Expression::Statement(id, _)
            | Expression::Assignment(id, _, _)
            | Expression::InitializeStruct(id, _, _)
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
        types: &TSlice<TypeId, Type<'a, TypeId>>,
        type_result: &TSlice<solver::EntityId, TypeId>,
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
            Expression::UnaryOperation(id, op, expr) => {
                let expr = expr.insert_types(types, type_result);
                Expression::UnaryOperation(type_result[id], op, Box::new(expr))
            }
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
            Expression::InitializeStruct(id, kind, fields) => {
                let struct_ty = type_result[id];
                let fields = fields
                    .into_iter()
                    .map(|(field, expr)| {
                        (
                            field.map(|field| {
                                types[struct_ty]
                                    .get_field(&field)
                                    .expect("type check: invalid field")
                            }),
                            expr.insert_types(types, type_result),
                        )
                    })
                    .collect();
                Expression::InitializeStruct(struct_ty, kind, fields)
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
                        types[obj_ty]
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

impl<'a> Expression<'a, ResolvedIdentifiers<'a>, ResolvedTypes<'a>> {
    pub fn ty(&self) -> TypeId {
        match self {
            &Expression::Block(ty, _, _)
            | &Expression::Variable(ty, _)
            | &Expression::Lit(ty, _)
            | &Expression::UnaryOperation(ty, _, _)
            | &Expression::Binop(ty, _, _, _)
            | &Expression::Statement(ty, _)
            | &Expression::Assignment(ty, _, _)
            | &Expression::InitializeStruct(ty, _, _)
            | &Expression::FunctionCall(ty, _, _)
            | &Expression::FieldAccess(ty, _, _)
            | &Expression::Match(ty, _, _, _)
            | &Expression::Loop(ty, _, _)
            | &Expression::Break(ty, _, _) => ty,
            &Expression::TypeRestriction(_, ()) => {
                unreachable!("type restriction after type check")
            }
        }
    }
}
