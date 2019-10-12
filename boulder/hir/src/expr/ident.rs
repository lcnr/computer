use tindex::TVec;

use shared_id::{TypeId, EMPTY_TYPE_ID};

use diagnostics::{CompileError, Meta};

use crate::{
    expr::{Expression, MatchArm},
    func::{ScopeId, Variable, VariableId},
    module::Module,
    traits::{ResolvedIdentifiers, UnresolvedIdentifiers, UnresolvedTypes},
    ty::{self, Type},
    Literal, Pattern, UnresolvedType, UnresolvedVariable,
};

pub struct ResolveIdentifiersContext<'a, 'b> {
    pub at: &'b [&'a str],
    pub variables: &'b mut TVec<VariableId, Variable<'a, Option<UnresolvedType<'a>>>>,
    pub variable_lookup: &'b mut Vec<Vec<(&'a str, VariableId)>>,
    pub scope_lookup: &'b mut TVec<ScopeId, Option<&'a str>>,
    pub types: &'b mut TVec<TypeId, Type<'a, TypeId>>,
    pub modules: &'b mut Module<'a>,
}

impl<'a> Expression<'a, UnresolvedIdentifiers<'a>, UnresolvedTypes<'a>> {
    pub fn resolve_identifiers(
        self,
        ctx: &mut ResolveIdentifiersContext<'a, '_>,
    ) -> Result<Expression<'a, ResolvedIdentifiers<'a>, UnresolvedTypes<'a>>, CompileError> {
        #[cfg(feature = "profiler")]
        profile_scope!("resolve_identifiers");
        fn get_id<'b>(
            name: Meta<'b, &'b str>,
            variable_lookup: &mut Vec<Vec<(&'b str, VariableId)>>,
        ) -> Option<Meta<'b, VariableId>> {
            for scope in variable_lookup.iter().rev() {
                for &(ref var, id) in scope.iter().rev() {
                    if var == &name.item {
                        return Some(name.replace(id));
                    }
                }
            }
            None
        }

        Ok(match self {
            Expression::Block((), scope, expressions) => {
                ctx.variable_lookup.push(Vec::new());
                let mut new = Vec::new();
                let scope_id = ctx.scope_lookup.push(scope.item);
                for expr in expressions {
                    new.push(expr.resolve_identifiers(ctx)?);
                }
                ctx.scope_lookup.pop();
                ctx.variable_lookup.pop();
                Expression::Block((), scope.replace(scope_id), new)
            }
            Expression::Variable((), var) => match var {
                UnresolvedVariable::Existing(name) => {
                    if let Some(id) = get_id(name.clone(), ctx.variable_lookup) {
                        Expression::Variable((), id)
                    } else if let Some(ty) =
                        ctx.modules.get_type(ctx.at, &Box::<str>::from(name.item))
                    {
                        if let ty::Kind::Unit = ctx.types[ty].kind {
                            Expression::Lit((), name.replace(Literal::Unit(ty)))
                        } else {
                            CompileError::new(
                                &name,
                                format_args!("Expected value, found type `{}`", name.item),
                            )?
                        }
                    } else {
                        CompileError::new(
                            &name,
                            format_args!("Cannot find value `{}` in this scope", name.item),
                        )?
                    }
                }
                UnresolvedVariable::New(name, type_name) => {
                    let lit = name.replace(Literal::Unit(EMPTY_TYPE_ID));
                    let id = ctx.variables.push(Variable {
                        name: name.clone(),
                        ty: type_name,
                    });
                    ctx.variable_lookup
                        .last_mut()
                        .unwrap()
                        .push((name.item, id));

                    Expression::Lit((), lit)
                }
            },
            Expression::Lit((), lit) => {
                let meta = lit.simplify();
                match lit.item {
                    Literal::Integer(v) => Expression::Lit((), meta.replace(Literal::Integer(v))),
                    Literal::Unit(ty) => {
                        let ty = ctx.modules.get_type(ctx.at, &ty).ok_or_else(|| {
                            CompileError::new(
                                &meta,
                                format_args!("Cannot find value `{}` in this scope", ty),
                            )
                            .unwrap()
                        })?;

                        if let ty::Kind::Unit = ctx.types[ty].kind {
                            Expression::Lit((), meta.replace(Literal::Unit(ty)))
                        } else {
                            CompileError::new(
                                &meta,
                                format_args!("Expected value, found type `{}`", ty),
                            )?
                        }
                    }
                }
            }
            Expression::UnaryOperation((), op, expr) => {
                Expression::UnaryOperation((), op, Box::new(expr.resolve_identifiers(ctx)?))
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
                        let meta = name.simplify();
                        let id = ctx.variables.push(Variable {
                            name: name.clone(),
                            ty,
                        });
                        ctx.variable_lookup
                            .last_mut()
                            .unwrap()
                            .push((name.item, id));
                        meta.replace(id)
                    }
                };

                Expression::Assignment((), id, Box::new(expr))
            }
            Expression::Statement((), expr) => {
                Expression::Statement((), Box::new(expr.resolve_identifiers(ctx)?))
            }
            Expression::InitializeStruct((), name, fields) => {
                let ty = ctx.modules.get_type(ctx.at, &name.item).ok_or_else(|| {
                    CompileError::new(
                        &name,
                        format_args!("Cannot find the type `{}` in this scope", name.item),
                    )
                    .unwrap()
                })?;

                if let ty::Kind::Struct(_) | ty::Kind::Union(_) = ctx.types[ty].kind {
                    let fields = fields
                        .into_iter()
                        .map(|(name, expr)| Ok((name, expr.resolve_identifiers(ctx)?)))
                        .collect::<Result<Vec<_>, _>>()?;
                    Expression::InitializeStruct((), name.replace(ty), fields)
                } else {
                    let kind_str = match ctx.types[ty].kind {
                        ty::Kind::U8 | ty::Kind::U16 | ty::Kind::U32 | ty::Kind::Uninhabited => {
                            "a builtin type"
                        }
                        ty::Kind::Unit => "a unit type",
                        ty::Kind::Sum(_) => "a sum type",
                        ty::Kind::Struct(_) | ty::Kind::Union(_) => unreachable!(),
                    };

                    CompileError::build(
                        &name,
                        format_args!("Expected struct or union type, found `{}`", name.item),
                    )
                    .with_help(format_args!("`{}` is {}", name.item, kind_str))
                    .build()?
                }
            }
            Expression::FunctionCall((), name, args) => {
                let mut new = Vec::new();
                for expr in args {
                    new.push(expr.resolve_identifiers(ctx)?);
                }

                if let Some(id) = ctx.modules.get_function(ctx.at, name.item) {
                    Expression::FunctionCall((), name.replace(id), new)
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
                        Pattern::Underscore(ty) => {
                            Pattern::Underscore(ty::resolve(ctx.at, ty, ctx.types, ctx.modules)?)
                        }
                        Pattern::Named(UnresolvedVariable::New(name, ty)) => {
                            let meta = name.simplify();
                            let id = ctx.variables.push(Variable {
                                name: name.clone(),
                                ty,
                            });
                            ctx.variable_lookup
                                .last_mut()
                                .unwrap()
                                .push((name.item, id));
                            Pattern::Named(meta.replace(id))
                        }
                        err @ Pattern::Named(UnresolvedVariable::Existing(_)) => {
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
            Expression::Loop((), scope, expressions) => {
                ctx.variable_lookup.push(Vec::new());
                let mut new = Vec::new();
                let scope_id = ctx.scope_lookup.push(scope.item.clone());
                for expr in expressions {
                    new.push(expr.resolve_identifiers(ctx)?);
                }
                ctx.scope_lookup.pop();
                ctx.variable_lookup.pop();
                Expression::Loop((), scope.replace(scope_id), new)
            }
            Expression::Break((), scope, expr) => {
                if let Some(ref s) = &scope.item {
                    for (i, id) in ctx
                        .scope_lookup
                        .iter()
                        .enumerate()
                        .filter_map(|(i, v)| v.as_ref().map(|v| (ScopeId::from(i), v)))
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
                    let scope_id = ctx.scope_lookup.last_id().unwrap();
                    if ctx.scope_lookup[scope_id].map_or(false, |n| n == "fn") {
                        CompileError::new(&scope, format_args!("`break` outside of scope"))?
                    } else {
                        Expression::Break(
                            (),
                            scope.replace(scope_id),
                            Box::new(expr.resolve_identifiers(ctx)?),
                        )
                    }
                }
            }
            Expression::TypeRestriction(expr, ty) => {
                Expression::TypeRestriction(Box::new(expr.resolve_identifiers(ctx)?), ty)
            }
        })
    }
}
