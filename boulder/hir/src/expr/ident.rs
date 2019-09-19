use std::collections::HashMap;

use diagnostics::{CompileError, Meta};

use crate::{
    expr::{Expression, MatchArm},
    func::{FunctionId, Variable, VariableId},
    ty::{self, Type, TypeId},
    Literal, Pattern, ResolvedIdentifiers, ScopeId, UnresolvedIdentifiers, UnresolvedType,
    UnresolvedTypes, UnresolvedVariable,
};

pub struct ResolveIdentifiersContext<'a, 'b> {
    pub variables: &'b mut Vec<Variable<'a, Option<UnresolvedType<'a>>>>,
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
                ctx.scope_lookup.pop();
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
                    ctx.variables.push(Variable {
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
                        ctx.variables.push(Variable { name, ty });
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
                        Pattern::Underscore(ty) => {
                            Pattern::Underscore(ty::resolve(ty, ctx.types, ctx.type_lookup)?)
                        }
                        Pattern::Named(UnresolvedVariable::New(name, ty)) => {
                            let id = VariableId(ctx.variables.len());
                            let meta = name.simplify();
                            ctx.variable_lookup
                                .last_mut()
                                .unwrap()
                                .push((name.item.clone(), id));
                            ctx.variables.push(Variable { name, ty });
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
                let scope_id = ScopeId(ctx.scope_lookup.len());
                ctx.scope_lookup.push(scope.item.clone());
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
                    let scope_id = ScopeId(ctx.scope_lookup.len() - 1);
                    if ctx.scope_lookup[scope_id.0]
                        .as_ref()
                        .map_or(false, |n| n.as_ref() == "fn")
                    {
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
