use std::convert::{identity, TryFrom};

use tindex::{TSlice, TVec};

use shared_id::{FunctionId, TypeId, EMPTY_TYPE_ID, FALSE_TYPE_ID, TRUE_TYPE_ID};

use diagnostics::CompileError;

use crate::{
    expr::{Binop, Expression, UnaryOperation},
    func::{FunctionDefinition, ScopeId},
    traits::{ResolvedIdentifiers, ResolvedTypes},
    Literal, Pattern, VariableId,
};

#[derive(Debug)]
pub struct ExitScopeMeta {
    origin: mir::BlockId,
    expr: mir::StepId,
    variables: TVec<VariableId, Option<mir::StepId>>,
}

#[derive(Debug, Clone, Copy)]
pub struct Temporary {
    step: mir::StepId,
    ty: TypeId,
}

pub fn initialized_mir_block(
    id: mir::BlockId,
    variables: &TSlice<VariableId, TypeId>,
    var_lookup: &mut TSlice<VariableId, Option<mir::StepId>>,
    temporaries: &mut [Temporary],
    func: &mut mir::Function,
) {
    let block = &mut func[id];
    for (i, var) in var_lookup
        .iter_mut()
        .enumerate()
        .filter_map(|(i, v)| v.as_mut().map(|v| (i, v)))
    {
        *var = block.add_input(variables[VariableId::from(i)]);
    }

    for temp in temporaries.iter_mut() {
        temp.step = block.add_input(temp.ty);
    }
}

#[derive(Debug)]
pub struct ToMirContext<'a> {
    pub types: &'a TSlice<TypeId, mir::Type>,
    pub variable_types: &'a TVec<VariableId, TypeId>,
    pub function_definitions: &'a TSlice<FunctionId, FunctionDefinition<'a, TypeId>>,
    pub var_lookup: &'a mut TVec<VariableId, Option<mir::StepId>>,
    pub scopes: &'a mut TVec<ScopeId, (mir::BlockId, TypeId, Vec<ExitScopeMeta>)>,
    pub temporaries: &'a mut Vec<Temporary>,
    pub curr: &'a mut mir::BlockId,
    pub func: &'a mut mir::Function,
}

impl<'a> Expression<'a, ResolvedIdentifiers<'a>, ResolvedTypes<'a>> {
    pub fn to_mir<'b>(self, ctx: &mut ToMirContext<'b>) -> Result<mir::StepId, CompileError> {
        #[cfg(feature = "profiler")]
        profile_scope!("to_mir");
        use mir::{Action, Object, Terminator};
        match self {
            Expression::Block(ty, _scope, mut v) => Ok(if let Some(last) = v.pop() {
                let end = ctx.func.add_block();
                ctx.scopes.push((end, ty, Vec::new()));
                for e in v {
                    e.to_mir(ctx)?;
                }

                let last_id = last.to_mir(ctx)?;
                let last_id = ctx.func[*ctx.curr].add_step(ty, Action::Extend(last_id));
                let (_, _, mut exits) = ctx.scopes.pop().unwrap();
                exits.push(ExitScopeMeta {
                    origin: *ctx.curr,
                    expr: last_id,
                    variables: ctx.var_lookup.clone(),
                });
                for exit in exits.iter() {
                    for (v, ex) in ctx.var_lookup.iter_mut().zip(&exit.variables) {
                        *v = ex.and(*v);
                    }
                }

                for exit in exits.into_iter() {
                    let mut vars: Vec<_> = exit
                        .variables
                        .into_iter()
                        .zip(&*ctx.var_lookup)
                        .filter_map(|(v, lk)| lk.and(v))
                        .chain(ctx.temporaries.iter().map(|t| t.step))
                        .collect();
                    vars.push(exit.expr);
                    ctx.func[exit.origin].add_terminator(Terminator::Goto(end, vars));
                }

                initialized_mir_block(
                    end,
                    ctx.variable_types,
                    ctx.var_lookup,
                    ctx.temporaries,
                    ctx.func,
                );
                let step = ctx.func[end].add_input(ty);
                *ctx.curr = end;
                step
            } else {
                assert_eq!(ty, EMPTY_TYPE_ID);
                ctx.func[*ctx.curr].add_step(ty, Action::LoadConstant(Object::Unit))
            }),
            Expression::Variable(ty, var) => {
                if let Some(step) = ctx.var_lookup[var.item] {
                    Ok(ctx.func[*ctx.curr].add_step(ty, Action::Extend(step)))
                } else {
                    let span = var.span_str();
                    CompileError::new(
                        &var,
                        format_args!("Use of possibly uninitialized variable: `{}`", span),
                    )
                }
            }
            Expression::Lit(ty, lit) => {
                let type_id = ty;
                let ty = &ctx.types[type_id];
                Ok(match &lit.item {
                    &Literal::Integer(i) => {
                        let obj = match ty {
                            mir::Type::U8 => u8::try_from(i).map(|i| Object::U8(i)),
                            mir::Type::U16 => u16::try_from(i).map(|i| Object::U16(i)),
                            mir::Type::U32 => u32::try_from(i).map(|i| Object::U32(i)),
                            ty => unreachable!("Unknown literal type: `{:?}`, {:?}", lit, ty),
                        }
                        .or_else(|_| {
                            CompileError::new(
                                &lit,
                                format_args!("Literal out of range for `{}`", ty),
                            )
                        })?;

                        ctx.func[*ctx.curr].add_step(type_id, Action::LoadConstant(obj))
                    }
                    &Literal::Unit(unit_id) => {
                        let id = ctx.func[*ctx.curr]
                            .add_step(unit_id, Action::LoadConstant(mir::Object::Unit));
                        ctx.func[*ctx.curr].add_step(type_id, Action::Extend(id))
                    }
                })
            }
            Expression::UnaryOperation(ty, op, expr) => {
                let expr = expr.to_mir(ctx)?;
                ctx.func[*ctx.curr].add_step(ty, Action::Extend(expr));
                match op.item {
                    UnaryOperation::Invert => Ok(ctx.func[*ctx.curr].add_step(
                        ty,
                        Action::UnaryOperation(mir::UnaryOperation::Invert, expr),
                    )),
                }
            }
            Expression::Binop(ty, op, a, b) => {
                let a_ty = a.ty();
                let b_ty = b.ty();
                let a = a.to_mir(ctx)?;
                ctx.temporaries.push(Temporary {
                    step: a,
                    ty: ctx.func[*ctx.curr][a].ty,
                });
                let mut b = b.to_mir(ctx)?;
                let mut a = ctx.temporaries.pop().unwrap().step;

                let action = match op.item {
                    Binop::Add => Action::Binop(mir::binop::Binop::Add, a, b),
                    Binop::Sub => Action::Binop(mir::binop::Binop::Sub, a, b),
                    Binop::Mul => Action::Binop(mir::binop::Binop::Mul, a, b),
                    Binop::Div => Action::Binop(mir::binop::Binop::Div, a, b),
                    Binop::Rem => Action::Binop(mir::binop::Binop::Rem, a, b),
                    Binop::Shl => Action::Binop(mir::binop::Binop::Shl, a, b),
                    Binop::Shr => Action::Binop(mir::binop::Binop::Shr, a, b),
                    Binop::BitOr => {
                        let a = ctx.func[*ctx.curr].add_step(ty, Action::Extend(a));
                        let b = ctx.func[*ctx.curr].add_step(ty, Action::Extend(b));
                        Action::Binop(mir::binop::Binop::BitOr, a, b)
                    }
                    Binop::BitAnd => {
                        let a = ctx.func[*ctx.curr].add_step(ty, Action::Extend(a));
                        let b = ctx.func[*ctx.curr].add_step(ty, Action::Extend(b));
                        Action::Binop(mir::binop::Binop::BitAnd, a, b)
                    }
                    Binop::Eq => {
                        if a_ty == TRUE_TYPE_ID || a_ty == FALSE_TYPE_ID {
                            a = ctx.func[*ctx.curr].add_step(ty, Action::Extend(a));
                        }

                        if b_ty == TRUE_TYPE_ID || b_ty == FALSE_TYPE_ID {
                            b = ctx.func[*ctx.curr].add_step(ty, Action::Extend(b));
                        }
                        Action::Binop(mir::binop::Binop::Eq, a, b)
                    }
                    Binop::Neq => {
                        if a_ty == TRUE_TYPE_ID || a_ty == FALSE_TYPE_ID {
                            a = ctx.func[*ctx.curr].add_step(ty, Action::Extend(a));
                        }

                        if b_ty == TRUE_TYPE_ID || b_ty == FALSE_TYPE_ID {
                            b = ctx.func[*ctx.curr].add_step(ty, Action::Extend(b));
                        }
                        Action::Binop(mir::binop::Binop::Neq, a, b)
                    }
                    Binop::Gt => Action::Binop(mir::binop::Binop::Gt, a, b),
                    Binop::Lt => Action::Binop(mir::binop::Binop::Gt, b, a),
                    Binop::Gte => Action::Binop(mir::binop::Binop::Gte, b, a),
                    Binop::Lte => Action::Binop(mir::binop::Binop::Gte, b, a),
                };
                Ok(ctx.func[*ctx.curr].add_step(ty, action))
            }
            Expression::Statement(ty, expr) => {
                assert_eq!(ty, EMPTY_TYPE_ID);
                expr.to_mir(ctx)?;
                Ok(ctx.func[*ctx.curr].add_step(ty, Action::LoadConstant(Object::Unit)))
            }
            Expression::Assignment(ty, var, expr) => {
                let expr = expr.to_mir(ctx)?;
                let block = &mut ctx.func[*ctx.curr];
                let extension = block.add_step(ctx.variable_types[var.item], Action::Extend(expr));
                ctx.var_lookup[var.item] = Some(extension);
                assert_eq!(ty, EMPTY_TYPE_ID);
                Ok(block.add_step(ty, Action::LoadConstant(Object::Unit)))
            }
            Expression::InitializeStruct(_, struct_kind, mut fields) => {
                match &ctx.types[struct_kind.item] {
                    &mir::Type::Struct(ref expected_fields) => {
                        let temp_base = ctx.temporaries.len();
                        let mut field_ids = Vec::new();
                        for (kind, expr) in fields.into_iter() {
                            field_ids.push(kind.item);
                            let id = expr.to_mir(ctx)?;
                            let field_ty = expected_fields[kind.item];
                            let id = ctx.func[*ctx.curr].add_step(field_ty, Action::Extend(id));
                            ctx.temporaries.push(Temporary {
                                step: id,
                                ty: field_ty,
                            });
                        }
                        let mut steps = ctx
                            .temporaries
                            .drain(temp_base..)
                            .map(|temp| temp.step)
                            .zip(field_ids)
                            .collect::<Vec<_>>();

                        steps.sort_by_key(|&(_, field)| field);

                        Ok(ctx.func[*ctx.curr].add_step(
                            struct_kind.item,
                            Action::InitializeStruct(
                                steps.into_iter().map(|(step, _)| step).collect(),
                            ),
                        ))
                    }
                    &mir::Type::Union(_) => {
                        let (kind, expr) = fields.pop().expect("single union field");
                        let expr = expr.to_mir(ctx)?;
                        Ok(ctx.func[*ctx.curr]
                            .add_step(struct_kind.item, Action::InitializeUnion(expr, kind.item)))
                    }
                    _ => unreachable!("invalid struct/union"),
                }
            }
            Expression::FunctionCall(ty, id, args) => {
                let args = args
                    .into_iter()
                    .enumerate()
                    .map(|(i, arg)| {
                        let arg = arg.to_mir(ctx)?;
                        Ok(ctx.func[*ctx.curr].add_step(
                            ctx.function_definitions[id.item].args[i].item,
                            Action::Extend(arg),
                        ))
                    })
                    .collect::<Result<Vec<_>, CompileError>>()?;
                Ok(ctx.func[*ctx.curr].add_step(ty, Action::CallFunction(id.item, args)))
            }
            Expression::FieldAccess(ty, obj, field) => {
                let obj = obj.to_mir(ctx)?;
                match ctx.types[ctx.func[*ctx.curr][obj].ty] {
                    mir::Type::Struct(_) => Ok(ctx.func[*ctx.curr]
                        .add_step(ty, Action::StructFieldAccess(obj, field.item))),
                    mir::Type::Union(_) => {
                        Ok(ctx.func[*ctx.curr]
                            .add_step(ty, Action::UnionFieldAccess(obj, field.item)))
                    }
                    _ => unreachable!("invalid struct for field access"),
                }
            }
            Expression::Match(ty_id, _, value, match_arms) => {
                let value = value.to_mir(ctx)?;

                let old_block = *ctx.curr;

                let mut arms = Vec::new();
                let mut arms_data = Vec::new();
                for arm in match_arms {
                    let mut available_variables = ctx.var_lookup.clone();
                    let mut temporaries = ctx.temporaries.clone();

                    let mut id = ctx.func.add_block();
                    initialized_mir_block(
                        id,
                        ctx.variable_types,
                        &mut available_variables,
                        &mut temporaries,
                        ctx.func,
                    );

                    let mut args: Vec<Option<mir::StepId>> = ctx
                        .var_lookup
                        .iter()
                        .copied()
                        .filter_map(identity)
                        .chain(ctx.temporaries.iter().map(|t| t.step))
                        .map(Some)
                        .collect();

                    let ty = match arm.pattern {
                        Pattern::Underscore(ty) => ty.item,
                        Pattern::Named(name) => {
                            let ty = ctx.variable_types[name.item];
                            available_variables[name.item] = Some(ctx.func[id].add_input(ty));
                            args.push(None);
                            ty
                        }
                    };

                    arms.push((ty, id, args));

                    let expr_id = arm.expr.to_mir(&mut ToMirContext {
                        types: ctx.types,
                        variable_types: ctx.variable_types,
                        function_definitions: ctx.function_definitions,
                        var_lookup: &mut available_variables,
                        scopes: ctx.scopes,
                        temporaries: &mut temporaries,
                        curr: &mut id,
                        func: ctx.func,
                    })?;
                    let extended_expr = ctx.func[id].add_step(ty_id, Action::Extend(expr_id));

                    arms_data.push((id, extended_expr, available_variables, temporaries));
                }

                let mut initialized_variables = if let Some((first, rest)) = arms_data.split_first()
                {
                    rest.iter().fold(first.2.clone(), |mut lookup, arm| {
                        for (v, arm) in lookup.iter_mut().zip(&arm.2) {
                            *v = v.and(*arm)
                        }
                        lookup
                    })
                } else {
                    ctx.var_lookup.clone()
                };

                let id = ctx.func.add_block();
                initialized_mir_block(
                    id,
                    ctx.variable_types,
                    &mut initialized_variables,
                    ctx.temporaries,
                    ctx.func,
                );

                let step = ctx.func[id].add_input(ty_id);

                for &(block, step, ref vars, ref temporaries) in arms_data.iter() {
                    let block = &mut ctx.func[block];
                    let mut steps: Vec<mir::StepId> = vars
                        .iter()
                        .copied()
                        .zip(initialized_variables.iter().copied())
                        .filter_map(|(v, needed)| needed.and(v))
                        .chain(temporaries.iter().map(|t| t.step))
                        .collect();
                    steps.push(step);
                    block.add_terminator(Terminator::Goto(id, steps));
                }

                ctx.func[old_block].add_terminator(Terminator::Match(value, arms));
                *ctx.curr = id;
                *ctx.var_lookup = initialized_variables;
                Ok(step)
            }
            Expression::Loop(ty, _scope, content) => Ok({
                let block = ctx.func.add_block();
                ctx.func[*ctx.curr].add_terminator(Terminator::Goto(
                    block,
                    ctx.var_lookup
                        .iter()
                        .copied()
                        .filter_map(identity)
                        .chain(ctx.temporaries.iter().map(|t| t.step))
                        .collect(),
                ));
                let variables = ctx.var_lookup.clone();
                initialized_mir_block(
                    block,
                    ctx.variable_types,
                    ctx.var_lookup,
                    ctx.temporaries,
                    ctx.func,
                );
                *ctx.curr = block;

                let end = ctx.func.add_block();
                ctx.scopes.push((end, ty, Vec::new()));
                for expr in content {
                    expr.to_mir(ctx)?;
                }

                ctx.func[*ctx.curr].add_terminator(Terminator::Goto(
                    block,
                    ctx.var_lookup
                        .into_iter()
                        .zip(variables)
                        .filter_map(|(&mut v, e)| e.and(v))
                        .chain(ctx.temporaries.iter().map(|t| t.step))
                        .collect(),
                ));

                let (_, _, exits) = ctx.scopes.pop().unwrap();
                *ctx.var_lookup = tindex::tvec![Some(mir::StepId::invalid()); ctx.var_lookup.len()];
                for exit in exits.iter() {
                    for (v, ex) in ctx.var_lookup.iter_mut().zip(&exit.variables) {
                        *v = ex.and(*v);
                    }
                }

                for exit in exits.into_iter() {
                    let mut vars: Vec<_> = exit
                        .variables
                        .into_iter()
                        .zip(&*ctx.var_lookup)
                        .filter_map(|(v, lk)| lk.and(v))
                        .chain(ctx.temporaries.iter().map(|t| t.step))
                        .collect();
                    vars.push(exit.expr);
                    let block = &mut ctx.func[exit.origin];
                    block.add_terminator(Terminator::Goto(end, vars));
                }

                initialized_mir_block(
                    end,
                    ctx.variable_types,
                    ctx.var_lookup,
                    ctx.temporaries,
                    ctx.func,
                );
                let step = ctx.func[end].add_input(ty);
                *ctx.curr = end;
                step
            }),
            Expression::Break(ty, scope_id, expr) => {
                let expr = expr.to_mir(ctx)?;
                let expr =
                    ctx.func[*ctx.curr].add_step(ctx.scopes[scope_id.item].1, Action::Extend(expr));
                let origin = *ctx.curr;

                *ctx.curr = ctx.func.add_block();
                initialized_mir_block(
                    *ctx.curr,
                    ctx.variable_types,
                    ctx.var_lookup,
                    ctx.temporaries,
                    ctx.func,
                );
                let step = ctx.func[*ctx.curr].add_input(ty);

                ctx.scopes[scope_id.item].2.push(ExitScopeMeta {
                    origin,
                    expr,
                    variables: ctx.var_lookup.clone(),
                });

                Ok(step)
            }
            Expression::TypeRestriction(_, ()) => unreachable!("type restriction after type check"),
        }
    }
}
