use std::convert::{identity, TryFrom};

use diagnostics::CompileError;

use crate::{
    expr::Expression,
    func::FunctionDefinition,
    ty::{self, TypeId},
    Binop, Literal, Pattern, ResolvedIdentifiers, ResolvedTypes,
};

#[derive(Debug)]
pub struct ExitScopeMeta {
    origin: mir::BlockId,
    expr: mir::StepId,
    variables: Vec<Option<mir::StepId>>,
}

#[derive(Debug, Clone, Copy)]
pub struct Temporary {
    step: mir::StepId,
    ty: mir::TypeId,
}

pub fn initialized_mir_block(
    id: mir::BlockId,
    variables: &[TypeId],
    var_lookup: &mut [Option<mir::StepId>],
    temporaries: &mut [Temporary],
    func: &mut mir::Function<mir::traits::InitialMirState>,
) {
    let block = &mut func[id];
    for (i, var) in var_lookup
        .iter_mut()
        .enumerate()
        .filter_map(|(i, v)| v.as_mut().map(|v| (i, v)))
    {
        *var = block.add_input(variables[i].to_mir());
    }

    for temp in temporaries.iter_mut() {
        temp.step = block.add_input(temp.ty);
    }
}

#[derive(Debug)]
pub struct ToMirContext<'a> {
    pub types: &'a [mir::Type],
    pub variable_types: &'a [TypeId],
    pub function_definitions: &'a [FunctionDefinition<'a, TypeId>],
    pub var_lookup: &'a mut Vec<Option<mir::StepId>>,
    pub scopes: &'a mut Vec<(mir::BlockId, mir::TypeId, Vec<ExitScopeMeta>)>,
    pub temporaries: &'a mut Vec<Temporary>,
    pub curr: &'a mut mir::BlockId,
    pub func: &'a mut mir::Function<mir::traits::InitialMirState>,
}

impl<'a> Expression<'a, ResolvedIdentifiers<'a>, ResolvedTypes<'a>> {
    pub fn to_mir<'b>(self, ctx: &mut ToMirContext<'b>) -> Result<mir::StepId, CompileError> {
        use mir::{binop::ExtendedBinop, Action, Object, Terminator};
        match self {
            Expression::Block(ty, _scope, mut v) => Ok(if let Some(last) = v.pop() {
                let end = ctx.func.add_block();
                ctx.scopes.push((end, ty.to_mir(), Vec::new()));
                for e in v {
                    e.to_mir(ctx)?;
                }

                let last_id = last.to_mir(ctx)?;
                let last_id = ctx.func[*ctx.curr].add_step(ty.to_mir(), Action::Extend(last_id));
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
                let step = ctx.func[end].add_input(ty.to_mir());
                *ctx.curr = end;
                step
            } else {
                assert_eq!(ty, ty::EMPTY_ID);
                ctx.func[*ctx.curr].add_step(ty.to_mir(), Action::LoadConstant(Object::Unit))
            }),
            Expression::Variable(ty, var) => {
                if let Some(step) = ctx.var_lookup[var.0] {
                    assert_eq!(ty.to_mir(), ctx.func[*ctx.curr][step].ty);
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
                Ok(ctx.func[*ctx.curr].add_step(type_id, Action::LoadConstant(obj)))
            }
            Expression::Binop(ty, op, a, b) => {
                let a = a.to_mir(ctx)?;
                ctx.temporaries.push(Temporary {
                    step: a,
                    ty: ctx.func[*ctx.curr][a].ty,
                });
                let b = b.to_mir(ctx)?;
                let a = ctx.temporaries.pop().unwrap().step;
                Ok(ctx.func[*ctx.curr].add_step(
                    ty.to_mir(),
                    match op.item {
                        Binop::Add => Action::Binop(ExtendedBinop::Add, a, b),
                        Binop::Sub => Action::Binop(ExtendedBinop::Sub, a, b),
                        Binop::Mul => Action::Binop(ExtendedBinop::Mul, a, b),
                        Binop::Div => Action::Binop(ExtendedBinop::Div, a, b),
                        Binop::BitOr => Action::Binop(ExtendedBinop::BitOr, a, b),
                        Binop::Lt => Action::Binop(ExtendedBinop::Lt, a, b),
                    },
                ))
            }
            Expression::Statement(ty, expr) => {
                assert_eq!(ty, ty::EMPTY_ID);
                expr.to_mir(ctx)?;
                Ok(ctx.func[*ctx.curr].add_step(ty.to_mir(), Action::LoadConstant(Object::Unit)))
            }
            Expression::Assignment(ty, var, expr) => {
                let expr = expr.to_mir(ctx)?;
                let block = &mut ctx.func[*ctx.curr];
                let extension =
                    block.add_step(ctx.variable_types[var.0].to_mir(), Action::Extend(expr));
                ctx.var_lookup[var.0] = Some(extension);
                assert_eq!(ty, ty::EMPTY_ID);
                Ok(block.add_step(ty.to_mir(), Action::LoadConstant(Object::Unit)))
            }
            Expression::FunctionCall(ty, id, args) => {
                let args = args
                    .into_iter()
                    .enumerate()
                    .map(|(i, arg)| {
                        let arg = arg.to_mir(ctx)?;
                        Ok(ctx.func[*ctx.curr].add_step(
                            ctx.function_definitions[id.0].args[i].item.to_mir(),
                            Action::Extend(arg),
                        ))
                    })
                    .collect::<Result<Vec<_>, CompileError>>()?;
                Ok(ctx.func[*ctx.curr]
                    .add_step(ty.to_mir(), Action::CallFunction(id.item.to_mir(), args)))
            }
            Expression::FieldAccess(ty, obj, field) => {
                let obj = obj.to_mir(ctx)?;
                Ok(ctx.func[*ctx.curr]
                    .add_step(ty.to_mir(), Action::FieldAccess(obj, field.to_mir())))
            }
            Expression::Match(ty_id, _, value, match_arms) => {
                let old_block = *ctx.curr;

                let value = value.to_mir(ctx)?;

                let mut arms = Vec::new();
                let mut arms_data = Vec::new();
                for arm in match_arms {
                    let mut available_variables = ctx.var_lookup.to_vec();
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
                        Pattern::Underscore(ty) => ty.to_mir(),
                        Pattern::Named(name) => {
                            let ty = ctx.variable_types[name.0].to_mir();
                            available_variables[name.0] = Some(ctx.func[id].add_input(ty));
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
                    let extended_expr =
                        ctx.func[id].add_step(ty_id.to_mir(), Action::Extend(expr_id));

                    arms_data.push((id, extended_expr, available_variables, temporaries));
                }

                let mut initialized_variables = if let Some((first, rest)) = arms_data.split_first()
                {
                    rest.iter().fold(first.2.to_vec(), |mut lookup, arm| {
                        for (v, arm) in lookup.iter_mut().zip(&arm.2) {
                            *v = v.and(*arm)
                        }
                        lookup
                    })
                } else {
                    ctx.var_lookup.to_vec()
                };

                let id = ctx.func.add_block();
                initialized_mir_block(
                    id,
                    ctx.variable_types,
                    &mut initialized_variables,
                    ctx.temporaries,
                    ctx.func,
                );

                let step = ctx.func[id].add_input(ty_id.to_mir());

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
                ctx.scopes.push((end, ty.to_mir(), Vec::new()));
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
                *ctx.var_lookup = vec![Some(mir::StepId::invalid()); ctx.var_lookup.len()];
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
                let step = ctx.func[end].add_input(ty.to_mir());
                *ctx.curr = end;
                step
            }),
            Expression::Break(ty, scope_id, expr) => {
                let expr = expr.to_mir(ctx)?;
                let expr = ctx.func[*ctx.curr]
                    .add_step(ctx.scopes[scope_id.item.0].1, Action::Extend(expr));
                let origin = *ctx.curr;

                *ctx.curr = ctx.func.add_block();
                initialized_mir_block(
                    *ctx.curr,
                    ctx.variable_types,
                    ctx.var_lookup,
                    ctx.temporaries,
                    ctx.func,
                );
                let step = ctx.func[*ctx.curr].add_input(ty.to_mir());

                ctx.scopes[scope_id.0].2.push(ExitScopeMeta {
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
