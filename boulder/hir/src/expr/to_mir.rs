use std::convert::{identity, TryFrom};

use diagnostics::CompileError;

use crate::{
    expr::Expression,
    func::FunctionDefinition,
    to_mir,
    ty::{self, TypeId},
    Binop, Literal, ResolvedIdentifiers, ResolvedTypes,
};

#[derive(Debug)]
pub struct ExitScopeMeta {
    origin: (mir::BlockId, mir::StepId),
    expr: mir::StepId,
    variables: Vec<Option<mir::StepId>>,
}

#[derive(Debug, Clone, Copy)]
pub struct Temporary {
    step: mir::StepId,
    ty: mir::TypeId,
}

#[derive(Debug)]
pub struct ToMirContext<'a> {
    pub types: &'a [mir::Type],
    pub variable_types: &'a [TypeId],
    pub function_definitions: &'a [FunctionDefinition<'a, TypeId>],
    pub var_lookup: &'a mut Vec<Option<mir::StepId>>,
    pub scopes: &'a mut Vec<(mir::BlockId, Vec<ExitScopeMeta>)>,
    pub temporaries: &'a mut Vec<Temporary>,
    pub curr: &'a mut mir::BlockId,
    pub func: &'a mut mir::Function,
}

impl<'a> Expression<'a, ResolvedIdentifiers<'a>, ResolvedTypes<'a>> {
    pub fn to_mir<'b>(self, ctx: &mut ToMirContext<'b>) -> Result<mir::StepId, CompileError> {
        use mir::{Action, Object, Step};
        match self {
            Expression::Block(ty, _, mut v) => Ok(if let Some(last) = v.pop() {
                let end = ctx.func.add_block();
                ctx.scopes.push((end, Vec::new()));
                for e in v {
                    e.to_mir(ctx)?;
                }

                let last_id = last.to_mir(ctx)?;
                let goto_id = ctx.func.block(*ctx.curr).add_step(Step::new(
                    ty::NEVER_ID.to_mir(),
                    Action::Goto(end, Vec::new()),
                ));
                let (_, mut exits) = ctx.scopes.pop().unwrap();
                exits.push(ExitScopeMeta {
                    origin: (*ctx.curr, goto_id),
                    expr: last_id,
                    variables: ctx.var_lookup.clone(),
                });
                for exit in exits.iter() {
                    for (v, ex) in ctx.var_lookup.iter_mut().zip(&exit.variables) {
                        *v = ex.and(*v);
                    }
                }

                for exit in exits.into_iter() {
                    let step = &mut ctx.func.block(exit.origin.0).content[(exit.origin.1).0];
                    if let mir::Action::Goto(_, ref mut vars) = step.action {
                        *vars = exit
                            .variables
                            .into_iter()
                            .zip(&*ctx.var_lookup)
                            .filter_map(|(v, lk)| lk.and(v))
                            .collect();
                        vars.push(exit.expr);
                    } else {
                        panic!(
                            "invalid break action `{:?}` in `{:?}`",
                            step.clone(),
                            ctx.func
                        );
                    }
                }

                to_mir::initialized_mir_block(end, ctx.variable_types, ctx.var_lookup, ctx.func);
                let step = ctx.func.block(end).add_input(ty.to_mir());
                *ctx.curr = end;
                step
            } else {
                assert_eq!(ty, ty::EMPTY_ID);
                ctx.func
                    .block(*ctx.curr)
                    .add_step(Step::new(ty.to_mir(), Action::LoadConstant(Object::Unit)))
            }),
            Expression::Variable(ty, var) => {
                if let Some(step) = ctx.var_lookup[var.0] {
                    assert_eq!(ty.to_mir(), ctx.func.block(*ctx.curr).step(step).ty);
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
                ctx.temporaries.push(Temporary {
                    step: a,
                    ty: ctx.func.block(*ctx.curr).step(a).ty,
                });
                let b = b.to_mir(ctx)?;
                let a = ctx.temporaries.pop().unwrap().step;
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
                let block = ctx.func.block(*ctx.curr);
                let extension = block.add_step(Step::new(
                    ctx.variable_types[var.0].to_mir(),
                    Action::Extend(expr),
                ));
                ctx.var_lookup[var.0] = Some(extension);
                assert_eq!(ty, ty::EMPTY_ID);
                Ok(block.add_step(Step::new(ty.to_mir(), Action::LoadConstant(Object::Unit))))
            }
            Expression::FunctionCall(ty, id, args) => {
                let args = args
                    .into_iter()
                    .enumerate()
                    .map(|(i, arg)| {
                        let arg = arg.to_mir(ctx)?;
                        Ok(ctx.func[*ctx.curr].add_step(Step::new(
                            ctx.function_definitions[id.0].args[i].item.to_mir(),
                            Action::Extend(arg),
                        )))
                    })
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

                    available_variables[arm.pattern.0] = Some(
                        ctx.func
                            .block(id)
                            .add_input(ctx.variable_types[arm.pattern.0].to_mir()),
                    );

                    arms.push((ctx.variable_types[arm.pattern.0].to_mir(), id, {
                        let mut args: Vec<mir::StepId> = ctx
                            .var_lookup
                            .iter()
                            .copied()
                            .filter_map(identity)
                            .chain(ctx.temporaries.iter().map(|t| t.step))
                            .collect();
                        args.push(match_step);
                        args
                    }));

                    let expr_id = arm.expr.to_mir(&mut ToMirContext {
                        types: ctx.types,
                        variable_types: ctx.variable_types,
                        function_definitions: ctx.function_definitions,
                        var_lookup: &mut available_variables,
                        scopes: ctx.scopes,
                        temporaries: ctx.temporaries,
                        curr: &mut id,
                        func: ctx.func,
                    })?;
                    let extended_expr =
                        ctx.func[id].add_step(Step::new(ty_id.to_mir(), Action::Extend(expr_id)));

                    arms_data.push((id, extended_expr, available_variables));
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
                        .chain(ctx.temporaries.iter().map(|t| t.step))
                        .collect();
                    steps.push(step);
                    block.add_step(Step::new(
                        ty::NEVER_ID.to_mir(),
                        mir::Action::Goto(id, steps),
                    ));
                });

                for temp in ctx.temporaries.iter_mut() {
                    temp.step = ctx.func.block(id).add_input(temp.ty);
                }

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
                let expr = expr.to_mir(ctx)?;
                let step = ctx.func.block(*ctx.curr).add_step(Step::new(
                    ty.to_mir(),
                    mir::Action::Goto(ctx.scopes[scope_id.item.0].0, Vec::new()),
                ));

                ctx.scopes[scope_id.0].1.push(ExitScopeMeta {
                    origin: (*ctx.curr, step),
                    expr,
                    variables: ctx.var_lookup.clone(),
                });

                Ok(step)
            }
            Expression::TypeRestriction(_, ()) => unreachable!("type restriction after type check"),
        }
    }
}
