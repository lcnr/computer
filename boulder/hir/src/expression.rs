use crate::*;

#[derive(Debug, Clone)]
pub enum Expression<'a, V, T> {
    Block(Meta<'a, ()>, Vec<Expression<'a, V, T>>),
    Variable(Meta<'a, V>),
    Lit(Meta<'a, Literal>),
    Binop(
        Meta<'a, Binop>,
        Box<Expression<'a, V, T>>,
        Box<Expression<'a, V, T>>,
    ),
    VariableDecl(Meta<'a, V>, Meta<'a, T>, Box<Expression<'a, V, T>>),
    Statement(Meta<'a, ()>, Box<Expression<'a, V, T>>),
    Assignment(Meta<'a, Box<str>>, Box<Expression<'a, V, T>>),
}

impl<'a, T> Expression<'a, Box<str>, T> {
    pub fn resolve_variables(
        self,
        variables: &mut std::vec::Vec<function::Variable<'a, T>>,
        lookup: &mut Vec<Vec<(Box<str>, VariableId)>>,
    ) -> Result<Expression<'a, VariableId, T>, CompileError> {
        fn get_id<'b>(
            name: Meta<'b, Box<str>>,
            lookup: &mut Vec<Vec<(Box<str>, VariableId)>>,
        ) -> Result<Meta<'b, VariableId>, CompileError> {
            for scope in lookup.iter().rev() {
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
            Expression::Block(meta, expressions) => {
                lookup.push(Vec::new());
                let mut new = Vec::new();
                for expr in expressions {
                    new.push(expr.resolve_variables(variables, lookup)?);
                }
                lookup.pop();
                Expression::Block(meta, new)
            }
            Expression::Variable(name) => Expression::Variable(get_id(name, lookup)?),
            Expression::Lit(lit) => Expression::Lit(lit),
            Expression::Binop(op, rhs, lhs) => Expression::Binop(
                op,
                Box::new(rhs.resolve_variables(variables, lookup)?),
                Box::new(lhs.resolve_variables(variables, lookup)?),
            ),
            Expression::VariableDecl(name, ty, expr) => Expression::VariableDecl(
                get_id(name, lookup)?,
                ty,
                Box::new(expr.resolve_variables(variables, lookup)?),
            ),
            Expression::Statement(meta, expr) => {
                Expression::Statement(meta, Box::new(expr.resolve_variables(variables, lookup)?))
            }
            _ => unimplemented!(),
        })
    }
}

impl Expression<'_, Box<str>, Box<str>> {
    pub fn meta(&self) -> Meta<'_, ()> {
        match self {
            Expression::Block(meta, _v) => meta.simplify(),
            Expression::Variable(var) => var.simplify(),
            Expression::Lit(lit) => lit.simplify(),
            Expression::Binop(_op, a, b) => a.meta().append(b.meta()),
            Expression::VariableDecl(var, _ty, expr) => var.simplify().append(expr.meta()),
            Expression::Statement(meta, expr) => expr.meta().append(meta.clone()),
            Expression::Assignment(var, expr) => var.simplify().append(expr.meta()),
        }
    }

    pub fn ty_meta(&self) -> Meta<'_, ()> {
        match self {
            Expression::Block(meta, v) => v.last().map_or(meta.clone(), |v| v.ty_meta()),
            e => e.meta(),
        }
    }

    pub fn type_ck<'a>(
        &self,
        ctx: &mut Vec<Context<UnresolvedType>>,
    ) -> Result<Box<str>, CompileError> {
        match self {
            Expression::Block(_meta, v) => {
                if let Some((last, start)) = v.split_last() {
                    ctx.push(Context::new());
                    for stmt in start {
                        let stmt_ty = stmt.type_ck(ctx)?;
                        if &*stmt_ty != "Empty" {
                            return CompileError::new(
                                &stmt.meta(),
                                format_args!(
                                    "Invalid expression in block, expected `{}`, found `{}`",
                                    "Empty", stmt_ty
                                ),
                            );
                        }
                    }

                    let last_ty = last.type_ck(ctx);
                    ctx.pop();
                    last_ty
                } else {
                    Ok("Empty".into())
                }
            }
            Expression::Variable(variable) => get_var_ty(ctx, variable).ok_or_else(|| {
                CompileError::new::<_, (), _>(
                    &variable,
                    format_args!("Unknown variable: `{}`", variable.span_str()),
                )
                .unwrap_err()
            }),
            Expression::Lit(ty) => match ty.item {
                Literal::Integer(_) => Ok("u32".into()),
            },
            Expression::Binop(op, a, b) => {
                let a_ty = a.type_ck(ctx)?;
                let b_ty = b.type_ck(ctx)?;
                if a_ty != b_ty {
                    CompileError::new(&op, format_args!("Mismatched types: {:?} is only implemented for identical types: {} != {}", op.item, a_ty, b_ty))
                } else {
                    Ok(a_ty)
                }
            }
            Expression::VariableDecl(var, ty, expr) => {
                let expr_ty = expr.type_ck(ctx)?;
                if ty.item != expr_ty {
                    CompileError::new(
                        &var,
                        format_args!(
                            "Mismatched types: Tried to assign {} to a variable of type {}",
                            ty.item, expr_ty
                        ),
                    )
                } else {
                    Ok("Empty".into())
                }
            }
            Expression::Statement(_meta, expr) => {
                let _check = expr.type_ck(ctx)?;
                Ok("Empty".into())
            }
            Expression::Assignment(var, expr) => {
                if let Some(ty) = get_var_ty(ctx, var) {
                    let expr_ty = expr.type_ck(ctx)?;
                    if ty != expr_ty {
                        CompileError::new(
                            &var,
                            format_args!(
                                "Mismatched types: Tried to assign {} to a variable of type {}",
                                expr_ty, ty
                            ),
                        )
                    } else {
                        Ok("Empty".into())
                    }
                } else {
                    CompileError::new(&var, format_args!("Unknown variable: `{}`", var.item))
                }
            }
        }
    }
}
