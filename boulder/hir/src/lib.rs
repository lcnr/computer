use boulder_core::{CompileError, Meta};

use std::collections::HashMap;

mod to_mir;

use to_mir::MirBuilder;

#[derive(Debug, Clone, Copy)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(u128),
}

#[derive(Debug, Clone)]
pub enum Expression<'a> {
    Block(Meta<'a, ()>, Vec<Expression<'a>>),
    Constant(Meta<'a, ()>, Box<str>, Box<str>),
    Variable(Meta<'a, Box<str>>),
    Lit(Meta<'a, Literal>),
    Binop(Meta<'a, Binop>, Box<Expression<'a>>, Box<Expression<'a>>),
    VariableDecl(Meta<'a, Box<str>>, Meta<'a, Box<str>>, Box<Expression<'a>>),
    Statement(Meta<'a, ()>, Box<Expression<'a>>),
    Assignment(Meta<'a, Box<str>>, Box<Expression<'a>>),
}

impl Expression<'_> {
    pub fn meta(&self) -> Meta<'_, ()> {
        match self {
            Expression::Block(meta, _v) => meta.simplify(),
            Expression::Constant(meta, _value, _ty) => meta.simplify(),
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

    pub fn ty<'a>(&self, ctx: &mut Vec<Context>) -> Result<Box<str>, CompileError> {
        match self {
            Expression::Block(_meta, v) => {
                if let Some((last, start)) = v.split_last() {
                    ctx.push(Context::new());
                    for stmt in start {
                        let stmt_ty = stmt.ty(ctx)?;
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

                    let last_ty = last.ty(ctx);
                    ctx.pop();
                    last_ty
                } else {
                    Ok("Empty".into())
                }
            }
            Expression::Constant(_meta, _value, ty) => Ok(ty.clone()),
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
                let a_ty = a.ty(ctx)?;
                let b_ty = b.ty(ctx)?;
                if a_ty != b_ty {
                    CompileError::new(&op, format_args!("Mismatched types: {:?} is only implemented for identical types: {} != {}", op.item, a_ty, b_ty))
                } else {
                    Ok(a_ty)
                }
            }
            Expression::VariableDecl(var, ty, expr) => {
                let expr_ty = expr.ty(ctx)?;
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
                let _check = expr.ty(ctx)?;
                Ok("Empty".into())
            }
            Expression::Assignment(var, expr) => {
                if let Some(ty) = get_var_ty(ctx, var) {
                    let expr_ty = expr.ty(ctx)?;
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

#[derive(Debug, Clone)]
pub struct Function<'a> {
    name: Meta<'a, Box<str>>,
    arguments: Vec<(Meta<'a, Box<str>>, Meta<'a, Box<str>>)>,
    ret: Meta<'a, Box<str>>,
    body: Expression<'a>,
}

impl<'a> Function<'a> {
    pub fn new(name: Meta<'a, Box<str>>) -> Self {
        Self {
            name,
            arguments: Vec::new(),
            ret: <Meta<'static, ()>>::default().replace("Empty".into()),
            body: Expression::Constant(Meta::default(), "Empty".into(), "Empty".into()),
        }
    }

    pub fn add_argument(&mut self, name: Meta<'a, Box<str>>, ty: Meta<'a, Box<str>>) {
        self.arguments.push((name, ty));
    }

    pub fn set_ret(&mut self, ty: Meta<'a, Box<str>>) {
        self.ret = ty;
    }

    pub fn set_body(&mut self, body: Expression<'a>) {
        self.body = body;
    }

    pub fn type_ck(&self, ctx: &mut Vec<Context>) -> Result<(), CompileError> {
        let context = Context {
            variables: self
                .arguments
                .iter()
                .map(|(a, b)| (a.item.clone(), b.item.clone()))
                .collect(),
            functions: HashMap::new(),
            _phantom: std::marker::PhantomData,
        };
        ctx.push(context);
        let body_ty = self.body.ty(ctx)?;
        if body_ty != self.ret.item {
            CompileError::build(
                &self.ret,
                format_args!(
                    "Mismatched types: expected `{}`, found `{}`",
                    self.ret.item, body_ty
                ),
            ).with_location(&self.body.ty_meta()).build()
        } else {
            ctx.pop();
            Ok(())
            
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Context<'a> {
    variables: HashMap<Box<str>, Box<str>>,
    functions: HashMap<Box<str>, Vec<Box<str>>>,
    _phantom: std::marker::PhantomData<&'a str>,
}

impl Context<'_> {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            _phantom: std::marker::PhantomData,
        }
    }
}

fn get_var_ty<'a, 'b: 'a>(global_ctx: &'b Vec<Context<'a>>, name: &str) -> Option<Box<str>> {
    for ctx in global_ctx.iter().rev() {
        let ty = ctx.variables.get(name);
        if ty.is_some() {
            return ty.cloned();
        }
    }
    None
}

#[derive(Debug, Default)]
pub struct Hir<'a> {
    functions: Vec<Function<'a>>,
    ctx: Context<'a>,
}

impl<'a> Hir<'a> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add_function(&mut self, func: Function<'a>) -> Result<(), CompileError> {
        if let Some(_old) = self.ctx.functions.insert(
            func.name.item.clone(),
            func.arguments
                .iter()
                .map(|(_, b)| b.item.clone())
                .collect(),
        ) {
            CompileError::new(
                &func.name,
                format_args!(
                    "Another function with the name {} is already defined",
                    func.name.item
                ),
            )
        } else {
            self.functions.push(func);
            Ok(())
        }
    }

    pub fn type_ck(&self) -> Result<(), CompileError> {
        let mut ctx = vec![self.ctx.clone()];
        for func in self.functions.iter() {
            func.type_ck(&mut ctx)?;
        }

        if let Some(c) = ctx.pop() {
            assert!(
                ctx.is_empty(),
                "Internal compiler error, more than 1 context after type check: {:?}",
                c
            );
        } else {
            panic!(
                "Internal compiler error, empty context after type check: {:?}",
                ctx
            );
        }
        Ok(())
    }

    pub fn finalize(self) -> Result<mir::Mir, CompileError> {
        MirBuilder::new(self.functions).build()
    }
}
