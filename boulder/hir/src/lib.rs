use boulder_core::{CompileError, Meta};

use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone)]
pub enum Expression<'a> {
    Block(Meta<'a, ()>, Vec<Expression<'a>>),
    Constant(Meta<'a, ()>, Box<str>, Box<str>),
    Variable(Meta<'a, ()>, Box<str>),
    Binop(
        Meta<'a, Binop>,
        Box<Expression<'a>>,
        Box<Expression<'a>>,
    ),
    VariableDecl(Meta<'a, (Box<str>, Box<str>)>, Box<Expression<'a>>),
    Assignment(Meta<'a, Box<str>>, Box<Expression<'a>>),
}

impl Expression<'_> {
    pub fn meta(&self) -> Meta<'_, ()> {
        match self {
            Expression::Block(meta, _v) => meta.simplify(),
            Expression::Constant(meta, _value, _ty) => meta.simplify(),
            Expression::Variable(meta, _var) => meta.clone(),
            Expression::Binop(_op, a, b) => a.meta().append(b.meta()),
            Expression::VariableDecl(var, expr) => var.simplify().append(expr.meta()),
            Expression::Assignment(var, expr) => var.simplify().append(expr.meta()),
        }
    }

    pub fn ty<'a>(&self, ctx: &mut Vec<Context>) -> Result<Box<str>, CompileError> {
        match self {
            Expression::Block(_meta, v) => {
                ctx.push(Context::new());
                if let Some((last, start)) = v.split_last() {
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
            Expression::Variable(meta, variable) => get_var_ty(ctx, variable).ok_or_else(|| {
                CompileError::new::<_, (), _>(
                    &meta,
                    format_args!("Unknown variable: `{}`", variable),
                ).unwrap_err()
            }),
            Expression::Binop(op, a, b) => {
                let a_ty = a.ty(ctx)?;
                let b_ty = b.ty(ctx)?;
                if a_ty != b_ty {
                    CompileError::new(&op, format_args!("Mismatched types: {:?} is only implemented for identical types: {} != {}", op.item, a_ty, b_ty))
                } else {
                    Ok(a_ty)
                }
            }
            Expression::VariableDecl(var, expr) => {
                let expr_ty = expr.ty(ctx)?;
                if var.1 != expr_ty {
                    CompileError::new(&var, format_args!("Mismatched types: Tried to assign {} to a variable of type {}", var.1, expr_ty))
                } else {
                    Ok("Empty".into())
                }
            }
            Expression::Assignment(var, expr) => {
                if let Some(ty) = get_var_ty(ctx, var) {
                    let expr_ty = expr.ty(ctx)?;
                    if ty != expr_ty {
                        CompileError::new(&var, format_args!("Mismatched types: Tried to assign {} to a variable of type {}", expr_ty, ty))
                    } else {
                        Ok("Empty".into())
                    }
                } else {
                    CompileError::new(
                        &var,
                        format_args!("Unknown variable: `{}`", var.item),
                    )
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
            variables: self.arguments.iter().map(|(a, b)| (a.item.clone(), b.item.clone())).collect(),
            functions: HashMap::new(),
            _phantom: std::marker::PhantomData,
        };
        ctx.push(context);
        let body_ty = self.body.ty(ctx)?;
        ctx.pop();
        if body_ty != self.ret.item {
            CompileError::new(
                &self.ret,
                format_args!(
                    "mismatched types: expected `{}`, found `{}`",
                    self.ret.item, body_ty
                ),
            )
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Context<'a> {
    variables: HashMap<Box<str>, Box<str>>,
    functions: HashMap<Box<str>, Vec<(Box<str>, Box<str>)>>,
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
pub struct HIR<'a> {
    functions: Vec<Function<'a>>,
    ctx: Context<'a>,
}

impl<'a> HIR<'a> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add_function(&mut self, func: Function<'a>) -> Result<(), CompileError> {
        if let Some(_old) = self.ctx.functions.insert(func.name.item.clone(), func.arguments.iter().map(|(a, b)| (a.item.clone(), b.item.clone())).collect()) {
            CompileError::new(&func.name, format_args!("Another function with the name {} is already defined", func.name.item))
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

        if let Some(_c) = ctx.pop() {
            assert!(ctx.is_empty(), "Internal compiler error, more than 1 context after type check: {:?}", ctx);
        } else {
            panic!("Internal compiler error, empty context after type check: {:?}", ctx);
        }
        Ok(())
    }
}
