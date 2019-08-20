use boulder_core::{CompileError, Meta};

use std::collections::HashMap;

pub mod expression;
pub mod function;
mod to_mir;

pub use function::{Function, VariableId};

use to_mir::MirBuilder;

#[derive(Debug, Clone)]
pub enum UnresolvedType {
    Named(Box<str>),
    Integer,
    Unknown,
}

pub type Expression<'a> = expression::Expression<'a, Box<str>, UnresolvedType>;

#[derive(Debug, Clone, Copy)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Copy)]
pub struct TypeId(usize);

#[derive(Debug, Clone)]
pub enum Type {
    TypeId(TypeId),
    Named(Box<str>),
    Integer,
    Unknown,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(u128),
}

#[derive(Debug, Default, Clone)]
pub struct Context<'a, T> {
    variables: HashMap<Box<str>, Box<str>>,
    functions: HashMap<Box<str>, Vec<T>>,
    _phantom: std::marker::PhantomData<&'a str>,
}

impl<T> Context<'_, T> {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            _phantom: std::marker::PhantomData,
        }
    }
}

fn get_var_ty<'a, 'b: 'a, T>(global_ctx: &'b Vec<Context<'a, T>>, name: &str) -> Option<Box<str>> {
    for ctx in global_ctx.iter().rev() {
        let ty = ctx.variables.get(name);
        if ty.is_some() {
            return ty.cloned();
        }
    }
    None
}

#[derive(Debug)]
pub struct Hir<'a, T> {
    functions: Vec<Function<'a, VariableId, T>>,
    ctx: Context<'a, T>,
}

impl<'a> Hir<'a, UnresolvedType> {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            ctx: Context::new(),
        }
    }

    pub fn add_function(
        &mut self,
        func: Function<'a, VariableId, UnresolvedType>,
    ) -> Result<(), CompileError> {
        if let Some(_old) = self.ctx.functions.insert(
            func.name.item.clone(),
            func.arguments
                .iter()
                .map(|a| func.variables[a.0].ty.item.clone())
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

    /*pub fn type_ck(&self) -> Result<(), CompileError> {
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
    }*/

    //pub fn finalize(self) -> Result<mir::Mir, CompileError> {
    //   MirBuilder::new(self.functions).build()
    //}
}
