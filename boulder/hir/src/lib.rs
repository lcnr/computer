use diagnostics::{CompileError, Meta};

use std::collections::HashMap;

pub mod expression;
pub mod function;
//mod to_mir;

pub use function::{Function, VariableId};

#[derive(Debug, Clone)]
pub enum UnresolvedType {
    Named(Box<str>),
    Integer,
    Unknown,
}

pub enum UnresolvedVariable<'a> {
    Simple(Meta<'a, Box<str>>),
    Typed(Meta<'a, Box<str>>, Meta<'a, Box<str>>),
}

pub type Expression<'a> = expression::Expression<'a, UnresolvedVariable<'a>>;

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

#[derive(Debug)]
pub struct Hir<'a, V, T> {
    functions: Vec<Function<'a, V, T>>,
}

impl<'a> Hir<'a, UnresolvedVariable<'a>, UnresolvedType> {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
        }
    }

    pub fn add_function(
        &mut self,
        func: Function<'a, UnresolvedVariable<'a>, UnresolvedType>,
    ) -> Result<(), CompileError> {
        if self.functions.iter().any(|f| f.name.item == func.name.item) {
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

    pub fn resolve_variables(self) -> Result<Hir<'a, Meta<'a, VariableId>, UnresolvedType>, CompileError> {
        Ok(Hir {
            functions: self.functions.into_iter().map(|f| f.resolve_variables()).collect::<Result<Vec<_>, CompileError>>()?,
        })
    }
}

impl<'a> Hir<'a, VariableId, UnresolvedType> {
    pub fn type_ck(mut self) -> Hir<'a, VariableId, TypeId> {
        unimplemented!()
    }
}