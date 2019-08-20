use diagnostics::{CompileError, Meta, Span};

pub mod expression;
pub mod function;
pub mod ty;

pub use function::{Function, VariableId};
pub use ty::{Type, TypeId};

use mir::Mir;

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

#[derive(Debug)]
pub struct Hir<'a, V, T, N> {
    functions: Vec<Function<'a, V, T, N>>,
    types: Vec<Type>,
}

impl<'a> Hir<'a, UnresolvedVariable<'a>, UnresolvedType, ()> {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            types: vec![
                Type {
                    name: "Empty".into(),
                    kind: ty::Kind::Empty,
                },
                Type {
                    name: "u8".into(),
                    kind: ty::Kind::U8,
                },
                Type {
                    name: "u16".into(),
                    kind: ty::Kind::U16,
                },
                Type {
                    name: "u32".into(),
                    kind: ty::Kind::U32,
                },
            ],
        }
    }

    pub fn add_function(
        &mut self,
        func: Function<'a, UnresolvedVariable<'a>, UnresolvedType, ()>,
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

    pub fn resolve_variables(
        self,
    ) -> Result<Hir<'a, Meta<'a, VariableId>, UnresolvedType, ()>, CompileError> {
        Ok(Hir {
            functions: self
                .functions
                .into_iter()
                .map(|f| f.resolve_variables())
                .collect::<Result<Vec<_>, CompileError>>()?,
            types: self.types,
        })
    }
}

impl<'a> Hir<'a, Meta<'a, VariableId>, UnresolvedType, ()> {
    pub fn resolve_types(
        self,
    ) -> Result<Hir<'a, Meta<'a, VariableId>, TypeId, TypeId>, CompileError> {
        let types = self.types;
        let functions = self
            .functions
            .into_iter()
            .map(|f| f.resolve_types(&types))
            .collect::<Result<Vec<_>, CompileError>>()?;

        Ok(Hir { functions, types })
    }
}

impl<'a> Hir<'a, Meta<'a, VariableId>, TypeId, TypeId> {
    pub fn to_mir(self) -> Result<Mir, CompileError> {
        let types: Vec<_> = self.types.into_iter().map(|t| t.to_mir()).collect();
        let functions = self
            .functions
            .into_iter()
            .map(|f| f.to_mir(&types))
            .collect::<Result<Vec<_>, CompileError>>()?;

        Ok(Mir { types, functions })
    }
}
