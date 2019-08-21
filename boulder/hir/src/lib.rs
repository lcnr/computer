use diagnostics::{CompileError, Meta, Span};

pub mod expression;
pub mod function;
pub mod ty;

pub use function::{Function, FunctionDefinition, FunctionId, VariableId};
pub use ty::{Type, TypeId};

use mir::Mir;

use std::{collections::HashMap, convert::TryFrom, fmt, marker::PhantomData};

#[derive(Debug, Clone)]
pub struct UnresolvedIdentifiers<'a>(PhantomData<&'a str>);

impl<'a> IdentifierState for UnresolvedIdentifiers<'a> {
    type Variable = UnresolvedVariable<'a>;
    type Function = Meta<'a, Box<str>>;
}

#[derive(Debug, Clone)]
pub struct ResolvedIdentifiers<'a>(PhantomData<&'a str>);

impl<'a> IdentifierState for ResolvedIdentifiers<'a> {
    type Variable = Meta<'a, VariableId>;
    type Function = Meta<'a, FunctionId>;
}

pub trait IdentifierState: fmt::Debug + Clone {
    type Variable: fmt::Debug + Clone;
    type Function: fmt::Debug + Clone;
}

#[derive(Debug, Clone)]
pub enum UnresolvedType {
    Named(Box<str>),
    Integer,
    Unknown,
}

#[derive(Debug, Clone)]
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
pub struct Hir<'a, V: IdentifierState, T, N> {
    functions: Vec<Function<'a, V, T, N>>,
    types: Vec<Type>,
}

impl<'a> Hir<'a, UnresolvedIdentifiers<'a>, UnresolvedType, ()> {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            types: vec![
                Type {
                    name: "Empty".into(),
                    kind: ty::Kind::Empty,
                },
                Type {
                    name: "Never".into(),
                    kind: ty::Kind::Never,
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
        func: Function<'a, UnresolvedIdentifiers<'a>, UnresolvedType, ()>,
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

    pub fn resolve_identifiers(
        self,
    ) -> Result<Hir<'a, ResolvedIdentifiers<'a>, UnresolvedType, ()>, CompileError> {
        let known_functions = self
            .functions
            .iter()
            .enumerate()
            .map(|(i, f)| {
                (
                    f.name.item.clone(),
                    f.name.simplify().replace(FunctionId(i)),
                )
            })
            .collect();

        Ok(Hir {
            functions: self
                .functions
                .into_iter()
                .map(|f| f.resolve_identifiers(&known_functions))
                .collect::<Result<Vec<_>, CompileError>>()?,
            types: self.types,
        })
    }
}

impl<'a> Hir<'a, ResolvedIdentifiers<'a>, UnresolvedType, ()> {
    pub fn resolve_types(
        self,
    ) -> Result<Hir<'a, ResolvedIdentifiers<'a>, TypeId, TypeId>, CompileError> {
        let types = self.types;
        let type_lookup = types
            .iter()
            .enumerate()
            .map(|(i, t)| (&t.name, ty::TypeId(i)))
            .collect::<HashMap<_, _>>();

        let functions = self
            .functions
            .iter()
            .map(|f| f.definition(&type_lookup))
            .collect::<Result<Vec<_>, CompileError>>()?;

        let functions = self
            .functions
            .into_iter()
            .map(|f| f.resolve_types(&functions, &types, &type_lookup))
            .collect::<Result<Vec<_>, CompileError>>()?;

        Ok(Hir { functions, types })
    }
}

impl<'a> Hir<'a, ResolvedIdentifiers<'a>, TypeId, TypeId> {
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
