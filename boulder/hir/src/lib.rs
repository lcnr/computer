use diagnostics::{CompileError, Meta, Span};

pub mod expression;
pub mod function;
pub mod ty;

pub use function::{Function, FunctionDefinition, FunctionId, VariableId};
pub use ty::{FieldId, Type, TypeId};

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

#[derive(Debug, Clone)]
pub struct UnresolvedTypes<'a>(PhantomData<&'a str>);

impl<'a> TypeState for UnresolvedTypes<'a> {
    type Type = ();
    type Field = Meta<'a, Box<str>>;
    type Restriction = Meta<'a, UnresolvedType<'a>>;
}

#[derive(Debug, Clone)]
pub struct ResolvingTypes<'a>(PhantomData<&'a str>);

impl<'a> TypeState for ResolvingTypes<'a> {
    type Type = solver::EntityId;
    type Field = Meta<'a, Box<str>>;
    type Restriction = ();
}

#[derive(Debug, Clone)]
pub struct ResolvedTypes<'a>(PhantomData<&'a str>);

impl<'a> TypeState for ResolvedTypes<'a> {
    type Type = TypeId;
    type Field = Meta<'a, FieldId>;
    type Restriction = ();
}

pub trait TypeState: fmt::Debug + Clone {
    type Type: fmt::Debug + Clone;
    type Field: fmt::Debug + Clone;
    type Restriction: fmt::Debug + Clone;
}

pub trait IdentifierState: fmt::Debug + Clone {
    type Variable: fmt::Debug + Clone;
    type Function: fmt::Debug + Clone;
}

#[derive(Debug, Clone)]
pub enum UnresolvedType<'a> {
    /// `A | B | C`
    Sum(Vec<Meta<'a, Box<str>>>),
    Named(Box<str>),
    Integer,
}

#[derive(Debug, Clone)]
pub enum UnresolvedVariable<'a> {
    Existing(Meta<'a, Box<str>>),
    New(Meta<'a, Box<str>>, Meta<'a, Option<UnresolvedType<'a>>>),
}

#[derive(Debug, Clone, Copy)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    BitOr,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(u128),
}

#[derive(Debug)]
pub struct Hir<'a, V: IdentifierState, N: TypeState, T, MV> {
    functions: Vec<Function<'a, V, N, T>>,
    types: Vec<Type<'a, MV>>,
}

impl<'a>
    Hir<'a, UnresolvedIdentifiers<'a>, UnresolvedTypes<'a>, Option<UnresolvedType<'a>>, Box<str>>
{
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            types: vec![
                Type {
                    name: Meta::<'static, ()>::default().replace("Empty".into()),
                    kind: ty::Kind::Unit,
                },
                Type {
                    name: Meta::<'static, ()>::default().replace("Never".into()),
                    kind: ty::Kind::Uninhabited,
                },
                Type {
                    name: Meta::<'static, ()>::default().replace("u8".into()),
                    kind: ty::Kind::U8,
                },
                Type {
                    name: Meta::<'static, ()>::default().replace("u16".into()),
                    kind: ty::Kind::U16,
                },
                Type {
                    name: Meta::<'static, ()>::default().replace("u32".into()),
                    kind: ty::Kind::U32,
                },
            ],
        }
    }

    pub fn add_function(
        &mut self,
        func: Function<
            'a,
            UnresolvedIdentifiers<'a>,
            UnresolvedTypes<'a>,
            Option<UnresolvedType<'a>>,
        >,
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

    pub fn add_type(&mut self, ty: Type<'a, Box<str>>) -> Result<(), CompileError> {
        if self.types.iter().any(|t| t.name.item == ty.name.item) {
            CompileError::new(
                &ty.name,
                format_args!(
                    "Another type with the name `{}` is already defined",
                    ty.name.item
                ),
            )
        } else {
            self.types.push(ty);
            Ok(())
        }
    }

    pub fn resolve_types(
        self,
    ) -> Result<
        Hir<'a, UnresolvedIdentifiers<'a>, UnresolvedTypes<'a>, Option<UnresolvedType<'a>>, TypeId>,
        CompileError,
    > {
        let lookup = self
            .types
            .iter()
            .enumerate()
            .map(|(i, ty)| (ty.name.item.clone(), TypeId(i)))
            .collect::<HashMap<_, _>>();

        let types = self
            .types
            .into_iter()
            .map(|ty| ty.resolve(&lookup))
            .collect::<Result<Vec<_>, _>>()?;

        ty::check_recursive_ty(&types)?;
        Ok(Hir {
            functions: self.functions,
            types,
        })
    }
}

impl<'a>
    Hir<'a, UnresolvedIdentifiers<'a>, UnresolvedTypes<'a>, Option<UnresolvedType<'a>>, TypeId>
{
    pub fn resolve_identifiers(
        self,
    ) -> Result<
        Hir<'a, ResolvedIdentifiers<'a>, UnresolvedTypes<'a>, Option<UnresolvedType<'a>>, TypeId>,
        CompileError,
    > {
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

impl<'a> Hir<'a, ResolvedIdentifiers<'a>, UnresolvedTypes<'a>, Option<UnresolvedType<'a>>, TypeId> {
    pub fn resolve_expr_types(
        self,
    ) -> Result<Hir<'a, ResolvedIdentifiers<'a>, ResolvedTypes<'a>, TypeId, TypeId>, CompileError>
    {
        let mut types = self.types;
        let mut type_lookup = types
            .iter()
            .enumerate()
            .map(|(i, t)| (t.name.item.clone(), ty::TypeId(i)))
            .collect::<HashMap<_, _>>();

        let functions = self
            .functions
            .iter()
            .map(|f| f.definition(&mut types, &mut type_lookup))
            .collect::<Result<Vec<_>, CompileError>>()?;

        let functions = self
            .functions
            .into_iter()
            .map(|f| f.resolve_expr_types(&functions, &mut types, &mut type_lookup))
            .collect::<Result<Vec<_>, CompileError>>()?;

        Ok(Hir { functions, types })
    }
}

impl<'a> Hir<'a, ResolvedIdentifiers<'a>, ResolvedTypes<'a>, TypeId, TypeId> {
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
