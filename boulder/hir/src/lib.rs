#[macro_use]
extern crate tindex;

use std::{
    collections::{hash_map::Entry, HashMap},
    fmt,
    marker::PhantomData,
};

use tindex::{TIndex, TVec};

use diagnostics::{CompileError, Meta};

pub mod expr;
pub mod func;
pub mod ty;

pub use func::{Function, FunctionDefinition, FunctionId, VariableId};
pub use ty::{FieldId, Type, TypeId};

use mir::Mir;

#[derive(Debug, Clone)]
pub struct UnresolvedIdentifiers<'a>(PhantomData<&'a str>);

impl<'a> IdentifierState for UnresolvedIdentifiers<'a> {
    type Variable = UnresolvedVariable<'a>;
    type Function = Meta<'a, Box<str>>;
    type Pattern = UnresolvedType<'a>;
    type Scope = Meta<'a, Option<Box<str>>>;
    type Type = Box<str>;
}

#[derive(Debug, Clone)]
pub struct ResolvedIdentifiers<'a>(PhantomData<&'a str>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ScopeId(usize);

impl From<usize> for ScopeId {
    fn from(v: usize) -> Self {
        Self(v)
    }
}

impl TIndex for ScopeId {
    fn as_index(self) -> usize {
        self.0
    }
}

impl<'a> IdentifierState for ResolvedIdentifiers<'a> {
    type Variable = Meta<'a, VariableId>;
    type Function = Meta<'a, FunctionId>;
    type Pattern = TypeId;
    type Scope = Meta<'a, ScopeId>;
    type Type = TypeId;
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
    type Pattern: fmt::Debug + Clone;
    type Scope: fmt::Debug + Clone;
    type Type: fmt::Debug + Clone;
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
    Lt,
    BitOr,
}

#[derive(Debug, Clone)]
pub enum Literal<V: IdentifierState> {
    Integer(u128),
    Unit(V::Type),
}

#[derive(Debug, Clone)]
pub enum Pattern<'a, V: IdentifierState> {
    Named(V::Variable),
    Underscore(Meta<'a, V::Pattern>),
}

#[derive(Debug)]
pub struct Hir<'a, V: IdentifierState, N: TypeState, T, MV> {
    functions: TVec<FunctionId, Function<'a, V, N, T>>,
    types: TVec<TypeId, Type<'a, MV>>,
    type_lookup: HashMap<Box<str>, TypeId>,
}

impl<'a>
    Hir<
        'a,
        UnresolvedIdentifiers<'a>,
        UnresolvedTypes<'a>,
        Option<UnresolvedType<'a>>,
        UnresolvedType<'a>,
    >
{
    pub fn new() -> Self {
        let types = tvec![
            Type {
                name: Meta::<'static, ()>::default().replace("Empty".into()),
                kind: ty::Kind::Unit,
            },
            Type {
                name: Meta::<'static, ()>::default().replace("Never".into()),
                kind: ty::Kind::Uninhabited,
            },
            Type {
                name: Meta::<'static, ()>::default().replace("True".into()),
                kind: ty::Kind::Unit,
            },
            Type {
                name: Meta::<'static, ()>::default().replace("False".into()),
                kind: ty::Kind::Unit,
            },
            Type {
                name: Meta::<'static, ()>::default().replace("Bool".into()),
                kind: ty::Kind::Sum(vec![2.into(), 3.into()]),
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
        ];

        let type_lookup = types
            .iter()
            .enumerate()
            .map(|(i, t)| (t.name.item.clone(), i.into()))
            .collect();

        Self {
            functions: TVec::new(),
            types,
            type_lookup,
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

    pub fn add_type(&mut self, ty: Type<'a, UnresolvedType<'a>>) -> Result<(), CompileError> {
        match self.type_lookup.entry(ty.name.item.clone()) {
            Entry::Occupied(o) => {
                let old = &self.types[*o.get()];
                CompileError::build(
                    &old.name,
                    format_args!("Defined multiple types with the name `{}`", o.key()),
                )
                .with_location(&ty.name)
                .build()
            }
            Entry::Vacant(entry) => {
                entry.insert(self.types.push(ty));
                Ok(())
            }
        }
    }

    pub fn resolve_types(
        self,
    ) -> Result<
        Hir<'a, UnresolvedIdentifiers<'a>, UnresolvedTypes<'a>, Option<UnresolvedType<'a>>, TypeId>,
        CompileError,
    > {
        let mut type_lookup = self.type_lookup;
        let mut types = self
            .types
            .iter()
            .map(|ty| Type {
                name: ty.name.clone(),
                kind: ty::Kind::Struct(Vec::new()),
            })
            .collect::<TVec<_, _>>();

        self.types
            .into_iter()
            .map(|t| t.resolve(&mut types, &mut type_lookup))
            .collect::<Result<Vec<()>, _>>()?;

        ty::check_recursive_ty(&types)?;
        Ok(Hir {
            functions: self.functions,
            types,
            type_lookup,
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
                    f.name.replace(i.into()),
                )
            })
            .collect();

        let mut types = self.types;
        let mut type_lookup = self.type_lookup;

        Ok(Hir {
            functions: self
                .functions
                .into_iter()
                .map(|f| f.resolve_identifiers(&known_functions, &mut types, &mut type_lookup))
                .collect::<Result<TVec<_, _>, CompileError>>()?,
            types,
            type_lookup,
        })
    }
}

impl<'a> Hir<'a, ResolvedIdentifiers<'a>, UnresolvedTypes<'a>, Option<UnresolvedType<'a>>, TypeId> {
    pub fn resolve_expr_types(
        self,
    ) -> Result<Hir<'a, ResolvedIdentifiers<'a>, ResolvedTypes<'a>, TypeId, TypeId>, CompileError>
    {
        let mut type_lookup = self.type_lookup;
        let mut types = self.types;

        let function_definitions = self
            .functions
            .iter()
            .map(|f| f.definition(&mut types, &mut type_lookup))
            .collect::<Result<TVec<_, _>, CompileError>>()?;

        let functions = self
            .functions
            .into_iter()
            .map(|f| f.resolve_expr_types(&function_definitions, &mut types, &mut type_lookup))
            .collect::<Result<TVec<_, _>, CompileError>>()?;

        Ok(Hir {
            functions,
            types,
            type_lookup,
        })
    }
}

impl<'a> Hir<'a, ResolvedIdentifiers<'a>, ResolvedTypes<'a>, TypeId, TypeId> {
    pub fn to_mir(self) -> Result<Mir<mir::traits::InitialMirState>, CompileError> {
        let types: TVec<_, _> = self.types.into_iter().map(|t| t.to_mir()).collect();

        let function_definitions = self
            .functions
            .iter()
            .map(|f| f.definition())
            .collect::<TVec<_, _>>();

        let functions = self
            .functions
            .into_iter()
            .map(|f| f.to_mir(&types, &function_definitions))
            .collect::<Result<TVec<_, _>, CompileError>>()?;

        Ok(Mir { types, functions })
    }
}
