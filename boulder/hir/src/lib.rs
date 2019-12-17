#[cfg(feature = "profiler")]
#[macro_use]
extern crate thread_profiler;

#[macro_use]
extern crate tindex;

use std::iter;

use tindex::TVec;

use shared_id::{FunctionId, TypeId, FALSE_TYPE_ID, TRUE_TYPE_ID};

use diagnostics::{CompileError, Meta};

use mir::Mir;

pub mod attr;
pub mod expr;
pub mod func;
mod mir_ctx;
pub mod module;
pub mod traits;
pub mod ty;

use func::{Function, VariableId};
use module::Module;
use traits::{
    IdentifierState, ResolvedIdentifiers, ResolvedTypes, TypeState, UnresolvedIdentifiers,
    UnresolvedTypes,
};
use ty::{Field, Type};

#[derive(Debug, Clone)]
pub enum UnresolvedType<'a> {
    /// `A | B | C`
    Sum(Vec<Meta<'a, Box<str>>>),
    Named(Box<str>),
    Integer,
}

#[derive(Debug, Clone)]
pub enum UnresolvedVariable<'a> {
    Existing(Meta<'a, &'a str>),
    New(Meta<'a, &'a str>, Meta<'a, Option<UnresolvedType<'a>>>),
}

impl<'a> diagnostics::Span<'a> for UnresolvedVariable<'a> {
    fn span(&self) -> Meta<'a, ()> {
        match self {
            UnresolvedVariable::Existing(s) => s.span(),
            UnresolvedVariable::New(n, _) => n.span(),
        }
    }
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
    modules: Module<'a>,
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
                name: Meta::fake("Empty".into()),
                at: Vec::new(),
                attributes: Vec::new(),
                kind: ty::Kind::Unit,
            },
            Type {
                name: Meta::fake("Never".into()),
                at: Vec::new(),
                attributes: Vec::new(),
                kind: ty::Kind::Uninhabited,
            },
            Type {
                name: Meta::fake("True".into()),
                at: Vec::new(),
                attributes: Vec::new(),
                kind: ty::Kind::Unit,
            },
            Type {
                name: Meta::fake("False".into()),
                at: Vec::new(),
                attributes: Vec::new(),
                kind: ty::Kind::Unit,
            },
            Type {
                name: Meta::fake("Bool".into()),
                at: Vec::new(),
                attributes: Vec::new(),
                kind: ty::Kind::Sum(
                    iter::once(TRUE_TYPE_ID)
                        .chain(iter::once(FALSE_TYPE_ID))
                        .collect()
                ),
            },
            Type {
                name: Meta::fake("u8".into()),
                at: Vec::new(),
                attributes: Vec::new(),
                kind: ty::Kind::U8,
            },
            Type {
                name: Meta::fake("u16".into()),
                at: Vec::new(),
                attributes: Vec::new(),
                kind: ty::Kind::U16,
            },
            Type {
                name: Meta::fake("u16Bytes".into()),
                at: Vec::new(),
                attributes: Vec::new(),
                kind: ty::Kind::Struct(tvec![
                    Field {
                        name: Meta::fake("a"),
                        ty: Meta::fake(UnresolvedType::Named("u8".into())),
                    },
                    Field {
                        name: Meta::fake("b"),
                        ty: Meta::fake(UnresolvedType::Named("u8".into())),
                    }
                ]),
            },
            Type {
                name: Meta::fake("u32".into()),
                at: Vec::new(),
                attributes: Vec::new(),
                kind: ty::Kind::U32,
            },
            Type {
                name: Meta::fake("u32Bytes".into()),
                at: Vec::new(),
                attributes: Vec::new(),
                kind: ty::Kind::Struct(tvec![
                    Field {
                        name: Meta::fake("a"),
                        ty: Meta::fake(UnresolvedType::Named("u8".into())),
                    },
                    Field {
                        name: Meta::fake("b"),
                        ty: Meta::fake(UnresolvedType::Named("u8".into())),
                    },
                    Field {
                        name: Meta::fake("c"),
                        ty: Meta::fake(UnresolvedType::Named("u8".into())),
                    },
                    Field {
                        name: Meta::fake("d"),
                        ty: Meta::fake(UnresolvedType::Named("u8".into())),
                    }
                ]),
            },
        ];

        let mut modules = Module::new(Meta::fake(()));
        for (i, ty) in types.index_iter().zip(types.iter()) {
            modules.add_type(&[], ty.name.item.clone(), i).unwrap();
            if let &ty::Kind::Sum(ref kinds) = &ty.kind {
                modules
                    .add_type(&[], Box::from(ty::sum_ty_name(kinds)), i)
                    .unwrap();
            }
        }

        Self {
            functions: TVec::new(),
            types,
            modules,
        }
    }

    pub fn add_module(
        &mut self,
        at: &[&'a str],
        module: Meta<'a, &'a str>,
    ) -> Result<(), CompileError> {
        let err = module.clone();
        match self.modules.add_module(at, module) {
            Ok(()) => Ok(()),
            Err(other) => CompileError::build(
                &other,
                format_args!("Defined multiple modules with the name `{}`", &err.item),
            )
            .with_location(&err)
            .build(),
        }
    }

    pub fn add_function(
        &mut self,
        at: &[&'a str],
        func: Function<
            'a,
            UnresolvedIdentifiers<'a>,
            UnresolvedTypes<'a>,
            Option<UnresolvedType<'a>>,
        >,
    ) -> Result<(), CompileError> {
        let name = func.name.item;
        let id = self.functions.push(func);
        match self.modules.add_function(at, name, id) {
            Ok(()) => Ok(()),
            Err(other) => CompileError::build(
                &self.functions[other].name,
                format_args!(
                    "Defined multiple functions with the name `{}`",
                    &self.functions[id].name.item
                ),
            )
            .with_location(&self.functions[id].name)
            .build(),
        }
    }

    pub fn add_type(
        &mut self,
        at: &[&'a str],
        ty: Type<'a, UnresolvedType<'a>>,
    ) -> Result<(), CompileError> {
        let name = ty.name.item.clone();
        let id = self.types.push(ty);
        match self.modules.add_type(at, name, id) {
            Ok(()) => Ok(()),
            Err(other) => CompileError::build(
                &self.types[other].name,
                format_args!(
                    "Defined multiple types with the name `{}`",
                    &self.types[id].name.item
                ),
            )
            .with_location(&self.types[id].name)
            .build(),
        }
    }

    pub fn resolve_types(
        self,
    ) -> Result<
        Hir<'a, UnresolvedIdentifiers<'a>, UnresolvedTypes<'a>, Option<UnresolvedType<'a>>, TypeId>,
        CompileError,
    > {
        let mut modules = self.modules;
        let mut types = self
            .types
            .iter()
            .map(|ty| Type {
                name: ty.name.clone(),
                at: ty.at.clone(),
                attributes: ty.attributes.clone(),
                kind: ty::Kind::Struct(TVec::new()),
            })
            .collect::<TVec<_, _>>();

        self.types
            .into_iter()
            .map(|t| t.resolve(&mut types, &mut modules))
            .collect::<Result<Vec<()>, _>>()?;

        ty::check_recursive_ty(&types)?;
        Ok(Hir {
            functions: self.functions,
            types,
            modules,
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
        let mut types = self.types;
        let mut modules = self.modules;

        Ok(Hir {
            functions: self
                .functions
                .into_iter()
                .map(|f| f.resolve_identifiers(&mut types, &mut modules))
                .collect::<Result<TVec<_, _>, CompileError>>()?,
            types,
            modules,
        })
    }
}

impl<'a> Hir<'a, ResolvedIdentifiers<'a>, UnresolvedTypes<'a>, Option<UnresolvedType<'a>>, TypeId> {
    pub fn resolve_expr_types(
        self,
    ) -> Result<Hir<'a, ResolvedIdentifiers<'a>, ResolvedTypes<'a>, TypeId, TypeId>, CompileError>
    {
        let mut modules = self.modules;
        let mut types = self.types;

        let function_definitions = self
            .functions
            .iter()
            .map(|f| f.definition(&mut types, &mut modules))
            .collect::<Result<TVec<_, _>, CompileError>>()?;

        let functions = self
            .functions
            .into_iter()
            .map(|f| f.resolve_expr_types(&function_definitions, &mut types, &mut modules))
            .collect::<Result<TVec<_, _>, CompileError>>()?;

        Ok(Hir {
            functions,
            types,
            modules,
        })
    }
}

impl<'a> Hir<'a, ResolvedIdentifiers<'a>, ResolvedTypes<'a>, TypeId, TypeId> {
    pub fn to_mir(mut self) -> Result<Mir<'a>, CompileError> {
        let function_definitions = self
            .functions
            .iter()
            .map(|f| f.definition())
            .collect::<TVec<_, _>>();

        let ctx = mir_ctx::ContextBuilder::build(&mut self, &function_definitions)?;

        let types: TVec<_, _> = self.types.into_iter().map(|t| t.to_mir()).collect();

        let functions = self
            .functions
            .into_iter()
            .map(|f| f.to_mir(&types, &function_definitions))
            .collect::<Result<TVec<_, _>, CompileError>>()?;

        Ok(Mir {
            types,
            functions,
            ctx,
        })
    }
}
