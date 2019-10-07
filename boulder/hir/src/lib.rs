#[macro_use]
extern crate tindex;

use std::iter;

use tindex::TVec;

use shared_id::{FunctionId, TypeId, BOOL_TYPE_ID, FALSE_TYPE_ID, TRUE_TYPE_ID};

use diagnostics::{CompileError, Meta};

use mir::Mir;

pub mod expr;
pub mod func;
pub mod module;
pub mod traits;
pub mod ty;

use func::{Function, VariableId};
use module::Module;
use traits::{
    IdentifierState, ResolvedIdentifiers, ResolvedTypes, TypeState, UnresolvedIdentifiers,
    UnresolvedTypes,
};
use ty::Type;

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

#[derive(Debug, Clone)]
pub struct Attribute<'a> {
    pub name: Meta<'a, &'a str>,
    pub args: Vec<Meta<'a, &'a str>>,
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
                name: Meta::<'static, ()>::default().replace("Empty".into()),
                at: Vec::new(),
                attributes: Vec::new(),
                kind: ty::Kind::Unit,
            },
            Type {
                name: Meta::<'static, ()>::default().replace("Never".into()),
                at: Vec::new(),
                attributes: Vec::new(),
                kind: ty::Kind::Uninhabited,
            },
            Type {
                name: Meta::<'static, ()>::default().replace("True".into()),
                at: Vec::new(),
                attributes: Vec::new(),
                kind: ty::Kind::Unit,
            },
            Type {
                name: Meta::<'static, ()>::default().replace("False".into()),
                at: Vec::new(),
                attributes: Vec::new(),
                kind: ty::Kind::Unit,
            },
            Type {
                name: Meta::<'static, ()>::default().replace("Bool".into()),
                at: Vec::new(),
                attributes: Vec::new(),
                kind: ty::Kind::Sum(
                    iter::once(TRUE_TYPE_ID)
                        .chain(iter::once(FALSE_TYPE_ID))
                        .collect()
                ),
            },
            Type {
                name: Meta::<'static, ()>::default().replace("u8".into()),
                at: Vec::new(),
                attributes: Vec::new(),
                kind: ty::Kind::U8,
            },
            Type {
                name: Meta::<'static, ()>::default().replace("u16".into()),
                at: Vec::new(),
                attributes: Vec::new(),
                kind: ty::Kind::U16,
            },
            Type {
                name: Meta::<'static, ()>::default().replace("u32".into()),
                at: Vec::new(),
                attributes: Vec::new(),
                kind: ty::Kind::U32,
            },
        ];

        let mut modules = Module::new(Meta::fake(()));
        for (i, ty) in types.iter().enumerate() {
            modules
                .add_type(&[], ty.name.item.clone(), TypeId::from(i))
                .unwrap();
        }

        modules
            .add_type(
                &[],
                format!("{} | {}", TRUE_TYPE_ID, FALSE_TYPE_ID).into(),
                BOOL_TYPE_ID,
            )
            .unwrap();

        Self {
            functions: TVec::new(),
            types,
            modules,
        }
    }

    pub fn add_module(
        &mut self,
        at: &[Box<str>],
        module: Meta<'a, Box<str>>,
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
        at: &[Box<str>],
        func: Function<
            'a,
            UnresolvedIdentifiers<'a>,
            UnresolvedTypes<'a>,
            Option<UnresolvedType<'a>>,
        >,
    ) -> Result<(), CompileError> {
        let name = func.name.item.clone();
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
        at: &[Box<str>],
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
    pub fn to_mir(self) -> Result<Mir, CompileError> {
        let types: TVec<_, _> = self.types.into_iter().map(|t| t.to_mir()).collect();

        let function_definitions = self
            .functions
            .iter()
            .map(|f| f.definition())
            .collect::<TVec<_, _>>();

        let lang_items = mir::LangItems {
            divide: self
                .functions
                .iter()
                .position(|f| {
                    f.attributes.iter().any(|attr| {
                        attr.name.item == "lang_item"
                            && attr.args.iter().any(|a| a.item == "divide")
                    })
                })
                .map_or_else(
                    || {
                        CompileError::new(
                            &Meta::<'static, ()>::default(),
                            "Missing lang item: `divide`",
                        )
                    },
                    |p| Ok(FunctionId::from(p)),
                )?,
        };

        let functions = self
            .functions
            .into_iter()
            .map(|f| f.to_mir(&types, &function_definitions))
            .collect::<Result<TVec<_, _>, CompileError>>()?;

        Ok(Mir {
            types,
            functions,
            lang_items,
        })
    }
}
