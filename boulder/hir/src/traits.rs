use std::{fmt::Debug, marker::PhantomData};

use diagnostics::Meta;

use shared_id::{FieldId, FunctionId, TypeId};

use crate::{func::ScopeId, UnresolvedType, UnresolvedVariable, VariableId};

pub trait IdentifierState: Debug + Clone {
    type Variable: Debug + Clone;
    type Function: Debug + Clone;
    type Pattern: Debug + Clone;
    type Scope: Debug + Clone;
    type Type: Debug + Clone;
}

#[derive(Debug, Clone)]
pub struct UnresolvedIdentifiers<'a>(PhantomData<&'a str>);

impl<'a> IdentifierState for UnresolvedIdentifiers<'a> {
    type Variable = UnresolvedVariable<'a>;
    type Function = Meta<'a, &'a str>;
    type Pattern = UnresolvedType<'a>;
    type Scope = Meta<'a, Option<&'a str>>;
    type Type = Box<str>;
}

#[derive(Debug, Clone)]
pub struct ResolvedIdentifiers<'a>(PhantomData<&'a str>);

impl<'a> IdentifierState for ResolvedIdentifiers<'a> {
    type Variable = Meta<'a, VariableId>;
    type Function = Meta<'a, FunctionId>;
    type Pattern = TypeId;
    type Scope = Meta<'a, ScopeId>;
    type Type = TypeId;
}

pub trait TypeState: Debug + Clone {
    type Type: Debug + Clone;
    type Field: Debug + Clone;
    type Restriction: Debug + Clone;
}

#[derive(Debug, Clone)]
pub struct UnresolvedTypes<'a>(PhantomData<&'a str>);

impl<'a> TypeState for UnresolvedTypes<'a> {
    type Type = ();
    type Field = Meta<'a, &'a str>;
    type Restriction = Meta<'a, UnresolvedType<'a>>;
}

#[derive(Debug, Clone)]
pub struct ResolvingTypes<'a>(PhantomData<&'a str>);

impl<'a> TypeState for ResolvingTypes<'a> {
    type Type = solver::EntityId;
    type Field = Meta<'a, &'a str>;
    type Restriction = solver::EntityId;
}

#[derive(Debug, Clone)]
pub struct ResolvedTypes<'a>(PhantomData<&'a str>);

impl<'a> TypeState for ResolvedTypes<'a> {
    type Type = TypeId;
    type Field = Meta<'a, FieldId>;
    type Restriction = TypeId;
}
