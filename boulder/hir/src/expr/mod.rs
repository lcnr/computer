use diagnostics::Meta;

use crate::{
    traits::{IdentifierState, TypeState},
    Literal, Pattern,
};

mod ident;
mod to_mir;
mod type_ck;

pub use ident::ResolveIdentifiersContext;
pub use to_mir::ToMirContext;
pub use type_ck::TypeConstraintsContext;

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperation {
    Invert,
    ToBytes,
    FromBytes,
}

#[derive(Debug, Clone, Copy)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Shl,
    Shr,
    Gt,
    Gte,
    Eq,
    Neq,
    Lte,
    Lt,
    BitOr,
    BitAnd,
}

#[derive(Debug, Clone)]
pub struct MatchArm<'a, V: IdentifierState, N: TypeState> {
    pub pattern: Pattern<'a, V>,
    pub expr: Expression<'a, V, N>,
}

#[derive(Debug, Clone)]
pub enum Expression<'a, V: IdentifierState, N: TypeState> {
    Block(N::Type, V::Scope, Vec<Expression<'a, V, N>>),
    Variable(N::Type, V::Variable),
    Lit(N::Type, Meta<'a, Literal<V>>),
    UnaryOperation(N::Type, Meta<'a, UnaryOperation>, Box<Expression<'a, V, N>>),
    Binop(
        N::Type,
        Meta<'a, Binop>,
        Box<Expression<'a, V, N>>,
        Box<Expression<'a, V, N>>,
    ),
    Statement(N::Type, Box<Expression<'a, V, N>>),
    Assignment(N::Type, V::Variable, Box<Expression<'a, V, N>>),
    InitializeStruct(
        N::Type,
        Meta<'a, V::Type>,
        Vec<(N::Field, Expression<'a, V, N>)>,
    ),
    FunctionCall(N::Type, V::Function, Vec<Expression<'a, V, N>>),
    FieldAccess(N::Type, Box<Expression<'a, V, N>>, N::Field),
    Match(
        N::Type,
        Meta<'a, ()>,
        Box<Expression<'a, V, N>>,
        Vec<MatchArm<'a, V, N>>,
    ),
    Loop(N::Type, V::Scope, Vec<Expression<'a, V, N>>),
    Break(N::Type, V::Scope, Box<Expression<'a, V, N>>),
    TypeRestriction(Box<Expression<'a, V, N>>, N::Restriction),
}

impl<'a, T: IdentifierState, N: TypeState> diagnostics::Span<'a> for Expression<'a, T, N>
where
    T::Variable: diagnostics::Span<'a>,
    T::Function: diagnostics::Span<'a>,
    T::Scope: diagnostics::Span<'a>,
    N::Field: diagnostics::Span<'a>,
{
    fn span(&self) -> Meta<'a, ()> {
        match self {
            Expression::Block(_, scope, _) => scope.span().simplify(),
            Expression::Variable(_, var) => var.span(),
            Expression::Lit(_, lit) => lit.simplify(),
            Expression::UnaryOperation(_, op, expr) => op.simplify().append(expr.span()),
            Expression::Binop(_, _op, a, b) => a.span().append(b.span()),
            Expression::Statement(_, expr) => expr.span().extend_right(';'),
            Expression::Assignment(_, var, expr) => var.span().simplify().append(expr.span()),
            Expression::InitializeStruct(_, name, _) => name.span(),
            Expression::FunctionCall(_, name, _args) => name.span(),
            Expression::FieldAccess(_, _, field) => field.span().extend_left('.'),
            Expression::Match(_, meta, _, _) => meta.clone(),
            Expression::Loop(_, scope, _) => scope.span().simplify(),
            Expression::Break(_, scope, _) => scope.span().simplify(),
            Expression::TypeRestriction(expr, _) => expr.span(),
        }
    }
}
