use hir::HIR;

pub mod tokenize;

pub struct ParseError;

pub fn parse<F>(meta: &mut Meta) -> Result<HIR, ParseError> {
    unimplemented!();
}