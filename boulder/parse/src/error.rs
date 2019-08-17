use boulder_core::Meta;

use std::fmt::{Debug, Display};

/// This has a private field to force people to use associated functions to create an object.
#[derive(Debug, Clone)]
pub struct ParseError(());

impl ParseError {
    pub fn new<T, R>(meta: &Meta<T>, err: impl Display) -> Result<R, Self> {
        eprintln!("[ERROR]: {}\n", err);
        eprintln!(
            "({}:{}): {}",
            meta.line,
            meta.line_offset(),
            meta.span_str()
        );
        Err(ParseError(()))
    }

    pub fn expected<T, R>(expected: &dyn Debug, meta: &Meta<T>) -> Result<R, Self> {
        ParseError::new(
            meta,
            format_args!("Expected {:?} found `{}`", expected, meta.span_str()),
        )
    }
}
