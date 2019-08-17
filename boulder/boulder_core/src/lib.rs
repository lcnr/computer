use std::{fmt, ops::Range};

// A wrapper storing metadata
#[derive(Clone)]
pub struct Meta<'a, T> {
    pub data: T,
    pub span: Range<usize>,
    pub source: &'a str,
    pub line: u32,
}

impl<'a, T: fmt::Debug> fmt::Debug for Meta<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Meta")
            .field("data", &self.data)
            .field("span", &&self.source[self.span.clone()])
            .field("line", &self.line)
            .finish()
    }
}
