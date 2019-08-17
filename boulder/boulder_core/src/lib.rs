use std::{fmt, ops::Range};

// A wrapper storing metadata
#[derive(Clone)]
pub struct Meta<'a, T> {
    pub item: T,
    pub span: Range<usize>,
    pub source: &'a str,
    pub line: u32,
}

impl<T> Meta<'_, T> {
    pub fn line_offset(&self) -> u32 {
        let mut offset = 0;
        for c in self.source[..self.span.start].chars().rev() {
            if c == '\n' {
                break;
            } else {
                offset += 1;
            }
        }

        offset
    }

    pub fn span_str(&self) -> &str {
        &self.source[self.span.clone()]
    }
}

impl<'a, T: fmt::Debug> fmt::Debug for Meta<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Meta")
            .field("item", &self.item)
            .field("span", &&self.source[self.span.clone()])
            .field("line", &self.line)
            .finish()
    }
}
