#[macro_use]
extern crate lazy_static;

use std::{
    fmt,
    io::{stderr, Write},
    marker::PhantomData,
    mem,
    ops::Range,
    ops::{Deref, DerefMut},
    sync::Mutex,
};

lazy_static! {
    static ref OUTPUT: Mutex<Box<dyn Write + Send>> = Mutex::new(Box::new(stderr()));
}

/// This has a private field to force people to use associated functions to create an object.
#[derive(Debug)]
pub struct CompileError(());

pub struct CompileErrorBuilder<R = ()>(PhantomData<R>);

impl<R> Drop for CompileErrorBuilder<R> {
    fn drop(&mut self) {
        panic!("`CompileErrorBuilder` must be used by calling `build`");
    }
}

impl<R> CompileErrorBuilder<R> {
    pub fn with_location<T>(self, meta: &Meta<T>) -> Self {
        let offset = meta.line_offset();
        let pos = format!("({}:{}): ", meta.line, offset);
        writeln!(OUTPUT.lock().unwrap(), "{}{}", pos, meta.line_str()).unwrap();
        for _ in 0..(offset as usize + pos.len()) {
            write!(OUTPUT.lock().unwrap(), " ").unwrap();
        }
        for _ in 0..meta.span.len().max(1) {
            write!(OUTPUT.lock().unwrap(), "^").unwrap();
        }
        writeln!(OUTPUT.lock().unwrap(), "").unwrap();
        self
    }

    pub fn with_help<D: fmt::Display>(self, help: D) -> Self {
        writeln!(OUTPUT.lock().unwrap(), "  help: {}", help).unwrap();
        self
    }

    pub fn build(self) -> Result<R, CompileError> {
        mem::forget(self);
        Err(CompileError(()))
    }
}

impl CompileError {
    pub fn new<T, R, D: fmt::Display>(meta: &Meta<T>, err: D) -> Result<R, Self> {
        Self::build(meta, err).build()
    }

    pub fn build<T, R, D: fmt::Display>(meta: &Meta<T>, err: D) -> CompileErrorBuilder<R> {
        writeln!(OUTPUT.lock().unwrap(), "[ERROR]: {}", err).unwrap();
        CompileErrorBuilder(PhantomData).with_location(meta)
    }

    pub fn expected<T, R>(expected: &dyn fmt::Debug, meta: &Meta<T>) -> Result<R, Self> {
        Self::new(
            meta,
            format_args!("Expected {:?} found `{}`", expected, meta.span_str()),
        )
    }

    pub fn set_output(output: Box<dyn Write + Send>) {
        *OUTPUT.lock().unwrap() = output;
    }
}

// A wrapper storing metadata
#[derive(Clone)]
pub struct Meta<'a, T> {
    pub item: T,
    pub span: Range<usize>,
    pub source: &'a str,
    pub line: u32,
}

impl<T> Deref for Meta<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.item
    }
}

impl<T> DerefMut for Meta<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.item
    }
}

impl<T: Default> Default for Meta<'static, T> {
    fn default() -> Self {
        Self {
            item: Default::default(),
            span: 0..0,
            source: "",
            line: 0,
        }
    }
}

impl<'a> Meta<'a, ()> {
    pub fn empty(src: &'a str, line: u32, span: Range<usize>) -> Self {
        Meta {
            item: (),
            span,
            source: src,
            line: line,
        }
    }

    pub fn append(self, other: Self) -> Self {
        Meta {
            item: (),
            span: self.span.start..other.span.end,
            source: self.source,
            line: self.line,
        }
    }
}

impl<'a, T> Meta<'a, T> {
    pub fn fake(item: T) -> Meta<'a, T> {
        Meta {
            item,
            span: 0..11,
            source: &"<fake meta>",
            line: 0
        }
    }

    pub fn simplify(&self) -> Meta<'a, ()> {
        Meta {
            item: (),
            span: self.span.clone(),
            source: self.source,
            line: self.line,
        }
    }

    pub fn extend_left(self, to: char) -> Self {
        let target = self.source[..self.span.start].rfind(to).unwrap();
        Self {
            span: target..self.span.end,
            ..self
        }
    }

    pub fn extend_right(self, to: char) -> Self {
        let target = self.source[self.span.end..].find(to).unwrap();
        Self {
            span: self.span.start..self.span.end + target + to.len_utf8(),
            ..self
        }
    }

    pub fn replace<U>(self, new: U) -> Meta<'a, U> {
        Meta {
            item: new,
            span: self.span,
            source: self.source,
            line: self.line,
        }
    }

    pub fn map<F, U>(self, f: F) -> Meta<'a, U>
    where
        F: FnOnce(T) -> U,
    {
        Meta {
            item: f(self.item),
            span: self.span,
            source: self.source,
            line: self.line,
        }
    }

    pub fn try_map<F, U>(self, f: F) -> Result<Meta<'a, U>, CompileError>
    where
        F: FnOnce(T) -> Result<U, CompileError>,
    {
        Ok(Meta {
            item: f(self.item)?,
            span: self.span,
            source: self.source,
            line: self.line,
        })
    }

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

    pub fn line_str(&self) -> &str {
        let line_start = self.source[..self.span.start]
            .rfind('\n')
            .map_or(0, |v| v + 1);
        self.source[line_start..].split('\n').next().unwrap()
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

pub trait Span<'a> {
    fn span(&self) -> Meta<'a, ()>;
}

impl<'a, T> Span<'a> for Meta<'a, T> {
    fn span(&self) -> Meta<'a, ()> {
        self.simplify()
    }
}
