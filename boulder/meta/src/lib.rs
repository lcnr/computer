use std::cmp;
use std::marker::PhantomData;
use std::ops::Drop;
use std::cell::Cell;
use std::mem;
use std::io::Write;

use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

#[derive(Debug)]
pub struct MetaData<'a> {
    file: usize,
    line: usize,
    pos: usize,
    len: usize,
    _marker: PhantomData<&'a str>,
}

impl<'a> MetaData<'a> {
    /// requires both metadata to come from the same file
    pub fn combine(&self, other: &MetaData<'a>) -> MetaData<'a> {
        assert_eq!(self.file, other.file);
        let pos = cmp::min(self.line, other.line);
        Self {
            file: self.file,
            line: cmp::min(self.line, other.line),
            pos,
            len: cmp::max(self.pos + self.len, other.pos + other.len) - pos,
            _marker: PhantomData,
        }
    }
}

#[derive(Debug)]
struct File {
    path: String,
    content: String,
}

#[derive(Debug)]
pub struct Meta {
    files: Vec<File>,
    success: Cell<bool>,
}

impl Meta {
    pub fn new() -> Self {
        Self { files: Vec::new(), success: Cell::new(true) }
    }

    pub fn add_file(&mut self, path: String, content: String) {
        self.files.push(File { path, content });
    }

    pub fn file<'a>(&'a self, meta: &MetaData<'a>) -> &'a str {
        &self.files[meta.file].path
    }

    pub fn resolve<'a>(&'a self, meta: &MetaData<'a>) -> &'a str {
        &self.files[meta.file].content[meta.pos..meta.pos + meta.len]
    }

    pub fn msg<'a, 'b>(&'a self, lvl: Level, msg: &'b str) -> DiagnosticBuilder<'a, 'b, '_> {
        DiagnosticBuilder { meta: self, lvl, msg, origin: &[] }
    }
}

#[derive(Debug)]
pub enum Level {
    Bug,
    Error,
    Warning,
    Note,
}

#[derive(Debug)]
pub struct DiagnosticBuilder<'a, 'b, 'c> {
    meta: &'a Meta,
    lvl: Level,
    msg: &'b str,
    origin: &'c [MetaData<'a>],
}

impl<'a, 'b, 'c> DiagnosticBuilder<'a, 'b, 'c> {
    pub fn origin(&mut self, origin: &'c [MetaData<'a>]) {
        self.origin = origin;
    }

    /// do not emit this diagnostic
    pub fn discard(self) {
        // this should not leak anything, as a Diagnostic Builder owns nothing on the heap
        mem::forget(self);
    }

    /// emit this diagnostic to stderr
    pub fn emit(self) {
        match self.lvl {
            Level::Bug | Level::Error => self.meta.success.set(false),
            Level::Warning | Level::Note => (),
        }

        let mut stderr = StandardStream::stderr(ColorChoice::Auto);
        match self.lvl {
            Level::Bug | Level::Error => stderr.set_color(ColorSpec::new().set_fg(Some(Color::Red))),
            Level::Warning | Level::Note => stderr.set_color(ColorSpec::new().set_fg(Some(Color::Yellow))),
        }.unwrap();
        
        writeln!(stderr, "{}: {}", match self.lvl {
            Level::Bug => "BUG",
            Level::Error => "ERROR",
            Level::Warning => "WARN",
            Level::Note => "NOTE",
        }, self.msg).unwrap();

        stderr.set_color(ColorSpec::new().set_fg(None)).unwrap();
        for origin in self.origin {
            self.display_origin(&mut stderr, origin);
        }
    }

    fn display_origin(&self, stderr: &mut StandardStream, origin: &MetaData) {
        writeln!(stderr, "{}:{}", self.meta.file(origin), origin.line).unwrap();
        writeln!(stderr, "{}", self.meta.resolve(origin)).unwrap();
    }
}

impl<'a, 'b, 'c> Drop for DiagnosticBuilder<'a, 'b, 'c> {
    fn drop(&mut self) {
        panic!("Diagnostics have to either be discarded or emitted! Cause:\n {:#?}", self);
    }
}
