# Boulder

WIP: A high level programming language

## Structure

- `boulder`: code written in boulder
- `cli`: a small binary which glues all other parts together
- `meta`: a crate which enables easy and helpful error messages
- `hir`: the high level intermediate language
- `parse`: a library which creates the HIR

## Goals

Learn about type theory, optimizations and other interesting subjects.
The current design idea should sadly not require any kind of type inference, as all
generic functions require explicit type annotations and operators can not
be overloaded.

## Design limitations

- support methods (`object.method` instead of `method(object)`)

## Inspirations

This language is mostly inspired by rust.
Other inspirations:

- Haskell
  - sum types
- Zig
  - constant evaluation
  - types as types

## Roadmap

- [ ] MVP

[commands]: ../COMMANDS.md