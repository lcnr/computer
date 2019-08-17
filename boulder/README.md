# Boulder

WIP: A high level programming language

## Structure

- `boulder`: code written in boulder
- `cli`: a small binary which glues all other parts together
- `hir`: the high level intermediate language
- `parse`: a library which creates the HIR

## Goals

Learn about type theory, optimizations and other interesting subjects.
Explore the uses of a strict and *clearly defined* set of undefined behavior.

## Non-Goals

- error recovery complicates the implementation and is not that helpful while compile times are low. (this might be added later)

## Inspirations

This language is mostly inspired by rust.
Other inspirations:

- Haskell
  - sum types
## Roadmap

- [ ] MVP

[commands]: ../COMMANDS.md