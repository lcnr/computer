# Boulder

WIP: A high level programming language

## Features

- implicit sum types, the folder `boulder/tests/compile_run/type/sum/` contains some examples.
- inferred types: for example `let a: u32 = 7u32` can be rewritten as `let a = 7u32`
- type ascriptions: the type of expressions and patterns can always be directly ascribed
    without having to resort to temporary variables, for example `let a: u32 = (7: u32 + 8): u32`

## Goals

Learn about type theory, optimizations and other interesting subjects.
Explore the uses of a strict and *clearly defined* set of undefined behavior.

## Non-Goals

- error recovery: this complicates the implementation and is not that helpful while compile times are low. (this might be added later)

## Inspirations

This language is mostly inspired by rust.
Other inspirations:

- Haskell
  - sum types

## Roadmap

- match `u8`
- sum to `u8`
- generate assembly
