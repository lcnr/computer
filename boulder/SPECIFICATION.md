# Specification

## Types

### Core types

Three unsigned integers `uN` where n denotes the size in bits: `u8`, `u16`, `u32`.
The unit type `Empty`, implicitly returned by functions and expressions terminated with `;`.
Empty blocks, `while` loops and an a missing `else` also return `Empty`.

### Unit types

Unit types are created with `struct <name>;` and have size 0. They can be instantiated using `<name>`.

### Struct

Functions without an explicit return type return the unit type `Empty`.
`;` changes the value of the previous expression to `Empty`,
this means that multiple `;` in a row are valid.

## Side effects

Side effects are only achievable by using the `read_volitile`, `write_volitile` and `halt` intrinsics.

## Undefined Behavior

The compiler is free to assume that UB can not be executed and may do anything in case UB is encountered.
Do not take UB lightly.

The list of currently known UB:

- integer overflow during addition, substraction and multiplication
- dividing by 0
- endless loops or recursion,
    halting should be implemented using the `halt` intrinsic.
