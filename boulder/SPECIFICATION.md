# Specification

## Types

### Core types

Three unsigned integers `uN` where n denotes the size in bits: `u8`, `u16`, `u32`.
The unit type `Empty`, implicitly returned by functions and expressions terminated with `;`.
Empty blocks, `while` loops and an a missing `else` also return `Empty`.

### Unit types

Unit types are created with `struct <name>;` and have size 0. They can be instantiated using `<name>`.

Functions without an explicit return type return the unit type `Empty`.
`;` changes the value of the previous expression to `Empty`,
this means that multiple `;` in a row are valid.

### Structs

Structs can be defined using `struct <name> { (<field>: <type>),* }` and instantiated using `<name> { (<field>: <value>),* }`.
Their layout is undefined any must be be relied upon, this means that transmuting structs is always UB.

### Unions

Unions can be defined using `union <name> { (<field>: <type>),* }` and instantiated using `<name> { <field>: <value> }`.
All fields of the union start at the same position, meaning that transmuting between unions is well definedas long as the currently
initialized type is part of both unions.

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
- struct layout
