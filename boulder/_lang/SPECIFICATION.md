# Specification

## Types

Functions without an explicit return type return `struct Empty`, which is a type with size 0 and exactly 1 value: `Empty`.
`;` changes the value of the previous expression to `Empty`, this means that multiple `;`.

## Side effects

Side effects are only achievable by using the `read_volitile`, `write_volitile` and `halt` intrinsics.

## Undefined Behavior

The compiler is free to assume that UB can not be executed and may do anything in case UB is encountered.
Do not take UB lightly.

The list of currently known UB:

- integer overflow during addition and subtraction
- endless loops or recursion must not be present if they do not contain side effects,
    halting should be implemented using the `halt` intrinsic.
