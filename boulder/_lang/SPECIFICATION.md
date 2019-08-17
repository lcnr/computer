# Specification

## Side effects

Side effects are only achievable by using the `read_volitile`, `write_volitile` and `halt` intrinsics.

## Undefined Behavior

The compiler is free to assume that UB can not be executed and may do anything in case UB is encountered.
Do not take UB lightly.

The list of currently known UB:

- integer overflow during addition and subtraction
- endless loops or recursion must not be present if they do not contain side effects,
    halting should be implemented using the `halt` intrinsic.
