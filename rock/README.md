# RObust Compile Kit

An assembler targeting the architecture specified in [COMMANDS.md][commands].

Assembly files should have the file ending `.ra` for `rock assembly`.

## Examples

```
start:
    loadc 0b1111_1011
    addc 4
    inv
.loop
    jmpzc .loop
```

# Roadmap

1. [ ] finish an MVP

[commands]: ../COMMANDS.md