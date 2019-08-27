# RObust Compile Kit

An assembler targeting the architecture specified in [COMMANDS.md][commands].

Assembly files should have the file ending `.ra` for `rock assembly`.

## Quick Start

All commands are terminated by a semi colon. As the targeted architecture has 256 different 256 byte blocks,
there are two kind of location types. `blocks` which can be any valid identifier or `.segments` which have to start with a `.`.
A segment can only be used as jump address within in the same block. This compiler currently does not allow for blocks which
are longer than 256 bytes. While most commands are listed in [COMMANDS.md][commands], there are some special commands, notably
`byte <data>` which gets compiled to a plain byte in memory.

## Examples

```
start:
    mov 0b1111_1011 A;
    mov 4 B;
    add A; # this is a single line comment, A is now 0xff
    inv A;
.loop:
    if z jmp .loop; # this program now loops forever
```


# Roadmap

- [x] MVP
  - [x] Tokenizer
  - [x] Parser
  - [x] resolve jmp targets
  - [x] code generation
  - [x] implement all commands
- [ ] include files
- [ ] use content of other blocks: `data_block.mem`

[commands]: ../COMMANDS.md
