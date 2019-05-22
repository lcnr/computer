# RObust Compile Kit

An assembler targeting the architecture specified in [COMMANDS.md][commands].

Assembly files should have the file ending `.ra` for `rock assembly`.

## Quick Start

All commands are terminated by a semi colon. As the targeted architecture has 256 different 256 byte blocks,
there are two kind of location types. `blocks` which can be any valid identifier or `.segments` which have to start with a `.`.
A segment can only be used as jump addresses within in the same block. This compiler currently does not allow for blocks which
are longer than 256 bytes. While most commands are listed in [COMMANDS.md][commands], there are some special commands, notably
`byte <data>` which stores 1 byte in memory.

## Examples

```
start:
    loadc 0b1111_1011;
    addc 4;
    inv;
.loop:
    jmpzc .loop;
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