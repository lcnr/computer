# Micro Command

Notes:

- To update a register, Update IR has to be `0b01`.

0. Increment P
1. Bus Input
2. Bus Input
3. Bus Input
---
4. Update Register
5. Update Register
6. Update Register
7. Reset P1 (P1 = 0)
---
8. ALU
9. ALU
10. ALU
11. ALU
---
12. Update RAM
13. RAM select M (default P)
14. Update IR
15. Update IR

|          | Bus Input | Update IR     | Update Register   |
| -------- | --------- | ------------- | ----------------- |
| `0b000`  | RAM       | don't         | A                 |
| `0b001`  | ALU       | UPDATE REG    | B                 |
| `0b010`  | -         | if Carry == 1 | C                 |
| `0b011`  | -         | if ALU == 0   | D                 |
| `0b100`  | A         | -             | M1                |
| `0b101`  | B         | -             | M2                |
| `0b110`  | C         | -             | P1                |
| `0b111`  | D         | -             | P2                |

## ALU

|          | Output     | Carry | Zero   |          | Output     | Carry  | Zero   |
| -------- | ---------- | ----- | ------ | -------- | ---------- | -----  | ------ |
| `0b0000` | A + B      | -     | -      | `0b1000` | -          | -      | -      |
| `0b0001` | A - B      | A < B | A == B | `0b1001` | -          | A >= B | A != B |
| `0b0010` | A and B    | A > B | -      | `0b1010` | -          | A <= B | -      |
| `0b0011` | A or B     | -     | -      | `0b1011` | -          | -      | -      |
| `0b0100` | A xor B    | -     | -      | `0b1100` | -          | -      | -      |
| `0b0101` | A shl B    | -     | -      | `0b1101` | -          | -      | -      |
| `0b0110` | A shr B    | -     | -      | `0b1110` | -          | -      | -      |
| `0b0111` | A          | -     | A == 0 | `0b1111` | inverted A | -      | A != 0 |

## Update IR

| Command | ALU      | Z/C |
| ------- | -------- | --- |
| A == 0  | `0b0111` | Z   |
| A != 0  | `0b1111` | Z   |
| A > B   | `0b0010` | C   |
| A >= B  | `0b1001` | C   |
| A == B  | `0b0001` | Z   |
| A != B  | `0b1001` | Z   |
| A <= B  | `0b1010` | C   |
| A < B   | `0b0001` | C   |

# Commands

*steps are stored in RAM with the order 1, 2, 4, 3 to prevent timing errors. (Huffman code 00 01 11 10)*

- `%n`: a unconditional command `n`, excluding constant access
- `$q`: a useable constant `q`. `%n $q` calls the command `n` with the constant `q`

| Opcode     | Name         | Function                                                          |
| ---------- | ------------ | ----------------------------------------------------------------- |
| `0x00`     | idle         | do nothing/simply increment PC by 1                               |
| `0x01`     | add A        | A = A + B; PC += 1;                                               |
| `0x02`     | add B        | B = A + B; PC += 1;                                               |
| `0x03`     | add C        | C = A + B; PC += 1;                                               |
| `0x04`     | add D        | D = A + B; PC += 1;                                               |
| `0x05`     | add mem      | mem[MR] = A + B; PC += 1;                                         |
| `0x06`     | add M1       | M1 = A + B; PC += 1;                                              |
| `0x07`     | add M2       | M2 = A + B; PC += 1;                                              |
| `0x08`     | sub A        | A = A - B; PC += 1;                                               |
| `0x09`     | sub B        | B = A - B; PC += 1;                                               |
| `0x0a`     | sub C        | C = A - B; PC += 1;                                               |
| `0x0b`     | sub D        | D = A - B; PC += 1;                                               |
| `0x0c`     | sub mem      | mem[MR] = A - B; PC += 1;                                         |
| `0x0d`     | sub M1       | M1 = A - B; PC += 1;                                              |
| `0x0e`     | sub M2       | M2 = A - B; PC += 1;                                              |
| `0x0f`     | and A        | A = A & B; PC += 1;                                               |
| `0x10`     | and B        | B = A & B; PC += 1;                                               |
| `0x11`     | and C        | C = A & B; PC += 1;                                               |
| `0x12`     | and D        | D = A & B; PC += 1;                                               |
| `0x13`     | and mem      | mem[MR] = A & B; PC += 1;                                         |
| `0x14`     | and M1       | M1 = A & B; PC += 1;                                              |
| `0x15`     | and M2       | M2 = A & B; PC += 1;                                              |
| `0x16`     | or A         | A = A \| B; PC += 1;                                              |
| `0x17`     | or B         | B = A \| B; PC += 1;                                              |
| `0x18`     | or C         | C = A \| B; PC += 1;                                              |
| `0x19`     | or D         | D = A \| B; PC += 1;                                              |
| `0x1a`     | or mem       | mem[MR] = A \| B; PC += 1;                                        |
| `0x1b`     | or M1        | M1 = A \| B; PC += 1;                                             |
| `0x1c`     | or M2        | M2 = A \| B; PC += 1;                                             |
| `0x1d`     | xor A        | A = A ^ B; PC += 1;                                               |
| `0x1e`     | xor B        | B = A ^ B; PC += 1;                                               |
| `0x1f`     | xor C        | C = A ^ B; PC += 1;                                               |
| `0x20`     | xor D        | D = A ^ B; PC += 1;                                               |
| `0x21`     | xor mem      | mem[MR] = A ^ B; PC += 1;                                         |
| `0x22`     | xor M1       | M1 = A ^ B; PC += 1;                                              |
| `0x23`     | xor M2       | M2 = A ^ B; PC += 1;                                              |
| `0x24`     | inv A        | A = !A; PC += 1;                                                  |
| `0x25`     | inv B        | B = !A; PC += 1;                                                  |
| `0x26`     | inv C        | C = !A; PC += 1;                                                  |
| `0x27`     | inv D        | D = !A; PC += 1;                                                  |
| `0x28`     | inv mem      | mem[MR] = !A; PC += 1;                                            |
| `0x29`     | inv M1       | M1 = !A; PC += 1;                                                 |
| `0x2a`     | inv M2       | M2 = !A; PC += 1;                                                 |
| `0x2b`     | shr A        | A = A >> B; PC += 1;                                              |
| `0x2c`     | shr B        | B = A >> B; PC += 1;                                              |
| `0x2d`     | shr C        | C = A >> B; PC += 1;                                              |
| `0x2e`     | shr D        | D = A >> B; PC += 1;                                              |
| `0x2f`     | shr mem      | mem[MR] = A >> B; PC += 1;                                        |
| `0x30`     | shr M1       | M1 = A >> B; PC += 1;                                             |
| `0x31`     | shr M2       | M2 = A >> B; PC += 1;                                             |
| `0x32`     | shl A        | A = A << B; PC += 1;                                              |
| `0x33`     | shl B        | B = A << B; PC += 1;                                              |
| `0x34`     | shl C        | C = A << B; PC += 1;                                              |
| `0x35`     | shl D        | D = A << B; PC += 1;                                              |
| `0x36`     | shl mem      | mem[MR] = A << B; PC += 1;                                        |
| `0x37`     | shl M1       | M1 = A << B; PC += 1;                                             |
| `0x38`     | shl M2       | M2 = A << B; PC += 1;                                             |
| `0x39`     | mov A B      | B = A; PC += 1;                                                   |
| `0x3a`     | mov A C      | C = A; PC += 1;                                                   |
| `0x3b`     | mov A D      | D = A; PC += 1;                                                   |
| `0x3c`     | mov A mem    | mem[MR] = A; PC += 1;                                             |
| `0x3d`     | mov A M1     | M1 = A; PC += 1;                                                  |
| `0x3e`     | mov A M2     | M2 = A; PC += 1;                                                  |
| `0x3f`     | mov B A      | A = B; PC += 1;                                                   |
| `0x40`     | mov B C      | C = B; PC += 1;                                                   |
| `0x41`     | mov B D      | D = B; PC += 1;                                                   |
| `0x42`     | mov B mem    | mem[MR] = B; PC += 1;                                             |
| `0x43`     | mov B M1     | M1 = B; PC += 1;                                                  |
| `0x44`     | mov B M2     | M2 = B; PC += 1;                                                  |
| `0x45`     | mov C A      | A = C; PC += 1;                                                   |
| `0x46`     | mov C B      | B = C; PC += 1;                                                   |
| `0x47`     | mov C D      | D = C; PC += 1;                                                   |
| `0x48`     | mov C mem    | mem[MR] = C; PC += 1;                                             |
| `0x49`     | mov C M1     | M1 = C; PC += 1;                                                  |
| `0x4a`     | mov C M2     | M2 = C; PC += 1;                                                  |
| `0x4b`     | mov D A      | A = D; PC += 1;                                                   |
| `0x4c`     | mov D B      | B = D; PC += 1;                                                   |
| `0x4d`     | mov D C      | C = D; PC += 1;                                                   |
| `0x4e`     | mov D mem    | mem[MR] = D; PC += 1;                                             |
| `0x4f`     | mov D M1     | M1 = D; PC += 1;                                                  |
| `0x50`     | mov D M2     | M2 = D; PC += 1;                                                  |
| `0x51`     | mov mem A    | A = mem[MR]; PC += 1;                                             |
| `0x52`     | mov mem B    | B = mem[MR]; PC += 1;                                             |
| `0x53`     | mov mem C    | C = mem[MR]; PC += 1;                                             |
| `0x54`     | mov mem D    | D = mem[MR]; PC += 1;                                             |
| `0x55`     | mov mem M1   | M1 = mem[MR]; PC += 1;                                            |
| `0x56`     | mov mem M2   | M2 = mem[MR]; PC += 1;                                            |
| `0x57$1`   | mov $1 A     | A = mem[PC + 1]; PC += 2;                                         |
| `0x58$1`   | mov $1 B     | B = mem[PC + 1]; PC += 2;                                         |
| `0x59$1`   | mov $1 C     | C = mem[PC + 1]; PC += 2;                                         |
| `0x5a$1`   | mov $1 D     | D = mem[PC + 1]; PC += 2;                                         |
| `0x5b$1`   | mov $1 M1    | M1 = mem[PC + 1]; PC += 2;                                        |
| `0x5c$1`   | mov $1 M2    | M2 = mem[PC + 1]; PC += 2;                                        |
| `0x60`     | jmp A        | P1 = A;                                                           |
| `0x61`     | jmp B        | P1 = B;                                                           |
| `0x62`     | jmp C        | P1 = C;                                                           |
| `0x63`     | jmp D        | P1 = D;                                                           |
| `0x64`     | jmp mem      | P1 = mem[MR];                                                     |
| `0x65$1`   | jmp $1       | P1 = mem[PC + 1];                                                 |
| `0x66`     | ljmp A       | P2 = A; P1 = 0;                                                   |
| `0x67`     | ljmp B       | P2 = B; P1 = 0;                                                   |
| `0x68`     | ljmp C       | P2 = C; P1 = 0;                                                   |
| `0x69`     | ljmp D       | P2 = D; P1 = 0;                                                   |
| `0x6a`     | ljmp mem     | P2 = mem[MR]; P1 = 0;                                             |
| `0x6b$1`   | ljmp $1      | P2 = mem[PC + 1]; P1 = 0;                                         |
| `0x6c`     | ret A B      | P2 = A; P1 = B;                                                   |
| `0x6d`     | ret A C      | P2 = A; P1 = C;                                                   |
| `0x6e`     | ret A D      | P2 = A; P1 = D;                                                   |
| `0x6f`     | ret A mem    | P2 = A; P1 = mem[MR];                                             |
| `0x70$1`   | ret A $1     | P2 = A; P1 = mem[PC + 1];                                         |
| `0x71`     | ret B A      | P2 = B; P1 = A;                                                   |
| `0x72`     | ret B C      | P2 = B; P1 = C;                                                   |
| `0x73`     | ret B D      | P2 = B; P1 = D;                                                   |
| `0x74`     | ret B mem    | P2 = B; P1 = mem[MR];                                             |
| `0x75$1`   | ret B $1     | P2 = B; P1 = mem[PC + 1];                                         |
| `0x76`     | ret C A      | P2 = C; P1 = A;                                                   |
| `0x77`     | ret C B      | P2 = C; P1 = B;                                                   |
| `0x78`     | ret C D      | P2 = C; P1 = D;                                                   |
| `0x79`     | ret C mem    | P2 = C; P1 = mem[MR];                                             |
| `0x7a$1`   | ret C $1     | P2 = C; P1 = mem[PC + 1];                                         |
| `0x7b`     | ret D A      | P2 = D; P1 = A;                                                   |
| `0x7c`     | ret D B      | P2 = D; P1 = B;                                                   |
| `0x7d`     | ret D C      | P2 = D; P1 = C;                                                   |
| `0x7e`     | ret D mem    | P2 = D; P1 = mem[MR];                                             |
| `0x7f$1`   | ret D $1     | P2 = D; P1 = mem[PC + 1];                                         |
| `0x80`     | ret mem A    | P2 = mem[MR]; P1 = A;                                             |
| `0x81`     | ret mem B    | P2 = mem[MR]; P1 = B;                                             |
| `0x82`     | ret mem C    | P2 = mem[MR]; P1 = C;                                             |
| `0x83`     | ret mem D    | P2 = mem[MR]; P1 = D;                                             |
| `0x84$1`   | ret mem $1   | P2 = mem[MR]; P1 = mem[PC + 1];                                   |
| `0x85$1`   | ret $1 A     | P2 = mem[PC + 1]; P1 = A;                                         |
| `0x86$1`   | ret $1 B     | P2 = mem[PC + 1]; P1 = B;                                         |
| `0x87$1`   | ret $1 C     | P2 = mem[PC + 1]; P1 = C;                                         |
| `0x88$1`   | ret $1 D     | P2 = mem[PC + 1]; P1 = D;                                         |
| `0x89$1`   | ret $1 mem   | P2 = mem[PC + 1]; P1 = mem[MR];                                   |
| `0xc0%1`   | if z %1      | if A == 0 { execute command %1 } else { PC += 2; }                |
| `0xc1%1`   | if n %1      | if A != 0 { execute command %1 } else { PC += 2; }                |
| `0xc2%1`   | if gt %1     | if A > B { execute command %1 } else { PC += 2; }                 |
| `0xc3%1`   | if gte %1    | if A >= B { execute command %1 } else { PC += 2; }                |
| `0xc4%1`   | if eq %1     | if A == B { execute command %1 } else { PC += 2; }                |
| `0xc5%1`   | if neq %1    | if A != B { execute command %1 } else { PC += 2; }                |
| `0xc6%1`   | if lte %1    | if A <= B { execute command %1 } else { PC += 2; }                |
| `0xc7%1`   | if lt %1     | if A < B { execute command %1 } else { PC += 2; }                 |
| `0xc8%1$2` | if z %1 $2   | if A == 0 { execute command %1 $2 } else { PC += 3; }             |
| `0xc9%1$2` | if nz %1 $2  | if A != 0 { execute command %1 $2 } else { PC += 3; }             |
| `0xca%1$2` | if gt %1 $2  | if A > B { execute command %1 $2 } else { PC += 3; }              |
| `0xcb%1$2` | if gte %1 $2 | if A >= B { execute command %1 $2 } else { PC += 3; }             |
| `0xcc%1$2` | if eq %1 $2  | if A == B { execute command %1 $2 } else { PC += 3; }             |
| `0xcd%1$2` | if neq %1 $2 | if A != B { execute command %1 $2 } else { PC += 3; }             |
| `0xce%1$2` | if lte %1 $2 | if A <= B { execute command %1 $2 } else { PC += 3; }             |
| `0xcf%1$2` | if lt %1 $2  | if A < B { execute command %1 $2 } else { PC += 3; }              |
