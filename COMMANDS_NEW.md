# Micro Commandz

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
7. 
---
8. ALU
9. ALU
10. ALU
11. 
---
12. Update RAM
13. RAM select M (default P)
14. Update IR
15. Update IR

|          | ALU           | Bus Input | Update IR     | Update Register   |
| -------- | ------------- | --------- | ------------- | ----------------- |
| `0b000`  | A & B         | RAM       | don't         | A                 |
| `0b001`  | A \| B        | ALU       | UPDATE REG    | B                 |
| `0b010`  | A ^ B         | -         | if Carry == 1 | C                 |
| `0b011`  | A - B         | -         | if ALU == 0   | D                 |
| `0b100`  | A + B         | A         | -             | M1                |
| `0b101`  | A << B        | B         | -             | M2                |
| `0b110`  | A >> B        | C         | -             | P1                |
| `0b111`  | !A            | D         | -             | P2                |

# Commands

*steps are stored in RAM with the order 1, 2, 4, 3 to prevent timing errors. (Huffman code 00 01 11 10)*

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
| `0x57vv`   | mov *v* A    | A = mem[PC + 1]; PC += 2;                                         |
| `0x58vv`   | mov *v* B    | B = mem[PC + 1]; PC += 2;                                         |
| `0x59vv`   | mov *v* C    | C = mem[PC + 1]; PC += 2;                                         |
| `0x5avv`   | mov *v* D    | D = mem[PC + 1]; PC += 2;                                         |
| `0x5bvv`   | mov *v* M1   | M1 = mem[PC + 1]; PC += 2;                                        |
| `0x5cvv`   | mov *v* M2   | M2 = mem[PC + 1]; PC += 2;                                        |
