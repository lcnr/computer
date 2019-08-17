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

| Opcode     | Name     | Function                                                           |
| ---------- | -------- | ------------------------------------------------------------------ |
| `0x00`     | idle     | do nothing/simply increment PC by 1                                |
| `0x01`     | add A    | A = A + B; PC += 1;                                       |
| `0x02`     | add B    | B = A + B; PC += 1;                                        |
| `0x03`     | add C    | C = A + B; PC += 1;                    |
| `0x04`     | add D    | D = A + B; PC += 1;                                      |
| `0x05`     | add mem  | mem[MR] = A + B; PC += 1;                                 | 
| `0x06`     | add M1   | M1 = A + B; PC += 1;                                   |
| `0x07`     | add M2   | M2 = A + B; PC += 1;                               |
| `0x08`     | addc A   | A = A + mem[PC+1]; PC += 2;                                   |
| `0x09`     | addc B   | B = B + mem[PC+1]; PC += 2;                                |
| `0x0a`     | addc C   | C = C + mem[PC+1]; PC += 2;                                    |
| `0x0b`     | addc D   | D = D + mem[PC+1]; PC += 2;                               |
| `0x0c`     | addc mem | mem[MR] = mem[MR] + mem[PC+1]; PC += 1;         |   <--- possible?
| `0x0d`     |   |                                     |
| `0x0e`     |   |                                     |
| `0x0f`     | inv      | Accu = !Accu; PC += 1;                                             |
| `0x10`     | loadc    | Accu = mem[PC + 1]; PC += 2;                                       |
| `0x11`     | loadm    | Accu = mem[MR]; PC += 1;                                           |
| `0x12`     | store    | mem[MR] = Accu; PC += 1;                                           |
| `0x13`     | zero     | Accu = 0; PC += 1;                                                 |
| `0x14`     | setsc    | M1 = mem[PC + 1]; PC += 2;                                         |
| `0x15`     | setsa    | M1 = Accu; PC += 1;                                                |
| `0x16`     | setbc    | M2 = mem[PC + 1]; PC += 2;                                         |
| `0x17`     | setba    | M2 = Accu; PC += 1;                                                |
| `0x18`     | gets     | Accu = M1; PC += 1;                                                |
| `0x19`     | getb     | Accu = M2; PC += 1;                                                |
| `0x1a`     | jmpc     | P1 = mem[PC + 1];                                                  |
| `0x1b`     | jmpm     | P1 = mem[MR];                                                      |
| `0x1c`     | jmpa     | P1 = Accu;                                                         |
| `0x1d`     | ljmpc    | P1 = 0; P2 = mem[PC + 1];                                          |
| `0x1e`     | ljmpm    | P1 = 0; P2 = mem[MR];                                              |
| `0x1f`     | ljmpa    | P1 = 0; P2 = Accu;                                                 |
| `0x2021`   | jmpzc    | if Accu == 0 { P1 = mem[PC + 2] } else { PC += 3 };                |
| `0x2120`   | jmpnzc   | if Accu != 0 { P1 = mem[PC + 2] } else { PC += 3 };                |
| `0x2223`   | jmpzm    | if Accu == 0 { P1 = mem[MR] } else { PC += 2 };                    |
| `0x2322`   | jmpnzm   | if Accu != 0 { P1 = mem[MR] } else { PC += 2 };                    |
| `0x2425`   | ljmpzc   | if Accu == 0 { P1 = 0; P2 = mem[PC + 1] } else { PC += 3 };        |
| `0x2524`   | ljmpnzc  | if Accu != 0 { P1 = 0; P2 = mem[PC + 1] } else { PC += 3 };        |
| `0x2627`   | ljmpzm   | if Accu == 0 { P1 = 0; P2 = mem[MR] } else { PC += 2 };            |
| `0x2726`   | ljmpnzm  | if Accu != 0 { P1 = 0; P2 = mem[MR] } else { PC += 2 };            |
| `0x28xx29` | jmpgtcc  | if Accu > mem[PC + 1] { P1 = mem[PC + 3] } else { PC += 4}         |
| `0x29xx28` | jmpltecc | if Accu <= mem[PC + 1] { P1 = mem[PC + 3] } else { PC += 4}        |
| `0x2axx2b` | jmpltcc  | if Accu < mem[PC + 1] { P1 = mem[PC + 3] } else { PC += 4}         |
| `0x2bxx2a` | jmpgtecc | if Accu >= mem[PC + 1] { P1 = mem[PC + 3] } else { PC += 4}        |
| `0x2cxx2d` | jmpeqcc  | if Accu == mem[PC + 1] { P1 = mem[PC + 3] } else { PC += 4}        |
| `0x2dxx2c` | jmpneqcc | if Accu != mem[PC + 1] { P1 = mem[PC + 3] } else { PC += 4}        |
| `0x2exx2f` | jmpgtcm  | if Accu > mem[PC + 1] { P1 = mem[MR] } else { PC += 3}             |
| `0x2fxx2e` | jmpltecm | if Accu <= mem[PC + 1] { P1 = mem[MR] } else { PC += 3}            |
| `0x30xx31` | jmpltcm  | if Accu < mem[PC + 1] { P1 = mem[MR] } else { PC += 3}             |
| `0x31xx30` | jmpgtecm | if Accu >= mem[PC + 1] { P1 = mem[MR] } else { PC += 3}            |
| `0x32xx33` | jmpeqcm  | if Accu == mem[PC + 1] { P1 = mem[MR] } else { PC += 3}            |
| `0x33xx32` | jmpneqcm | if Accu != mem[PC + 1] { P1 = mem[MR] } else { PC += 3}            |
| `0x3435`   | jmpgtmc  | if Accu > mem[MR] { P1 = mem[PC + 2] } else { PC += 3}             |
| `0x3534`   | jmpltemc | if Accu <= mem[MR] { P1 = mem[PC + 2] } else { PC += 3}            |
| `0x3637`   | jmpltmc  | if Accu < mem[MR] { P1 = mem[PC + 2] } else { PC += 3}             |
| `0x3736`   | jmpgtemc | if Accu >= mem[MR] { P1 = mem[PC + 2] } else { PC += 3}            |
| `0x3839`   | jmpeqmc  | if Accu == mem[MR] { P1 = mem[PC + 2] } else { PC += 3}            |
| `0x3938`   | jmpneqmc | if Accu != mem[MR] { P1 = mem[PC + 2] } else { PC += 3}            |
| `0x3axx3b` | ljmpgtcc | if Accu > mem[PC + 1] { P1 = 0; P2 = mem[PC + 3] } else { PC += 4} |
| `0x3bxx3a` | ljmpltecc| if Accu <= mem[PC + 1] { P1 = 0; P2 = mem[PC + 3] } else { PC += 4}|
| `0x3cxx3d` | ljmpltcc | if Accu < mem[PC + 1] { P1 = 0; P2 = mem[PC + 3] } else { PC += 4} |
| `0x3dxx3c` | ljmpgtecc| if Accu >= mem[PC + 1] { P1 = 0; P2 = mem[PC + 3] } else { PC += 4}|
| `0x3exx3f` | ljmpeqcc | if Accu == mem[PC + 1] { P1 = 0; P2 = mem[PC + 3] } else { PC += 4}|
| `0x3fxx3e` | ljmpneqcc| if Accu != mem[PC + 1] { P1 = 0; P2 = mem[PC + 3] } else { PC += 4}|
| `0x40xx41` | ljmpgtcm | if Accu > mem[PC + 1] { P1 = 0; P2 = mem[MR] } else { PC += 3}     |
| `0x41xx40` | ljmpltecm| if Accu <= mem[PC + 1] { P1 = 0; P2 = mem[MR] } else { PC += 3}    |
| `0x42xx43` | ljmpltcm | if Accu < mem[PC + 1] { P1 = 0; P2 = mem[MR] } else { PC += 3}     |
| `0x43xx42` | ljmpgtecm| if Accu >= mem[PC + 1] { P1 = 0; P2 = mem[MR] } else { PC += 3}    |
| `0x44xx45` | ljmpeqcm | if Accu == mem[PC + 1] { P1 = 0; P2 = mem[MR] } else { PC += 3}    |
| `0x45xx44` | ljmpneqcm| if Accu != mem[PC + 1] { P1 = 0; P2 = mem[MR] } else { PC += 3}    |
| `0x4647`   | ljmpgtmc | if Accu > mem[MR] { P1 = 0; P2 = mem[PC + 2] } else { PC += 3}     |
| `0x4746`   | ljmpltemc| if Accu <= mem[MR] { P1 = 0; P2 = mem[PC + 2] } else { PC += 3}    |
| `0x4849`   | ljmpltmc | if Accu < mem[MR] { P1 = 0; P2 = mem[PC + 2] } else { PC += 3}     |
| `0x4948`   | ljmpgtemc| if Accu >= mem[MR] { P1 = 0; P2 = mem[PC + 2] } else { PC += 3}    |
| `0x4a4b`   | ljmpeqmc | if Accu == mem[MR] { P1 = 0; P2 = mem[PC + 2] } else { PC += 3}    |
| `0x4b4a`   | ljmpneqmc| if Accu != mem[MR] { P1 = 0; P2 = mem[PC + 2] } else { PC += 3}    |
| `0xfe`     | swapm    | (Accu, mem[MR]) = (mem[MR], Accu); PC += 1;                        |
| `0xff`     | reset    | set registers, Accu and PC to 0                                    |
