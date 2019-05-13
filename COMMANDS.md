# Micro Commands

Notes:

- The highest bit of `Bus Input` is `1 & 2 & 3 & 9`.

0. Increment P
1. ALU
2. ALU
3. ALU
---
4. Bus to M1
5. Bus to M2
6. Bus to P1
7. Bus to P2
---
8. Update Accu
9. Accu select Bus (default ALU)
10. Update RAM
11. RAM select M (default P)
---
12. Bus Input
13. Bus Input
14. Update IR
15. Update IR

|           | ALU           | Bus Input | Update IR     |
| --------- | ------------- | --------- | ------------- |
| `0b000`   | Accu + Bus    | RAM       | don't         |
| `0b001`   | Accu - Bus    | Accu      | unconditional |
| `0b010`   | Accu & Bus    | M1        | if Carry == 1 |
| `0b011`   | Accu \| Bus   | M2        | if Bus == 0   |
| `0b100`   | Accu ^ Bus    | P1        | -             |
| `0b101`   | !Accu         | P2        | -             |
| `0b110`   | Accu << Bus   | Zero      | -             |
| `0b111`   | Accu >> Bus   | Zero      | -             |

# Commands

*steps are stored in RAM with the order 1, 2, 4, 3 to prevent timing errors. (Huffman code 00 01 11 10)*

| Opcode   | Name     | Function                                                      |
| -------- | -------- | ------------------------------------------------------------- |
| `0x00`   | idle     | do nothing/simply increment PC by 1                           |
| `0x01`   | addc     | Accu = Accu + mem[PC + 1]; PC += 2;                           |
| `0x02`   | addm     |Accu = Accu + mem[MR]; PC += 1;                                |
| `0x03`   | subc     | Accu = Accu - mem[PC + 1]; PC += 2;                           |
| `0x04`   | subm     | Accu = Accu - mem[MR]; PC += 1;                               |
| `0x05`   | shlc     | Accu = Accu << mem[PC + 1]; PC += 2;                          |
| `0x06`   | shlm     | Accu = Accu << mem[MR]; PC += 1;                              |
| `0x07`   | shrc     | Accu = Accu >> mem[PC + 1]; PC += 2;                          |
| `0x08`   | shrm     | Accu = Accu >> mem[MR]; PC += 1;                              |
| `0x09`   | andc     | Accu = Accu & mem[PC + 1]; PC += 2;                           |
| `0x0a`   | andm     | Accu = Accu & mem[MR]; PC += 1;                               |
| `0x0b`   | orc      | Accu = Accu \| mem[PC + 1]; PC += 2;                          |
| `0x0c`   | orm      | Accu = Accu \| mem[MR]; PC += 1;                              |
| `0x0d`   | xorc     | Accu = Accu ^ mem[PC + 1]; PC += 2;                           |
| `0x0e`   | xorm     | Accu = Accu ^ mem[MR]; PC += 1;                               |
| `0x0f`   | inv      | Accu = !Accu; PC += 1;                                        |
| `0x10`   | loadc    | Accu = Mem[PC + 1]; PC += 2;1                                 |
| `0x11`   | loadm    | Accu = mem[MR]; PC += 1;                                      |
| `0x12`   | store    | mem[MR] = Accu; PC += 1;                                      |
| `0x13`   | zero     | Accu = 0; PC += 1;                                            |
| `0x14`   | setc M1  | M1 = mem[PC + 1]; PC += 2;                                    |
| `0x15`   | seta M1  | M1 = Accu; PC += 1;                                           |
| `0x16`   | setc M2  | M2 = mem[PC + 1]; PC += 2;                                    |
| `0x17`   | seta M2  | M2 = Accu; PC += 1;                                           |
| `0x18`   | get M1   | Accu = M1; PC += 2;                                           |
| `0x19`   | get M2   | Accu = M2; PC += 1;                                           |
| `0x1a`   | jmpc     | P1 = Mem[PC + 1];                                             |
| `0x1b`   | jmpm     | P1 = Mem[MR];                                                 |
| `0x1c`   | jmpa     | P1 = Accu;                                                    |
| `0x1d`   | ljmpc    | P1 = 0; P2 = Mem[PC + 1];                                     |
| `0x1e`   | ljmpm    | P1 = 0; P2 = Mem[MR];                                         |
| `0x1f`   | ljmpa    | P1 = 0; P2 = Accu;                                            |
| `0x2021` | jmpzc    | if Accu == 0 { P1 = Mem[PC + 2] } else { PC += 3 };           |
| `0x2120` | jmpnzc   | if Accu != 0 { P1 = Mem[PC + 2] } else { PC += 3 };           |
| `0x2223` | jmpzm    | if Accu == 0 { P1 = Mem[MR] } else { PC += 2 }; TODO          |
| `0x2322` | jmpnzm   | if Accu != 0 { P1 = Mem[MR] } else { PC += 2 }; TODO          |
| `0x2425` | jmpza    | if Accu == 0 { P1 = Accu } else { PC += 2 }; TODO             |
| `0x2524` | jmpnza   | if Accu != 0 { P1 = Accu } else { PC += 2 }; TODO             |
| `0x2627` | ljmpzc   | if Accu == 0 { P1 = 0; P2 = Mem[PC + 1] } else { PC += 3 }; TO|
| `0x2726` | ljmpnzc  | if Accu != 0 { P1 = 0; P2 = Mem[PC + 1] } else { PC += 3 }; TO|
| `0x2829` | ljmpzm   | if Accu == 0 { P1 = 0; P2 = Mem[MR] } else { PC += 2 }; TODO  |
| `0x2928` | ljmpnzm  | if Accu != 0 { P1 = 0; P2 = Mem[MR] } else { PC += 2 }; TODO  |
| `0x25`   | ljmpza   | if Accu == 0 { P1 = 0; P2 = Accu } else { PC += 2 }; TODO     |
| `0x2b`   | ljmpnza  | if Accu != 0 { P1 = 0; P2 = Accu } else { PC += 2 }; TODO     |
| `0xff`   | reset    | set registers, Accu and PC to 0                               |