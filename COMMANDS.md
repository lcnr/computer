# Micro Commands

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

## ALU

- `0b000`: `Accu + Bus`
- `0b001`: `Accu - Bus`
- `0b010`: `Accu & Bus`
- `0b011`: `Accu | Bus`
- `0b100`: `Accu ^ Bus`
- `0b101`: `!Accu`
- `0b110`: `Accu << Bus`
- `0b111`: `Accu >> Bus`

## Bus Input

- `0b00`: RAM
- `0b01`: Accu
- `0b10`: M1
- `0b11`: M2

## Update IR

- `0b00`: don't update
- `0b01`: unconditional update
- `0b10`: update on Carry
- `0b11`: update on Zero

# Commands

*steps are stored in RAM with the order 1, 2, 4, 3 to prevent timing errors. (Huffman code 00 01 11 10)*

- `0x00`, idle: do nothing/simply increment PC by 1,
- `0x01`, addc: Accu = Accu + mem[PC + 1]; PC += 2;
- `0x02`, addm: Accu = Accu + mem[MR]; PC += 1;
- `0x03`, subc: Accu = Accu - mem[PC + 1]; PC += 2;
- `0x04`, subm: Accu = Accu - mem[MR]; PC += 1;
- `0x05`, shlc: Accu = Accu << mem[PC + 1]; PC += 2;
- `0x06`, shlm: Accu = Accu << mem[MR]; PC += 1;
- `0x07`, shrc: Accu = Accu >> mem[PC + 1]; PC += 2;
- `0x08`, shrm: Accu = Accu >> mem[MR]; PC += 1;
- `0x09`, andc: Accu = Accu & mem[PC + 1]; PC += 2;
- `0x0a`, andm: Accu = Accu & mem[MR]; PC += 1;
- `0x0b`, orc: Accu = Accu | mem[PC + 1]; PC += 2;
- `0x0c`, orm: Accu = Accu | mem[MR]; PC += 1;
- `0x0d`, xorc: Accu = Accu ^ mem[PC + 1]; PC += 2;
- `0x0e`, xorm: Accu = Accu ^ mem[MR]; PC += 1;
- `0x0f`, inv: Accu = !Accu; PC += 1;

- `0x10`, loadc: Accu = Mem[PC + 1]; PC += 2;
- `0x11`, loadm: Accu = mem[MR]; PC += 1;
- `0x12`, store: mem[MR] = Accu; PC += 1;
- `0x13`, zero: Accu = 0; PC += 1;
- `0x14`, setc M1: M1 = mem[PC + 1]; PC += 2; TODO
- `0x15`, seta M1: M1 = Accu; PC += 1; TODO
- `0x16`, setc M2: M2 = mem[PC + 1]; PC += 2; TODO
- `0x17`, seta M2: M2 = Accu; PC += 1; TODO

- `0x18`, jmpc: P1 = Mem[PC + 1]; TODO
- `0x19`, jmpm: P1 = Mem[MR]; TODO
- `0x1a`, jmpa: P1 = Accu; TODO
- `0x1b`, ljmpc: P1 = 0; P2 = Mem[PC + 1]; TODO
- `0x1c`, ljmpm: P1 = 0; P2 = Mem[MR]; TODO
- `0x1d`, ljmpa: P1 = 0; P2 = Accu; TODO
- `0x1e`, jmpzc: if Accu == 0 { P1 = Mem[PC + 1] } else { PC += 2 }; TODO
- `0x1f`, jmpzm: if Accu == 0 { P1 = Mem[MR] } else { PC += 1 }; TODO
- `0x20`, jmpza: if Accu == 0 { P1 = Accu } else { PC += 1 }; TODO
- `0x21`, ljmpzc: if Accu == 0 { P1 = 0; P2 = Mem[PC + 1] } else { PC += 2 }; TODO
- `0x22`, ljmpzm: if Accu == 0 { P1 = 0; P2 = Mem[MR] } else { PC += 1 }; TODO
- `0x23`, ljmpza: if Accu == 0 { P1 = 0; P2 = Accu } else { PC += 1 }; TODO
- `0x24`, jmpnzc: if Accu != 0 { P1 = Mem[PC + 1] } else { PC += 2 }; TODO
- `0x25`, jmpnzm: if Accu != 0 { P1 = Mem[MR] } else { PC += 1 }; TODO
- `0x26`, jmpnza: if Accu != 0 { P1 = Accu } else { PC += 1 }; TODO
- `0x27`, ljmpnzc: if Accu != 0 { P1 = 0; P2 = Mem[PC + 1] } else { PC += 2 }; TODO
- `0x28`, ljmpnzm: if Accu != 0 { P1 = 0; P2 = Mem[MR] } else { PC += 1 }; TODO
- `0x29`, ljmpnza: if Accu != 0 { P1 = 0; P2 = Accu } else { PC += 1 }; TODO

- `0xff`, reset: set registers, RAM, PC to 0