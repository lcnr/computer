# Micro Commands

0. Increment P
1. Bus to RAM
2. Bus select Accu (default RAM)
3. RAM select M (default P)
4. Bus to M1
5. Bus to M2
6. Bus to P1
7. Bus to P2
8. Update Accu
9. Accu select Bus (default ALU)
10. Zero Bus
11. Update IR
12. Update IR
13. ALU
14. ALU
15. ALU

## ALU

- `0b000`: `Accu + Bus`
- `0b001`: `Accu - Bus`
- `0b010`: `Accu & Bus`
- `0b011`: `Accu | Bus`
- `0b100`: `Accu ^ Bus`
- `0b101`: `!Accu`
- `0b110`: `Accu << Bus`
- `0b111`: `Accu >> Bus`

## Update IR

- `0b00`: don't update
- `0b01`: unconditional update
- `0b10`: update on Carry
- `0b11`: update on Zero

# Commands

*steps are stored in RAM with the order 1, 2, 4, 3 to prevent timing errors. (Huffman code 00 01 11 10)*

- `0x00`, idle: do nothing/simply increment PC by 1,

- `0x01`, addc: Accu = Accu + mem[PC + 1]; PC += 2;
- `0x02`, addm: Accu = Accu + mem[MR]; PC += 1; TODO
- `0x03`, subc: Accu = Accu - mem[PC + 1]; PC += 2; TODO
- `0x04`, subm: Accu = Accu - mem[MR]; PC += 1; TODO
- `0x05`, shlc: Accu = Accu << mem[PC + 1]; PC += 2; TODO
- `0x06`, shlm: Accu = Accu << mem[MR]; PC += 1; TODO
- `0x07`, shrc: Accu = Accu >> mem[PC + 1]; PC += 2; TODO
- `0x08`, shrm: Accu = Accu >> mem[MR]; PC += 1; TODO
- `0x09`, andc: Accu = Accu & mem[PC + 1]; PC += 2; TODO
- `0x0a`, andm: Accu = Accu & mem[MR]; PC += 1; TODO
- `0x0b`, orc: Accu = Accu | mem[PC + 1]; PC += 2; TODO
- `0x0c`, orm: Accu = Accu | mem[MR]; PC += 1; TODO
- `0x0d`, xorc: Accu = Accu ^ mem[PC + 1]; PC += 2; TODO
- `0x0e`, xorm: Accu = Accu ^ mem[MR]; PC += 1; TODO
- `0x0f`, inv: Accu = !Accu; PC += 1; TODO

- `0x10`, loadc: Accu = Mem[PC + 1]; PC += 2; TODO
- `0x11`, loadm: Accu = mem[MR]; PC += 1; TODO
- `0x12`, store: mem[MR] = Accu; PC += 1; TODO
- `0x13`, setc M1: M1 = mem[PC + 1]; PC += 2; TODO
- `0x14`, seta M1: M1 = Accu; PC += 1; TODO
- `0x15`, setc M2: M2 = mem[PC + 1]; PC += 2; TODO
- `0x16`, seta M2: M2 = Accu; PC += 1; TODO
- `0x17`, zero: Accu = 0; PC += 1; TODO

- `0x18`, jmpc: P1 = Mem[PC + 1]; PC += 2; TODO
- `0x19`, jmpa: P1 = Accu; PC += 1; TODO
- `0x1a`, ljmpc: P1 = 0; P2 = Mem[PC + 1]; PC += 2; TODO
- `0x1b`, ljmpa: P1 = 0; P2 = Accu; PC += 1; TODO
- `0x1c`, jmpzc: if Accu == 0 { P1 = Mem[PC + 1] }; PC += 2; TODO
- `0x1d`, jmpza: if Accu == 0 { P1 = Accu }; PC += 1; TODO
- `0x1e`, ljmpzc: if Accu == 0 { P1 = 0; P2 = Mem[PC + 1] }; PC += 2; TODO
- `0x1f`, ljmpza: if Accu == 0 { P1 = 0; P2 = Accu }; PC += 1; TODO
- `0x20`, jmpnzc: if Accu != 0 { P1 = Mem[PC + 1] }; PC += 2; TODO
- `0x21`, jmpnza: if Accu != 0 { P1 = Accu }; PC += 1; TODO
- `0x22`, ljmpnzc: if Accu != 0 { P1 = 0; P2 = Mem[PC + 1] }; PC += 2; TODO
- `0x23`, ljmpnza: if Accu != 0 { P1 = 0; P2 = Accu }; PC += 1; TODO

- `0xff`, reset: set registers, RAM, PC to 0