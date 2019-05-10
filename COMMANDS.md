# Micro Commands

0. Increment P
1. Bus to RAM
2. Bus select RAM (default Accu)
3. RAM select P (default M)
4. Bus to M1
5. Bus to M2
6. Bus to P1
7. Bus to P2
8. Update Accu
9. Accu select Bus (default ALU)
10. Zero Bus
11. _EMPTY_
12. _EMPTY_
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

# Commands

*steps are stored in RAM with the order 1, 2, 4, 3 to prevent timing errors. (Huffman code 00 01 11 10)

- `0x00`, idle: do nothing/simply increment PC by 1,
    1. `0000 0000 0000 0000`
    2. `0000 0000 0000 0000`
    3. `0000 0000 0000 0000`
    4. `0000 0000 0000 0001`
- `0xff`, reset: set registers, RAM, PC to 0
    1. `0000 0111 1111 0000`
    2. `0000 0111 1111 0000`
    3. `0000 0111 1111 0000`
    4. `0000 0111 1111 0000`