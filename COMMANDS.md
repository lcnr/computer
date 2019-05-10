# Micro Commands

0. Bus to RAM
1. RAM to Bus
2. RAM select P
3. Increment P
4. Bus to M1
5. Bus to M2
6. Bus to P1
7. Bus to P2
8. Accu to Bus
9. Bus to Accu
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

- `0x00`, idle: do nothing/simply increment PC by 1,
    1. `0000 0110 1111 0000`
    2. `0000 0110 1111 0000`
    3. `0000 0110 1111 0000`
    4. `0000 0110 1111 0000`
- `0xff`, reset: set registers, RAM, PC to 0
    1. `0000 0000 0000 0000`
    2. `0000 0000 0000 0000`
    3. `0000 0000 0000 0000`
    4. `0000 0000 0000 0000`