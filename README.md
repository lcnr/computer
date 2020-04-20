# A computer build from scratch

8 bit registers with 16 bit memory addresses. The available commands are listed in [COMMANDS.md](https://github.com/lcnr/computer/blob/master/COMMANDS.rs).

## Roadmap

- [x] build the computer in [logisim]
    - [x] micro instructions can be decoded using `cargo run -p mirco <opcode>`
- [x] build an assembler targeting this architecture ([rock])
- [x] build an emulator ([remu])
- [x] create a high level language which can be compiled to this architecture ([boulder])
- [ ] build it in real life

[logisim]: http://www.cburch.com/logisim
[rock]: https://github.com/lcnr/computer/blob/master/rock/README.md
[remu]: https://github.com/lcnr/computer/blob/master/remu/README.md
[boulder]: https://github.com/lcnr/computer/blob/master/boulder/README.md
