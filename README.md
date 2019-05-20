# A computer build from scratch

## Logisim

### Linux

Install logisim using `sudo apt install logisim`.

Download the repository using `git clone https://github.com/lcnr/computer.git`.

### Editing this project

1. Get the newest version using `git pull` in the project directory.
2. Run logisim using `logisim logisim.circ`.
3. Commit your changes with `git add . ; git commit -m "<msg>"`
where message is a quick explanation of your changes.
4. Push them to the repository: `git push`

To fix a small mistake in the previous commit, use `git reset --soft HEAD~1`
to undo the last commit and resume with step 3.

## Roadmap

- [ ] build the computer in [logisim]
  - [x] basic functionality
  - [ ] conditional jumps
    - [x] jmp on Accu == 0
    - [x] jmp on Accu != 0
    - [ ] jmp on Accu cmp Bus
- [ ] build an assembler targeting this architecture ([rock])
- [ ] create a high level language which can be compiled to this architecture ([boulder])
- [ ] build it in real life

[logisim]: http://www.cburch.com/logisim
[rock]: ./rock/README.md
[boulder]: ./boulder/README.md
