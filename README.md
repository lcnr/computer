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

1. [] build the computer in [logisim]
    1.1. [x] basic functionality
    1.2. [ ] conditional jumps
        1.2.1 [x] jmp on Accu == 0
        1.2.2 [x] jmp on Accu != 0
        1.2.3 [] jmp on Accu cmp Bus
1. [ ] build a assembler targeting this architecture ([rock])
1. [ ] build it in real life

[logisim]: http://www.cburch.com/logisim
[rock]: ./rock/README.md
