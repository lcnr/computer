# lir2mir

Compiling lir into human readable assembly language.

## Notes

Some notes and implementation details of the current implementation.

### Functions

Exported functions are aligned to a block boundary, other functions may not be.

Functions store their return values at the start of their memory storage.