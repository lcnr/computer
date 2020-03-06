# lir2mir

Compiling lir into human readable assembly language.

## Notes

Some notes and implementation details of the current implementation.

### Stack

Function return values and temporaries of recursive function calls
are stored on a stack. The stack has its own block with the section
address of the last used element stored in the first byte of the block.

### Requirements

Each block input must bind to a unique location.

### Functions

Exported functions are aligned to a block boundary, other functions may not be.

Functions store their return values at the start of their memory storage.