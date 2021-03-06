# Notes

## Union layout

I initially wanted union layout to be undefined, to allow for potential optimizations.
This was highly problematic as one often extends unions by using implicit sum types.

```boulder
let condition: Bool = True;
let x: u16 | u32 = 7: u32;
let y: u16 | u32 | Empty = if condition {
    y = x;
} else {
    y = Empty;
}
```

As it is inefficient to check the type of `x` before initializing `y`,  `x` and `y` must have a similar layout.
The easiest way this can be archived is by defining the layout of all unions. This still allows for most optimizations and
simplifies the compiler.

## Lir and liveliness

Calculating the liveliness of a memory location goes as follows:

- start at the terminator of each block, all used locations are alive.
- going backwards through each step:c
    - writing to a location causes it to be alive and then instantly kills it.
    - reading a location makes it alive.

A graph where each node represents a location and each edges mean that these two locations
are alive at the same time. This graph is build by adding an edge to every other living location, 
everytime a location is made alive.
