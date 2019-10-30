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
