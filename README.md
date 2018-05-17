# 6502 Language


This is very early work to write a compiler for the 6502 processor. It will compile to CA65 Assembly code.

## Language plans

1. C-like syntax, but almost everything is an expression. This is similar to Rust.

        let c := if (cond) { a } else { b }

2. A full set of pointer and list-like types for memory control and banking support:

    - 1 byte pointers with a known high byte
    - 1 byte pointers which are indices in a known array
    - 2 byte pointers which are assumed to be in the currently loaded bank
    - 2 byte pointers with a known bank byte
    - 3 byte far pointers
    - Arrays with up to 256 bytes
    - Arrays arranged as a struct-of-arrays, with up to 256 elements
    - Long arrays with 2 byte indices

3. Well-optimized code with tuning options (size, speed) on a per-function basis.