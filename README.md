Tests for Nock execution in HVM.

Currently Nock is executed via a naive interpreter. Only direct 24 bit atoms are supported. Nock 11 hints are typically larger than that, so make sure to remove them from your code.

To run benchmarks:
  - Install [Bend](https://github.com/HigherOrderCO/Bend/tree/main) (Rust, GCC required)
  - Compile programs to C, then run the binaries. For example:
  
  ```
  bend gen-c nock-turn-10.bend >> nock-turn-10.c
  gcc nock-turn-10.c -o turn1.bin -O2 -lm -lpthread
  ./turn1.bin
  ```

Simple things to try:
  - [ ] Register `pico.hoon`
  - [ ] Try running `++turn-alt`
  - [ ] Extend atoms to arbitrary size `(List u28)`

Harder milestones:
  - [ ] Compile formulas (and cores?) to HVM lambdas