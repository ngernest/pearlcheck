# Pearlcheck

The code from *A simple incremental development of a property-based testing tool*
*(Functional Pearl)* by Braquehais et al. (Haskell '17), a tutorial demonstrating 
how Haskell's LeanCheck property-based testing library is implemented.

[Link to paper](http://jmct.cc/pearlcheck.pdf).

The code is located in [`src`](src) :
- [`Parts1To4.hs`](src/Parts1To4.hs) : Parts 1 - 4 of the paper
- [`Part5Onwards.hs`](src/Part5Onwards.hs): Parts 5 - 7 of the paper

To compile, run `stack build`. This repo has been tested with GHC 9.2.5. 

(Some function names have been lightly modified to avoid re-defining 
the same function twice.)