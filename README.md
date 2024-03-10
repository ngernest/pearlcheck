# PearlCheck

This repo contains the code from [*A simple incremental development of a property-based testing tool*](http://jmct.cc/pearlcheck.pdf) 
by Braquehais et al. (Haskell '17), a functional pearl demonstrating how Haskell's 
LeanCheck property-based testing library is implemented.

The code is located in [`src`](src) :
- [`Parts1To4.hs`](src/Parts1To4.hs) : Parts 1 - 4 of the paper
- [`Part5Onwards.hs`](src/Part5Onwards.hs): Parts 5 - 7 of the paper

To compile, run `stack build`. This repo has been tested with GHC 9.2.5. 

(Some function names have been lightly modified to avoid re-defining 
the same function twice.)

## A note about generating random functions

To generate & show functions, the code from Koen Claessen's [*Shrinking and Showing Functions*](https://dl.acm.org/doi/10.1145/2430532.2364516)
functional pearl (Haskell '12) is currently being reproduced in [`PartialFunctions.hs`](src/PartialFunctions.hs). (Work in progress)

Other resources on generating functions for QuickCheck:
- Ulf Norell's talk on [generating random functions](https://vimeo.com/143848099) using QuviQ QuickCheck in Erlang
- [GitHub discussion](https://github.com/c-cube/qcheck/issues/8) on generating functions in Qcheck in OCaml
  - See also [Jan Midtgaard's examples](https://github.com/jmid/qcheck-fun) for exercising Qcheck's function generator
- [Li-Yao Xia's repo](https://github.com/Lysxia/test-fun) of function generators (extends the Claessen paper)
- Jane Street's [blog post](https://blog.janestreet.com/quickcheck-for-core/) on Core.Quickcheck in OCaml