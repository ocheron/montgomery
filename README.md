# montgomery

Experiments about arithmetic in Montgomery form.

With GHC 9.0 function `powModSecInteger` is removed from `integer-gmp`. The code
here tries to implement similar functionality and with the same techniques but
in Haskell only.  A big number `Bn` is a sequence of machine words, but without
trying to remove non-significant limbs like `Integer` does.  Type `Block` in
`basement` is used as backend.

Compared to `expSafe` based on GMP,  Haskell implementation `bnPowMont` gives a
x4-x8 slowdown depending on bit size of the numbers:

```txt
Benchmark montgomery-bench: RUNNING...
montgomery/expSafe/curve25519            mean 12.25 μs  ( +- 83.92 ns  )
montgomery/expSafe/ffdhe3072             mean 9.903 ms  ( +- 61.70 μs  )
montgomery/bnPowMont/curve25519          mean 81.29 μs  ( +- 186.2 ns  )
montgomery/bnPowMont/ffdhe3072           mean 36.45 ms  ( +- 47.86 μs  )
Benchmark montgomery-bench: FINISH
```
