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
montgomery/expSafe/curve25519            mean 12.24 μs  ( +- 37.43 ns  )
montgomery/expSafe/ffdhe3072             mean 9.914 ms  ( +- 72.31 μs  )
montgomery/bnPowMont/curve25519          mean 93.91 μs  ( +- 216.8 ns  )
montgomery/bnPowMont/ffdhe3072           mean 41.74 ms  ( +- 190.8 μs  )
Benchmark montgomery-bench: FINISH
```
