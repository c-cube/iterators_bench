# iterators-bench

Benchmark several implementations of iterators, for OCaml 4.03/4.06.0/4.06.1 + flambda

- `gen`: the library on opam
- `sequence`: the library on opam
- `g`: local (reimplem of gen)
- `core.sequence`: core
- `base.sequence`: base
- `cps`: local
- `cps2`: local
- `fold`: local
- `list`: local
- `lazy_list`: local
- `ulist`: local

```
make build
./bench.native
```
