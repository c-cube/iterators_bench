# iterators-bench

Benchmark several implementations of iterators, for OCaml 4.03/4.06.0/4.06.1 + flambda/4.07.1 + flambda

- `gen`: the library on opam
- `gen_no_optim`: local
- `g`: local (reimplem of gen)
- `g_exn`: local

- `coroutine`: local
- `cps`: local
- `cps2`: local
- `fold`: local
- `list`: local
- `lazy_list`: local
- `ulist`: local
- `uncons`: local

- `std_seq`: module Seq (since OCaml 4.07)
- `iter`: the library on opam
- `batseq`: batteries on opam
- `oseq`: oseq on opam
- `core.sequence`: core on opam
- `base.sequence`: base on opam

```
make build
./bench.native
```
