# Too Many CPS

Collect and implement CPS transformation algorithms in OCaml, Rust, and MoonBit.

### *Representing control: A study of the CPS transformation* by Olivier Danvy and Andrzej Filinski.

|                        | OCaml | Rust | MoonBit |
|------------------------|:-----:|:----:|:-------:|
| Naive                  |   [code](ocaml/lib/fischer_plotkin_cps.ml)   |   [code](rust/naive.rs)  |    [code](moonbit/naive/cps.mbt)    |
| Higher Order           |   [code](ocaml/lib/nielson_one_pass_cps.ml)   |   [code](rust/higher_order.rs)  |    [code](moonbit/higher_order/cps.mbt)    |
| Nine Steps             |   [code](ocaml/lib/danvy_nine_steps.ml)   |   ×  |    ×    |
| Properly Tail Recursive|   [code](ocaml/lib/danvy_tail_cps.ml)   |   ×  |    ×    |
| TODO...                |   ×   |   ×  |    ×    |

### [How to compile with continuations](https://matt.might.net/articles/cps-conversion/) by Matt Might.

|                        | OCaml | Rust | MoonBit |
|------------------------|:-----:|:----:|:-------:|
| Naive                  |   [code](ocaml/lib/matt_naive_cps.ml)   |   ×  |    ×    |
| Higher Order           |   [code](ocaml/lib/matt_higher_order_cps.ml)   |   ×  |    ×    |
| Hybrid                 |   [code](ocaml/lib/matt_hybrid_cps.ml)   |   ×  |    ×    |
| Rich                   |   [code](ocaml/lib/matt_rich_cps.ml)   |   ×  |    ×    |

### *Compiling with continuations, continued* by Andrew Kennedy.

|                        | OCaml | Rust | MoonBit |
|------------------------|:-----:|:----:|:-------:|
| Untyped Naive          |   [code](ocaml/lib/andrew_kennedy_untyped_naive_cps.ml)   |   ×  |    ×    |
| Untyped Tail           |   [code](ocaml/lib/andrew_kennedy_untyped_tail_cps.ml)   |   ×  |    ×    |
| TODO...                |   ×   |   ×  |    ×    |

### *The essence of compiling with continuations* by Cormac Flanagan, Amr Sabry, Bruce F. Duba, Matthias Felleisen.

also known as ANF (A-Normal Form)

|           | OCaml | Rust | MoonBit |
|-----------|:-----:|:----:|:-------:|
| Figure 9: A linear-time A-normalization algorithm|   [code](ocaml/lib/flanagan_anf.ml)   |   ×  |    [code](moonbit/anf_flanagan/anf.mbt)    |
| ↑↑↑ Syntax Partitioned Version |   [code](ocaml/lib/syntax_partitioned_anf.ml)   |   ×  |    [code](moonbit/anf_syntax_partitioned/anf.mbt)    |

