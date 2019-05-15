## Installation

1) install [OPAM](https://opam.ocaml.org/doc/Install.html)
2) Configure OPAM with `opam init`
3) eval `opam config env`
4) `opam install core utop`
5) `dune runtest` and you should see no errors!

This template uses [`ppx_inline_test`](https://github.com/janestreet/ppx_inline_test) from Jane Street to run inline tests, rather than oUnit. No reason other than I found it easier to configure with Dune, and I like the idea of colocation code and testing logic.