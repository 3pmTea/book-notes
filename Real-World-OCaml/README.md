# Real World OCaml

## [Read the book online](http://dev.realworldocaml.org/index.html)

## Installing OCaml(without root account)

1. Install opam(the OCaml package manager)

    Download the binary from [github](https://github.com/ocaml/opam/releases), rename it as "opam" and move into PATH.

2. Configure opam
    ```bash
    opam init --disable-sandboxing
    ```

3. Install OCaml packages
    ```bash
    opam install core utop
    opam install async yojson core_extended core_bench cohttp async_graphics cryptokit menhir
    ```

4. Utop configuration
    
    Put the code below into ~/.ocamlinit

    ```ocaml
    #use "topfind";;
    #thread;;
    #require "core.top";;
    #require "core.syntax";;
    ```

5. Play with OCaml!