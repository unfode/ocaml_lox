# OCaml Lox

This is an interpreter for the Lox programming language specified in the book [*Crafting Interpreters*](https://craftinginterpreters.com/) by Robert Nystrom. It's implemented using OCaml.

## How to build and run

This project uses the Dune build system. To build the interpreter, run

```shell
dune build --profile release
```

Then you are expected to find the executable file at `./_build/default/bin/main.exe`.

To scan, parse, and interpret a lox script (e.g., `./lox-scripts/factorial.lox`), run

```shell
./_build/default/bin/main.exe ./lox-scripts/factorial.lox
```

You can find some simple Lox scripts in `./lox-scripts`.