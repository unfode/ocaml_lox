# OCaml Lox

This is an interpreter for the Lox programming language specified in the book [*Crafting Interpreters*](https://craftinginterpreters.com/) by Robert Nystrom. It's implemented using OCaml.

## How to build and run

This project uses the Dune build system. To build the interpreter, run

```shell
dune build --profile release
```

Then you are expected to find the executable file at `./_build/default/bin/main.exe`.

To scan, parse, and interpret a lox script, run

```shell
./_build/default/bin/main.exe <script path>
```

You can find some Lox scripts in `./lox-scripts`.