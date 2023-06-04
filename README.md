# OCaml Lox

This is an interpreter for the Lox programming language specified in the book [*Crafting Interpreters*](https://craftinginterpreters.com/) by Robert Nystrom. It's implemented using OCaml.

## How to Build and Run

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

## Implementation Overview

The meat of this interpreter is in `./lib/interpreter.ml`.

Starting with the source code (type `string`), we

1. scan (function `scan()`) the source code into tokens (type `token_t list`)
2. parse (function `parse()`) the tokens into statements (type `statement_t list`)
3. interpret (function `execute_statements()`) the statements

This implementation follows most of the implementation in [*Crafting Interpreters*](https://craftinginterpreters.com/), except

+ It doesn't use a resolver. Instead, it creates a new environment when a new variable is declared.
+ Classes are not yet implemented.