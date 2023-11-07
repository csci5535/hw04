# hw04/ocaml

This repository contains some minimal OCaml starter files for Homework Assignment 4.

```
.
├── hw04.ml       a template for your submission (if you wish)
└── test_hw04.ml  a template for your tests (if you wish)
```

## Dependencies

First, make sure you have `ocaml` and `opam` installed from your system's package manager. Then, you can install the build and library dependencies as follows:

```
opam install dune base ounit2
```

## Build and Run Tests

You can build and run tests as follows:

```
dune runtest
```

Or, the `Makefile``

```
make
```

simply calls the above.