# `ometrics`

```bash
git submodule update --init
opam switch create . ocaml-base-compiler.4.12.0
```

Currently, try to find the most accurate list of OCaml entries which
have not been properly documented.

```bash
dune build
cp _build/default/main.exe PATH_TO_GIT_REPO
cd PATH_TO_GIT_REPO
./main.exe
