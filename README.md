# `ometrics`

OCaml analysis in a merge request changes.

## Build
Build from source:

```bash
$ git clone git@gitlab.com:nomadic-labs/ometrics.git
$ cd ometrics
$ git submodule update --init
$ opam switch create . ocaml-base-compiler.4.12.0
$ make build
```

You can also install with opam:
```
$ opam install ometrics
```

## License
The code is now released under the MIT license.

## An introduction to the library

`ometrics` aims at finding metrics between a current state and a previous commit.
It is intended to be used in a merge request CI to easily detect bad behaviours and thus,
facilitate the review.

## Documentation analysis

`ometrics` will try to find undocumented changes.

```
$ ometrics check --html --output ometrics.html
```


