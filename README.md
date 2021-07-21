# `ometrics`

## `mr.py`

Find the list of OCaml entries which have not been properly
documented.

```bash
dune build
cp _build/default/main.exe mr.py PATH_TO_GIT_REPO
cd PATH_TO_GIT_REPO
./mr.py OLD_COMMIT
```
