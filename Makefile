all: build

build:
	@dune build

test: build
	ln -sf _build/default/bin/main.exe ometrics
	@dune runtest

clean:
	@dune clean
	rm -rf _coverage
	rm -rf ometrics

coverage-summary:
	@dune runtest -f --instrument-with bisect_ppx test/ || true
	@bisect-ppx-report summary

coverage-html:
	@dune runtest -f --instrument-with bisect_ppx test/ || true
	@bisect-ppx-report html
