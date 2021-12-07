all: build

build:
	@dune build

test:
	@dune runtest

clean:
	@dune clean
	rm -rf _coverage

coverage-summary:
	@dune runtest -f --instrument-with bisect_ppx test/ || true
	@bisect-ppx-report summary

coverage-html:
	@dune runtest -f --instrument-with bisect_ppx test/ || true
	@bisect-ppx-report html
