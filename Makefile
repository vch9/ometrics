all: build

build:
	dune build

test: build
	dune build @runtest

clean:
	@dune clean
	rm -rf _coverage
	rm -rf ometrics

PWD=$(shell pwd)

instrumented-test:
	mkdir -p _coverage
	BISECT_FILE="${PWD}/_coverage/" dune build --instrument-with bisect_ppx
	BISECT_FILE="${PWD}/_coverage/" dune runtest -f --instrument-with bisect_ppx

coverage-summary: instrumented-test
	@bisect-ppx-report summary --per-file --coverage-path _coverage/

coverage-html: instrumented-test
	@bisect-ppx-report html --coverage-path _coverage/
