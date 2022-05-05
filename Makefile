CC=dune

default: fmt
	$(CC) build

dep:
	$(CC) build @install

fmt:
	ocamlformat --inplace **/*.ml

check:
	$(CC) runtest

checkfmt:
# linux moment

	$(CC) build @fmt --auto-promote || $(CC) build @fmt --auto-promote 


.PHONY: checkfmt