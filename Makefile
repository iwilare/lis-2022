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
	$(CC) build @fmt --auto-promote