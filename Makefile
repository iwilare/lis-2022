CC=dune

default: fmt
	$(CC) build

dep:
	$(CC) install

fmt:
	ocamlformat --inplace **/*.ml

check:
	$(CC) runtest
