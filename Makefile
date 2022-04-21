CC=dune

default:
	$(CC) build

dep:
	$(CC) install

check:
	$(CC) runtest
