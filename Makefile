
all:
	dune build

test:
	time dune runtest

doc:
	dune build @doc

clean:
	dune clean
