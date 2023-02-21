.PHONY: all clean test

all: toplevel.native

test: toplevel.native testlexerparser.py
	@python3 testlexerparser.py all

toplevel.native: toplevel.ml parser.mly scanner.mll ast.ml
	ocamlbuild $@

clean:
	ocamlbuild -clean