.PHONY: all clean

all: toplevel.native

toplevel.native: toplevel.ml parser.mly scanner.mll ast.ml
	ocamlbuild $@

clean:
	ocamlbuild -clean