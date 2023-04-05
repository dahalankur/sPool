# Compiles the sPool compiler and runs tests 

EXEC := toplevel.native
CXX  := gcc

.PHONY: all clean testparser

all: $(EXEC) builtins.o list.o

testparser: $(EXEC) runtests.py
	@python3 runtests.py lexerparser all

testcodegen: $(EXEC) runtests.py
	@python3 runtests.py codegen all

%.o: %.c
	@$(CXX) -c $^

$(EXEC): toplevel.ml parser.mly scanner.mll ast.ml semant.ml sast.ml codegen.ml
	@ocamlbuild $@ -package llvm -package llvm.analysis

# microc.native : parser.mly scanner.mll codegen.ml semant.ml microc.ml
# 	opam exec -- \
# 	ocamlbuild -use-ocamlfind microc.native

# TODO: change it to the one microc uses
clean:
	@ocamlbuild -clean
	@rm -f *.o *.s *.ll