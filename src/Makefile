# Compiles the sPool compiler and runs tests 

EXEC := toplevel.native
CXX  := gcc

.PHONY: all clean testparser

all: $(EXEC) builtins.o

testparser: $(EXEC) runtests.py
	@python3 runtests.py lexerparser all

builtins.o: builtins.c
	@$(CXX) -c $^

$(EXEC): toplevel.ml parser.mly scanner.mll ast.ml semant.ml sast.ml codegen.ml
	@ocamlbuild $@ -package llvm -package llvm.analysis

clean:
	@ocamlbuild -clean
	@rm -f *.o *.s *.ll