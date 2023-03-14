# Compiles the sPool compiler and runs tests 

EXEC := toplevel.native

.PHONY: all clean testparser

all: $(EXEC)

testparser: $(EXEC) runtests.py
	@python3 runtests.py lexerparser all

# TODO: test semant has currently no tests; add tests as we write semantic checker for sPool
# testsemant: $(EXEC) runtests.py
# 	@python3 runtests.py semant all

$(EXEC): toplevel.ml parser.mly scanner.mll ast.ml semant.ml sast.ml codegen.ml
	@ocamlbuild $@ -package llvm -package llvm.analysis

clean:
	@ocamlbuild -clean