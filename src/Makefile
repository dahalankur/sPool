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
	@$(CXX) -c -pthread $^

$(EXEC): toplevel.ml parser.mly scanner.mll ast.ml semant.ml sast.ml codegen.ml
	@ocamlbuild $@ -package llvm -package llvm.analysis -cflags -w,+a-4-70-42

clean:
	@ocamlbuild -clean
	@rm -f *.o *.s *.ll