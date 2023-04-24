# Compiles the sPool compiler and runs tests 

EXEC := toplevel.native
CXX  := gcc

.PHONY: all clean testparser

all: $(EXEC) builtins.o list.o

testparser: $(EXEC) runtests.py
	@python3 runtests.py lexerparser all _

testcodegen: $(EXEC) runtests.py
	@python3 runtests.py codegen all _

# github CI/CD does not like %.o: %.c
builtins.o: builtins.c
	@$(CXX) -O2 -c -pthread $^

list.o: list.c
	@$(CXX) -O2 -c $^

$(EXEC): toplevel.ml parser.mly scanner.mll ast.ml semant.ml sast.ml codegen.ml
	@ocamlbuild $@ -package llvm -package llvm.analysis -cflags -w,+a-4-70-42

clean:
	@ocamlbuild -clean
	@rm -f *.o *.s *.ll