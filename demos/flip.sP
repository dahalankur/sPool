# flip.sP
# Demos sPool's higher order functions
# Written by Team Nautilus (ankur, etha, max, yuma)
#
# Compile: Use the compile.sh script to compile this source file.
# Example: When inside the src/ directory, run the following command to compile:
#          ./compile.sh ../demos/flip.sP a.out
#          This will create an executable file named a.out in the src/ directory,
#          which can be simply run to execute the program.

# flips the order of the arguments of an (int, int -> int) function
def (int, int -> int) flip((int, int -> int) f):
    def int flipped(int a, int b):
        return f(b, a);
    return flipped;

# a generic subtract function
(int, int -> int) subtract = lambda int (int x, int y): return x - y;
(int, int -> int) flippedSubtract = flip(subtract)

println(int_to_string(subtract(100, 1)))        # 99
println(int_to_string(flippedSubtract(100, 1))) # -99
