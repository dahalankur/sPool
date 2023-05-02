# idempotence.sP
# Demos the "store" functionality of sPool.
# Written by Team Nautilus (ankur, etha, max, yuma)
#
# Compile: Use the compile.sh script to compile this source file.
# Example: When inside the src/ directory, run the following command to compile:
#          ./compile.sh ../demos/idempotence.sP a.out
#          This will create an executable file named a.out in the src/ directory,
#          which can be simply run to execute the program.

# shared variables are declared on the heap; they can be mutated by threads and 
# functions, and are accessible by all threads and functions
shared int global = 0

# getGlobal is an (int -> int) function with "store" enabled -- it caches 
# the result of the function call and returns the cached result if the same
# argument is passed in again
def store int getGlobal(int x):
    return global
;

println("------------------------------")
println("--------WITH STORE------------")
println("------------------------------")

print("getGlobal(1) = ")
println(int_to_string(getGlobal(1))) # returns 0

println("changing global to 1")
global = 1

print("getGlobal(1) = ")
println(int_to_string(getGlobal(1))) # still returns 0, because the result was cached in the first call

print("getGlobal(2) = ")
println(int_to_string(getGlobal(2))) # returns 1, because for argument x = 2, the result was not cached
             # so the function is actually called instead of performing a cache lookup



# to see how a non-store function would behave, observe the following:

println("")
println("------------------------------")
println("---------W/O STORE------------")
println("------------------------------")

def int getGlobal2(int x):
    return global;

print("getGlobal2(1) = ")
println(int_to_string(getGlobal2(1)))

println("changing global to 2")
global = 2

print("getGlobal2(1) = ")
println(int_to_string(getGlobal2(1))) # returns 2, because the result was not cached in the first call