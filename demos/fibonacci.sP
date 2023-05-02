# fibonacci.sP
# Implements the fibonacci sequence using both automatic memoization and
# without automatic memoization, and prints the results.
# Significant speedup is observed when automatic memoization is used.
# Written by Team Nautilus (ankur, etha, max, yuma)
#
# Compile: Use the compile.sh script to compile this source file.
# Example: When inside the src/ directory, run the following command to compile:
#          ./compile.sh ../demos/fibonacci.sP a.out
#          This will create an executable file named a.out in the src/ directory,
#          which can be simply run to execute the program.


# the 'store' keyword makes it such that the function is memoized automatically
def store int fibonacci(int n):
    int result = 0
    if (n <= 1):
        result = n
    else 
        result = fibonacci(n - 1) + fibonacci(n - 2);
    return result
;

# same function as above, but without the 'store' keyword
def int fib_nostore(int n):
    int result = 0
    if (n <= 1):
        result = n
    else 
        result = fib_nostore(n - 1) + fib_nostore(n - 2);
    return result
;

println("------------------------------------------------------------")
println("Fibonacci numbers from 0 to 46 using automatic memoization: ")
println("------------------------------------------------------------")
int i = 0
while (i <= 46):
    print("fib(")
    print(int_to_string(i))
    print(") = ")
    println(int_to_string(fibonacci(i)))
    i = i + 1
;

println("------------------------------------------------------------")
println("")

println("-------------------------------------------------------------")
println("Fibonacci numbers from 0 to 46 without automatic memoization: ")
println("-------------------------------------------------------------")
int j = 0
while (j <= 46):
    print("fib(")
    print(int_to_string(j))
    print(") = ")
    println(int_to_string(fib_nostore(j)))
    j = j + 1
;
println("-------------------------------------------------------------")
