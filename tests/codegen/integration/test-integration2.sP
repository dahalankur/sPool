# test-integration2.sP
# Implements the fibonacci sequence using both automatic memoization and
# without automatic memoization, and prints the results.
# Significant speedup is observed when automatic memoization is used.
# Written by Team Nautilus (ankur, etha, max, yuma)



# the 'store' keyword makes it such that the function is memoized automatically
def store int fibonacci(int n):
    int result = 0
    if (n <= 1):
        result = n
    else 
        result = fibonacci(n - 1) + fibonacci(n - 2);
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

def (int, int -> int) flip((int, int -> int) f):
    def int flipped(int a, int b):
        return f(b, a);
    return flipped;

# a generic subtract function
(int, int -> int) subtract = lambda int (int x, int y): return x - y;
(int, int -> int) flippedSubtract = flip(subtract)

println(int_to_string(subtract(100, 1)))        # 99
println(int_to_string(flippedSubtract(100, 1))) # -99