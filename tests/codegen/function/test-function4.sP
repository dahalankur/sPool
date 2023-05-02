# testing exhausting a function's store while using shared variables to break idempotence
shared int x = 100
def store int X(int y):
    return y + x;

# cache size for each function is 32; so 32 results are cached before the earlier ones are overwritten
println(int_to_string(X(10))) # 110
println(int_to_string(X(20))) # 120
x = 200
println(int_to_string(X(10))) # still 110 because it is cached

# fill up the cache
int i = 50
while (i <= 79):
    X(i)
    i = i + 1
;
println(int_to_string(X(10))) # still 110, next non-cached result should overwrite the first cached result

X(99) # this will overwrite the spot for 10, meaning the result for 10 is no longer cached

println(int_to_string(X(10))) # 210!