# testing downgrading a store function when passed as parameter

shared int y = 10

def store int addY(int x):
    return x + y;

println(int_to_string(addY(5))) # 15
y = 12
println(int_to_string(addY(5))) # still 15, thanks to cached result

def int hof((int -> int) f):
    return f(5); # downgraded to non-store for this call, therefore will return 17 now

println(int_to_string(hof(addY))) # 17

println(int_to_string(addY(5))) # 15, still cached