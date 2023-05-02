# testing idempotence while using store
shared int y = 100

def store int getY(int a):
    return y
;

println(int_to_string(getY(5)))
y = 200
println(int_to_string(getY(5))) # aha! this will still return 100, which was cached in store


def quack hi():
    println(int_to_string(getY(6))) # only this will return 200 because param 6 is not cached
    println(int_to_string(getY(5))) # still 100!
    return
;

hi()