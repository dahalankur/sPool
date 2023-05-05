# a store function that returns INT_MIN will not cache!
def store int hi(int x):
    println(int_to_string(x))
    return -2147483647 - 1
;

hi(5)
hi(5)