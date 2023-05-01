def (int -> int)Fun():
    return lambda int (int i): return i;
;

(int -> int) x = Fun()

# testing capturing reassigned function inside another function
def int hi():
    return x(12)
;

int i = 50
while (i < 76):
    println(int_to_string(i))
    i = i + 1;