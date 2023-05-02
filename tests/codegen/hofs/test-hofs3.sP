def quack List_int_print(list<int> l):
    print("[ ")
    int i = 0
    while (i < List_len(l)):
        print(int_to_string(List_at(l, i)))
        print(" ")
        i = i + 1;
    println("]")
    return
;

def list<int> List(int len, int value):
    int i = 0
    list<int> result = []
    while (i < len):
        List_insert(result, i, value)
        i = i + 1;
    return result;

def list<int> List_int_map(list<int> l, (int -> int) f):
    int i = 0
    int len = List_len(l)
    list<int> result = List(len, -1)

    while (i < len):
        List_replace(result, i, f(List_at(l, i)))
        i = i + 1;

    return result;

def int square (int x):
    return x * x
;

# testing passing functions as parameters
List_int_print(List_int_map([2,3,4,5,6], square))
List_int_print(List_int_map([2,3,4,5,6], lambda int (int x): return -x;))