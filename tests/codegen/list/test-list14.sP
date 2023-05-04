# Testing lists as formal parameters and modifying them in functions and threads

def quack printList(list<int> l):
    int len = List_len(l)
    int i = 0
    print("[ ")
    while (i < len): 
        print(int_to_string(List_at(l, i)))
        print(" ")
        i = i + 1;
    println("]")
    return
;

shared int global = 0
int local = 5

def quack modifyList(list<int> l):
    List_insert(l, 0, -1)
    List_insert(l, List_len(l), -2)

    printList(l)

    global = List_at(l, List_len(l) - 1) + List_at(l, 1) + global
    local = global
    List_replace(l, 1, -3)
    return
;

modifyList([1])
list<int> l = [-9]
modifyList(l)
printList(l) # modification should carry over for lists and shared variables
println(int_to_string(global)) # -12
println(int_to_string(local)) # still 5

Thread_join({
    modifyList(l)
    printList(l)
    println(int_to_string(global)) # -2
    println(int_to_string(local)) # still 5
})