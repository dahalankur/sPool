# testing list capture in closure and modification and seeing if 
# it is reflected in the outer scope

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

list<int> l = [1, 2, 3, 4, 5]
list<int> x = l

if (true):
    if (true):
    printList(l)
    List_remove(l, 0)
    l = [1]
    printList(l);
;

printList(l)
printList(x) # still points to literal pointed to by l initially!

def quack captureAndModifyLists():
    printList(l)
    printList(x)
    List_remove(x, 0)
    List_remove(l, 0)
    {{{ l }}} # just a test to see capture does not fail for nested anonymous functions (that's what threads are under the hood)
    return
;

captureAndModifyLists()
printList(l)
printList(x)