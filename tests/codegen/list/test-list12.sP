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
shared int x = 123
int y = 23
{}

if (true):
    printList(l)
    l = [1]
;

printList(l)