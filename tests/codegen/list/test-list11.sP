def quack List_int_print(list<int> l):
    int len = List_len(l)
    int i = 0
    print("[")
    while (i < len):
        print(int_to_string(List_at(l, i)))
        if (i < (len - 1)):
            print(", ")
        ;
        i = i + 1
    ;
    println("]")
    return
;

list<int> l1 = [1,2,3]
list<int> l2 = l1

List_int_print(l1)
List_int_print(l2)

List_remove(l1, 0) # delete head

List_int_print(l1)
List_int_print(l2)
