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

def list<int> x():
    return [1, 2, 3]
;

def list<list<int>> y():
    list<list<int>> result = [[1, 2, 3], [4, 5, 6]]
    return result
;

def quack z(list<list<int>> y):
    List_replace(y, 0, [1, 2, 3, 4, 9, 9, 8])
    list<int> res = [9, 9]
    List_insert(y, List_len(y), res)
    return
;


printList(x())
printList(List_at(y(), 1))
list<list<int>> res = y()
printList(List_at(res, 0))
printList(List_at(res, 1))
z(res)
printList(List_at(res, 0))
printList(List_at(res, 1))
printList(List_at(res, 2))