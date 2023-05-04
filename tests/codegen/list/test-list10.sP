list<int> l = [1,2,3]

def quack printListAndInsertIntoHead():
    println(int_to_string(List_at(l,1)))
    List_insert(l,0,-1)
    return
;

def quack insertIntoHead(list<int> l):
    List_insert(l,0,-5)
    return
;

printListAndInsertIntoHead()

list<int> y = [1,2,3]

println(int_to_string(List_at(l,0)))
insertIntoHead(y)
println(int_to_string(List_at(y,0)))