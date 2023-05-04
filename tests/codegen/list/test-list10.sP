list<int> l = [1,2,3]
def quack hi():
    println(int_to_string(List_at(l,1)))
    List_insert(l,0,-1)
    return
;

def quack bye(list<int> l):
    List_insert(l,0,-5)
    return
;

hi()

list<int> y = [1,2,3]

println(int_to_string(List_at(l,0)))
bye(y)
println(int_to_string(List_at(y,0)))