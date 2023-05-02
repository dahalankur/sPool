# A fun exercise with closures and captured environments!

shared int x = 1
int y = 2
list<list<int>> l = [[1, 2, 3], [4, 5]]

def quack list_int_print(list<int> l):
    print("[ ")
    int i = 0
    while (i < List_len(l)):
        print(int_to_string(List_at(l, i)))
        print(" ")
        i = i + 1;
    println("]")
    return
;

def quack hi():
    int insideHi = 123
    def quack bye():
        insideHi = insideHi + 12323
        println("bye!")
            def quack byeee(): 
                println("Byeeeee")
                println(int_to_string(y))
                return;
        println(int_to_string(x + y))
        x = 0
        y = y + 55
        byeee()
        return;
    
    x = 100
    y = 99
    bye()
    return;

def quack ok(): 
    Thread_join({ 
        print("I am inside a thread, and 1 + 2 = ")
        print(int_to_string(x + y + 1)) })
    println("")
    list_int_print(List_at(l, 1))
    List_remove(l, 1)
    return;

println(int_to_string(x)) # 1
println(int_to_string(y)) # 2
hi() #  bye!, 102, Byeeeee, 2
println(int_to_string(x)) # 0
println(int_to_string(y)) # 2
ok() # 1 + 2 = 3, [4, 5]
list_int_print(List_at(l, 0)) # [1, 2, 3]
