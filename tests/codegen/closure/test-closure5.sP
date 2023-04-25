int  x = 123

def quack main():
    int i = 0

    println(int_to_string(x))
    
    while (i < 10):
        # repeatedly defining a function in a loop
        def int hi(): return i;
        i = i + 1
        println(int_to_string(hi()))
        println(int_to_string(x))
        x = x + 1
        ;
    return;

main()