# nested functions
def quack hi():
    int i = -99
    println("Inside hi()")
    def quack bye(int i):
        print("Inside bye with arg i = ")
        println(int_to_string(i)) # shadowing captured i with the argument
        return;
    print("Inside hi with variable i = ")
    println(int_to_string(i))
    bye(55)
    return
;

hi()


# TODO: lambdas, higher order functions (passing functions as args, returning functions from functions, assigning functions to variables, etc.)