# a function that takes in a function and returns a function
def store (int, bool -> bool) specialFun ((int -> bool) f, thread t):
    thread_join(t)
    int c = 123 + 1.321
    bool b = true
    def bool g (int x):
        return f(x) && b;
    return lambda bool (int x, bool c): return g(x) || c;;

specialFun(lambda bool (int x): return x > 0;, thread_create(lambda quack (): sleep(1);))