def quack hi():
    println("HI")
    return;

def quack hof((quack -> quack) f):
    f()
    def quack nested():
        println("nested")
        f()
        return;
    nested()
    return;

hof(hi)

hof(lambda quack ():
    println("QUACK")
    return;)
