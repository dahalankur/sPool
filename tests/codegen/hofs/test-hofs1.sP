# After defining function, assign it to a different function
def (quack -> quack) ok():
    return lambda quack ():
        println("hello bye")
        return;
;

(quack->quack) ret = lambda quack (): return;

ret = ok()

ret()