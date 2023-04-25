# return a lambda and define it in the same line
def (quack -> quack) ok():
    return lambda quack ():
        println("hello bye")
        return;
;

(quack->quack) ret = ok()

ret()