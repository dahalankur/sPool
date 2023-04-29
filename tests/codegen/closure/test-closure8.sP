int x = 10
shared int y = 10
# (int->int) countup = lambda quack (int num): 
#                         while (num > 0):

#                         ;
#                         return
#                     ;

def quack countup (int num):
    if (num > 0):
        countup(num - 1)
        println(int_to_string(num))
    ;
    return
;

def quack countdownToNum(int num):
    if (y > 0):
        println(int_to_string(y))
        y = y - 1
        countdownToNum(num)
    ;
    return
;

def quack closure((int->quack) down):
    println("counting down")
    down(0)

    println("")
    println("counting up")
    countup(x)
    return
;

print("-----Value of y before: ")
println(int_to_string(y))
closure(countdownToNum)
print("-----Value of y after: ")
println(int_to_string(y))

