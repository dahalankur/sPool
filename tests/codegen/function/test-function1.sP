# recursive function:

def int factorial(int x):
    int result = 0
    if (x <= 1):
        result = 1
    else 
        result = x * factorial(x - 1)
    ;
    return result
;

int i = 0
while (i < 10):
    println(int_to_string(factorial(i)))
    i = i + 1;