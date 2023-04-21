# testing closure of non-shared variables

int a = 5

def quack printA():
    println(int_to_string(a))
    return;

a = 99

printA()