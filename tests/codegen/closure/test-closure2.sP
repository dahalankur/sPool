# testing closure of shared variable

shared int a = 5

def quack printA():
    println(int_to_string(a))
    return;

a = 99

printA()