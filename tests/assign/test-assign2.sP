# lambdas can be assigned to variables too!
myfun = lambda quack (int j):
           return j || false;

another_var = myfun(1)

myvar = 1
anotherVar = myvar + myvar || false && myvar * -myvar # just a simple expression :)
nice2beme = myvar + (((myvar || false) && myvar) * -(myvar / 1)) # parentheses change precedence!