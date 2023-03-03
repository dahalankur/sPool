# functions that specify store and don't specify store are both valid
def store quack f(int a): return;
def       quack g(int b): return;

# However, functions that do not specify arg names are not valid
def store quack h(int, bool): return; # raises parsing error