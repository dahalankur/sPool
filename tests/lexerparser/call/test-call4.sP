# function calls with non-literal expressions
factorial((15 + 17) - (22 / 2))
ackermann(2 * (2 + 2) - 1, (1 % 2) / - 1)
isFalse((!true) && (false || true))
applyTwice(lambda int (int x, bool y): 
    if (y): 
        return x 
    else return (x + 1);;, 3)