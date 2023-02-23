# dangling else 
if (x > 2):
    x = x - 2
    if (x > 4):
        x = x - 4
        if (x > 8):
            x = x - 8;
    else 
        x = x + 4;;
