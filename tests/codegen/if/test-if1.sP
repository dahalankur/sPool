if (true && (false || true)):
    # we are in!
    println("I'm in captain")
    if (false):
        println("Not here though...sigh")
    else println("In here too!");
else
    println("You'll never get here, you'll never get here!") # else of outermost if
;

if (false):
    # empty statement list is allowed!
else
    if (true):
        if (true):
            if (true):
                if (true):
                    println("I'm in too deep")
                    if (false):
                    else println("And I'm tryin to keep")
                    ;
                ;
            ;
        else print("1");
        print(int_to_string(1 + 1)) # will print 2!
    ;
;