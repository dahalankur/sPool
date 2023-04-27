int x = 213
shared int y = 324
# wildly nested anonymous functions/threads and testing their captures
def int X():
if (true):
    if (true):
        if(true):
            if(true):
                if(true):
                    {}
                    {}
                    {x + y}
                    if (true):
                        if(true):
                            if(true):
                                if (true):
                                    if (true):
                                        {{{{x + y}
                                        x - x - x - y / 2
                                        }}}
                                        {y - x}
                                    ;
                                ;
                            ;
                        ;
                        {y + y - x}
                    ;
                ;
            ;
        ;
        {y % x}
    ;
;
    return 1
;

# no observable output, just has to compile in order to succeed