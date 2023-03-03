# nested lambdas with arrow types

lambda (quack -> (quack -> string)) ():
    lambda (quack -> string) (): 
        lambda string ():
            return "hello there";;;
