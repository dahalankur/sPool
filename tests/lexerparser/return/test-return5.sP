# returning a lambda within a lambda
(string -> (int -> int)) stoi = lambda (int -> int) (string intStr):
    return lambda int (): 
        return 3;;
