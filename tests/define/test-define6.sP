# lambda inside a lambda
(int, bool, thread -> (int -> int)) otherFun = lambda (int -> int) (int i, bool b, thread t):
                                                    return lambda int (int j): return i + j
                                                    ;
                                                ;

# lambda inside a lambda inside a lambda
(int, bool, thread -> (int -> (int -> int))) otherFun = lambda (int -> (int -> int)) (int i, bool b, thread t):
                                                        return lambda (int -> int) (int j):
                                                                return lambda int (int k): return i + j + k
                                                                ;
                                                            ;;