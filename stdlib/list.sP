def list<int> List_int_pmap(list<int> l, (int -> int) f):
    int i = 0
    int j = 0
    int len = List_len(l)

    # create a list of threads and the resulting int list
    list<thread> threads = []
    list<int> result = List(len, -1)

    # helper function to carry function computation
    (int -> quack) pmap_helper = lambda quack (int index):
        List_replace(result, index, f(List_at(l, index)));
    
    while (i < len):
        List_insert(threads, i, Thread("pmap_helper", int_to_string(i)))
        i = i + 1;

    # wait for the threads to be done
    while (j < List_len(threads)):
        Thread_join(List_at(threads, j))
        j = j + 1;
    
    return result;

def list<int> List_int_filter(list<int> l, (int -> bool) p):
    int i = 0
    int j = 0
    int elem = -1
    int len = List_len(l)
    list<int> result = []

    while (i < len):
        elem = List_at(l, i)
        if (p(elem)):
            List_insert(result, j, elem)
            j = j + 1;
        i = i + 1;
    return result;

