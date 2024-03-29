############################################
# start of sPool List_int Standard Library #
############################################

# helper
def list<int> List__helper__(int len, int value):
    int i = 0
    list<int> result = []
    while (i < len):
        List_insert(result, i, value)
        i = i + 1;
    return result;

def list<int> List_int_map(list<int> l, (int -> int) f):
    int i = 0
    int len = List_len(l)
    list<int> result = List__helper__(len, -1)

    while (i < len):
        List_replace(result, i, f(List_at(l, i)))
        i = i + 1;

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


def list<int> List_int_rev(list<int> l):
    int i = 0
    int len = List_len(l)
    list<int> result = List__helper__(len, -1)

    while (i < len):
        List_replace(result, i, List_at(l, len - i - 1))
        i = i + 1;
    return result;


def int List_int_fold((int, int -> int) combine, int zero, list<int> l):
    int i = 0
    int len = List_len(l)
    while (i < len):
        zero = combine(zero, List_at(l, i))
        i = i + 1;
    return zero;


def list<int> List_int_append(list<int> l1, list<int> l2):
    int i = 0
    int len1 = List_len(l1)
    int len2 = List_len(l2)
    list<int> result = List__helper__(len1 + len2, -1)

    while (i < len1):
        List_replace(result, i, List_at(l1, i))
        i = i + 1;
    while (i < len1 + len2):
        List_replace(result, i, List_at(l2, i - len1))
        i = i + 1;
    
    return result;

##########################################
# end of sPool List_int Standard Library #
##########################################

