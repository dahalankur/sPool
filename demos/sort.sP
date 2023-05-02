# sort.sP
# Implements the merge sort algorithm in sPool
# Written by Team Nautilus (ankur, etha, max, yuma)
#
# Compile: The standard library needs to be imported while compiling this source
#          file. To do so, use the compile.sh script with the -stdlib flag.
# Example: When inside the src/ directory, run the following command to compile:
#          ./compile.sh -stdlib ../demos/sort.sP a.out
#          This will create an executable file named a.out in the src/ directory,
#          which can be simply run to execute the program.

def quack printList(list<int> l):
    int len = List_len(l)
    int i = 0
    print("[ ")
    while (i < len):
        print(int_to_string(List_at(l, i)))
        print(" ")
        i = i + 1;
    println("]")
    return
;

def list<int> cdr(list<int> l):
    list<int> result = []
    int len = List_len(l)
    int i = 1
    while (i < len):
        result = List_int_append(result, [List_at(l, i)])
        i = i + 1
    ;
    return result
;

def list<int> merge(list<int> a, list<int> b):
    int len_a = List_len(a)
    int len_b = List_len(b)
    list<int> result = []
    if (len_a == 0):
        result = b
    else if (len_b == 0):
        result = a
        else
            if (List_at(a, 0) < List_at(b, 0)):
                result = List_int_append(result, [List_at(a, 0)])
                result = List_int_append(result, merge(cdr(a), b))
            else
                result = List_int_append(result, [List_at(b, 0)])
                result = List_int_append(result, merge(a, cdr(b)))
            ;;;
    return result
;


def list<int> mergeSort(list<int> l):
    int len = List_len(l)
    list<int> result = []
    if (len <= 1):
        result = l
    else
        int mid = len / 2
        list<int> left = []
        list<int> right = []
        int i = 0
        while (i < mid):
            left = List_int_append(left, [List_at(l, i)])
            i = i + 1
        ;
        while (i < len):
            right = List_int_append(right, [List_at(l, i)])
            i = i + 1
        ;
        result = merge(mergeSort(left), mergeSort(right))
    ;
    return result
;


printList(mergeSort([]))
printList(mergeSort([1]))
printList(mergeSort([1, 3, 2, 4, 5, 6, 7, 8, 9, 10]))
printList(mergeSort([-10, -9, -8, -7, -6, -5, -4, -3, -2, -1]))
printList(mergeSort([1, -3, 2, -4, 5, -6, 7, -8, 9, -10]))
printList(mergeSort([10, -9, 8, -7, 6, -5, 4, -3, 2, -1]))
printList(mergeSort([1, 3, 2, 4, 5, 6, 437, 8, 9, 10, 1112, 12, 13, 14, 15, 16, 17, 18, 19, 20]))
printList(mergeSort([1, -3, 2, -4, 5, -6, 7, -8, 9, -10, 0, 100, -100, 1000, -1000, 10000, -10000]))
