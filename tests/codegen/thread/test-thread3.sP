list<thread> ts = []
list<int> ls    = []
int NUM_THREADS = 20

# pre-fill ls with dummy data
int j = 0
while(j < NUM_THREADS):
    List_insert(ls, j, -1)
    j = j + 1
;

int i = 0

def quack printList():
    int len = List_len(ls) # captured list ls!
    int i = 0
    print("[ ")
    while (i < len):
        print(String_concat(int_to_string(List_at(ls, i)), " "))
        i = i + 1
    ;
    println("]")
    return
;

def quack replaceInList(int index, int value):
    List_replace(ls, index, value) # again, testing closures by capturing ls
    return;

while (i < NUM_THREADS):
    Thread_join({replaceInList(i, i + 1)})
    i = i + 1
;

printList()