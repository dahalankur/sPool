mutex lock = Mutex()
shared int globalCount = 0

def quack add11ToCount():
    Mutex_lock(lock)
    int temp = globalCount
    temp = temp + 11
    globalCount = temp
    Mutex_unlock(lock)
    return;

int i = 0
list<thread> ts = []
int NUM_THREADS = 1000


while (i < NUM_THREADS):
    List_insert(ts, i, { add11ToCount() } )
    i = i + 1
;

i = 0
while (i < NUM_THREADS):
    Thread_join(List_at(ts, i))
    i = i + 1
;

print("globalCount = ")
println(int_to_string(globalCount))