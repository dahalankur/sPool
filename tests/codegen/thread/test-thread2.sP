# dispatch print to a bunch of threads and wait for them to be done
list<thread> threads = []
int num_threads = 10
int i = 0

while (i < num_threads):
    List_insert(threads, i, { println("Hello, world!") })
    i = i + 1
;

i = 0
while (i < num_threads):
    Thread_join(List_at(threads, i))
    i = i + 1
;