# mutex.sP
# Demos sPool's threads and synchronization primitives (mutexes)
# Written by Team Nautilus (ankur, etha, max, yuma)
#
# Compile: Use the compile.sh script to compile this source file.
# Example: When inside the src/ directory, run the following command to compile:
#          ./compile.sh ../demos/mutex.sP a.out
#          This will create an executable file named a.out in the src/ directory,
#          which can be simply run to execute the program.

shared int x = 0 # a "global" variable from the perspective of the threads
mutex lock = Mutex()

def quack add11ToX():
    # Uncommenting calls to Mutex_lock and Mutex_unlock will cause the program to work as expected
    # because the lock will be acquired and released before the next thread can access the shared variable
    # This is critical when synchronizing access to shared mutable variables

    # This acts like a "barrier", allowing only one thread to access the critical region at any given time
    # other threads will wait until the lock is released before they can access the critical region
    # Mutex_lock(lock)

    # reassigning to temp variable to increase surface area for race conditions
    int temp = x
    temp = temp + 11
    x = temp
    print("X = ")
    println(int_to_string(x))

    # Mutex_unlock(lock)
    return
;

int i = 0
int num_threads = 50
list<thread> ts = []

while (i < num_threads):
    List_insert(ts, i, { add11ToX() }) # invoke the function add11ToX in a new thread
    i = i + 1
;

# wait for all threads to be done
i = 0
while (i < num_threads):
    Thread_join(List_at(ts, i))
    i = i + 1
;

# when calls to lock and unlock are commented out, this will not always print 
# 550 because of race conditions
print("Final value of x = ")
println(int_to_string(x)) 