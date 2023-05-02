# nondeterminism.sP
# Demos sPool's threads and the nondeterminism that can occur
# when not properly synchronizing threads.
# Written by Team Nautilus (ankur, etha, max, yuma)
#
# Compile: Use the compile.sh script to compile this source file.
# Example: When inside the src/ directory, run the following command to compile:
#          ./compile.sh ../demos/nondeterminism.sP a.out
#          This will create an executable file named a.out in the src/ directory,
#          which can be simply run to execute the program.

# expected output: sequential printing of 1 to 20 twice with no interleaving of the two threads
#                  when lock is used. Otherwise, the output is nondeterministic.
mutex lock = Mutex()

def quack printTo20(int id):
    # Mutex_lock(lock)
    print("Thread #")
    print(int_to_string(id))
    println(" has started.")
    int i = 1
    while (i <= 20):
        println(int_to_string(i))
        i = i + 1;
    print("Thread #")
    print(int_to_string(id))
    println(" has finished.")
    # Mutex_unlock(lock)
    return;

# dispatch printTo20() asynchronously to two threads
thread t1 = { printTo20(1) }
thread t2 = { printTo20(2) }

# wait for the invoked threads to be done before the main thread exits
Thread_join(t1)
Thread_join(t2)