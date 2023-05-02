# concurrency.sP
# Demos sPool's threads and the nondeterminism that can occur
# when not properly synchronizing threads.
# Written by Team Nautilus (ankur, etha, max, yuma)
#
# Compile: Use the compile.sh script to compile this source file.
# Example: When inside the src/ directory, run the following command to compile:
#          ./compile.sh ../demos/concurrency.sP a.out
#          This will create an executable file named a.out in the src/ directory,
#          which can be simply run to execute the program.

def quack printI(int i):
    print("I = ")
    println(int_to_string(i))
    return
;

int i = 0
int num_threads = 20

while (i < num_threads):
    thread t = { printI(i + 1) }
    # Thread_join(t) # Uncomment this line to make it deterministic

    # when commented, there are multiple threads targeting the same
    # printI function, so the output is non-deterministic and threads "step over"
    # each other while executing the function concurrently. Without Thread_join
    # call, the main thread will exit before the spawned threads finish
    
    i = i + 1
;