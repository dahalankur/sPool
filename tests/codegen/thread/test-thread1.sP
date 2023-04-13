# dispatch the print function to another thread, and wait for it to finish 
# so that the main thread will not return before the print function is finished
# executing
Thread_join({println("Hello, I am not in main thread!")})

thread t1 = { println(int_to_string(1 + float_to_int(1.99))) }
Thread_join(t1) # wait for it to be done