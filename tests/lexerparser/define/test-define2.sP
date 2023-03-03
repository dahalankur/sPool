string myname = "quack quack i'm a duck"
string another = myname + "quack!!@"

mutex lock = lock()

thread t1 = Thread(print("hi"))
thread t2 = Thread(print("hi!!&&"))

mutex l2 = lock