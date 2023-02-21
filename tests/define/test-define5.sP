# arrow types with multiple arguments
(int, bool, thread, mutex, list<list<int>> -> quack) myFun = lambda quack (int i, bool b, thread t, mutex m, list<list<int>> l):
                            i = b || t || m || l
                            lock(m)
                            List_append(l, [-123])
                            unlock(m)
                            return

; # closing the lambda