# a simple test for closures that test capturing function pointers 
# in nested layers of functions
int x = 123
def (int -> int) identity((int -> int) f): return f;

(int -> int) f = lambda int (int x): return x;
(int -> int) g = identity(f)

{{}}
Thread_join({{{{g(2)}}}})
print("done without errors")