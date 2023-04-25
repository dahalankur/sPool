def int square (int x):
    return x * x
;

 def quack hof():
     def quack no():
         println("no")
         return;
     println(int_to_string(square(2)))
     no()
     return;


hof()