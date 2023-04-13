string x = "hello"
string y = " world"
string z = ""

if (!String_eq(x, y)):
    z = String_concat(x, y)    
;

println(z)