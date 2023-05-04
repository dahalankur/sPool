# Test List assignment
# lists a, b, and c all refer to the same list, any changes done to any list 
# should also be reflected by printing out other lists.

list<string> a = ["I", "You", "have", "some", "homework"]

list<string> b = a

list<string> c = []

c = a

int i = 0

while (i < List_len(b)):
    println(List_at(b, i))
    i = i + 1;

i = 0

println("")
while (i < List_len(c)):
    println(List_at(c, i))
    i = i + 1;

List_remove(b, 0)

i = 0


println("")
while (i < List_len(a)):
    println(List_at(a, i))
    i = i + 1;

List_replace(a, 0, "SHE")

i = 0

println("")
while (i < List_len(c)):
    println(List_at(c, i))
    i = i + 1;