# Test List Insert Built-in Function

list<int> a = [1, 2, 3]

List_insert(a, 0, -199) # Insert at head

int index = 0

print("[ ")
while (index < List_len(a)):
    print(int_to_string(List_at(a, index)))
    print(" ")
    index = index + 1
;
println("]")

List_insert(a, 2, 99) # Insert at middle

index = 0

print("[ ")
while (index < List_len(a)):
    print(int_to_string(List_at(a, index)))
    print(" ")
    index = index + 1
;
println("]")

List_insert(a, 5, 199) # Insert at end

index = 0

print("[ ")
while (index < List_len(a)):
    print(int_to_string(List_at(a, index)))
    print(" ")
    index = index + 1
;
println("]")