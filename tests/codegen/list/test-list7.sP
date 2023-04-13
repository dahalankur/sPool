# test List_len

list<int> l = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]

println(int_to_string(List_len(l))) # should be 15
List_remove(l, 0)
List_remove(l, 0)
List_remove(l, 0)
List_remove(l, 0)
List_remove(l, 0)
List_remove(l, 0)
println(int_to_string(List_len(l))) # should be 9 after 6 removals
List_insert(l, 0, -1)
List_insert(l, 0, -2)
println(int_to_string(List_len(l))) # should be 11 after 2 insertions
