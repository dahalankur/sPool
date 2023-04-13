# test List_remove

list<string> l =  ["GOODBYE", "HELLO", "VOID", "WORLD", "TOMORROW"]

List_remove(l, 0) # Remove front
List_remove(l, 1) # Remove middle
List_remove(l, 2) # Remove front

int i = 0

while (i < List_len(l)):
    println(List_at(l, i))
    i = i + 1;
