# test List replace

list<string> mylist = ["COMP 15", "is", "very", "interesting"]

List_replace(mylist, 0, "COMP 107") # replace in the front
List_replace(mylist, 2, "super") # replace in the middle
List_replace(mylist, 3, "cool") # replace at the end

int i = 0

while (i < List_len(mylist)):
    println(List_at(mylist, i))
    i = i + 1;
