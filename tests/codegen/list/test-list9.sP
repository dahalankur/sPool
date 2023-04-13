# testing nested lists
list<list<int>> nested_list = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

int i = 0
while (i < List_len(nested_list)):
    int j = 0
    while (j < List_len(List_at(nested_list, i))):
        int element = List_at(List_at(nested_list, i), j)
        println(int_to_string(element))
        j = j + 1;
    i = i + 1;

list<list<list<string>>> nested_string_list = [[["a"]], [["b", "z"]], [["c"]]]
i = 0
while (i < List_len(nested_string_list)):
    list<list<string>> list1 = List_at(nested_string_list, i)
    int j = 0
    while (j < List_len(list1)):
        int k = 0
        list<string> list2 = List_at(list1, j)
        while (k < List_len(list2)):
            string s = List_at(list2, k)
            println(s)
            k = k + 1;
        j = j + 1;
    i = i + 1;