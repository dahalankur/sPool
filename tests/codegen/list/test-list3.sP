# Test List PolyMorphism

list<int> a = [1,2,3,4,5]
list<string> b = ["a", "b"]
list<bool> c = [true, false, false, true]
list<float> d = [1.1, 2.2, 3.3]
list<list<int>> e = [[], [1,2,3], [9,8,7], [1]]

println(int_to_string(List_at(a, 0)))
println(List_at(b, 1))
println(bool_to_string(List_at(c, 0)))
println(float_to_string(List_at(d, 2)))
println(int_to_string(List_at(List_at(e, 1), 1)))