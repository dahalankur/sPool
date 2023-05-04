# tests assignment of functions to variables and returning functions from 
# functions

def (int -> int) initList():
    list<int> mylist = [1,2,3,4,5]
    def int insertIntoMyListAndReturnElemAtIndex(int index):
        # captures mylist from its parent function
        List_insert(mylist, index, List_at(mylist, index) - 1)
        return List_at(mylist, index)
    ;
    return insertIntoMyListAndReturnElemAtIndex
;


(int->int) func = initList()
(int -> int) anotherFunc = func 
# reassigning functions a couple of times
func = anotherFunc
anotherFunc = func 

println(int_to_string(anotherFunc(0)))
println(int_to_string(anotherFunc(0)))
println(int_to_string(anotherFunc(0)))
println(int_to_string(anotherFunc(7)))
