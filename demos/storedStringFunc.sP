# storedStringFunc.sP
# an illustration of how to leverage the stored int->int function to other 
# data types. Here we choose to write a HOF that converts a string->string
# function to a stored string->string function with the help of int<->string
# encoding and decoding functions.


# define the detault alphabet to be lower case a-z, with the first
# character to be an unused character
string alphabet = "/abcdefghijklmnopqrstuvwxyz"

# convert a letter to its integer code
def int intOfLetter(string letter):
    return String_find(alphabet, letter);

# convert an integer code to its corresponding letter
def string letterOfInt(int code):
    return String_substr(alphabet, code, code + 1);

# convert a string of letters to an integer
def int convertStrToInt(string aStr):
    int idx = 0
    int lenStr = String_len(aStr)
    int result = 0

    while (idx < lenStr):
        result = result * 27 + intOfLetter(String_substr(aStr, idx, idx + 1))
        idx = idx + 1
    ;

    return result
;

# decompose an integer to its corresponding string
def string decompIntToStr(int aNum):
    string result = ""
    int moded = -1

    while (aNum != 0):
        moded = aNum % 27
        result = String_concat(letterOfInt(moded), result)
        aNum = aNum / 27
    ;

    return result
;

# makeStr2Store
# Purpose: make a string to string function a "stored" function
#          such that it has the access to previously computed values stored in
#          the "cache"
# Parameter: a function of type string->string where the strings are limited to
#            be consisted of lowercase English letters 'a' to 'z'; string of 
#            length 7 or shorter are supported
# Return:    a function of type string->string that does the same thing as the
#            input function, but has the store feature.
#
# An illustration of the returned function:;
#
#           |--------------------------------------------------------------------------------------------------
#           |                                                                                                 |
#           |                 |------------------------------------------------------                         |
#           |                 |                                                     |                         |
#           |                 |                                                     |                         |
#           |                 |                |----------|                         |                         |
#    str    | (encodeto) int  | (decodeto) str |          |   str    (encodeto) int |         (decodeto) str  | str
# ------->  | ------------->  | -------------> | origFunc | -------> -------------->| ----->|---------------> |----->
#           |  is cached? No  |                |----------|                         |      /                  |
#           |     \           |                                                     |     /                   |
#           |      \Yes       |                                                     |    /                    |
#           |       \         |------------------------------------------------------   /                     |
#           |        \                                                 stored int      /                      |
#           |         \--------<found the result in the store table>------------------/                       |
#           |                                                                                                 |
#           |--------------------------------------------------------------------------------------------------
#
def (string->string) makeStr2Store((string->string) aFunc):

    # utilizes the store function of int->int under the hood
    def store int storeHelperFunc(int x):
        string strX = decompIntToStr(x)
        string output = aFunc(strX)
        return convertStrToInt(output)
        ;

    # this is the stored version of the original function to be returned
    def string storedFunc(string inputStr):
        int intArg = convertStrToInt(inputStr)
        int result = storeHelperFunc(intArg) # calls the stored helper function
        return decompIntToStr(result)
    ;

    return storedFunc
;


# a simple function that prints its argument and returns its argument
def string printStrIdt(string x):
    println(x)
    return x;

# make the stored-version of printStrIdt
(string->string) stored_printStrIdt = makeStr2Store(printStrIdt)


# should print hello three times and print hellow once
println("----------Non-stored print---------------")
printStrIdt("hello")
printStrIdt("hello")
printStrIdt("hello")
printStrIdt("hellow")
println("-----------------------------------------")

println("----------Stored print-------------------")
# should print hello exactly once, followed by hellow
stored_printStrIdt("hello")
stored_printStrIdt("hello")
stored_printStrIdt("hello")
stored_printStrIdt("hellow")
println("-----------------------------------------")