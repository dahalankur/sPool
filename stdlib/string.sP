##########################################
# start of sPool String Standard Library #
##########################################

def string String_rev(string word):
    int    len = String_len(word)
    string rev = ""

    while (len > 0):
       rev = String_concat(rev, String_substr(word, len - 1, len))
       len = len - 1;

    return rev;


def int String_find(string haystack, string needle):
    int l1 = String_len(haystack)
    int l2 = String_len(needle)
    int answer = -1

    if (l1 < l2):
        answer = -1
    else
        int i = 0
        while (i < l1 - l2 + 1):
            if (String_eq(String_substr(haystack, i, i + l2), needle)):
                answer = i
                i = l1 - l2 + 1 # Hack: 'break' the loop
            else
                i = i + 1;
        ;
    ;
    
    return answer;

########################################
# end of sPool String Standard Library #
########################################
