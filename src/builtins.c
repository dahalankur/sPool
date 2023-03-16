// builtins.c
// Implements the built-in functions for sPool
// To be linked with the generated sPool executable
//
// Written by: Team Nautilus (Ankur, Yuma, Max, Etha)

#include <stdio.h>
#include <stdlib.h>

// TODO: hook this up to llvm somehow
const char *int_to_str(int num) {
    int length = snprintf(NULL, 0, "%d", num);
    char *str = malloc(length+ 1);
    snprintf(str, length + 1, "%d", num);
    return str;
}

int main(void) {
    printf("Enter a number: \n");
    int num;
    scanf("%d", &num);
    printf("%s\n", int_to_str(num));
}