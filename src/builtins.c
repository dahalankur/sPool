// builtins.c
// Implements the built-in functions for sPool
// To be linked with the generated sPool executable
//
// Written by: Team Nautilus (Ankur, Yuma, Max, Etha)

#include <stdio.h>
#include <stdlib.h>

#define DEBUG 0

const char *int_to_string(int num) {
    int length = snprintf(NULL, 0, "%d", num);
    char *str = malloc(length + 1);
    snprintf(str, length + 1, "%d", num);
    return str;
} 

#if DEBUG
int main(void) {
    printf("Enter a number: \n");
    int num;
    scanf("%d", &num);
    printf("%s\n", int_to_string(num));
}
#endif