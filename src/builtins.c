// builtins.c
// Implements the built-in functions for sPool
// To be linked with the generated sPool executable
//
// Written by: Team Nautilus (Ankur, Yuma, Max, Etha)

#include <stdio.h>
#include <stdlib.h>

#define DEBUG 0

const char *int_to_string(int num) 
{
    int length = snprintf(NULL, 0, "%d", num);
    char *str = malloc(length + 1);
    snprintf(str, length + 1, "%d", num);
    return str;
}

const char *float_to_string(double num) 
{
    int length = snprintf(NULL, 0, "%f", num);
    char *str = malloc(length + 1);
    snprintf(str, length + 1, "%f", num);
    return str;
}

const char *bool_to_string(int b) 
{
    if (b == 1) return "true"; return "false";
}

double int_to_float(int num)
{
    return (double)num;
}

int float_to_int(double num)
{
    return (int)num;
}

#if DEBUG
int main(void) {
    printf("Enter a number: \n");
    int num;
    scanf("%d", &num);
    printf("Int_to_string: %s\n", int_to_string(num));
}
#endif