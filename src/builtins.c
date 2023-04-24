// builtins.c
// Implements functions for sPool that are later linked with the sPool compiler
//
// Written by: Team Nautilus (Ankur, Yuma, Max, Etha)

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <pthread.h>

#define DEBUG 0

const char *int_to_string(int num) 
{
    int length = snprintf(NULL, 0, "%d", num);
    char *str = malloc(length + 1); assert(str);
    snprintf(str, length + 1, "%d", num);
    return str;
}

const char *float_to_string(double num) 
{
    int length = snprintf(NULL, 0, "%f", num);
    char *str = malloc(length + 1); assert(str);
    snprintf(str, length + 1, "%f", num);
    return str;
}

const char *bool_to_string(int b) 
{
    return b? "true": "false";
}

double int_to_float(int num)
{
    return (double)num;
}

int float_to_int(double num)
{
    return (int)num;
}

const char *string_concat(const char *s1, const char *s2)
{
    int l1 = strlen(s1);
    int l2 = strlen(s2);
    int len = l1 + l2;
    char *result = malloc(len + 1); assert(result);
    
    for (int i = 0; i < strlen(s1); i++) result[i]      = s1[i];
    for (int j = 0; j < strlen(s2); j++) result[j + l1] = s2[j];
                                         result[len]    = '\0';

    return result;
}

const char *string_substr(const char *s1, int m, int n)
{
    int len = strlen(s1);
    assert((m < n) && (m >= 0 && m <= (len - 1)) && (n >= 0 && n <= len));
    char *result = malloc(n - m + 1); assert(result);

    int i = 0;
    while (m < n) {
        result[i++] = s1[m++];
    }
    result[i] = '\0';

    return result;
}

int string_eq(const char *s1, const char *s2)
{
    return strcmp(s1, s2) == 0;
}

// a helper for mutex to circumvent LLVM's "cannot allocate unsized type" error
pthread_mutex_t **Mutex_init()
{
    pthread_mutex_t **mutex = (pthread_mutex_t **)malloc(sizeof(pthread_mutex_t *)); assert(mutex);
    *mutex = (pthread_mutex_t *)malloc(sizeof(pthread_mutex_t)); assert(*mutex);
    assert(pthread_mutex_init(*mutex, NULL) == 0);
    return mutex;
}

#if DEBUG
int main(void) {
    printf("Enter a number: \n");
    int num;
    scanf("%d", &num);
    printf("Int_to_string: %s\n", int_to_string(num));
}
#endif