// store.c
// Implements functions related to the store feature for sPool that are later 
// linked with the sPool compiler
//
// Written by: Team Nautilus (Ankur, Yuma, Max, Etha)

#include <stdio.h>
#include <stdint.h>
#include <pthread.h>
#include <assert.h>

#define nullptr NULL
#define DEBUG 0
#define INCLUDEMAIN 0

static pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

#define STORE_SIZE 32

typedef struct Node *Node;

typedef struct Elem {
    int param;
    int result;
} Elem;

typedef struct Store {
    int curr_index;
    int full; // bool -> is it full?
    Elem stored[STORE_SIZE];
} Store;

void store_insert(void *s, int param, int result) {
    assert(s);
    pthread_mutex_lock(&lock);
#if DEBUG
    printf("\nstore_insert\nInserting arg %d = result %d\n", param, result);
#endif
    Store *store = (Store *)s;
    int index = store->curr_index;
    Elem e = {param = param, result = result};
    store->stored[index] = e;
    store->curr_index = (index + 1) % STORE_SIZE;
    store->full = store->full || ((store->curr_index) == 0);
    pthread_mutex_unlock(&lock);
#if DEBUG
    printf("\ndone inserting arg %d = result %d\n", param, result);
#endif
}

int store_lookup(void *s, int param) {
    assert(s);
    pthread_mutex_lock(&lock);
#if DEBUG
    printf("\nstore_lookup\nLooking up %d\n", param);
#endif
    Store *store = (Store *)s;
    int start = 0;
    int end = store->full ? (STORE_SIZE - 1) : store->curr_index;
    int result = INT32_MIN;

    while (start <= end) {
        Elem e = store->stored[start];
        if (e.param == param) {
            result = e.result;
            break;
        }
        start += 1;
    }
    pthread_mutex_unlock(&lock);
#if DEBUG
    printf("\nreturned %d\n", result);
#endif
    return result;
}

#if INCLUDEMAIN
int main() {

    Store stores;

    return 0;
}
#endif