#include <stdio.h>
#include <stdint.h>
#include <pthread.h>

#define nullptr NULL
#define DEBUG 0

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

void store_insert(Store *store, int param, int result) {
    pthread_mutex_lock(&lock);
    int index = store->curr_index;
    Elem e = {param = param, result = result};
    store->stored[index] = e;
    store->curr_index = (index + 1) % STORE_SIZE;
    store->full = store->full || ((store->curr_index) == 0);
    pthread_mutex_unlock(&lock);
}

int store_lookup(Store *store, int param) {
    pthread_mutex_lock(&lock);
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
    return result;
}

#if DEBUG
int main() {

    Store stores;
    // stores.curr_index = 0;
    // stores.full = 0;

    // int r = hi(21, 0);
    // // storing 
    // HiParam p;
    // p.x = 21;
    // p.y = 0;
    // HiElem e;
    // e.p = p;
    // e.result = r;
    // // update store
    // stores.storedvals[stores.curr_index] = e;
    // stores.curr_index = (stores.curr_index + 1) % 32;
    // stores.full = stores.full || (stores.curr_index == 0);
    // // retrieving 
    // // assume someone wants to call with 21 and 0 again
    // int x = 21;
    // int y = 0;
    // int start = 0;
    // int result = -1;
    // while (start < stores.curr_index) {
    //     if ((x == stores.storedvals[start].p.x) && (y == stores.storedvals[start].p.y)) {
    //         result = stores.storedvals[start].result;
    //     }
    //     start++;
    // }
    // if (result == -1) {
    //     result = hi(x, y);
    // }
    return 0;
}
#endif