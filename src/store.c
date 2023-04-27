#include <stdio.h>

int hi(int x, int y) {
    int z = 0;
    if (y != 0) {
        z = x;
    }
    return z;
}

typedef struct HiParam {
    int x;
    int y;
} HiParam;

typedef struct HiElem {
    HiParam p;
    int result;
} HiElem;

typedef struct Store {
    int curr_index;
    int full;
    HiElem storedvals[32];
} Store;

int main() {

    Store stores;
    stores.curr_index = 0;
    stores.full = 0;

    int r = hi(21, 0);
    // storing 
    HiParam p;
    p.x = 21;
    p.y = 0;
    HiElem e;
    e.p = p;
    e.result = r;
    // update store
    stores.storedvals[stores.curr_index] = e;
    stores.curr_index = (stores.curr_index + 1) % 32;
    stores.full = stores.full || (stores.curr_index == 0);
    // retrieving 
    // assume someone wants to call with 21 and 0 again
    int x = 21;
    int y = 0;
    int start = 0;
    int result = -1;
    while (start < stores.curr_index) {
        if ((x == stores.storedvals[start].p.x) && (y == stores.storedvals[start].p.y)) {
            result = stores.storedvals[start].result;
        }
        start++;
    }
    if (result == -1) {
        result = hi(x, y);
    }
    return 0;
}