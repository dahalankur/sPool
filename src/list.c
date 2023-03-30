#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#define nullptr NULL
#define DEBUG 0

typedef struct Node
{
    void *data;
    struct Node *next;
} Node;

int List_len(Node *l) 
{
    int len = 0;
    Node *temp = l;

    while (temp != nullptr) {
        len++;
        temp = temp->next;
    }
    return len;
}

void *List_at(Node *l, int index)
{
    int len = List_len(l);
    assert((index >= 0) && (index < len));

    Node *temp = l;
    for (int i = 0; i < index; i++) temp = temp->next;
    return temp->data;
}

// data to be inserted should have already been
// allocated on the heap by the time this function 
// is called.
Node *List_insert(Node *head, int index, void *v) // TODO: deal with the returned list in codegen; do not make this transparent to the caller
{
    int len = List_len(head); assert((index >= 0) && (index <= len));
    
    Node *curr = head;
    Node *prev = nullptr;

    for (int i = 0; i < index; i++) {
        prev = curr;
        curr = curr->next;
    }

    Node *node = malloc(sizeof(*node)); assert(node);

    if (prev == nullptr) { // adding to the head of the list
        node->data = v;
        node->next = curr;
        return node;
    } else {
        prev->next = node;
        node->next = curr;
        node->data = v;
        return head;
    }

    return head; // suppress warnings
}

// for debugging -- TODO: add list_to_string in builtins?
void List_int_print(Node *l)
{
    Node *t = l;
    int i = 0;
    int len = List_len(l);
    printf("[ ");
    while (i < len) {
        int *data = (int *)List_at(l, i); 
        printf("%d ", *data);
        t = t->next;
        i = i + 1;
    }
    printf("]\n");
}

#if DEBUG
int main()
{
    int *a = malloc(sizeof(a));
    int *b = malloc(sizeof(b));
    int *c = malloc(sizeof(c));
    
    *a = 1; *b = 2; *c = 3;

    Node *l;
    
    l = List_insert(l, 0, a);
    l = List_insert(l, 1, b); 
    l = List_insert(l, 2, c);

    printf("%d\n", List_len(l));
    // printf("%d\n", *(int *)(List_at(l, 0)));

    List_print(l);
}
#endif