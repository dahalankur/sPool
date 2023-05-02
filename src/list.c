// list.c
// Implements list functions for sPool that are later linked with the sPool compiler
//
// Written by: Team Nautilus (Ankur, Yuma, Max, Etha)

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

int List_len(Node **l) 
{
    if (!l || !*l) {
        return 0;
    }

    int len = 0;
    Node *temp = *l;

    while (temp != nullptr) {
        len++;
        temp = temp->next;
    }
    return len;
}

void *List_at(Node **l, int index)
{
    if (!l || !*l) {
        return nullptr;
    }
    
    int len = List_len(l);
    assert((index >= 0) && (index < len));

    Node *temp = *l;
    for (int i = 0; i < index; i++) temp = temp->next;
    return temp->data;
}

// data to be inserted should have already been
// allocated on the heap by the time this function 
// is called.
void List_insert(Node **head, int index, void *v)
{

    int len = List_len(head); assert((index >= 0) && (index <= len));
    
    Node *curr = *head; Node *prev = nullptr;
    
    for (int i = 0; i < index; i++) {
        prev = curr;
        curr = curr->next;
    }

    Node *node = malloc(sizeof(*node)); assert(node); 
    node->data = v; node->next = curr;

    if (prev == nullptr) { // adding to the head of the list
        *head = node;
    } else {
        prev->next = node;
    }
}

void List_remove(Node **head, int index)
{
    int len = List_len(head); assert((index >= 0) && (index < len));
    
    Node *curr = *head; Node *prev = nullptr;

    for (int i = 0; i < index; i++) {
        prev = curr;
        curr = curr->next;
    }

    if (prev == nullptr) { // removing the head of the list
        *head = curr->next;
    } else {
        prev->next = curr->next;
    }

    if (curr) free(curr);
}

void List_replace(Node **head, int index, void *v)
{
    assert(head && *head);
    int len = List_len(head); assert((index >= 0) && (index < len));

    Node *curr = *head; void *old = nullptr;
    for (int i = 0; i < index; i++) {
        curr = curr->next;
    }
    old = curr->data;
    curr->data = v;

    if (old) free(old);
}

#if DEBUG
void List_int_print(Node **l)
{
    if (!l || !*l) {
        printf("[]\n");
        return;
    }
    
    Node *t = *l;
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
#endif

#if DEBUG
int main()
{
    int *a = malloc(sizeof(a));
    int *b = malloc(sizeof(b));
    int *c = malloc(sizeof(c));
    int *d = malloc(sizeof(d));
    
    *a = 1; *b = 2; *c = 3; *d = 4;

    Node *l = nullptr; // our list....
    
    List_insert(&l, 0, a);
    List_insert(&l, 1, b); 
    List_insert(&l, 2, c);
    List_insert(&l, 0, d);

    printf("%d\n", List_len(&l));
    List_int_print(&l);

    List_replace(&l, 0, a);
    List_replace(&l, 3, b);
    List_int_print(&l);
}
#endif


