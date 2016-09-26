// Steven Raaijmakers, 10804242
// Code simulates a linked list in C

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "ll.h"

// List-node
typedef struct node {
    char *val;
    struct node* next;
    struct node* prev;
} node;

// List (pointer to first item in "list")
struct list {
    node * head;
    int size;
};

// Init list (malloc + null)
list* list_init() {
    list *init = malloc(sizeof(list));
    init->head = NULL;
    init->size = 0;
    return init;
}

// Add node to "list"
void list_add(struct list *l, char *d){
    node *new_node = malloc(sizeof(node));
    new_node->val = d;
    new_node->next = l->head;
    new_node->prev = NULL;

    // When there is a head (!= first time)
    if(l->head){
        l->head->prev = new_node;
    }
    l->head = new_node;
    l->size++;
}

// Return char at index from list
char* list_at_index(struct list *l, int index){
    node * current = l->head;
    if(index > l->size-1){
        return NULL;
    }
    int i = 0;
    while(current){
        if(i == index){
            return current->val;
        }
        else {
            current = current->next;
            i++;
        }
    }
    return NULL;
}

// Remove node
char* list_remove(struct list *l, char *d){
    node * current = l->head;
    while(current){
        // Check if current->val is same as searched string.
        if(strcmp(current->val, d) == 0){
            if(current->prev){
                current->prev->next = current->next;
            }
            else { // first item removed
                l->head = current->next;
            }
            // If not last item
            if(current->next){
                current->next->prev = current->prev;
            }
            free(current);
            l->size--;
            return d;
        }
        current = current->next;
    }
    // No match!
    return NULL;
}

// Cleanup all nodes in "List" L
int list_cleanup(struct list *l){
    int i = 0;
    if(!l->head){
        return i;
    }
    node * current = l->head;
    node * tmp = current->next;
    while(current){
        free(current);
        current = tmp;
        if(current){
            tmp = current->next;
        }
    }
    free(l);
    return i;
}

// Print list
void list_print(struct list *l){
    if (!l){
        printf("List is NULL!\n");
    }
    else {
        int i = 0;
        node * current = l->head;
        while (current) {
            printf("(%d) %s \n", i, current->val);
            i++;
            current = current->next;
        }
    }
}
