// Steven Raaijmakers, 10804242
// Code simulates a linked list in C

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <error.h>
#include <assert.h>
#include <math.h>

#include "ll.h"
struct tnode;

// List-node
struct node {
    char val;
    struct tnode* child;
    node* next;
    node* prev;
};

// List (pointer to first item in "list")
struct list {
    node* head;
    int size;
};

// Init list (malloc + null)
list* list_init(){
    list *init = malloc(sizeof(list));
    init->head = NULL;
    init->size = 0;
    return init;
}

// Add node to "list"
void list_add(struct list *l, char c, void* child){
    node *new_node = malloc(sizeof(node));
    new_node->val = c;
    new_node->child = child;
    new_node->next = l->head;
    new_node->prev = NULL;

    // When there is a head (!= first time)
    if(l->head){
        l->head->prev = new_node;
    }
    l->head = new_node;
    l->size++;
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
            // DONT PRINT NULL CHAR
            if(current->val != '\0'){
                printf("%c - ", current->val);
            }
            i++;
            current = current->next;
        }
    }
}

// FUNCTIONS ADDED FOR TRIE:
// Finds character in list and returns tnode it points to
struct tnode* list_search(struct list *l, char c){
    node* current = l->head;
    while(current){
        if(tolower(current->val) == tolower(c)){
            return current->child;
        }
        current = current->next;
    }
    return NULL;
}

// Return tnode at index from list
struct tnode* tnode_at_index(struct list *l, int index){
    node * current = l->head;
    if(index > l->size-1){
        return NULL;
    }
    int i = 0;
    while(current){
        if(i == index){
            return current->child;
        }
        else {
            current = current->next;
            i++;
        }
    }
    return NULL;
}

int list_size(struct list *l){
    return l->size;
}

// Return char at index from list
char char_at_index(struct list *l, int index){
    node * current = l->head;
    if(index > l->size-1){
        return '\0';
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
    return '\0';
}
