// Steven Raaijmakers, 10804242
// Trie implementation (combined with LinkedList)

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <time.h>

#include "ll.h"
#include "trie.h"

// Tbone
struct tnode {
    struct tnode* parent;
    struct list* children;
    bool word;
};

// Trie: pointer to first tbone
struct trie {
    tnode* root;
};

// Initialize trie
// Malloc space, and initialise children
struct trie* trie_init() {
    trie* init = malloc(sizeof(trie));
    init->root = malloc(sizeof(tnode));
    init->root->parent = NULL;
    init->root->children = list_init();
    return init;
}

// Add word to Trie
// Uses helper function trie_add_helper (recursive)
int trie_add(struct trie* t, char* w) {
    if(trie_lookup(t, w)){
        return 0;
    }
    int index = 0;
    struct tnode* current = list_search(t->root->children, w[index]);
    struct tnode* o = t->root;
    while(current){
        o = current;
        index++;
        current = list_search(current->children, w[index]);
    }
    hang(o, w, index);
    return 1;
}

// "hangs" string (starting with index) at parent node.
void hang(struct tnode* parent, char* w, unsigned int index){
    struct tnode* current = parent;
    while(index <= strlen(w)){
        tnode* new = malloc(sizeof(tnode));
        new->children = list_init();
        new->parent = parent;
        if (index == strlen(w)) {
            new->word = true;
        }
        else {
            new->word = false;
        }
        list_add(current->children, w[index], new);
        index++;
        current = new;
    }
}

// Check if word is alredy in Trie T
// Used as check in other functions (saves time).
int trie_lookup(struct trie* t, char* w) {
    unsigned int n = 0;
    struct tnode* current = list_search(t->root->children, w[n]);
    while(current) {
        if (n == strlen(w)) {
            return 1;
        }
        n++;
        current = list_search(current->children, w[n]);
    }
    return 0;
}

// Doesn't work
int trie_remove(struct trie* t, char* w) {
    // Find place and corresponding depth where to hang W
    unsigned int index = 0;
    struct tnode* current = list_search(t->root->children, w[index]);
    struct tnode* position = t->root;
    while(current){
        position = current;
        index++;
        current = list_search(current->children, w[index]);
    }
    erase(position);
    return 1;
}

// Doesn't work
void erase(struct tnode* start){
    printf("Size: %d", list_size(start->parent->children));
    list_add(start->parent->children, 'a', NULL);
    struct tnode* current = start;
    printf("Hier:");
    list_print(start->parent->children);
}

// Finds all words starting with Prefix in Trie
void trie_prefix(struct trie* t, char* prefix) {
    unsigned int index = 0;
    struct tnode* current = list_search(t->root->children, prefix[index]);
    struct tnode* new = t->root;
    // Find corresponding tnode to end of prefix
    while(current){
        new = current;
        index++;
        current = list_search(current->children, prefix[index]);
    }
    // Print children of this just found tnode
    if (index == strlen(prefix)) {
        char* word = calloc(1, 512);
        strcpy(word, prefix);
        print_sub(new, word, index);
    }
    else {
        printf("None\n");
    }
}

// Print all words
void trie_print(struct trie* t) {
    tnode* current = t->root;
    char* word = calloc(1, 512);
    print_sub(current, word, 0);
}

// Prints all words in subtree T
void print_sub(struct tnode *t, char* word, int depth){
    if(t->word){
        printf("%s\n", word);
    }
    for(int i = 0; i < list_size(t->children); i++){
        struct tnode* o = tnode_at_index(t->children, i);
        char c = char_at_index(t->children, i);
        word[depth] = c;
        print_sub(o, word, depth + 1);
    }
}

// Counts word in Trie
// Uses helper function trie_count_helper
int trie_count(struct trie* t) {
    return trie_count_help(t->root);
}

// Recursive helper function for wordcounter
int trie_count_help(struct tnode *t){
    int n = 0;
    if(t->word){
        n++;
    }
    for(int i = 0; i < list_size(t->children); i++){
        struct tnode* o = tnode_at_index(t->children, i);
        n = n + trie_count_help(o);
    }
    return n;
}

// Free's memory used by Trie T
int trie_cleanup(struct trie* t) {
    return 0;
}
