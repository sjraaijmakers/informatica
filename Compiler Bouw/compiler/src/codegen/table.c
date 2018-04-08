#include "dbug.h"
#include "memory.h"
#include "str.h"
#include "table.h"

/* Basic functions */
table *init_table() {
    table *t = (table *)MEMmalloc(sizeof(table));
    t->first = NULL;
    return t;
}

void free_table(table *table){
    entry *temp;
    entry *cur = table->first;
    while(cur){
        temp = cur->next;
        MEMfree(cur);
        cur = temp;
    }
    table = MEMfree(table);
}

void print_table(table *table, FILE *file){
    entry *cur = table->first;
    while(cur){
        fprintf(file, "%s\n", (char*)cur->name);
        cur = cur->next;
    }
}

void print_global_table(table *table, FILE *file) {
    entry *cur = table->first;
    while(cur){
        fprintf(file, "%s\n", STRsubStr(cur->name, 0, 13));
        cur = cur->next;
    }
}

/* Create entry, suitable for table */
entry *make_entry(char *name){
    entry *tmp = (entry*)MEMmalloc(sizeof(entry));
    tmp->name = name;
    tmp->next = NULL;
    return tmp;
}

/* Adds to table if not exists, and returns the position,
 * if it does exist, just returns the position
 */
int add_to_table(table *table, entry *new_entry) {
    if (!table) {
        return -1;
    } else if (!table->first) {
        table->first = new_entry;
        return 0;
    }

    entry *cur = table->first;
    entry *prev = NULL;
    int store = 0;
    while (cur) {
        if (STReq(cur->name, new_entry->name)) {
            return store;
        }
        prev = cur;
        cur = cur->next;
        store++;
    }
    prev->next = new_entry;
    return store;
}

int in_table(table *table, entry *e) {
    if (!table) {
        return -1;
    }

    entry *cur = table->first;
    int store = 0;
    while (cur) {
        if (STReq(cur->name, e->name)) {
            return store;
        }
        cur = cur->next;
        store++;
    }

    return -1;
}
