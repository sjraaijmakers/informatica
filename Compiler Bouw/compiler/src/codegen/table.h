#ifndef _TABLE_H_
#define _TABLE_H_

typedef struct TABLE_ENTRY entry;
struct TABLE_ENTRY {
    char *name;
    entry *next;
};

typedef struct TABLE {
    entry *first;
} table;

extern table *init_table();
extern void free_table(table *table);
extern void print_table(table *table, FILE *file);
extern void print_global_table(table *table, FILE *file);
extern int add_to_table(table *table, entry *new_entry);
extern int in_table(table *table, entry *entry);
extern entry *make_entry(char *name);

#endif
