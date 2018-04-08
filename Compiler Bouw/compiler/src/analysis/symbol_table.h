#ifndef _SYMBOL_TABLE_H_
#define _SYMBOL_TABLE_H_
#include "types.h"

node *STdoStart(node *syntaxtree);
node *STprogram(node *arg_node, info *arg_info);
node *STdeclarations(node *arg_node, info *arg_info);

/* Declaration traversals */
node *STglobaldec(node *arg_node, info *arg_info);
node *STglobaldef(node *arg_node, info *arg_info);
node *STfundef(node *arg_node, info *arg_info);
node *STfundec(node *arg_node, info *arg_info);
node *STfor(node *arg_node, info *arg_info);

node *STvardec(node *arg_node, info *arg_info);
node *STparam(node *arg_node, info *arg_info);
node *STid(node *arg_node, info *arg_info);

/* Helper functions */
extern bool exists_in_current(char *name, node *symboltable, bool is_function);
extern bool exists_in_ancestors(char *name, node* st, bool is_function);
extern node *get_from_current(char *name, node *symboltable, bool is_function, int *store);
extern node *get_from_ancestors(char *name, node *symboltable, bool is_function, int *lookup, int *store, bool *is_global);

node *get_last_entry(node *st);
node *add_to_st(node *current, char *s, basictype type, node *ste_param, bool is_function, bool import, bool export);

#endif
