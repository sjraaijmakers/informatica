#include "symbol_table.h"

#include "types.h"
#include "tree_basic.h"
#include "traverse.h"
#include "dbug.h"

#include "memory.h"
#include "ctinfo.h"
#include "str.h"

struct INFO {
    node *current;
};

#define CURRENT_SYMBOLTABLE(n)  ((n)->current)

static info *MakeInfo(void) {
    info *result;
    DBUG_ENTER("MakeInfo");
    result = (info *)MEMmalloc(sizeof(info));
    CURRENT_SYMBOLTABLE(result) = NULL;
    DBUG_RETURN(result);
}

static info *FreeInfo(info *info) {
    DBUG_ENTER("FreeInfo");
    info = MEMfree(info);
    DBUG_RETURN(info);
}

/* HELPER FUNCTIONS */
bool exists_in_ancestors (char *name, node* symboltable, bool is_function) {
    node *entry = get_from_ancestors(name, symboltable, is_function, NULL, NULL, NULL);
    return entry != NULL;
}

bool exists_in_current(char *name, node *symboltable, bool is_function) {
    node *entry = get_from_current(name, symboltable, is_function, NULL);
    return entry != NULL;
}

node *get_from_ancestors(char *name, node *symboltable, bool is_function, int *lookup, int *store, bool *is_global) {
    node *current_table = symboltable;
    if (lookup)
        *lookup = 0;
    if (is_global)
        *is_global = FALSE;

    while (current_table) {
        node *current_entry = get_from_current(name, current_table, is_function, store);
        if (current_entry) {
            if (!SYMBOLTABLE_PARENT(current_table) && is_global) {
                *is_global = TRUE;
            }
            return current_entry;
        }
        current_table = SYMBOLTABLE_PARENT(current_table);
        if (lookup)
            (*lookup)++;
    }
    if (lookup)
        *lookup = -1;
    return NULL;
}

node *get_from_current(char *name, node *symboltable, bool is_function, int *store) {
    node *current_entry = SYMBOLTABLE_ENTRIES(symboltable);
    if (store)
        *store = 0;
    while (current_entry) {
        if(STReq(SYMBOLTABLEENTRY_NAME(current_entry), name) &&
            SYMBOLTABLEENTRY_ISFUNCTION(current_entry) == is_function) {
            return current_entry;
        }
        current_entry = SYMBOLTABLEENTRY_NEXT(current_entry);
        if (store)
            (*store)++;
    }
    if (store)
        *store = -1;
    return NULL;
}

/* Get last entry of a symbol table */
node *get_last_entry(node *symboltable) {
    if (!SYMBOLTABLE_ENTRIES(symboltable)) {
        return NULL;
    }
    node *current_entry = SYMBOLTABLE_ENTRIES(symboltable);
    while (SYMBOLTABLEENTRY_NEXT(current_entry)) {
        current_entry = SYMBOLTABLEENTRY_NEXT(current_entry);
    }
    return current_entry;
}

/* Add new symbol table entry to a symbol table */
node *add_to_st(node *current, char *name, basictype type, node *param, bool is_function, bool import, bool export) {
    if (exists_in_current(name, current, is_function)) {
        if (is_function) {
            CTIerror("Redefinition of function %s at line %d, column %d", name, NODE_LINE(current), NODE_COL(current));
        } else {
            CTIerror("Redefinition of var %s at line %d, column %d", name, NODE_LINE(current), NODE_COL(current));
        }
        return NULL;
    }
    node *last = get_last_entry(current);
    node *new_ste = TBmakeSymboltableentry(name, type, is_function, import, export, NULL);
    SYMBOLTABLEENTRY_PARAMS(new_ste) = param;
    if (!last) {
        SYMBOLTABLE_ENTRIES(current) = new_ste;
    } else {
        SYMBOLTABLEENTRY_NEXT(last) = new_ste;
    }
    return new_ste;
}

/* NODES */

node *STparam(node *arg_node, info *arg_info) {
    DBUG_ENTER("STparam");

    node *current = CURRENT_SYMBOLTABLE(arg_info);
    char *name = PARAM_NAME(arg_node);
    basictype type = PARAM_TYPE(arg_node);
    add_to_st(current, name, type, NULL, FALSE, FALSE, FALSE);

    PARAM_NEXT(arg_node) = TRAVopt(PARAM_NEXT(arg_node), arg_info);

    DBUG_RETURN(arg_node);
}

node *STid(node *arg_node, info *arg_info) {
    DBUG_ENTER("STid");

    node *current = CURRENT_SYMBOLTABLE(arg_info);
    char *name = ID_NAME(arg_node);

    if (!exists_in_ancestors(name, current, FALSE)) {
        CTIerror("Undefined reference to variable %s at line %d, column %d", name, NODE_LINE(arg_node), NODE_COL(arg_node));
    }

    ID_SYMBOLTABLE(arg_node) = current;

    DBUG_RETURN(arg_node);
}

/* For fundef, add function + params to current symboltable & create new symboltable for its funbody */
node *STfundef(node *arg_node, info *arg_info) {
    DBUG_ENTER("STfundef");

    node *current = CURRENT_SYMBOLTABLE(arg_info);
    node *previous = current;

    char *name = FUNHEADER_NAME(FUNDEF_FUNHEADER(arg_node));
    basictype type = FUNHEADER_RETTYPE(FUNDEF_FUNHEADER(arg_node));

    add_to_st(current, name, type, FUNHEADER_PARAMS(FUNDEF_FUNHEADER(arg_node)), TRUE, FALSE, FUNDEF_EXPORT(arg_node));

    current = TBmakeSymboltable(previous, NULL);

    CURRENT_SYMBOLTABLE(arg_info) = current;
    FUNDEF_SYMBOLTABLE(arg_node) = current;

    FUNDEF_FUNHEADER(arg_node) = TRAVdo(FUNDEF_FUNHEADER(arg_node), arg_info);
    FUNDEF_FUNBODY(arg_node) = TRAVdo(FUNDEF_FUNBODY(arg_node), arg_info);

    CURRENT_SYMBOLTABLE(arg_info) = previous;

    FUNDEF_NEXT(arg_node) = TRAVopt(FUNDEF_NEXT(arg_node), arg_info);

    DBUG_RETURN(arg_node);
}

/* For vardec, check if var is in current or its parrents. if not; add to current */
node *STvardec(node *arg_node, info *arg_info) {
    DBUG_ENTER("STvardec");

    char *name = VARDEC_NAME(arg_node);
    // node *current = CURRENT_SYMBOLTABLE(arg_info);

    // if(exists_in_ancestors(name, current, FALSE)){

    // }
    

    VARDEC_EXPR(arg_node) = TRAVopt(VARDEC_EXPR(arg_node), arg_info);


    basictype type = VARDEC_TYPE(arg_node);
    node *current_st = CURRENT_SYMBOLTABLE(arg_info);
    add_to_st(current_st, name, type, NULL, FALSE, FALSE, FALSE);

    VARDEC_NEXT(arg_node) = TRAVopt(VARDEC_NEXT(arg_node), arg_info);

    DBUG_RETURN(arg_node);
}

node *STfundec(node *arg_node, info *arg_info) {
    DBUG_ENTER("STfundec");

    char *name = FUNHEADER_NAME(FUNDEC_FUNHEADER(arg_node));
    basictype type = FUNHEADER_RETTYPE(FUNDEC_FUNHEADER(arg_node));
    node *current_st = CURRENT_SYMBOLTABLE(arg_info);

    add_to_st(current_st, name, type, FUNHEADER_PARAMS(FUNDEC_FUNHEADER(arg_node)), TRUE, TRUE, FALSE);

    DBUG_RETURN(arg_node);
}

node *STglobaldec(node *arg_node, info *arg_info) {
    DBUG_ENTER("STGlobalDec");

    char *name = GLOBALDEC_NAME(arg_node);
    basictype type = GLOBALDEC_TYPE(arg_node);
    node *current_st = CURRENT_SYMBOLTABLE(arg_info);
    add_to_st(current_st, name, type, NULL, FALSE, TRUE, FALSE);

    DBUG_RETURN(arg_node);
}

node *STglobaldef(node *arg_node, info *arg_info) {
    DBUG_ENTER("STglobaldef");

    GLOBALDEF_EXPR(arg_node) = TRAVopt(GLOBALDEF_EXPR(arg_node), arg_info);

    char *name = GLOBALDEF_NAME(arg_node);
    basictype type = GLOBALDEF_TYPE(arg_node);
    node *current_st = CURRENT_SYMBOLTABLE(arg_info);
    add_to_st(current_st, name, type, NULL, FALSE, FALSE, GLOBALDEF_EXPORT(arg_node));

    DBUG_RETURN(arg_node);
}

node *STdeclarations(node *arg_node, info *arg_info) {
    DBUG_ENTER("STdeclarations");

    if (NODE_TYPE(DECLARATIONS_DECLARATION(arg_node)) == N_fundef) {
        DECLARATIONS_NEXT(arg_node) = TRAVopt(DECLARATIONS_NEXT(arg_node), arg_info);
        DECLARATIONS_DECLARATION(arg_node) = TRAVdo(DECLARATIONS_DECLARATION(arg_node), arg_info);
    } else {
        DECLARATIONS_DECLARATION(arg_node) = TRAVdo(DECLARATIONS_DECLARATION(arg_node), arg_info);
        DECLARATIONS_NEXT(arg_node) = TRAVopt(DECLARATIONS_NEXT(arg_node), arg_info);
    }

    DBUG_RETURN(arg_node);
}

node *STprogram(node *arg_node, info *arg_info) {
    DBUG_ENTER("STprogram");

    /* Create the global symbol table */
    node *global_symboltable = TBmakeSymboltable(NULL, NULL);
    PROGRAM_SYMBOLTABLE(arg_node) = global_symboltable;
    CURRENT_SYMBOLTABLE(arg_info) = global_symboltable;

    /* Traverse the declarations */
    PROGRAM_DECLARATIONS(arg_node) = TRAVdo(PROGRAM_DECLARATIONS(arg_node), arg_info);

    DBUG_RETURN(arg_node);
}

/* Start function */
node *STdoStart(node *syntaxtree) {
    info *arg_info;

    DBUG_ENTER("STdoStart");

    arg_info = MakeInfo();

    TRAVpush(TR_st);
    syntaxtree = TRAVdo(syntaxtree, arg_info);
    TRAVpop();

    arg_info = FreeInfo(arg_info);

    DBUG_RETURN(syntaxtree);
}
