/* Links symboltables to id, and functions. Also checks if ID is declared upon use */

#include "symbol_table_check.h"

#include "types.h"
#include "tree_basic.h"
#include "traverse.h"
#include "dbug.h"

#include "memory.h"
#include "ctinfo.h"
#include "str.h"
#include "symbol_table.h"

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

/* On functioncall, check if function has been defined (forward decl possible) */
node *STCfuncall(node *arg_node, info *arg_info){
    DBUG_ENTER("STCfundef");

    char *name = FUNCALL_NAME(arg_node);
    node *current = CURRENT_SYMBOLTABLE(arg_info);
    FUNCALL_PARAMS(arg_node) = TRAVopt(FUNCALL_PARAMS(arg_node), arg_info);

    if (!exists_in_ancestors(name, current, TRUE)) {
        CTIerror("Undefined reference to %s at line %d, column %d", name, NODE_LINE(arg_node), NODE_COL(arg_node));
    }

    DBUG_RETURN(arg_node);
}

/* Set symboltables */
node *STCfundef(node *arg_node, info *arg_info) {
    DBUG_ENTER("STCfundef");

    node *previous = CURRENT_SYMBOLTABLE(arg_info);
    CURRENT_SYMBOLTABLE(arg_info) = FUNDEF_SYMBOLTABLE(arg_node);

    FUNDEF_FUNHEADER(arg_node) = TRAVdo(FUNDEF_FUNHEADER(arg_node), arg_info);
    FUNDEF_FUNBODY(arg_node) = TRAVdo(FUNDEF_FUNBODY(arg_node), arg_info);

    CURRENT_SYMBOLTABLE(arg_info) = previous;

    DBUG_RETURN(arg_node);
}

node *STCprogram(node *arg_node, info *arg_info) {
    DBUG_ENTER("STCprogram");

    CURRENT_SYMBOLTABLE(arg_info) = PROGRAM_SYMBOLTABLE(arg_node);
    PROGRAM_DECLARATIONS(arg_node) = TRAVdo(PROGRAM_DECLARATIONS(arg_node), arg_info);

    DBUG_RETURN(arg_node);
}

/* Start function */
node *STCdoStart(node *syntaxtree) {
    info *arg_info;

    DBUG_ENTER("STCdoStart");

    arg_info = MakeInfo();

    TRAVpush(TR_stc);
    syntaxtree = TRAVdo(syntaxtree, arg_info);
    TRAVpop();

    arg_info = FreeInfo(arg_info);

    DBUG_RETURN(syntaxtree);
}
