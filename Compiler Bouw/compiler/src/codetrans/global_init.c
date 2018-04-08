/* Turns all globaldef assignments into definitions, and puts assigments into __init() */

#include "global_init.h"

#include "types.h"
#include "tree_basic.h"
#include "traverse.h"
#include "dbug.h"

#include "memory.h"
#include "free.h"
#include "str.h"

struct INFO {
    node *init_function;
    node *last_statements;
    bool exprs_encountered;
};

#define INIT_FUNCTION(n)  ((n)->init_function)
#define LAST_STATEMENTS(n)  ((n)->last_statements)
#define INFO_EXPRS_ENCOUNTERED(n)  ((n)->exprs_encountered)

static info *MakeInfo(void) {
    info *result;
    DBUG_ENTER("MakeInfo");
    result = (info *)MEMmalloc(sizeof(info));
    INIT_FUNCTION(result) = NULL;
    LAST_STATEMENTS(result) = NULL;
    INFO_EXPRS_ENCOUNTERED(result) = FALSE;
    DBUG_RETURN(result);
}

static info *FreeInfo(info *info) {
    DBUG_ENTER("FreeInfo");
    info = MEMfree(info);
    DBUG_RETURN(info);
}

node *GIglobaldef(node *arg_node, info *arg_info) {
    DBUG_ENTER("GIglobaldef");

    node *expr = GLOBALDEF_EXPR(arg_node);
    expr = TRAVopt(expr, arg_info);

    if(expr) {
        node *init_function = INIT_FUNCTION(arg_info);
        node *id = TBmakeId(STRcpy(GLOBALDEF_NAME(arg_node)));
        node *new_statement = TBmakeAssign(expr, id);
        node *new_statements = TBmakeStatements(new_statement, NULL);

        node *last_statements = LAST_STATEMENTS(arg_info);
        if (!last_statements) {
            FUNBODY_STATEMENTS(FUNDEF_FUNBODY(init_function)) = new_statements;
        } else {
            STATEMENTS_NEXT(last_statements) = new_statements;
        }
        LAST_STATEMENTS(arg_info) = new_statements;
        GLOBALDEF_EXPR(arg_node) = NULL;

        INFO_EXPRS_ENCOUNTERED(arg_info) = TRUE;
    }
    DBUG_RETURN(arg_node);
}

node *GIprogram(node *arg_node, info *arg_info) {
    DBUG_ENTER("GIprogram");

    node *funheader = TBmakeFunheader(STRcpy("__init"), BT_void, NULL);
    node *funbody = TBmakeFunbody(NULL, NULL);
    node *new_decl = TBmakeFundef(TRUE, funheader, funbody, NULL);
    node *st = TBmakeSymboltable(PROGRAM_SYMBOLTABLE(arg_node), NULL);
    FUNDEF_SYMBOLTABLE(new_decl) = st;

    INIT_FUNCTION(arg_info) = new_decl;

    node *old_decls = TRAVdo(PROGRAM_DECLARATIONS(arg_node), arg_info);

    if (INFO_EXPRS_ENCOUNTERED(arg_info)) {
        node *new_decls = TBmakeDeclarations(new_decl, old_decls);
        PROGRAM_DECLARATIONS(arg_node) = new_decls;
    } else {
        FREEdoFreeTree(new_decl);
    }

    DBUG_RETURN(arg_node);
}

node *GImain(node *syntaxtree) {
    info *arg_info;
    DBUG_ENTER("GImain");
    arg_info = MakeInfo();

    TRAVpush(TR_gi);
    syntaxtree = TRAVdo(syntaxtree, arg_info);
    TRAVpop();

    arg_info = FreeInfo(arg_info);
    DBUG_RETURN(syntaxtree);
}
