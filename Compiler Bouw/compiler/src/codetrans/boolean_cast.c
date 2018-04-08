/* Handles castings from/to booleans */

#include "boolean_cast.h"

#include "types.h"
#include "tree_basic.h"
#include "traverse.h"
#include "dbug.h"

#include "free.h"
#include "copy.h"
#include "str.h"

#include "memory.h"
#include "ctinfo.h"
#include "symbol_table.h"
#include "general_functions.h"

struct INFO {
    basictype type;
};

#define INFO_TYPE(n)  ((n)->type)

static info *MakeInfo(void) {
    DBUG_ENTER("MakeInfo");
    info *result;
    result = (info *)MEMmalloc(sizeof(info));
    INFO_TYPE(result) = BT_unknown;
    DBUG_RETURN(result);
}

static info *FreeInfo(info *info) {
    DBUG_ENTER ("FreeInfo");
    info = MEMfree(info);
    DBUG_RETURN(info);
}

node *BCbool(node *arg_node, info *arg_info) {
    DBUG_ENTER("BCbool");
    INFO_TYPE(arg_info) = BT_bool;
    DBUG_RETURN(arg_node);
}

node *BCint(node *arg_node, info *arg_info) {
    DBUG_ENTER("BCint");
    INFO_TYPE(arg_info) = BT_int;
    DBUG_RETURN(arg_node);
}

node *BCfloat(node *arg_node, info *arg_info) {
    DBUG_ENTER("BCfloat");
    INFO_TYPE(arg_info) = BT_float;
    DBUG_RETURN(arg_node);
}

/* Set type, only when node is expr */
node *BCid(node *arg_node, info *arg_info) {
    DBUG_ENTER("BCid");

    if (ID_SYMBOLTABLE(arg_node)) {
        char *name = ID_NAME(arg_node);
        node *entry = get_from_ancestors(name, ID_SYMBOLTABLE(arg_node), FALSE, NULL, NULL, NULL);
        INFO_TYPE(arg_info) = SYMBOLTABLEENTRY_TYPE(entry);
    }

    DBUG_RETURN(arg_node);
}

/* Set type, only when node is expr */
node *BCbinop(node *arg_node, info *arg_info) {
    DBUG_ENTER("BCbinop");

    BINOP_LEFT(arg_node) = TRAVdo(BINOP_LEFT(arg_node), arg_info);
    BINOP_RIGHT(arg_node) = TRAVdo(BINOP_RIGHT(arg_node), arg_info);

    if (is_relop(BINOP_OP(arg_node))) {
        INFO_TYPE(arg_info) = BT_bool;
    }

    DBUG_RETURN(arg_node);
}

/* Rewrite boolean castings to ternary expressions */
node *BCcast(node *arg_node, info *arg_info) {
    DBUG_ENTER("BCcast");

    INFO_TYPE(arg_info) = BT_unknown;

    CAST_EXPR(arg_node) = TRAVdo(CAST_EXPR(arg_node), arg_info);
    node *cast_expr = CAST_EXPR(arg_node);
    basictype cast_type = CAST_TYPE(arg_node);
    basictype expr_type = INFO_TYPE(arg_info);

    if (cast_type == BT_bool) {
        node *new_expr = NULL;
        if (expr_type == BT_bool) {
            new_expr = COPYdoCopy(cast_expr);
        } else if (expr_type == BT_int) {
            new_expr = TBmakeBinop(BO_ne, COPYdoCopy(cast_expr), TBmakeInt(0));
        } else if(expr_type == BT_float) {
            new_expr = TBmakeBinop(BO_ne, COPYdoCopy(cast_expr), TBmakeFloat(0.0));
        }
        arg_node = FREEdoFreeTree(arg_node);
        arg_node = new_expr;
    } else if (cast_type == BT_int) {
        node *new_expr = NULL;
        if (expr_type == BT_int) {
            new_expr = COPYdoCopy(cast_expr);
        } else if (expr_type == BT_bool) {
            new_expr = TBmakeTernary(COPYdoCopy(cast_expr), TBmakeInt(1), TBmakeInt(0));
        } else {
            new_expr = COPYdoCopy(arg_node);
        }
        arg_node = FREEdoFreeTree(arg_node);
        arg_node = new_expr;
    } else if (cast_type == BT_float) {
        node *new_expr = NULL;
        if (expr_type == BT_float) {
            new_expr = COPYdoCopy(cast_expr);
        } else if (expr_type == BT_bool) {
            new_expr = TBmakeTernary(COPYdoCopy(cast_expr), TBmakeFloat(1.0), TBmakeFloat(0.0));
        } else {
            new_expr = COPYdoCopy(arg_node);
        }
        arg_node = FREEdoFreeTree(arg_node);
        arg_node = new_expr;
    }

    INFO_TYPE(arg_info) = BT_unknown;
    DBUG_RETURN(arg_node);
}

node *BCmain(node *syntaxtree) {
    DBUG_ENTER("BCmain");

    info *arg_info = MakeInfo();

    TRAVpush(TR_bc);
    syntaxtree = TRAVdo(syntaxtree, arg_info);
    TRAVpop();

    arg_info = FreeInfo(arg_info);

    DBUG_RETURN(syntaxtree);
}
