/* Rewrite all for loops to while loops */

#include "for_to_while.h"

#include "types.h"
#include "tree_basic.h"
#include "traverse.h"
#include "dbug.h"

#include "free.h"
#include "copy.h"
#include "str.h"

#include "general_functions.h"

/* Append statements to a block */
void append_to_block(node *block, node *new_statements){
    node *last_statements = get_last_statements(BLOCK_STATEMENTS(block));
    if (!last_statements) {
        BLOCK_STATEMENTS(block) = new_statements;
    } else {
        STATEMENTS_NEXT(last_statements) = new_statements;
    }
}

/* Rewrite all for's */
node *FTWfor(node *arg_node, info *arg_info){
    DBUG_ENTER("FTWfor");

    FOR_STARTEXPR(arg_node) = TRAVdo(FOR_STARTEXPR(arg_node), arg_info);
    FOR_ENDEXPR(arg_node) = TRAVdo(FOR_ENDEXPR(arg_node), arg_info);
    FOR_STEPEXPR(arg_node) = TRAVopt(FOR_STEPEXPR(arg_node), arg_info);

    /* Create while expression */
    binop binop = BO_lt;
    node *step = FOR_STEPEXPR(arg_node);
    if (step && NODE_TYPE(step) == N_int && INT_VALUE(step) < 0) {
        binop = BO_gt;
    }

    node *while_expr = TBmakeBinop(binop, TBmakeId(STRcpy(FOR_COUNTERNAME(arg_node))),
                                  TBmakeId(STRcat(FOR_COUNTERNAME(arg_node), "_end")));

    /* Create increment statement */
    node *right_expr = TBmakeInt(1);
    if (FOR_STEPEXPR(arg_node)) {
        right_expr = COPYdoCopy(FOR_STEPEXPR(arg_node));
    }

    node *inc = TBmakeBinop(BO_add, TBmakeId(STRcpy(FOR_COUNTERNAME(arg_node))), right_expr);
    node *a_inc = TBmakeAssign(inc, TBmakeId(STRcpy(FOR_COUNTERNAME(arg_node))));

    FOR_BLOCK(arg_node) = TRAVdo(FOR_BLOCK(arg_node), arg_info);

    /* Add last statements to block */
    append_to_block(FOR_BLOCK(arg_node),  TBmakeStatements(a_inc, NULL));
    node *newBlock = COPYdoCopy(FOR_BLOCK(arg_node));

    /* Create while node, and replace for */
    arg_node = FREEdoFreeTree(arg_node);
    arg_node = TBmakeWhile(while_expr, newBlock);

    DBUG_RETURN(arg_node);
}

node *FTWstatements(node *arg_node, info *arg_info) {
    DBUG_ENTER("FTWstatements");

    node *statement = STATEMENTS_STATEMENT(arg_node);
    if (NODE_TYPE(statement) == N_for) {
        node *start_assign = TBmakeAssign(COPYdoCopy(FOR_STARTEXPR(statement)),
                                          TBmakeId(STRcpy(FOR_COUNTERNAME(statement))));
        node *end_assign = TBmakeAssign(COPYdoCopy(FOR_ENDEXPR(statement)),
                                        TBmakeId(STRcat(FOR_COUNTERNAME(statement), "_end")));
        node *end_statements = TBmakeStatements(end_assign, arg_node);
        node *start_statements = TBmakeStatements(start_assign, end_statements);

        STATEMENTS_STATEMENT(arg_node) = TRAVdo(STATEMENTS_STATEMENT(arg_node), arg_info);
        STATEMENTS_NEXT(arg_node) = TRAVopt(STATEMENTS_NEXT(arg_node), arg_info);
        DBUG_RETURN(start_statements);
    }

    STATEMENTS_STATEMENT(arg_node) = TRAVdo(STATEMENTS_STATEMENT(arg_node), arg_info);
    STATEMENTS_NEXT(arg_node) = TRAVopt(STATEMENTS_NEXT(arg_node), arg_info);

    DBUG_RETURN(arg_node);
}

/* Start function */
node *FTWmain(node *syntaxtree) {

    DBUG_ENTER("FTWmain");

    TRAVpush(TR_ftw);
    syntaxtree = TRAVdo(syntaxtree, NULL);
    TRAVpop();

    DBUG_RETURN(syntaxtree);
}
