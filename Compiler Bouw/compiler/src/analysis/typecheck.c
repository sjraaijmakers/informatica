#include "typecheck.h"

#include "types.h"
#include "tree_basic.h"
#include "traverse.h"
#include "dbug.h"
#include "symbol_table.h"

#include "free.h"
#include "str.h"

#include "memory.h"
#include "ctinfo.h"

#include "general_functions.h"

struct INFO {
    basictype type;
    basictype returntype;
    node *symboltable;
};

#define INFO_TYPE(n)  ((n)->type)
#define INFO_RETURNTYPE(n)  ((n)->returntype)
#define CURRENT_SYMBOLTABLE(n) ((n)->symboltable)

static info *MakeInfo(void) {
    DBUG_ENTER("MakeInfo");
    info *result;
    result = (info *)MEMmalloc(sizeof(info));
    INFO_TYPE(result) = BT_unknown;
    INFO_RETURNTYPE(result) = BT_unknown;
    CURRENT_SYMBOLTABLE(result) = NULL;
    DBUG_RETURN(result);
}

static info *FreeInfo(info *info) {
    DBUG_ENTER ("FreeInfo");
    info = MEMfree(info);
    DBUG_RETURN(info);
}

/* ************************************************* */

void type_error(basictype expected, basictype actual, int line, int col) {
    CTIerror("TypeError: Expected %s, but found %s at line %d, column %d", get_type(expected), get_type(actual), line, col);
}

void binop_unequal_type_error(basictype left, basictype right, binop op, int line, int col) {
    CTIerror("TypeError: Tried to apply %s to unequal types %s and %s at line %d, column %d", get_binop(op), get_type(left), get_type(right), line, col);
}

void binop_unsupported_type_error(basictype left, basictype right, binop op, int line, int col) {
    CTIerror("TypeError: Tried to apply %s to unsupported types %s and %s at line %d, column %d", get_binop(op), get_type(left), get_type(right), line, col);
}

void monoptype_error(basictype type, monop op, int line, int col) {
    CTIerror("TypeError: Tried to apply %s to type %s at line %d, column %d", get_monop(op), get_type(type), line, col);
}

void funcalltype_error(char * expected, char* actual, int line, int col) {
    CTIerror("TypeError: Expected %s, but found %s at line %d, column %d", expected, actual, line, col);
}

void casttype_error(basictype cast_type, basictype expr_type, int line, int col) {
    CTIerror("TypeError: Attempting to cast %s to %s at line %d, column %d", get_type(expr_type), get_type(cast_type), line, col);
}

void assign_for_induction_var_error(int line, int col) {
    CTIerror("Error: Attempting to assign for induction var at line %d, column %d", line, col);
}

/* ************************************************* */

node *TCint(node *arg_node, info *arg_info) {
    DBUG_ENTER("TCint");
    INFO_TYPE(arg_info) = BT_int;
    DBUG_RETURN(arg_node);
}

node *TCfloat(node *arg_node, info *arg_info) {
    DBUG_ENTER("TCfloat");
    INFO_TYPE(arg_info) = BT_float;
    DBUG_RETURN(arg_node);
}

node *TCbool(node *arg_node, info *arg_info) {
    DBUG_ENTER("TCbool");
    INFO_TYPE(arg_info) = BT_bool;
    DBUG_RETURN(arg_node);
}

node *TCid(node *arg_node, info *arg_info) {
    DBUG_ENTER("TCid");

    char *name = ID_NAME(arg_node);
    node *entry = get_from_ancestors(name, CURRENT_SYMBOLTABLE(arg_info), FALSE, NULL, NULL, NULL);
    INFO_TYPE(arg_info) = SYMBOLTABLEENTRY_TYPE(entry);

    DBUG_RETURN(arg_node);
}

node *TCfuncall(node *arg_node, info *arg_info) {
    DBUG_ENTER("TCfuncall");

    char *name = FUNCALL_NAME(arg_node);
    node *entry = get_from_ancestors(name, CURRENT_SYMBOLTABLE(arg_info), TRUE, NULL, NULL, NULL);

    node *current_param = SYMBOLTABLEENTRY_PARAMS(entry);
    char *expected = STRcat(name, "(");
    while (current_param) {
        expected = STRcat(expected, get_type(PARAM_TYPE(current_param)));
        if (PARAM_NEXT(current_param)) {
            expected = STRcat(expected, ", ");
        }
        current_param = PARAM_NEXT(current_param);
    }
    expected = STRcat(expected, ")");

    node *current_exprs = FUNCALL_PARAMS(arg_node);
    char *actual = STRcat(name, "(");
    while (current_exprs) {
        EXPRS_EXPR(current_exprs) = TRAVdo(EXPRS_EXPR(current_exprs), arg_info);
        actual = STRcat(actual, get_type(INFO_TYPE(arg_info)));
        if (EXPRS_NEXT(current_exprs)) {
            actual = STRcat(actual, ", ");
        }
        current_exprs = EXPRS_NEXT(current_exprs);
    }
    actual = STRcat(actual, ")");

    if (!STReq(actual, expected)) {
        funcalltype_error(expected, actual, NODE_LINE(arg_node), NODE_COL(arg_node));
    }

    INFO_TYPE(arg_info) = SYMBOLTABLEENTRY_TYPE(entry);

    DBUG_RETURN(arg_node);
}

node *TCcast(node *arg_node, info *arg_info) {
    DBUG_ENTER("TCcast");

    CAST_EXPR(arg_node) = TRAVdo(CAST_EXPR(arg_node), arg_info);

    if (INFO_TYPE(arg_info) == BT_void) {
        casttype_error(CAST_TYPE(arg_node), INFO_TYPE(arg_info), NODE_LINE(arg_node), NODE_COL(arg_node));
    }

    INFO_TYPE(arg_info) = CAST_TYPE(arg_node);

    DBUG_RETURN(arg_node);
}

node *TCmonop(node *arg_node, info *arg_info) {
    DBUG_ENTER("TCmonop");

    monop op = MONOP_OP(arg_node);
    MONOP_EXPR(arg_node) = TRAVdo(MONOP_EXPR(arg_node), arg_info);
    basictype type = INFO_TYPE(arg_info);

    if ((op == MO_not && type != BT_bool) || (op == MO_neg && type == BT_bool)) {
        monoptype_error(type, op, NODE_LINE(arg_node), NODE_COL(arg_node));
    }

    DBUG_RETURN(arg_node);
}

node *TCbinop(node *arg_node, info *arg_info) {
    DBUG_ENTER("TCbinop");

    binop op = BINOP_OP(arg_node);

    BINOP_LEFT(arg_node) = TRAVdo(BINOP_LEFT(arg_node), arg_info);
    basictype left_type = INFO_TYPE(arg_info);

    BINOP_RIGHT(arg_node) = TRAVdo(BINOP_RIGHT(arg_node), arg_info);
    basictype right_type = INFO_TYPE(arg_info);

    if (left_type != right_type) {
        binop_unequal_type_error(left_type, right_type, op, NODE_LINE(arg_node), NODE_COL(arg_node));
    }

    if ((op == BO_sub || op == BO_div || op == BO_lt || op == BO_le || op == BO_gt || op == BO_ge)
     && right_type == BT_bool) {
         binop_unsupported_type_error(left_type, right_type, op, NODE_LINE(arg_node), NODE_COL(arg_node));
    }

    if (op == BO_mod && right_type != BT_int) {
        binop_unsupported_type_error(left_type, right_type, op, NODE_LINE(arg_node), NODE_COL(arg_node));
    }

   if (is_relop(BINOP_OP(arg_node))) {
        INFO_TYPE(arg_info) = BT_bool;
    }

    DBUG_RETURN(arg_node);
}

node *TCternary(node *arg_node, info *arg_info) {
    DBUG_ENTER("TCternary");

    TERNARY_PRED(arg_node) = TRAVdo(TERNARY_PRED(arg_node), arg_info);
    basictype pred_type = INFO_TYPE(arg_info);

    TERNARY_THEN(arg_node) = TRAVdo(TERNARY_THEN(arg_node), arg_info);
    basictype then_type = INFO_TYPE(arg_info);

    TERNARY_ELSE(arg_node) = TRAVdo(TERNARY_ELSE(arg_node), arg_info);
    basictype else_type = INFO_TYPE(arg_info);

    if (pred_type != BT_bool) {
        type_error(BT_bool, pred_type, NODE_LINE(arg_node), NODE_COL(arg_node));
    } else if (then_type != BT_bool) {
        type_error(BT_bool, then_type, NODE_LINE(arg_node), NODE_COL(arg_node));
    } else if (else_type != BT_bool) {
        type_error(BT_bool, else_type, NODE_LINE(arg_node), NODE_COL(arg_node));
    }

    INFO_TYPE(arg_info) = BT_bool;

    DBUG_RETURN(arg_node);
}

/* ************************************************* */

node *TCassign(node *arg_node, info *arg_info) {
    DBUG_ENTER("TCassign");

    ASSIGN_ID(arg_node) = TRAVdo(ASSIGN_ID(arg_node), arg_info);
    basictype expected_type = INFO_TYPE(arg_info);

    ASSIGN_EXPR(arg_node) = TRAVdo(ASSIGN_EXPR(arg_node), arg_info);
    basictype actual_type = INFO_TYPE(arg_info);

    if (actual_type != expected_type) {
        type_error(expected_type, actual_type, NODE_LINE(arg_node), NODE_COL(arg_node));
    }
    if (STReq(STRsubStr(ID_NAME(ASSIGN_ID(arg_node)), 0, 4), "_for")) {
        assign_for_induction_var_error(NODE_LINE(arg_node), NODE_COL(arg_node));
    }

    DBUG_RETURN(arg_node);
}

node *TCvardec(node *arg_node, info *arg_info) {
    DBUG_ENTER("TCvardec");
    
    if (VARDEC_EXPR(arg_node)) {
        VARDEC_EXPR(arg_node) = TRAVdo(VARDEC_EXPR(arg_node), arg_info);

        basictype expected_type = VARDEC_TYPE(arg_node);
        basictype actual_type = INFO_TYPE(arg_info);
        if (actual_type != expected_type) {
            type_error(expected_type, actual_type, NODE_LINE(arg_node), NODE_COL(arg_node));
        }
    }

    VARDEC_NEXT(arg_node) = TRAVopt(VARDEC_NEXT(arg_node), arg_info);

    DBUG_RETURN(arg_node);
}

node *TCifelse(node *arg_node, info *arg_info) {
    DBUG_ENTER("TCifelse");

    IFELSE_IFEXPR(arg_node) = TRAVdo(IFELSE_IFEXPR(arg_node), arg_info);

    basictype expected_type = BT_bool;
    basictype actual_type = INFO_TYPE(arg_info);
    if (actual_type != expected_type) {
        type_error(expected_type, actual_type, NODE_LINE(arg_node), NODE_COL(arg_node));
    }

    /* Traverse child nodes */
    IFELSE_IFBLOCK(arg_node) = TRAVdo(IFELSE_IFBLOCK(arg_node), arg_info);
    IFELSE_ELSEBLOCK(arg_node) = TRAVopt(IFELSE_ELSEBLOCK(arg_node), arg_info);

    DBUG_RETURN(arg_node);
}

node *TCwhile(node *arg_node, info *arg_info) {
    DBUG_ENTER("TCwhile");

    WHILE_EXPR(arg_node) = TRAVdo(WHILE_EXPR(arg_node), arg_info);

    basictype expected_type = BT_bool;
    basictype actual_type = INFO_TYPE(arg_info);
    if (actual_type != expected_type) {
        type_error(expected_type, actual_type, NODE_LINE(arg_node), NODE_COL(arg_node));
    }

    /* Traverse child nodes */
    WHILE_BLOCK(arg_node) = TRAVdo(WHILE_BLOCK(arg_node), arg_info);

    DBUG_RETURN(arg_node);
}

node *TCdowhile(node *arg_node, info *arg_info) {
    DBUG_ENTER("TCdowhile");

    DOWHILE_EXPR(arg_node) = TRAVdo(DOWHILE_EXPR(arg_node), arg_info);

    basictype expected_type = BT_bool;
    basictype actual_type = INFO_TYPE(arg_info);
    if (actual_type != expected_type) {
        type_error(expected_type, actual_type, NODE_LINE(arg_node), NODE_COL(arg_node));
    }

    /* Traverse child nodes */
    DOWHILE_BLOCK(arg_node) = TRAVdo(DOWHILE_BLOCK(arg_node), arg_info);

    DBUG_RETURN(arg_node);
}

node *TCreturn(node *arg_node, info *arg_info) {
    DBUG_ENTER("TCreturn");

    basictype expected_type = INFO_RETURNTYPE(arg_info);
    basictype actual_type = BT_void;
    if (RETURN_RETVALUE(arg_node)) {
        RETURN_RETVALUE(arg_node) = TRAVdo(RETURN_RETVALUE(arg_node), arg_info);
        actual_type = INFO_TYPE(arg_info);
    }
    if (actual_type != expected_type) {
        type_error(expected_type, actual_type, NODE_LINE(arg_node), NODE_COL(arg_node));
    }

    DBUG_RETURN(arg_node);
}

/* ************************************************* */

node *TCfor(node *arg_node, info *arg_info) {
    DBUG_ENTER("TCfor");

    basictype expected_type = BT_int;

    FOR_STARTEXPR(arg_node) = TRAVdo(FOR_STARTEXPR(arg_node), arg_info);
    basictype actual_type = INFO_TYPE(arg_info);
    if (actual_type != expected_type) {
        type_error(expected_type, actual_type, NODE_LINE(arg_node), NODE_COL(arg_node));
    }

    FOR_ENDEXPR(arg_node) = TRAVdo(FOR_ENDEXPR(arg_node), arg_info);
    actual_type = INFO_TYPE(arg_info);
    if (actual_type != expected_type) {
        type_error(expected_type, actual_type, NODE_LINE(arg_node), NODE_COL(arg_node));
    }

    FOR_STEPEXPR(arg_node) = TRAVopt(FOR_STEPEXPR(arg_node), arg_info);
    actual_type = INFO_TYPE(arg_info);
    if (actual_type != expected_type) {
        type_error(expected_type, actual_type, NODE_LINE(arg_node), NODE_COL(arg_node));
    }

    /* Traverse other child nodes */
    FOR_BLOCK(arg_node) = TRAVdo(FOR_BLOCK(arg_node), arg_info);

    DBUG_RETURN(arg_node);
}

node *TCfundef(node *arg_node, info *arg_info) {
    DBUG_ENTER("TCfundef");

    node *previous = CURRENT_SYMBOLTABLE(arg_info);
    CURRENT_SYMBOLTABLE(arg_info) = FUNDEF_SYMBOLTABLE(arg_node);

    basictype previous_rettype = INFO_RETURNTYPE(arg_info);
    INFO_RETURNTYPE(arg_info) = FUNHEADER_RETTYPE(FUNDEF_FUNHEADER(arg_node));

    /* Traverse child nodes */
    FUNDEF_FUNHEADER(arg_node) = TRAVdo(FUNDEF_FUNHEADER(arg_node), arg_info);
    FUNDEF_FUNBODY(arg_node) = TRAVdo(FUNDEF_FUNBODY(arg_node), arg_info);
    FUNDEF_NEXT(arg_node) = TRAVopt(FUNDEF_NEXT(arg_node), arg_info);

    CURRENT_SYMBOLTABLE(arg_info) = previous;
    INFO_RETURNTYPE(arg_info) = previous_rettype;

    DBUG_RETURN(arg_node);
}

node *TCprogram(node *arg_node, info *arg_info) {
    DBUG_ENTER("TCprogram");

    node *previous = CURRENT_SYMBOLTABLE(arg_info);
    CURRENT_SYMBOLTABLE(arg_info) = PROGRAM_SYMBOLTABLE(arg_node);

    /* Traverse the declarations */
    PROGRAM_DECLARATIONS(arg_node) = TRAVdo(PROGRAM_DECLARATIONS(arg_node), arg_info);

    CURRENT_SYMBOLTABLE(arg_info) = previous;

    DBUG_RETURN(arg_node);
}

/* ************************************************* */

node *TCdoTypecheck(node *syntaxtree) {
    DBUG_ENTER("TCdoTypecheck");

    info *arg_info = MakeInfo();

    TRAVpush(TR_tc);
    syntaxtree = TRAVdo(syntaxtree, arg_info);
    TRAVpop();

    arg_info = FreeInfo(arg_info);

    DBUG_RETURN(syntaxtree);
}
