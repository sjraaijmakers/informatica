#include "arithmetic_optimisation.h"

#include "types.h"
#include "tree_basic.h"
#include "traverse.h"
#include "dbug.h"

#include "memory.h"
#include "free.h"
#include "str.h"
#include "copy.h"
#include "general_functions.h"

int int_apply_arithop(int left, int right, binop op) {
    switch (op) {
        case BO_add: return left + right;
        case BO_sub: return left - right;
        case BO_mul: return left * right;
        case BO_div: return left / right;
        case BO_mod: return left % right;
        default: return -1;
    }
}

float float_apply_arithop(float left, float right, binop op) {
    switch (op) {
        case BO_add: return left + right;
        case BO_sub: return left - right;
        case BO_mul: return left * right;
        case BO_div: return left / right;
        default: return -1.0;
    }
}

bool int_apply_relop(int left, int right, binop op) {
    switch (op) {
        case BO_lt: return left < right;
        case BO_le: return left <= right;
        case BO_gt: return left > right;
        case BO_ge: return left >= right;
        case BO_eq: return left == right;
        case BO_ne: return left != right;
        default: return FALSE;
    }
}

bool float_apply_relop(float left, float right, binop op) {
    switch (op) {
        case BO_lt: return left < right;
        case BO_le: return left <= right;
        case BO_gt: return left > right;
        case BO_ge: return left >= right;
        case BO_eq: return left == right;
        case BO_ne: return left != right;
        default: return FALSE;
    }
}

bool bool_apply_binop(bool left, bool right, binop op) {
    switch (op) {
        case BO_add:
        case BO_and: return left && right;
        case BO_mul:
        case BO_or: return left || right;
        case BO_eq: return left == right;
        case BO_ne: return left != right;
        default: return FALSE;
    }
}

node *AOcast(node *arg_node, info *arg_info){
    DBUG_ENTER("AOcast");

    CAST_EXPR(arg_node) = TRAVdo(CAST_EXPR(arg_node), arg_info);
    node *expr = CAST_EXPR(arg_node);
    basictype cast_type = CAST_TYPE(arg_node);

    node *new_node = NULL;

    if (NODE_TYPE(expr) == N_int || NODE_TYPE(expr) == N_float || NODE_TYPE(expr) == N_bool) {
        if ((NODE_TYPE(expr) == N_int && cast_type == BT_int)
         || (NODE_TYPE(expr) == N_float && cast_type == BT_float)
         || (NODE_TYPE(expr) == N_bool && cast_type == BT_bool)) {
            /* Casting to the same type is redundant */
            new_node = COPYdoCopy(expr);
        } else if (cast_type == BT_int) {
            if (NODE_TYPE(expr) == N_float) {
                new_node = TBmakeInt((int) FLOAT_VALUE(expr));
            } else if (NODE_TYPE(expr) == N_bool) {
                new_node = TBmakeInt((int) BOOL_VALUE(expr));
            }
        } else if (cast_type == BT_float) {
            if (NODE_TYPE(expr) == N_int) {
                new_node = TBmakeFloat((float) INT_VALUE(expr));
            } else if (NODE_TYPE(expr) == N_bool) {
                new_node = TBmakeFloat((float) BOOL_VALUE(expr));
            }
        } else if (cast_type == BT_bool) {
            if (NODE_TYPE(expr) == N_int) {
                new_node = TBmakeBool((bool) INT_VALUE(expr));
            } else if (NODE_TYPE(expr) == N_float) {
                new_node = TBmakeBool((bool) FLOAT_VALUE(expr));
            }
        }
    }

    if (new_node) {
        arg_node = FREEdoFreeTree(arg_node);
        arg_node = new_node;
    }

    DBUG_RETURN(arg_node);
}

node *AOmonop(node *arg_node, info *arg_info) {
    DBUG_ENTER("AOmonop");

    MONOP_EXPR(arg_node) = TRAVdo(MONOP_EXPR(arg_node), arg_info);
    node *expr = MONOP_EXPR(arg_node);
    monop op = MONOP_OP(arg_node);

    node *new_node = NULL;

    if (NODE_TYPE(expr) == N_int) {
        if (op == MO_neg) {
            new_node = TBmakeInt(-INT_VALUE(expr));
        }
    } else if (NODE_TYPE(expr) == N_float) {
        if (op == MO_neg) {
            new_node = TBmakeFloat(-FLOAT_VALUE(expr));
        }
    } else if (NODE_TYPE(expr) == N_bool) {
        if (op == MO_not) {
            new_node = TBmakeBool(!BOOL_VALUE(expr));
        }
    }

    if (new_node) {
        FREEdoFreeTree(arg_node);
        arg_node = new_node;
    }

    DBUG_RETURN(arg_node);
}

node *AObinop(node *arg_node, info *arg_info) {
    DBUG_ENTER("AObinop");

    BINOP_LEFT(arg_node) = TRAVdo(BINOP_LEFT(arg_node), arg_info);
    BINOP_RIGHT(arg_node) = TRAVdo(BINOP_RIGHT(arg_node), arg_info);
    node *left = BINOP_LEFT(arg_node);
    node *right = BINOP_RIGHT(arg_node);
    binop op = BINOP_OP(arg_node);

    node *new_node = NULL;

    if (NODE_TYPE(left) == NODE_TYPE(right)) {
        /* Arithmetic simplification */
        if (NODE_TYPE(left) == N_int) {
            if (is_arithop(op) && !(op == BO_div && INT_VALUE(right) == 0)) {
                int new_value = int_apply_arithop(INT_VALUE(left), INT_VALUE(right), op);
                new_node = TBmakeInt(new_value);
            } else if (is_relop(op)) {
                bool new_value = int_apply_relop(INT_VALUE(left), INT_VALUE(right), op);
                new_node = TBmakeBool(new_value);
            }
        } else if (NODE_TYPE(left) == N_float) {
            if (is_arithop(op) && !(op == BO_div && FLOAT_VALUE(right) == 0)) {
                float new_value = float_apply_arithop(FLOAT_VALUE(left), FLOAT_VALUE(right), op);
                new_node = TBmakeFloat(new_value);
            } else if (is_relop(op)) {
                bool new_value = float_apply_relop(FLOAT_VALUE(left), FLOAT_VALUE(right), op);
                new_node = TBmakeBool(new_value);
            }
        } else if (NODE_TYPE(left) == N_bool) {
            bool new_value = bool_apply_binop(BOOL_VALUE(left), BOOL_VALUE(right), op);
            new_node = TBmakeBool(new_value);
        } else if (NODE_TYPE(right) == N_id && STReq(ID_NAME(left), ID_NAME(right))) {
            /* ID minus the same ID = 0 */
            if (op == BO_sub) {
                new_node = TBmakeInt(0);
            }
        }
    } else if (NODE_TYPE(left) == N_id) {
        if ((NODE_TYPE(right) == N_int && INT_VALUE(right) == 0) || (NODE_TYPE(right) == N_float && FLOAT_VALUE(right) == 0.0)) {
            if (op == BO_mul) {
                /* ID * 0 = 0 */
                new_node = COPYdoCopy(right);
            } else if (op == BO_add || op == BO_sub) {
                /* ID +/- 0 = ID */
                new_node = COPYdoCopy(left);
            }
        }
    } else if (NODE_TYPE(right) == N_id) {
        if ((NODE_TYPE(left) == N_int && INT_VALUE(left) == 0) || (NODE_TYPE(left) == N_float && FLOAT_VALUE(left) == 0.0)) {
            if (op == BO_mul || op == BO_div) {
                /* 0 / or * ID = 0 */
                new_node = COPYdoCopy(left);
            } else if (op == BO_add || op == BO_sub) {
                /* 0 +/- ID = ID */
                new_node = COPYdoCopy(right);
            }
        }
    }

    if (new_node) {
        FREEdoFreeTree(arg_node);
        arg_node = new_node;
    }

    DBUG_RETURN(arg_node);
}

node *AOternary(node *arg_node, info *arg_info) {
    DBUG_ENTER("AOternary");

    TERNARY_PRED(arg_node) = TRAVdo(TERNARY_PRED(arg_node), arg_info);
    TERNARY_THEN(arg_node) = TRAVdo(TERNARY_THEN(arg_node), arg_info);
    TERNARY_ELSE(arg_node) = TRAVdo(TERNARY_ELSE(arg_node), arg_info);
    node *pred_node = TERNARY_PRED(arg_node);
    node *then_node = TERNARY_THEN(arg_node);
    node *else_node = TERNARY_ELSE(arg_node);

    node *new_node = NULL;

    if (NODE_TYPE(pred_node) == N_bool) {
        if (BOOL_VALUE(pred_node)) {
            new_node = COPYdoCopy(then_node);
        } else {
            new_node = COPYdoCopy(else_node);
        }
    }

    if (new_node) {
        FREEdoFreeTree(arg_node);
        arg_node = new_node;
    }

    DBUG_RETURN(arg_node);
}

node *AOdoOptimiseArithmetics(node *syntaxtree) {
    DBUG_ENTER("AOdoOptimiseArithmetics");

    TRAVpush(TR_ao);
    syntaxtree = TRAVdo(syntaxtree, NULL);
    TRAVpop();

    DBUG_RETURN(syntaxtree);
}
