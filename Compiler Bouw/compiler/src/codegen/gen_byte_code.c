#include "gen_byte_code.h"

#include "types.h"
#include "tree_basic.h"
#include "traverse.h"
#include "dbug.h"

#include "memory.h"
#include "ctinfo.h"
#include "str.h"
#include "symbol_table.h"
#include "table.h"

#include "print.h"
#include "globals.h"
#include "general_functions.h"

struct INFO {
    basictype type;
    node *current_st;
    int branch_count;

    bool localfuns;

    table *global;
    table *constant;
    table *function_i;
    table *function_e;
    table *variable_i;
    table *variable_e;

    FILE *out_file;
};

#define TABLE_GLOBAL(n)  ((n)->global)
#define TABLE_CONSTANT(n)  ((n)->constant)
#define TABLE_FUNC_I(n)  ((n)->function_i)
#define TABLE_FUNC_E(n)  ((n)->function_e)
#define TABLE_VAR_I(n)  ((n)->variable_i)
#define TABLE_VAR_E(n)  ((n)->variable_e)
#define INFO_FILE(n)  ((n)->out_file)

#define CURRENT_SYMBOLTABLE(n)  ((n)->current_st)
#define INFO_TYPE(n)  ((n)->type)
#define INFO_BRANCH_COUNT(n) ((n)->branch_count)
#define INFO_LOCALFUNS(n) ((n)->localfuns)

static info *MakeInfo(void) {
    info *result;
    DBUG_ENTER("MakeInfo");
    result = (info *)MEMmalloc(sizeof(info));

    INFO_TYPE(result) = BT_unknown;
    CURRENT_SYMBOLTABLE(result) = NULL;
    INFO_BRANCH_COUNT(result) = 1;
    INFO_LOCALFUNS(result) = FALSE;

    TABLE_GLOBAL(result) = init_table();
    TABLE_CONSTANT(result) = init_table();
    TABLE_FUNC_I(result) = init_table();
    TABLE_FUNC_E(result) = init_table();
    TABLE_VAR_I(result) = init_table();
    TABLE_VAR_E(result) = init_table();

    DBUG_RETURN(result);
}

static info *FreeInfo(info *info) {
    DBUG_ENTER("FreeInfo");

    free_table(TABLE_GLOBAL(info));
    free_table(TABLE_CONSTANT(info));
    free_table(TABLE_FUNC_I(info));
    free_table(TABLE_FUNC_E(info));
    free_table(TABLE_VAR_I(info));
    free_table(TABLE_VAR_E(info));

    info = MEMfree(info);
    DBUG_RETURN(info);
}

/*
 * GBC functions
 */

char *get_importvar_string(char *name, basictype type) {
    return STRcatn(4, ".importvar \"", name, "\" ", get_type(type));
}

char *get_exportvar_string(char *name, int store) {
    return STRcatn(4, ".exportvar \"", name, "\" ", itoa(store));
}

char *get_importfun_string(basictype type, char *name, char *param_string) {
    return STRcatn(6, ".importfun \"", name, "\" ", get_type(type), " ", param_string);
}

char *get_exportfun_string(basictype type, char *name, char *param_string) {
    return STRcatn(7, ".exportfun \"", name, "\" ", get_type(type), " ", param_string, name);
}

char *get_global_string(basictype type, char *name) {
    char *typestring = get_type(type);
    if (type == BT_int) {
        typestring = STRcat(typestring, " ");
    }
    return STRcatn(4, ".global ", typestring, " ", name);
}

/* Creates branch name, based on counter and type of branch */
char *create_branch_name(char *type, info *arg_info){
    char *branch_name = STRcatn(3, itoa(INFO_BRANCH_COUNT(arg_info)), "_", type);
    INFO_BRANCH_COUNT(arg_info)++;
    return branch_name;
}

char *get_param_string(node *param) {
    if (!param) {
        return "";
    }
    char *name = "";
    while (param) {
        name = STRcatn(3, name, get_type(PARAM_TYPE(param)), " ");
        param = PARAM_NEXT(param);
    }
    return name;
}

/*
 * Expressions
 */

/* Binairy operation */
node *GBCbinop(node *arg_node, info *arg_info) {
    DBUG_ENTER("GBCbinop");

    BINOP_LEFT(arg_node) = TRAVdo(BINOP_LEFT(arg_node), arg_info);
    BINOP_RIGHT(arg_node) = TRAVdo(BINOP_RIGHT(arg_node), arg_info);
    binop op = BINOP_OP(arg_node);
    basictype type = INFO_TYPE(arg_info);

    char *command = STRcat(get_type_prefix(type), get_binop_command(op));
    fprintf(INFO_FILE(arg_info), "    %s\n", command);

    if (is_relop(BINOP_OP(arg_node))) {
        INFO_TYPE(arg_info) = BT_bool;
    }

    DBUG_RETURN(arg_node);
}

/* Mon operation */
node *GBCmonop(node *arg_node, info *arg_info) {
    DBUG_ENTER("GBCmonop");

    MONOP_EXPR(arg_node) = TRAVdo(MONOP_EXPR(arg_node), arg_info);
    monop op = MONOP_OP(arg_node);
    basictype type = INFO_TYPE(arg_info);

    char *command = STRcat(get_type_prefix(type), get_monop_command(op));

    fprintf(INFO_FILE(arg_info), "    %s\n", command);

    DBUG_RETURN(arg_node);
}

/* Cast expression */
node *GBCcast(node *arg_node, info *arg_info) {
    DBUG_ENTER("GBCcast");

    CAST_EXPR(arg_node) = TRAVdo(CAST_EXPR(arg_node), arg_info);
    basictype type = INFO_TYPE(arg_info);
    basictype cast_type = CAST_TYPE(arg_node);

    if (type != cast_type) {

        if (cast_type == BT_int) {
            fprintf(INFO_FILE(arg_info), "    f2i\n");
        } else if (cast_type == BT_float) {
            fprintf(INFO_FILE(arg_info), "    i2f\n");
        }
    }

    INFO_TYPE(arg_info) = cast_type;
    DBUG_RETURN(arg_node);
}

/* Function call */
node *GBCfuncall(node *arg_node, info *arg_info){
    DBUG_ENTER("GBCfuncall");

    char *name = FUNCALL_NAME(arg_node);

    int lookup = 0;
    bool is_global = FALSE;
    node *fun = get_from_ancestors(name, CURRENT_SYMBOLTABLE(arg_info), TRUE, &lookup, NULL, &is_global);
    lookup--;

    basictype type = SYMBOLTABLEENTRY_TYPE(fun);
    node *params_list = SYMBOLTABLEENTRY_PARAMS(fun);

    int importfun_store = in_table(TABLE_FUNC_I(arg_info), make_entry(get_importfun_string(type, name, get_param_string(params_list))));
    int params = size_of_exprs(FUNCALL_PARAMS(arg_node));

    char *instruction = "isr";
    if (is_global) {
        fprintf(INFO_FILE(arg_info), "    %sg\n", instruction);
    } else if (lookup == 0) {
        fprintf(INFO_FILE(arg_info), "    %s\n", instruction);
    } else if(lookup == -1) {
        fprintf(INFO_FILE(arg_info), "    %sl\n", instruction);
    } else if(lookup > 0) {
        fprintf(INFO_FILE(arg_info), "    %sn %d\n", instruction, lookup);
    }

    FUNCALL_PARAMS(arg_node) = TRAVopt(FUNCALL_PARAMS(arg_node), arg_info);
    if (importfun_store > -1) {
        fprintf(INFO_FILE(arg_info), "    jsre %d\n", importfun_store);
    } else {
        fprintf(INFO_FILE(arg_info), "    jsr %d %s\n", params, name);
    }

    INFO_TYPE(arg_info) = SYMBOLTABLEENTRY_TYPE(fun);
    DBUG_RETURN(arg_node);
}

/* Identifier */
node *GBCid(node *arg_node, info *arg_info) {
    DBUG_ENTER("GBCid");

    char *name = ID_NAME(arg_node);
    int lookup = 0, store = 0;
    node *entry = get_from_ancestors(name, CURRENT_SYMBOLTABLE(arg_info), FALSE, &lookup, &store, NULL);
    basictype type = SYMBOLTABLEENTRY_TYPE(entry);

    int importvar_store = in_table(TABLE_VAR_I(arg_info), make_entry(get_importvar_string(name, type)));
    int global_store = in_table(TABLE_GLOBAL(arg_info), make_entry(get_global_string(type, name)));

    char *command = STRcat(get_type_prefix(type), "load");
    if (lookup == 0) {
        if (store == 0 || store == 1 || store == 2 || store == 3) {
            fprintf(INFO_FILE(arg_info), "    %s_%d\n", command, store);
        } else {
            fprintf(INFO_FILE(arg_info), "    %s %d\n", command, store);
        }
    } else if (importvar_store > -1) {
        fprintf(INFO_FILE(arg_info), "    %se %d\n", command, importvar_store);
    } else if (global_store > -1) {
        fprintf(INFO_FILE(arg_info), "    %sg %d\n", command, global_store);
    } else if (lookup > 0) {
        fprintf(INFO_FILE(arg_info), "    %sn %d %d\n", command, lookup, store);
    }

    INFO_TYPE(arg_info) = type;
    DBUG_RETURN(arg_node);
}


/* Integer */
node *GBCint(node *arg_node, info *arg_info){
    DBUG_ENTER("GBCint");

    if(INT_VALUE(arg_node) == 0 || INT_VALUE(arg_node) == 1) {
        fprintf(INFO_FILE(arg_info), "    iloadc_%s\n", itoa(INT_VALUE(arg_node)));
    }
    else if (INT_VALUE(arg_node) == -1) {
        fprintf(INFO_FILE(arg_info), "    iloadc_m1\n");
    }
    else {
        char *name = STRcat(".const int ", itoa(INT_VALUE(arg_node)));
        int store = add_to_table(TABLE_CONSTANT(arg_info), make_entry(name));
        fprintf(INFO_FILE(arg_info), "    iloadc %d\n", store);
    }

    INFO_TYPE(arg_info) = BT_int;
    DBUG_RETURN(arg_node);
}

/* Float */
node *GBCfloat(node *arg_node, info *arg_info){
    DBUG_ENTER("GBCfloat");

    if (FLOAT_VALUE(arg_node) == 0.0) {
        fprintf(INFO_FILE(arg_info), "    floadc_0\n");
    }
    else if (FLOAT_VALUE(arg_node) == 1.0) {
        fprintf(INFO_FILE(arg_info), "    floadc_1\n");
    }
    else {
        char *name = STRcat(".const float ", ftoa(FLOAT_VALUE(arg_node)));
        int store = add_to_table(TABLE_CONSTANT(arg_info), make_entry(name));
        fprintf(INFO_FILE(arg_info), "    floadc %d\n", store);
    }

    INFO_TYPE(arg_info) = BT_float;
    DBUG_RETURN(arg_node);
}

/* Boolean */
node *GBCbool(node *arg_node, info *arg_info){
    DBUG_ENTER("GBCbool");

    if(BOOL_VALUE(arg_node)) {
        fprintf(INFO_FILE(arg_info), "    bloadc_t\n");
    } else {
        fprintf(INFO_FILE(arg_info), "    bloadc_f\n");
    }

    INFO_TYPE(arg_info) = BT_bool;
    DBUG_RETURN(arg_node);
}

/*
 * Statements
 */

/* Initial node */
node *GBCstatements(node *arg_node, info *arg_info) {
    DBUG_ENTER("GBCstatements");

    STATEMENTS_STATEMENT(arg_node) = TRAVdo(STATEMENTS_STATEMENT(arg_node), arg_info);

    node *statement = STATEMENTS_STATEMENT(arg_node);
    if (NODE_TYPE(statement) == N_funcall) {
        char *name = FUNCALL_NAME(statement);
        node *entry = get_from_ancestors(name, CURRENT_SYMBOLTABLE(arg_info), TRUE, NULL, NULL, NULL);
        basictype type = SYMBOLTABLEENTRY_TYPE(entry);

        if (type != BT_void) {
            fprintf(INFO_FILE(arg_info), "    %spop\n", get_type_prefix(type));
        }
    }

    STATEMENTS_NEXT(arg_node) = TRAVopt(STATEMENTS_NEXT(arg_node), arg_info);

    DBUG_RETURN(arg_node);
}

/* Function body */
node *GBCfunbody(node *arg_node, info *arg_info) {
    DBUG_ENTER("GBCfunbody");

    bool previous = INFO_LOCALFUNS(arg_info);
    INFO_LOCALFUNS(arg_info) = FALSE;

    node *localfunvars = FUNBODY_LOCALFUNVARS(arg_node);
    if (localfunvars && NODE_TYPE(localfunvars) != N_fundef) {
        FUNBODY_LOCALFUNVARS(arg_node) = TRAVdo(localfunvars, arg_info);
    }

    FUNBODY_STATEMENTS(arg_node) = TRAVopt(FUNBODY_STATEMENTS(arg_node), arg_info);

    INFO_LOCALFUNS(arg_info) = TRUE;
    FUNBODY_LOCALFUNVARS(arg_node) = TRAVopt(FUNBODY_LOCALFUNVARS(arg_node), arg_info);
    INFO_LOCALFUNS(arg_info) = previous;

    DBUG_RETURN(arg_node);
}

/* Variable declaration */
node *GBCvardec(node *arg_node, info *arg_info){
    DBUG_ENTER("GBCvardec");

    if (VARDEC_EXPR(arg_node) && !INFO_LOCALFUNS(arg_info)) {
        VARDEC_EXPR(arg_node) = TRAVdo(VARDEC_EXPR(arg_node), arg_info);

        char *name = VARDEC_NAME(arg_node);
        int lookup = 0, store = 0;
        node *entry = get_from_ancestors(name, CURRENT_SYMBOLTABLE(arg_info), FALSE, &lookup, &store, NULL);
        basictype type = SYMBOLTABLEENTRY_TYPE(entry);

        /* Derive and print store command */
        char *command = STRcat(get_type_prefix(type), "store");

        if (lookup > 0) {
            fprintf(INFO_FILE(arg_info), "    %sn %d %d\n", command, lookup, store);
        } else {
            fprintf(INFO_FILE(arg_info), "    %s %d\n", command, store);
        }
    }

    node *next = VARDEC_NEXT(arg_node);
    bool localfuns = INFO_LOCALFUNS(arg_info);
    if (next) {
        if (localfuns || NODE_TYPE(next) == N_vardec) {
            VARDEC_NEXT(arg_node) = TRAVdo(next, arg_info);
        }
    }

    DBUG_RETURN(arg_node);
}

/* Assignment */
node *GBCassign(node *arg_node, info *arg_info){
    DBUG_ENTER("GBCassign");

    node *expr = ASSIGN_EXPR(arg_node);
    char *name = ID_NAME(ASSIGN_ID(arg_node));
    int lookup = 0, store = 0;
    node *entry = get_from_ancestors(name, CURRENT_SYMBOLTABLE(arg_info), FALSE, &lookup, &store, NULL);
    basictype type = SYMBOLTABLEENTRY_TYPE(entry);

    /* Use optimisation when expression is binop and is assign to local var */
    if (NODE_TYPE(expr) == N_binop && lookup == 0) {
        node *left = BINOP_LEFT(expr);
        node *right = BINOP_RIGHT(expr);
        binop op = BINOP_OP(expr);
        if (
          (op == BO_add
            && ((NODE_TYPE(left) == N_id && NODE_TYPE(right) == N_int && STReq(ID_NAME(left), name))
                || (NODE_TYPE(left) == N_int && NODE_TYPE(right) == N_id && STReq(ID_NAME(right), name)))
          ) ||
          (op == BO_sub
            && (NODE_TYPE(left) == N_id && NODE_TYPE(right) == N_int && STReq(ID_NAME(left), name))
          )
        ) {
            char *instruction = NULL;
            if (op == BO_add){
                instruction = "iinc";
            }
            else if (op == BO_sub) {
                instruction = "idec";
            }

            if ((NODE_TYPE(left) == N_int && INT_VALUE(left) == 1)
             || (NODE_TYPE(right) == N_int && INT_VALUE(right) == 1)) {
                instruction = STRcatn(3, instruction, "_", itoa(1));
                fprintf(INFO_FILE(arg_info), "    %s %d\n", instruction, store);
            } else {
                char *val = NULL;
                if (NODE_TYPE(right) == N_int) {
                    val = itoa(INT_VALUE(right));
                } else if (NODE_TYPE(left) == N_int) {
                    val = itoa(INT_VALUE(left));
                }
                char *e_name = STRcat(".const int ", val);
                int index = add_to_table(TABLE_CONSTANT(arg_info), make_entry(e_name));
                fprintf(INFO_FILE(arg_info), "    %s %d %d\n", instruction, store, index);
            }
            DBUG_RETURN(arg_node);
        }
    }

    ASSIGN_EXPR(arg_node) = TRAVdo(ASSIGN_EXPR(arg_node), arg_info);

    int importvar_store = in_table(TABLE_VAR_I(arg_info), make_entry(get_importvar_string(name, type)));
    int global_store = in_table(TABLE_GLOBAL(arg_info), make_entry(get_global_string(type, name)));

    /* Derive and print store command */
    char *command = STRcat(get_type_prefix(type), "store");

    if (lookup == 0) {
        fprintf(INFO_FILE(arg_info), "    %s %d\n", command, store);
    } else if (importvar_store > -1) {
        fprintf(INFO_FILE(arg_info), "    %se %d\n", command, importvar_store);
    } else if (global_store > -1) {
        fprintf(INFO_FILE(arg_info), "    %sg %d\n", command, global_store);
    } else if (lookup > 0) {
        fprintf(INFO_FILE(arg_info), "    %sn %d %d\n", command, lookup, store);
    }

    DBUG_RETURN(arg_node);
}

/* If else statements */
node *GBCifelse(node *arg_node, info *arg_info){
    DBUG_ENTER("GBCifelse");

    bool hasElse = IFELSE_ELSEBLOCK(arg_node) != NULL;
    char *else_branch = hasElse ? create_branch_name("else", arg_info) : NULL;
    char *end_branch = create_branch_name("end", arg_info);

    node *if_expr = IFELSE_IFEXPR(arg_node);
    if (NODE_TYPE(if_expr) == N_bool) {
        if (BOOL_VALUE(if_expr)) {
            IFELSE_IFBLOCK(arg_node) = TRAVdo(IFELSE_IFBLOCK(arg_node), arg_info);
        } else {
            if (hasElse) {
                IFELSE_ELSEBLOCK(arg_node) = TRAVdo(IFELSE_ELSEBLOCK(arg_node), arg_info);
            }
        }
    } else {
        if_expr = TRAVdo(if_expr, arg_info);
        if (hasElse) {
            fprintf(INFO_FILE(arg_info), "    branch_f %s\n", else_branch);
            IFELSE_IFBLOCK(arg_node) = TRAVdo(IFELSE_IFBLOCK(arg_node), arg_info);
            fprintf(INFO_FILE(arg_info), "    jump %s\n", end_branch);

            fprintf(INFO_FILE(arg_info), "%s:\n", else_branch);
            IFELSE_ELSEBLOCK(arg_node) = TRAVdo(IFELSE_ELSEBLOCK(arg_node), arg_info);
        } else {
            fprintf(INFO_FILE(arg_info), "    branch_f %s\n", end_branch);
            IFELSE_IFBLOCK(arg_node) = TRAVdo(IFELSE_IFBLOCK(arg_node), arg_info);
        }
        fprintf(INFO_FILE(arg_info), "%s:\n", end_branch);
    }

    DBUG_RETURN(arg_node);
}

/* While loop */
node *GBCwhile(node *arg_node, info *arg_info){
    DBUG_ENTER("GBCwhile");

    char *while_branch = create_branch_name("while", arg_info);
    char *end_branch = create_branch_name("end", arg_info);

    fprintf(INFO_FILE(arg_info), "%s:\n", while_branch);

    node *expr = WHILE_EXPR(arg_node);
    if (NODE_TYPE(expr) == N_bool) {
        if (BOOL_VALUE(expr)) {
            WHILE_BLOCK(arg_node) = TRAVdo(WHILE_BLOCK(arg_node), arg_info);
            fprintf(INFO_FILE(arg_info), "    jump %s\n", while_branch);
        }
    } else {
        expr = TRAVdo(expr, arg_info);
        fprintf(INFO_FILE(arg_info), "    branch_f %s\n", end_branch);
        WHILE_BLOCK(arg_node) = TRAVdo(WHILE_BLOCK(arg_node), arg_info);
        fprintf(INFO_FILE(arg_info), "    jump %s\n", while_branch);
    }

    fprintf(INFO_FILE(arg_info), "%s:\n", end_branch);

    DBUG_RETURN(arg_node);
}

/* Dowhile loop */
node *GBCdowhile(node *arg_node, info *arg_info){
    DBUG_ENTER("GBCdowhile");

    char *branch_name = create_branch_name("dowhile", arg_info);
    fprintf(INFO_FILE(arg_info), "%s:\n", branch_name);

    DOWHILE_BLOCK(arg_node) = TRAVdo(DOWHILE_BLOCK(arg_node), arg_info);
    DOWHILE_EXPR(arg_node) = TRAVdo(DOWHILE_EXPR(arg_node), arg_info);

    fprintf(INFO_FILE(arg_info), "    branch_t %s\n", branch_name);

    DBUG_RETURN(arg_node);
}

/* Return statement */
node *GBCreturn(node *arg_node, info *arg_info) {
    DBUG_ENTER("GBCreturn");

    basictype type = BT_void;
    if (RETURN_RETVALUE(arg_node)) {
        RETURN_RETVALUE(arg_node) = TRAVdo(RETURN_RETVALUE(arg_node), arg_info);
        type = INFO_TYPE(arg_info);
    }

    char *command = STRcat(get_type_prefix(type), "return");

    fprintf(INFO_FILE(arg_info), "    %s\n", command);

    DBUG_RETURN(arg_node);
}

/* Block */
node *GBCblock(node *arg_node, info *arg_info){
    DBUG_ENTER("GBCblock");
    BLOCK_STATEMENTS(arg_node) = TRAVopt(BLOCK_STATEMENTS(arg_node), arg_info);
    DBUG_RETURN(arg_node);
}

/* Ternary */
node *GBCternary(node *arg_node, info *arg_info){
    DBUG_ENTER("GBCternary");

    TERNARY_PRED(arg_node) = TRAVdo(TERNARY_PRED(arg_node), arg_info);

    char *branch_name = create_branch_name("false_expr", arg_info);
    fprintf(INFO_FILE(arg_info), "    branch_f %s\n", branch_name);

    TERNARY_THEN(arg_node) = TRAVdo(TERNARY_THEN(arg_node), arg_info);

    char *branch_name_2 = create_branch_name("end", arg_info);
    fprintf(INFO_FILE(arg_info), "    jump %s\n", branch_name_2);

    fprintf(INFO_FILE(arg_info), "%s:\n", branch_name);
    TERNARY_ELSE(arg_node) = TRAVdo(TERNARY_ELSE(arg_node), arg_info);

    fprintf(INFO_FILE(arg_info), "%s:\n", branch_name_2);

    DBUG_RETURN(arg_node);
}

node *GBCfundef(node *arg_node, info *arg_info){
    DBUG_ENTER("GBCfundef");

    fprintf(INFO_FILE(arg_info), "\n");

    char *funname = FUNHEADER_NAME(FUNDEF_FUNHEADER(arg_node));

    fprintf(INFO_FILE(arg_info), "%s:\n", funname);

    node *previous = CURRENT_SYMBOLTABLE(arg_info);
    CURRENT_SYMBOLTABLE(arg_info) = FUNDEF_SYMBOLTABLE(arg_node);

    int size = size_of_vardecs(FUNDEF_SYMBOLTABLE(arg_node)) -
                 size_of_params(FUNHEADER_PARAMS((FUNDEF_FUNHEADER(arg_node))));

    if (size > 0) {
        fprintf(INFO_FILE(arg_info), "    esr %d\n", size);
    }

    if (FUNDEF_EXPORT(arg_node)) {
        node *header = FUNDEF_FUNHEADER(arg_node);
        basictype rettype = FUNHEADER_RETTYPE(header);
        node *params = FUNHEADER_PARAMS(header);
        add_to_table(TABLE_FUNC_E(arg_info),
                     make_entry(get_exportfun_string(rettype, funname, get_param_string(params))));
    }

    FUNDEF_FUNHEADER(arg_node) = TRAVdo(FUNDEF_FUNHEADER(arg_node), arg_info);
    FUNDEF_FUNBODY(arg_node) = TRAVdo(FUNDEF_FUNBODY(arg_node), arg_info);

    CURRENT_SYMBOLTABLE(arg_info) = previous;

    if (INFO_LOCALFUNS(arg_info)) {
        FUNDEF_NEXT(arg_node) = TRAVopt(FUNDEF_NEXT(arg_node), arg_info);
    }

    DBUG_RETURN(arg_node);
}

node *GBCsymboltableentry(node *arg_node, info *arg_info) {
    DBUG_ENTER("GBCsymboltableentry");

    char *name = SYMBOLTABLEENTRY_NAME(arg_node);
    basictype type = SYMBOLTABLEENTRY_TYPE(arg_node);
    node *params = SYMBOLTABLEENTRY_PARAMS(arg_node);
    bool is_function = SYMBOLTABLEENTRY_ISFUNCTION(arg_node);
    bool import = SYMBOLTABLEENTRY_IMPORT(arg_node);
    bool export = SYMBOLTABLEENTRY_EXPORT(arg_node);

    if (is_function) {
        if (import) {
            char *importfun_name = get_importfun_string(type, name, get_param_string(params));
            add_to_table(TABLE_FUNC_I(arg_info), make_entry(importfun_name));
        }
    } else {
        if (import) {
            add_to_table(TABLE_VAR_I(arg_info), make_entry(get_importvar_string(name, type)));
        } else {
            char *global_string = get_global_string(type, name);
            int global_store = add_to_table(TABLE_GLOBAL(arg_info), make_entry(global_string));
            if (export) {
                add_to_table(TABLE_VAR_E(arg_info), make_entry(get_exportvar_string(name, global_store)));
            }
        }
    }

    SYMBOLTABLEENTRY_NEXT(arg_node) = TRAVopt(SYMBOLTABLEENTRY_NEXT(arg_node), arg_info);

    DBUG_RETURN(arg_node);
}

/* Program node */
node *GBCprogram(node *arg_node, info *arg_info){
    DBUG_ENTER("GBCprogram");

    node *previous = CURRENT_SYMBOLTABLE(arg_info);
    CURRENT_SYMBOLTABLE(arg_info) = TRAVdo(PROGRAM_SYMBOLTABLE(arg_node), arg_info);
    PROGRAM_DECLARATIONS(arg_node) = TRAVdo(PROGRAM_DECLARATIONS(arg_node), arg_info);
    CURRENT_SYMBOLTABLE(arg_info) = previous;

    fprintf(INFO_FILE(arg_info), "\n");

    /* Print all tables */
    print_table(TABLE_CONSTANT(arg_info), INFO_FILE(arg_info));
    print_table(TABLE_FUNC_E(arg_info), INFO_FILE(arg_info));
    print_table(TABLE_VAR_E(arg_info), INFO_FILE(arg_info));
    print_global_table(TABLE_GLOBAL(arg_info), INFO_FILE(arg_info));
    print_table(TABLE_FUNC_I(arg_info), INFO_FILE(arg_info));
    print_table(TABLE_VAR_I(arg_info), INFO_FILE(arg_info));

    DBUG_RETURN(arg_node);
}

node *GBCdoGenByteCode(node *syntaxtree) {
    info *arg_info;

    DBUG_ENTER("GBCdoGenByteCode");

    arg_info = MakeInfo();

    if (global.outfile) {
        INFO_FILE(arg_info) = fopen(global.outfile, "w");
    }
    else {
        INFO_FILE(arg_info) = stdout;
    }

    TRAVpush(TR_gbc);
    syntaxtree = TRAVdo(syntaxtree, arg_info);
    TRAVpop();

    arg_info = FreeInfo(arg_info);

    DBUG_RETURN(syntaxtree);
}
