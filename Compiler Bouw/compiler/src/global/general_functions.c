/* Defines some general functions */

#include "general_functions.h"

#include "types.h"
#include "tree_basic.h"
#include "traverse.h"
#include "dbug.h"
#include "print.h"

#include "free.h"
#include "copy.h"
#include "str.h"

#include "memory.h"
#include "ctinfo.h"

/* Rewrite enum basictype to string */
char *get_type(basictype type){
    switch(type){
        case BT_void: return "void";
        case BT_bool: return "bool";
        case BT_int: return "int";
        case BT_float: return "float";
        case BT_unknown: return "unknown";
        default: return NULL;
    }
}

char *get_type_prefix(basictype type) {
    switch (type) {
        case BT_bool: return "b";
        case BT_int: return "i";
        case BT_float: return "f";
        default: return "";
    }
}

/* Rewrite binop to signs */
char *get_binop(binop op) {
    switch (op) {
        case BO_add: return "+";
        case BO_sub: return "-";
        case BO_mul: return "*";
        case BO_div: return "/";
        case BO_mod: return "%%";
        case BO_lt: return "<";
        case BO_le: return "<=";
        case BO_gt: return ">";
        case BO_ge: return ">=";
        case BO_eq: return "==";
        case BO_ne: return "!=";
        case BO_or: return "||";
        case BO_and: return "&&";
        case BO_unknown: return "unknown";
    }
    return NULL;
}

char *get_binop_command(binop op) {
    switch (op) {
        case BO_add: return "add";
        case BO_sub: return "sub";
        case BO_mul: return "mul";
        case BO_div: return "div";
        case BO_mod: return "rem";
        case BO_ne:  return "ne";
        case BO_eq:  return "eq";
        case BO_lt:  return "lt";
        case BO_le:  return "le";
        case BO_gt:  return "gt";
        case BO_ge:  return "ge";
        default: return "unk";
    }
}

/* Rewrite monop to signs */
char *get_monop(monop op) {
    switch (op) {
        case MO_not: return "!";
        case MO_neg: return "-";
        case MO_unknown: return "unknown";
        default: return NULL;
    }
}

char *get_monop_command(monop op) {
    switch (op) {
        case MO_neg: return "neg";
        case MO_not: return "not";
        default: return "unk";
    }
}

/* Find last statements */
node *get_last_statements(node *statements){
    if(!statements){
        return NULL;
    }
    node *current = statements;
    while(STATEMENTS_NEXT(current)){
        current = STATEMENTS_NEXT(current);
    }
    return current;
}


/* Integer to string */
char *itoa(int i){
    char s[200];
    sprintf(s, "%d", i);
    return STRcpy(s);
}

/* Float to string */
char *ftoa(float f) {
    char s[200];
    sprintf(s, "%f", f);
    return STRcpy(s);
}

int size_of_params(node *params){
    int tot = 0;
    node *current = params;
    while(current){
        current = PARAM_NEXT(current);
        tot++;
    }
    return tot;
}

int size_of_exprs(node *exprs){
    int tot = 0;
    node *current = exprs;
    while(current){
        current = EXPRS_NEXT(current);
        tot++;
    }
    return tot;
}

int size_of_vardecs(node *st){
    int tot = 0;
    node *current = SYMBOLTABLE_ENTRIES(st);
    while(current){
        if(!SYMBOLTABLEENTRY_ISFUNCTION(current)){
            tot++;
        }
        current = SYMBOLTABLEENTRY_NEXT(current);
    }
    return tot;
}

bool is_arithop(binop op) {
    return op == BO_add || op == BO_sub || op == BO_mul || op == BO_div || op == BO_mod;
}

bool is_relop(binop op) {
    return !is_arithop(op) && op != BO_unknown;
}
