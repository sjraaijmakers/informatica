#ifndef _GENERAL_FUNCTIONS_H_
#define _GENERAL_FUNCTIONS_H_
#include "types.h"

extern node *get_last_statements(node *statements);
extern char *get_type(basictype type);
extern char *get_binop(binop op);
extern char *get_monop(monop op);
extern char *get_monop_command(monop op);
extern char *get_binop_command(binop op);
extern char *get_type_prefix(basictype type);

extern char *ftoa(float f);
extern char *itoa(int i);

extern int size_of_exprs(node *exprs);
extern int size_of_params(node *exprs);
extern int size_of_vardecs(node *st);

extern bool is_arithop(binop op);

extern bool is_relop(binop op);

#endif
