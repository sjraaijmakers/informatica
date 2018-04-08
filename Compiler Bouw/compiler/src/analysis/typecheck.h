#ifndef _TYPECHECK_H_
#define _TYPECHECK_H_

#include "types.h"

/* ************************************************* */

void type_error(basictype expected, basictype actual, int line, int col);
void binoptype_error(basictype left, basictype right, binop op, int line, int col);

/* ************************************************* */

node *TCint(node *arg_node, info *arg_info);
node *TCfloat(node *arg_node, info *arg_info);
node *TCbool(node *arg_node, info *arg_info);
node *TCid(node *arg_node, info *arg_info);
node *TCfuncall(node *arg_node, info *arg_info);
node *TCcast(node *arg_node, info *arg_info);
node *TCmonop(node *arg_node, info *arg_info);
node *TCbinop(node *arg_node, info *arg_info);
node *TCternary(node *arg_node, info *arg_info);

/* ************************************************* */

node *TCassign(node *arg_node, info *arg_info);
node *TCvardec(node *arg_node, info *arg_info);
node *TCifelse(node *arg_node, info *arg_info);
node *TCwhile(node *arg_node, info *arg_info);
node *TCdowhile(node *arg_node, info *arg_info);
node *TCfor(node *arg_node, info *arg_info);
node *TCreturn(node *arg_node, info *arg_info);

/* ************************************************* */

node *TCfor(node *arg_node, info *arg_info);
node *TCfundef(node *arg_node, info *arg_info);
node *TCprogram(node *arg_node, info *arg_info);

/* ************************************************* */

node *TCdoTypecheck(node *syntaxtree);

#endif
