#ifndef _GEN_BYTE_CODE_H_
#define _GEN_BYTE_CODE_H_
#include "types.h"

extern void print_indentation(int indentation);

/* Basictypes */
extern node *GBCint(node *arg_node, info *arg_info);
extern node *GBCfloat(node *arg_node, info *arg_info);
extern node *GBCbool(node *arg_node, info *arg_info);

/* Civic */
extern node *GBCvardec(node *arg_node, info *arg_info);
extern node *GBCprogram(node *arg_node, info *arg_info);
extern node *GBCstatements(node *arg_node, info *arg_info);
extern node *GBCsymboltableentry(node *arg_node, info *arg_info);
extern node *GBCblock(node *arg_node, info *arg_info);
extern node *GBCid(node *arg_node, info *arg_info);
extern node *GBCfundef(node *arg_node, info *arg_info);
extern node *GBCfunbody(node *arg_node, info *arg_info);

/* Statements */
extern node *GBCassign(node *arg_node, info *arg_info);
extern node *GBCfuncall(node *arg_node, info *arg_info);
extern node *GBCifelse(node *arg_node, info *arg_info);
extern node *GBCwhile(node *arg_node, info *arg_info);
extern node *GBCdowhile(node *arg_node, info *arg_info);
extern node *GBCreturn(node *arg_node, info *arg_info);
extern node *GBCternary(node *arg_node, info *arg_info);

/* Expressions */
extern node *GBCcast(node *arg_node, info *arg_info);
extern node *GBCbinop(node *arg_node, info *arg_info);
extern node *GBCmonop(node *arg_node, info *arg_info);

extern node *GBCdoGenByteCode( node *syntaxtree);

#endif
