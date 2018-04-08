#ifndef _SYMBOL_TABLE_CHECK_H_
#define _SYMBOL_TABLE_CHECK_H_
#include "types.h"

node *STCdoStart(node *syntaxtree);

node *STCprogram(node *arg_node, info *arg_info);
node *STCfundef(node *arg_node, info *arg_info);

node *STCfuncall(node *arg_node, info *arg_info);

#endif
