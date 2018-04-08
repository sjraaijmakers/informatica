#ifndef _GLOBAL_INIT_H_
#define _GLOBAL_INIT_H_
#include "types.h"

extern node *GIdoStart(node *syntaxtree);
extern node *GIglobaldef(node *arg_node, info *arg_info);
extern node *GIprogram(node *arg_node, info *arg_info);

#endif