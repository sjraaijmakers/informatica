#ifndef _ARITHMETIC_OPTIMISATION_H_
#define _ARITHMETIC_OPTIMISATION_H_
#include "types.h"

node *AOcast(node *arg_node, info *arg_info);
node *AObinop(node *arg_node, info *arg_info);
node *AOmonop(node *arg_node, info *arg_info);
node *AOternary(node *arg_node, info *arg_info);
node *AOdoOptimiseArithmetics(node *syntaxtree);

#endif
