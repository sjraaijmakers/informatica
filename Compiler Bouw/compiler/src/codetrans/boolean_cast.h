#ifndef _BOOLEAN_CAST_H_
#define _BOOLEAN_CAST_H_
#include "types.h"

extern node *BCbool(node *arg_node, info *arg_info);
extern node *BCint(node *arg_node, info *arg_info);
extern node *BCfloat(node *arg_node, info *arg_info);
extern node *BCid(node *arg_node, info *arg_info);
extern node *BCcast(node *arg_node, info *arg_info);
extern node *BCbinop(node *arg_node, info *arg_info);

extern node *BCmain( node *syntaxtree);

#endif
