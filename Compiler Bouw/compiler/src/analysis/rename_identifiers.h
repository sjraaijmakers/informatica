#ifndef _RENAME_IDENTIFIERS_H_
#define _RENAME_IDENTIFIERS_H_
#include "types.h"

node *RIid(node *arg_node, info *arg_info);
node *RIvardec(node *arg_node, info *arg_info);
node *RIfor(node *arg_node, info *arg_info);
node *RIfuncall(node *arg_node, info *arg_info);
node *RIfundef(node *arg_node, info *arg_info);
node *RIdoRename(node *syntaxtree);

#endif
