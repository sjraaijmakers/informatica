/* Puts return at end of every fundef */

#include "boolean_cast.h"

#include "types.h"
#include "tree_basic.h"
#include "traverse.h"
#include "dbug.h"

#include "free.h"
#include "copy.h"
#include "str.h"

#include "memory.h"
#include "ctinfo.h"
#include "symbol_table.h"

#include "general_functions.h"

node *ERfundef(node *arg_node, info *arg_info) {
    DBUG_ENTER("ERfundef");

    FUNDEF_FUNBODY(arg_node) = TRAVdo(FUNDEF_FUNBODY(arg_node), arg_info);

    node *header = FUNDEF_FUNHEADER(arg_node);
    basictype rettype = FUNHEADER_RETTYPE(header);
    char *name = FUNHEADER_NAME(header);

    node *final = get_last_statements(FUNBODY_STATEMENTS(FUNDEF_FUNBODY(arg_node)));

    if ((final && NODE_TYPE(STATEMENTS_STATEMENT(final)) != N_return) || !final) {
        if (rettype == BT_void) {
            node *new_return = TBmakeReturn(NULL);
            if (final) {
                STATEMENTS_NEXT(final) = TBmakeStatements(new_return, NULL);
            } else {
                final = TBmakeStatements(new_return, NULL);
                FUNBODY_STATEMENTS(FUNDEF_FUNBODY(arg_node)) = final;
            }
        } else {
            CTIerror("Function \"%s\" must end with a return of type %s\n", name, get_type(rettype));
        }
    }

    FUNDEF_NEXT(arg_node) = TRAVopt(FUNDEF_NEXT(arg_node), arg_info);

    DBUG_RETURN(arg_node);
}

node *ERdoEnforce(node *syntaxtree) {
    DBUG_ENTER("ERdoEnforce");

    TRAVpush(TR_er);
    syntaxtree = TRAVdo(syntaxtree, NULL);
    TRAVpop();

    DBUG_RETURN(syntaxtree);
}
