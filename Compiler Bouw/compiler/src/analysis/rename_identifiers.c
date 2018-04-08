#include "rename_identifiers.h"

#include "types.h"
#include "tree_basic.h"
#include "traverse.h"
#include "dbug.h"

#include "memory.h"
#include "ctinfo.h"
#include "str.h"
#include "table.h"
#include "print.h"
#include "lookup_table.h"
#include "gen_byte_code.h"
#include "symbol_table.h"
#include "copy.h"

#include "general_functions.h"

struct INFO {
    char *curfun;
    lut_t *localfuns;
    lut_t *vars;
    node *final_vardec;
    node *current_fundef;
    int counter;
};

#define INFO_CURFUN(n)  ((n)->curfun)
#define INFO_LOCALFUNS(n) ((n)->localfuns)
#define INFO_VARS(n) ((n)->vars)
#define INFO_FINAL_VARDEC(n) ((n)->final_vardec)
#define INFO_CURRENT_FUNDEF(n) ((n)->current_fundef)
#define INFO_BRANCH_COUNT(n) ((n)->counter)

static info *MakeInfo(void) {
    DBUG_ENTER("MakeInfo");

    info *result;
    result = (info *)MEMmalloc(sizeof(info));

    INFO_CURFUN(result) = NULL;
    INFO_LOCALFUNS(result) = LUTgenerateLut();
    INFO_VARS(result) = LUTgenerateLut();
    INFO_FINAL_VARDEC(result) = NULL;
    INFO_CURRENT_FUNDEF(result) = NULL;
    INFO_BRANCH_COUNT(result) = 0;

    DBUG_RETURN(result);
}

static info *FreeInfo(info *info) {
    DBUG_ENTER("FreeInfo");

    LUTremoveLut(INFO_LOCALFUNS(info));
    LUTremoveLut(INFO_VARS(info));
    info = MEMfree(info);

    DBUG_RETURN(info);
}

node *RIid(node *arg_node, info *arg_info) {
    DBUG_ENTER("RIid");
    char *newname = LUTsearchInLutSs(INFO_VARS(arg_info), ID_NAME(arg_node));
    if (!STReq(newname, ID_NAME(arg_node))) {
        ID_NAME(arg_node) = STRcpy(newname);
    }
    DBUG_RETURN(arg_node);
}

node *RIvardec(node *arg_node, info *arg_info) {
    DBUG_ENTER("RIvardec");

    VARDEC_EXPR(arg_node) = TRAVopt(VARDEC_EXPR(arg_node), arg_info);

    char *oldname = VARDEC_NAME(arg_node);
    char *newname = STRcatn(3, INFO_CURFUN(arg_info), "_", oldname);
    VARDEC_NAME(arg_node) = STRcpy(newname);
    LUTupdateLutS(INFO_VARS(arg_info), oldname, STRcpy(newname), NULL);

    if (!VARDEC_NEXT(arg_node) || NODE_TYPE(VARDEC_NEXT(arg_node)) != N_vardec) {
        INFO_FINAL_VARDEC(arg_info) = arg_node;
    }

    VARDEC_NEXT(arg_node) = TRAVopt(VARDEC_NEXT(arg_node), arg_info);

    DBUG_RETURN(arg_node);
}

node *RIfor(node *arg_node, info *arg_info) {
    DBUG_ENTER("RIfor");

    /* First traverse the for-header */
    FOR_STARTEXPR(arg_node) = TRAVdo(FOR_STARTEXPR(arg_node), arg_info);
    FOR_ENDEXPR(arg_node) = TRAVdo(FOR_ENDEXPR(arg_node), arg_info);
    FOR_STEPEXPR(arg_node) = TRAVopt(FOR_STEPEXPR(arg_node), arg_info);

    /* For var names + update info */
    char *oldname = FOR_COUNTERNAME(arg_node);
    char *newname = STRcatn(4, "_for_", oldname, "_", itoa(INFO_BRANCH_COUNT(arg_info)));
    FOR_COUNTERNAME(arg_node) = STRcpy(newname);
    INFO_BRANCH_COUNT(arg_info)++;

    char *previous_for_var = LUTsearchInLutSs(INFO_VARS(arg_info), oldname);
    LUTupdateLutS(INFO_VARS(arg_info), oldname, STRcpy(newname), NULL);

    /* Traverse inner for-block */
    FOR_BLOCK(arg_node) = TRAVopt(FOR_BLOCK(arg_node), arg_info);

    /* Reset info var names */
    LUTupdateLutS(INFO_VARS(arg_info), oldname, STRcpy(previous_for_var), NULL);

    /* Add new vardec */
    node *start_vardec = TBmakeVardec(BT_int, newname, NULL, NULL);
    node *end_vardec = TBmakeVardec(BT_int, STRcat(newname, "_end"), NULL, NULL);

    node *current_fundef = INFO_CURRENT_FUNDEF(arg_info);
    node *final_vardec = INFO_FINAL_VARDEC(arg_info);

    if (final_vardec) {
        node *next = VARDEC_NEXT(final_vardec);
        VARDEC_NEXT(final_vardec) = start_vardec;
        VARDEC_NEXT(start_vardec) = end_vardec;
        VARDEC_NEXT(end_vardec) = next;
    } else {
        node *next = FUNBODY_LOCALFUNVARS(FUNDEF_FUNBODY(current_fundef));
        FUNBODY_LOCALFUNVARS(FUNDEF_FUNBODY(current_fundef)) = start_vardec;
        VARDEC_NEXT(start_vardec) = end_vardec;
        VARDEC_NEXT(end_vardec) = next;
    }

    DBUG_RETURN(arg_node);
}

node *RIfuncall(node *arg_node, info *arg_info) {
    DBUG_ENTER("RIfuncall");

    FUNCALL_NAME(arg_node) = STRcpy(LUTsearchInLutSs(INFO_LOCALFUNS(arg_info), FUNCALL_NAME(arg_node)));

    FUNCALL_PARAMS(arg_node) = TRAVopt(FUNCALL_PARAMS(arg_node), arg_info);

    DBUG_RETURN(arg_node);
}

node *RIfundef(node *arg_node, info *arg_info) {
    DBUG_ENTER("RIfundef");

    /* Set info struct vars */
    node *previous_fundef = INFO_CURRENT_FUNDEF(arg_info);
    node *previous_final_vardec = INFO_FINAL_VARDEC(arg_info);
    INFO_CURRENT_FUNDEF(arg_info) = arg_node;
    INFO_FINAL_VARDEC(arg_info) = NULL;

    /* RENAMING OF LOCAL FUNCTIONS */
    char *old_curfun = INFO_CURFUN(arg_info);
    if (old_curfun) {
        INFO_CURFUN(arg_info) = FUNHEADER_NAME(FUNDEF_FUNHEADER(arg_node));
    } else {
        INFO_CURFUN(arg_info) = STRcat("__", FUNHEADER_NAME(FUNDEF_FUNHEADER(arg_node)));
    }

    lut_t *previous_localfuns = LUTduplicateLut(INFO_LOCALFUNS(arg_info));
    lut_t *previous_vars = LUTduplicateLut(INFO_VARS(arg_info));

    node *localfunvar = FUNBODY_LOCALFUNVARS(FUNDEF_FUNBODY(arg_node));
    while (localfunvar) {
        if (NODE_TYPE(localfunvar) == N_vardec) {
            localfunvar = VARDEC_NEXT(localfunvar);
        } else {
            char *funname = FUNHEADER_NAME(FUNDEF_FUNHEADER(localfunvar));
            char *new_funname = STRcatn(3, INFO_CURFUN(arg_info), "_", funname);
            FUNHEADER_NAME(FUNDEF_FUNHEADER(localfunvar)) = STRcpy(new_funname);
            LUTupdateLutS(INFO_LOCALFUNS(arg_info), funname, STRcpy(new_funname), NULL);

            localfunvar = FUNDEF_NEXT(localfunvar);
        }
    }

    /* Traversals */
    FUNDEF_FUNHEADER(arg_node) = TRAVdo(FUNDEF_FUNHEADER(arg_node), arg_info);
    FUNDEF_FUNBODY(arg_node) = TRAVdo(FUNDEF_FUNBODY(arg_node), arg_info);

    /* Reset info struct vars */
    INFO_CURFUN(arg_info) = old_curfun;
    LUTremoveLut(INFO_LOCALFUNS(arg_info));
    INFO_LOCALFUNS(arg_info) = previous_localfuns;
    LUTremoveLut(INFO_VARS(arg_info));
    INFO_VARS(arg_info) = previous_vars;
    INFO_CURRENT_FUNDEF(arg_info) = previous_fundef;
    INFO_FINAL_VARDEC(arg_info) = previous_final_vardec;

    /* Traverse next */
    FUNDEF_NEXT(arg_node) = TRAVopt(FUNDEF_NEXT(arg_node), arg_info);

    DBUG_RETURN(arg_node);
}

node *RIdoRename(node *syntaxtree) {
    DBUG_ENTER("RIdoRename");

    info *arg_info = MakeInfo();

    TRAVpush(TR_ri);
    syntaxtree = TRAVdo(syntaxtree, arg_info);
    TRAVpop();

    arg_info = FreeInfo(arg_info);

    DBUG_RETURN(syntaxtree);
}
