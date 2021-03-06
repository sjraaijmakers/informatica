
/**
 * @file check_node.c
 *
 * Functions needed by chkm traversal.
 * 
 * THIS FILE HAS BEEN GENERATED USING 
 * $Id: check_node.c.xsl 15657 2007-11-13 13:57:30Z cg $.
 * DO NOT EDIT THIS FILE AS MIGHT BE CHANGED IN A LATER VERSION.
 *
 * ALL CHANGES MADE TO THIS FILE WILL BE OVERWRITTEN!
 *
 */

/**
 * @defgroup check Touch all Tree Functions to catch every node, son and attribute
 *
 * Functions needed by free traversal.
 *
 * @{
 */


#include "check_node.h"
#include "check_attribs.h"
#include "tree_basic.h"
#include "traverse.h"
#include "dbug.h"

#define CHKMTRAV( node, info) (node != NULL) ? TRAVdo( node, info) : node
#define CHKMCOND( node, info)                                    \
    ? CHKMTRAV( node, info)                                      \
    : (node)



/*******************************************************************************
 *
 * @fn CHKMpostfun
 *
 * This is the postfun function of the CHKM Traversal  
 *
 ******************************************************************************/
node *
CHKMpostfun (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMpostfun");

  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMassign
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node Assign node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMassign (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMassign");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  ASSIGN_EXPR (arg_node) = CHKMTRAV (ASSIGN_EXPR (arg_node), arg_info);
  ASSIGN_ID (arg_node) = CHKMTRAV (ASSIGN_ID (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMbinop
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node BinOp node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMbinop (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMbinop");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  BINOP_LEFT (arg_node) = CHKMTRAV (BINOP_LEFT (arg_node), arg_info);
  BINOP_RIGHT (arg_node) = CHKMTRAV (BINOP_RIGHT (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMblock
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node Block node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMblock (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMblock");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  BLOCK_STATEMENTS (arg_node) =
    CHKMTRAV (BLOCK_STATEMENTS (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMbool
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node Bool node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMbool (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMbool");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMcast
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node Cast node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMcast (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMcast");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  CAST_EXPR (arg_node) = CHKMTRAV (CAST_EXPR (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMdeclarations
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node Declarations node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMdeclarations (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMdeclarations");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  DECLARATIONS_NEXT (arg_node) =
    CHKMTRAV (DECLARATIONS_NEXT (arg_node), arg_info);
  DECLARATIONS_DECLARATION (arg_node) =
    CHKMTRAV (DECLARATIONS_DECLARATION (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMdowhile
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node DoWhile node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMdowhile (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMdowhile");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  DOWHILE_EXPR (arg_node) = CHKMTRAV (DOWHILE_EXPR (arg_node), arg_info);
  DOWHILE_BLOCK (arg_node) = CHKMTRAV (DOWHILE_BLOCK (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMerror
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node Error node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMerror (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMerror");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  ERROR_NEXT (arg_node) = CHKMTRAV (ERROR_NEXT (arg_node), arg_info);
  ERROR_MESSAGE (arg_node) =
    CHKMattribString (ERROR_MESSAGE (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMexprs
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node Exprs node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMexprs (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMexprs");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  EXPRS_NEXT (arg_node) = CHKMTRAV (EXPRS_NEXT (arg_node), arg_info);
  EXPRS_EXPR (arg_node) = CHKMTRAV (EXPRS_EXPR (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMfloat
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node Float node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMfloat (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMfloat");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMfor
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node For node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMfor (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMfor");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  FOR_COUNTERNAME (arg_node) =
    CHKMattribString (FOR_COUNTERNAME (arg_node), arg_info);
  FOR_STARTEXPR (arg_node) = CHKMTRAV (FOR_STARTEXPR (arg_node), arg_info);
  FOR_ENDEXPR (arg_node) = CHKMTRAV (FOR_ENDEXPR (arg_node), arg_info);
  FOR_STEPEXPR (arg_node) = CHKMTRAV (FOR_STEPEXPR (arg_node), arg_info);
  FOR_BLOCK (arg_node) = CHKMTRAV (FOR_BLOCK (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMfunbody
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node FunBody node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMfunbody (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMfunbody");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  FUNBODY_LOCALFUNVARS (arg_node) =
    CHKMTRAV (FUNBODY_LOCALFUNVARS (arg_node), arg_info);
  FUNBODY_STATEMENTS (arg_node) =
    CHKMTRAV (FUNBODY_STATEMENTS (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMfuncall
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node FunCall node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMfuncall (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMfuncall");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  FUNCALL_NAME (arg_node) =
    CHKMattribString (FUNCALL_NAME (arg_node), arg_info);
  FUNCALL_PARAMS (arg_node) = CHKMTRAV (FUNCALL_PARAMS (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMfundec
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node FunDec node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMfundec (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMfundec");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  FUNDEC_FUNHEADER (arg_node) =
    CHKMTRAV (FUNDEC_FUNHEADER (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMfundef
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node FunDef node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMfundef (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMfundef");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  FUNDEF_NEXT (arg_node) = CHKMTRAV (FUNDEF_NEXT (arg_node), arg_info);
  FUNDEF_SYMBOLTABLE (arg_node) =
    CHKMattribLink (FUNDEF_SYMBOLTABLE (arg_node), arg_info);
  FUNDEF_FUNHEADER (arg_node) =
    CHKMTRAV (FUNDEF_FUNHEADER (arg_node), arg_info);
  FUNDEF_FUNBODY (arg_node) = CHKMTRAV (FUNDEF_FUNBODY (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMfunheader
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node FunHeader node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMfunheader (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMfunheader");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  FUNHEADER_NAME (arg_node) =
    CHKMattribString (FUNHEADER_NAME (arg_node), arg_info);
  FUNHEADER_PARAMS (arg_node) =
    CHKMTRAV (FUNHEADER_PARAMS (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMglobaldec
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node GlobalDec node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMglobaldec (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMglobaldec");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  GLOBALDEC_NAME (arg_node) =
    CHKMattribString (GLOBALDEC_NAME (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMglobaldef
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node GlobalDef node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMglobaldef (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMglobaldef");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  GLOBALDEF_NAME (arg_node) =
    CHKMattribString (GLOBALDEF_NAME (arg_node), arg_info);
  GLOBALDEF_EXPR (arg_node) = CHKMTRAV (GLOBALDEF_EXPR (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMid
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node Id node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMid (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMid");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  ID_NAME (arg_node) = CHKMattribString (ID_NAME (arg_node), arg_info);
  ID_SYMBOLTABLE (arg_node) =
    CHKMattribLink (ID_SYMBOLTABLE (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMifelse
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node IfElse node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMifelse (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMifelse");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  IFELSE_IFEXPR (arg_node) = CHKMTRAV (IFELSE_IFEXPR (arg_node), arg_info);
  IFELSE_IFBLOCK (arg_node) = CHKMTRAV (IFELSE_IFBLOCK (arg_node), arg_info);
  IFELSE_ELSEBLOCK (arg_node) =
    CHKMTRAV (IFELSE_ELSEBLOCK (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMint
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node Int node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMint (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMint");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMmonop
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node MonOp node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMmonop (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMmonop");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  MONOP_EXPR (arg_node) = CHKMTRAV (MONOP_EXPR (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMparam
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node Param node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMparam (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMparam");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  PARAM_NEXT (arg_node) = CHKMTRAV (PARAM_NEXT (arg_node), arg_info);
  PARAM_NAME (arg_node) = CHKMattribString (PARAM_NAME (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMprogram
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node Program node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMprogram (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMprogram");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  PROGRAM_SYMBOLTABLE (arg_node) =
    CHKMattribLink (PROGRAM_SYMBOLTABLE (arg_node), arg_info);
  PROGRAM_DECLARATIONS (arg_node) =
    CHKMTRAV (PROGRAM_DECLARATIONS (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMreturn
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node Return node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMreturn (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMreturn");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  RETURN_RETVALUE (arg_node) =
    CHKMTRAV (RETURN_RETVALUE (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMstatements
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node Statements node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMstatements (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMstatements");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  STATEMENTS_NEXT (arg_node) =
    CHKMTRAV (STATEMENTS_NEXT (arg_node), arg_info);
  STATEMENTS_STATEMENT (arg_node) =
    CHKMTRAV (STATEMENTS_STATEMENT (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMsymboltable
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node SymbolTable node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMsymboltable (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMsymboltable");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  SYMBOLTABLE_PARENT (arg_node) =
    CHKMattribLink (SYMBOLTABLE_PARENT (arg_node), arg_info);
  SYMBOLTABLE_ENTRIES (arg_node) =
    CHKMTRAV (SYMBOLTABLE_ENTRIES (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMsymboltableentry
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node SymbolTableEntry node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMsymboltableentry (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMsymboltableentry");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  SYMBOLTABLEENTRY_NEXT (arg_node) =
    CHKMTRAV (SYMBOLTABLEENTRY_NEXT (arg_node), arg_info);
  SYMBOLTABLEENTRY_NAME (arg_node) =
    CHKMattribString (SYMBOLTABLEENTRY_NAME (arg_node), arg_info);
  SYMBOLTABLEENTRY_PARAMS (arg_node) =
    CHKMattribLink (SYMBOLTABLEENTRY_PARAMS (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMternary
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node Ternary node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMternary (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMternary");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  TERNARY_PRED (arg_node) = CHKMTRAV (TERNARY_PRED (arg_node), arg_info);
  TERNARY_THEN (arg_node) = CHKMTRAV (TERNARY_THEN (arg_node), arg_info);
  TERNARY_ELSE (arg_node) = CHKMTRAV (TERNARY_ELSE (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMvardec
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node VarDec node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMvardec (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMvardec");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  VARDEC_NEXT (arg_node) = CHKMTRAV (VARDEC_NEXT (arg_node), arg_info);
  VARDEC_NAME (arg_node) =
    CHKMattribString (VARDEC_NAME (arg_node), arg_info);
  VARDEC_EXPR (arg_node) = CHKMTRAV (VARDEC_EXPR (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/** <!--******************************************************************-->
 *
 * @fn CHKMwhile
 *
 * @brief Touched the node and its sons/attributes
 *
 * @param arg_node While node to process
 * @param arg_info pointer to info structure
 *
 * @return processed node
 *
 ***************************************************************************/
node *
CHKMwhile (node * arg_node, info * arg_info)
{
  DBUG_ENTER ("CHKMwhile");
  NODE_ERROR (arg_node) = CHKMTRAV (NODE_ERROR (arg_node), arg_info);
  WHILE_EXPR (arg_node) = CHKMTRAV (WHILE_EXPR (arg_node), arg_info);
  WHILE_BLOCK (arg_node) = CHKMTRAV (WHILE_BLOCK (arg_node), arg_info);
  DBUG_RETURN (arg_node);
}

/**
 * @}
 */
