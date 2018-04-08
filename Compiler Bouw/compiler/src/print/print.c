/* Handles printing the AST */

#include "print.h"
#include "traverse.h"
#include "tree_basic.h"
#include "dbug.h"
#include "memory.h"
#include "globals.h"

#include "general_functions.h"


/*
 * INFO structure
 */
struct INFO {
  bool firsterror;
  int indentation;
};

#define INFO_FIRSTERROR(n) ((n)->firsterror)
#define INFO_INDENTATION(n) ((n)->indentation)

static info *MakeInfo() {
  info *result;

  result = MEMmalloc(sizeof(info));

  INFO_FIRSTERROR(result) = FALSE;
  INFO_INDENTATION(result) = 0;

  return result;
}

static info *FreeInfo(info *info) {
  info = MEMfree(info);

  return info;
}

void print_indentation(int indentation) {
    for (int i = 0; i < indentation; i++) {
        printf("    ");
    } 
}

/* *** PROGRAM *** */
node *PRTprogram(node *arg_node, info *arg_info) {
  DBUG_ENTER("PRTprogram");
  PROGRAM_DECLARATIONS(arg_node) = TRAVdo(PROGRAM_DECLARATIONS(arg_node), arg_info);
  printf("\n");
  PROGRAM_SYMBOLTABLE(arg_node) = TRAVopt(PROGRAM_SYMBOLTABLE(arg_node), arg_info);
  DBUG_RETURN(arg_node);
}

/* *** DECLARATIONS *** */
node *PRTdeclarations(node *arg_node, info *arg_info) {
  DBUG_ENTER("PRTdeclarations");
  DECLARATIONS_DECLARATION(arg_node) = TRAVdo(DECLARATIONS_DECLARATION(arg_node), arg_info);
  DECLARATIONS_NEXT(arg_node) = TRAVopt(DECLARATIONS_NEXT(arg_node), arg_info);
  DBUG_RETURN(arg_node);
}

node *PRTglobaldec(node *arg_node, info *arg_info) {
  DBUG_ENTER("PRTglobaldec");
  printf("extern ");
  printf("%s ", get_type(GLOBALDEC_TYPE(arg_node)));
  printf("%s", GLOBALDEC_NAME(arg_node));
  printf(";\n");
  DBUG_RETURN(arg_node);
}

node *PRTfundec(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTfundec");
  printf("extern ");
  FUNDEC_FUNHEADER(arg_node) = TRAVdo(FUNDEC_FUNHEADER(arg_node), arg_info);
  printf(";\n");
  DBUG_RETURN(arg_node);
}

node *PRTglobaldef(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTglobaldef");
  if (GLOBALDEF_EXPORT(arg_node)) {
    printf("export ");
  }
  printf("%s ", get_type(GLOBALDEF_TYPE(arg_node)));
  printf("%s", GLOBALDEF_NAME(arg_node));
  if (GLOBALDEF_EXPR(arg_node) != NULL) {
    printf(" = ");
    GLOBALDEF_EXPR(arg_node) = TRAVdo(GLOBALDEF_EXPR(arg_node), arg_info);
  }
  printf(";\n");

  DBUG_RETURN(arg_node);
}

/* *** FUNCTIONS *** */
node *PRTfundef(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTfundef");

  print_indentation(INFO_INDENTATION(arg_info));
  if (FUNDEF_EXPORT(arg_node)) {
    printf("export ");
  }
  FUNDEF_FUNHEADER(arg_node) = TRAVdo(FUNDEF_FUNHEADER(arg_node), arg_info);
  FUNDEF_FUNBODY(arg_node) = TRAVdo(FUNDEF_FUNBODY(arg_node), arg_info);
  if (FUNDEF_SYMBOLTABLE(arg_node) != NULL) {
    FUNDEF_SYMBOLTABLE(arg_node) = TRAVdo(FUNDEF_SYMBOLTABLE(arg_node), arg_info);
  }
  printf("\n");
  FUNDEF_NEXT(arg_node) = TRAVopt(FUNDEF_NEXT(arg_node), arg_info);

  DBUG_RETURN(arg_node);
}

node *PRTfunheader(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTfunheader");

  printf("%s ", get_type(FUNHEADER_RETTYPE(arg_node)));
  printf("%s", FUNHEADER_NAME(arg_node));
  printf("(");
  FUNHEADER_PARAMS(arg_node) = TRAVopt(FUNHEADER_PARAMS(arg_node), arg_info);
  printf(")");
  DBUG_RETURN(arg_node);
}

node *PRTparam(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTparam");

  printf("%s ", get_type(PARAM_TYPE(arg_node)));
  printf("%s", PARAM_NAME(arg_node));
  if (PARAM_NEXT(arg_node) != NULL) {
    printf(", ");
    PARAM_NEXT(arg_node) = TRAVdo(PARAM_NEXT(arg_node), arg_info);
  }

  DBUG_RETURN(arg_node);
}

node *PRTfunbody(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTfunbody");
  printf(" {\n");
  INFO_INDENTATION(arg_info)++;
  FUNBODY_LOCALFUNVARS(arg_node) = TRAVopt(FUNBODY_LOCALFUNVARS(arg_node), arg_info);
  FUNBODY_STATEMENTS(arg_node) = TRAVopt(FUNBODY_STATEMENTS(arg_node), arg_info);
  INFO_INDENTATION(arg_info)--;
  print_indentation(INFO_INDENTATION(arg_info));
  printf("}");

  DBUG_RETURN(arg_node);
}

/* *** VARDEC *** */
node *PRTvardec(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTvardec");

  print_indentation(INFO_INDENTATION(arg_info));
  printf("%s ", get_type(VARDEC_TYPE(arg_node)));
  printf("%s", VARDEC_NAME(arg_node));
  if (VARDEC_EXPR(arg_node) != NULL) {
    printf(" = ");
    VARDEC_EXPR(arg_node) = TRAVdo(VARDEC_EXPR(arg_node), arg_info);
  }
  printf(";\n");

  VARDEC_NEXT(arg_node) = TRAVopt(VARDEC_NEXT(arg_node), arg_info);

  DBUG_RETURN(arg_node);
}

/* *** STATEMENTS *** */
node *PRTstatements(node *arg_node, info *arg_info) {
  DBUG_ENTER("PRTstatements");

  print_indentation(INFO_INDENTATION(arg_info));
  STATEMENTS_STATEMENT(arg_node) = TRAVopt(STATEMENTS_STATEMENT(arg_node), arg_info);
  if (NODE_TYPE(STATEMENTS_STATEMENT(arg_node)) == N_assign
      || NODE_TYPE(STATEMENTS_STATEMENT(arg_node)) == N_funcall
      || NODE_TYPE(STATEMENTS_STATEMENT(arg_node)) == N_return) {
        printf(";");
      }
  printf("\n");
  STATEMENTS_NEXT(arg_node) = TRAVopt(STATEMENTS_NEXT(arg_node), arg_info);

  DBUG_RETURN(arg_node);
}

node *PRTassign(node *arg_node, info *arg_info) {
  DBUG_ENTER("PRTassign");
  printf("%s", ID_NAME(ASSIGN_ID(arg_node)));
  printf(" = ");
  ASSIGN_EXPR(arg_node) = TRAVdo(ASSIGN_EXPR(arg_node), arg_info);
  DBUG_RETURN(arg_node);
}

node *PRTfuncall(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTfuncall");
  printf("%s", FUNCALL_NAME(arg_node));
  printf("(");
  FUNCALL_PARAMS(arg_node) = TRAVopt(FUNCALL_PARAMS(arg_node), arg_info);
  printf(")");
  DBUG_RETURN(arg_node);
}

node *PRTifelse(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTif");
  printf("if (");
  IFELSE_IFEXPR(arg_node) = TRAVdo(IFELSE_IFEXPR(arg_node), arg_info);
  printf(") ");
  IFELSE_IFBLOCK(arg_node) = TRAVdo(IFELSE_IFBLOCK(arg_node), arg_info);
  if (IFELSE_ELSEBLOCK(arg_node) != NULL) {
    printf(" else ");
    IFELSE_ELSEBLOCK(arg_node) = TRAVdo(IFELSE_ELSEBLOCK(arg_node), arg_info);
  }

  DBUG_RETURN(arg_node);
}

node *PRTfor(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTfor");
  printf("for (int ");
  printf("%s", FOR_COUNTERNAME(arg_node));
  printf(" = ");
  FOR_STARTEXPR(arg_node) = TRAVdo(FOR_STARTEXPR(arg_node), arg_info);
  printf(", ");
  FOR_ENDEXPR(arg_node) = TRAVdo(FOR_ENDEXPR(arg_node), arg_info);
  if (FOR_STEPEXPR(arg_node) != NULL) {
    printf(", ");
    FOR_STEPEXPR(arg_node) = TRAVdo(FOR_STEPEXPR(arg_node), arg_info);
  }
  printf(") ");
  FOR_BLOCK(arg_node) = TRAVdo(FOR_BLOCK(arg_node), arg_info);
  DBUG_RETURN(arg_node);
}

node *PRTwhile(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTwhile");
  printf("while (");
  WHILE_EXPR(arg_node) = TRAVdo(WHILE_EXPR(arg_node), arg_info);
  printf(") ");
  WHILE_BLOCK(arg_node) = TRAVdo(WHILE_BLOCK(arg_node), arg_info);
  DBUG_RETURN(arg_node);
}

node *PRTdowhile(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTdowhile");
  printf("do ");
  DOWHILE_BLOCK(arg_node) = TRAVdo(DOWHILE_BLOCK(arg_node), arg_info);
  printf(" while (");
  DOWHILE_EXPR(arg_node) = TRAVdo(DOWHILE_EXPR(arg_node), arg_info);
  printf(");\n");

  DBUG_RETURN(arg_node);
}

node *PRTblock(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTblock");
  printf("{\n");
  INFO_INDENTATION(arg_info)++;
  BLOCK_STATEMENTS(arg_node) = TRAVopt(BLOCK_STATEMENTS(arg_node), arg_info);
  INFO_INDENTATION(arg_info)--;
  print_indentation(INFO_INDENTATION(arg_info));
  printf("}");

  DBUG_RETURN(arg_node);
}

node *PRTreturn(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTreturn");
  printf("return ");
  RETURN_RETVALUE(arg_node) = TRAVopt(RETURN_RETVALUE(arg_node), arg_info);
  DBUG_RETURN(arg_node);
}

/* *** EXPRESSIONS *** */
node *PRTexprs(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTexprs");
  EXPRS_EXPR(arg_node) = TRAVdo(EXPRS_EXPR(arg_node), arg_info);
  if (EXPRS_NEXT(arg_node) != NULL) {
    printf(", ");
    EXPRS_NEXT(arg_node) = TRAVdo(EXPRS_NEXT(arg_node), arg_info);
  }
  DBUG_RETURN(arg_node);
}

node *PRTmonop(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTmonop");
  printf("(%s", get_monop(MONOP_OP(arg_node)));
  MONOP_EXPR(arg_node) = TRAVdo(MONOP_EXPR(arg_node), arg_info);
  printf(")");
  DBUG_RETURN(arg_node);
}

node *PRTbinop(node *arg_node, info *arg_info) {
  DBUG_ENTER("PRTbinop");
  printf("(");
  BINOP_LEFT(arg_node) = TRAVdo(BINOP_LEFT(arg_node), arg_info);
  printf(" %s ", get_binop(BINOP_OP(arg_node)));
  BINOP_RIGHT(arg_node) = TRAVdo(BINOP_RIGHT(arg_node), arg_info);
  printf(")");

  DBUG_RETURN(arg_node);
}

node *PRTcast(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTcast");
  printf("(%s) ", get_type(CAST_TYPE(arg_node)));
  CAST_EXPR(arg_node) = TRAVdo(CAST_EXPR(arg_node), arg_info);
  DBUG_RETURN(arg_node);
}

node *PRTid(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTid");
  printf("%s", ID_NAME(arg_node));
  DBUG_RETURN(arg_node);
}

node *PRTint(node *arg_node, info *arg_info) {
  DBUG_ENTER("PRTint");
  printf("%i", INT_VALUE(arg_node));
  DBUG_RETURN(arg_node);
}

node *PRTfloat(node *arg_node, info *arg_info) {
  DBUG_ENTER("PRTfloat");
  printf("%f", FLOAT_VALUE(arg_node));
  DBUG_RETURN(arg_node);
}

node *PRTbool(node *arg_node, info *arg_info) {
  DBUG_ENTER("PRTbool");
  if (BOOL_VALUE(arg_node)) {
    printf("true");
  }
  else {
    printf("false");
  }
  DBUG_RETURN(arg_node);
}

/* *** SYMBOL TABLE *** */
node *PRTsymboltable(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTsymboltable");
  printf(" --> SymbolTable:  ");
  SYMBOLTABLE_ENTRIES(arg_node) = TRAVopt(SYMBOLTABLE_ENTRIES(arg_node), arg_info);
  printf("\n");
  DBUG_RETURN(arg_node);
}

node *PRTsymboltableentry(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTsymboltableentries");
  printf("[%s %s", get_type(SYMBOLTABLEENTRY_TYPE(arg_node)), SYMBOLTABLEENTRY_NAME(arg_node));

  if (SYMBOLTABLEENTRY_PARAMS(arg_node) != NULL) {
    printf(" (");
    SYMBOLTABLEENTRY_PARAMS(arg_node) = TRAVdo(SYMBOLTABLEENTRY_PARAMS(arg_node), arg_info);
    printf(")");
  }
  printf("]");
  if (SYMBOLTABLEENTRY_NEXT(arg_node) != NULL) {
    printf(", ");
    SYMBOLTABLEENTRY_NEXT(arg_node) = TRAVdo(SYMBOLTABLEENTRY_NEXT(arg_node), arg_info);
  }

  DBUG_RETURN(arg_node);
}

node *PRTternary(node *arg_node, info *arg_info){
  DBUG_ENTER("PRTternary");

  printf("(");
  TERNARY_PRED(arg_node) = TRAVdo(TERNARY_PRED(arg_node), arg_info);
  printf("?");
  TERNARY_THEN(arg_node) = TRAVdo(TERNARY_THEN(arg_node), arg_info);
  printf(":");
  TERNARY_ELSE(arg_node) = TRAVdo(TERNARY_ELSE(arg_node), arg_info);
  printf(")");

  DBUG_RETURN(arg_node);
}

/* *** ERRORS *** */
node *PRTerror(node *arg_node, info *arg_info) {
  bool first_error;

  DBUG_ENTER("PRTerror");

  if (NODE_ERROR(arg_node) != NULL) {
    NODE_ERROR(arg_node) = TRAVdo(NODE_ERROR(arg_node), arg_info);
  }

  first_error = INFO_FIRSTERROR(arg_info);

  if((global.outfile != NULL)
      && (ERROR_ANYPHASE(arg_node) == global.compiler_anyphase)) {

    if (first_error) {
      printf("\n/******* BEGIN TREE CORRUPTION ********\n");
      INFO_FIRSTERROR(arg_info) = FALSE;
    }

    printf("%s\n", ERROR_MESSAGE(arg_node));

    if (ERROR_NEXT(arg_node) != NULL) {
      TRAVopt(ERROR_NEXT(arg_node), arg_info);
    }

    if (first_error) {
      printf("********  END TREE CORRUPTION  *******/\n");
      INFO_FIRSTERROR(arg_info) = TRUE;
    }
  }

  DBUG_RETURN(arg_node);
}



/** <!-- ****************************************************************** -->
 * @brief Prints the given syntaxtree
 *
 * @param syntaxtree a node structure
 *
 * @return the unchanged nodestructure
 ******************************************************************************/

node *PRTdoPrint( node *syntaxtree) {
  info *info;

  DBUG_ENTER("PRTdoPrint");

  DBUG_ASSERT((syntaxtree!= NULL), "PRTdoPrint called with empty syntaxtree");

  printf("\n\n------------------------------\n\n");

  info = MakeInfo();

  TRAVpush(TR_prt);

  syntaxtree = TRAVdo(syntaxtree, info);

  TRAVpop();

  info = FreeInfo(info);

  printf("\n------------------------------\n\n");

  DBUG_RETURN(syntaxtree);
}

/**
 * @}
 */
