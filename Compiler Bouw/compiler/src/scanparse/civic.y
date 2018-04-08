%{

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <memory.h>

#include "types.h"
#include "tree_basic.h"
#include "str.h"
#include "dbug.h"
#include "ctinfo.h"
#include "free.h"
#include "globals.h"

static node *parseresult = NULL;
extern int yylex();
static int yyerror(char *errname);

%}

%union {
  nodetype            nodetype;
  char               *id;
  int                 cint;
  float               cflt;
  bool                cbool;
  node               *node;
  basictype           basictype;
}

%token BRACKET_L BRACKET_R COMMA SEMICOLON CB_L CB_R SQB_L SQB_R
%token MINUS PLUS STAR SLASH PERCENT LE LT GE GT EQ NE OR AND NOT
%token LET FLOAT INT BOOL
%token IF ELSE DO WHILE FOR
%token VOID RETURN EXTERN EXPORT
%token OTHER

%nonassoc IF_NO_ELSE
%nonassoc ELSE

%right LET
%left OR
%left AND
%left EQ NE
%left LE LT GE GT
%left PLUS MINUS
%left STAR SLASH PERCENT
%right NOT CAST
%nonassoc PARENTHESISED

%token <cint> INTVAL
%token <cflt> FLOATVAL
%token <id> ID
%token <cbool> BOOLVAL

%type <cbool> export
%type <basictype> type
%type <node> program declarations declaration
%type <node> statements statement assign block
%type <node> exprs expr constant monop binop funcall
%type <node> globaldec globaldef
%type <node> funheader funbody param localfunvar
%type <node> id

%start program

%%

program:
      declarations {
        parseresult = TBmakeProgram($1);
      };

declarations:
      declaration declarations {
        $$ = TBmakeDeclarations($1, $2);
      }
      | declaration {
        $$ = TBmakeDeclarations($1, NULL);
      };

declaration:
      EXTERN funheader SEMICOLON {
        $$ = TBmakeFundec($2);
      }
      | export funheader CB_L funbody CB_R {
        $$ = TBmakeFundef($1, $2, $4, NULL);
      }
      | globaldef {
        $$ = $1;
      }
      | globaldec {
        $$ = $1;
      };

export:
      EXPORT {
        $$ = TRUE;
      }
      | %empty {
        $$ = FALSE;
      };

funheader:
      VOID ID BRACKET_L param BRACKET_R {
        $$ = TBmakeFunheader($2, BT_void, $4);
      }
      | type ID BRACKET_L param BRACKET_R {
        $$ = TBmakeFunheader($2, $1, $4);
      }
      | VOID ID BRACKET_L BRACKET_R {
        $$ = TBmakeFunheader($2, BT_void, NULL);
      }
      | type ID BRACKET_L BRACKET_R {
        $$ = TBmakeFunheader($2, $1, NULL);
      };

globaldef:
      export type ID SEMICOLON {
        $$ = TBmakeGlobaldef($1, $2, $3, NULL);
      }
      | export type ID LET expr SEMICOLON {
        $$ = TBmakeGlobaldef($1, $2, $3, $5);
      };

funbody:
      localfunvar statements {
        $$ = TBmakeFunbody($1, $2);
      }
      | statements {
        $$ = TBmakeFunbody(NULL, $1);
      };

localfunvar:
      type ID LET expr SEMICOLON {
        $$ = TBmakeVardec($1, $2, $4, NULL);
      }
      | type ID SEMICOLON {
        $$ = TBmakeVardec($1, $2, NULL, NULL);
      }
      | funheader CB_L funbody CB_R {
        $$ = TBmakeFundef(FALSE, $1, $3, NULL);
      }
      | type ID LET expr SEMICOLON localfunvar {
        $$ = TBmakeVardec($1, $2, $4, $6);
      }
      | type ID SEMICOLON localfunvar {
        $$ = TBmakeVardec($1, $2, NULL, $4);
      }
      | funheader CB_L funbody CB_R localfunvar {
        if (NODE_TYPE($5) == N_vardec) {
          yyerror("Vardec after local fundef");
        }
        $$ = TBmakeFundef(FALSE, $1, $3, $5);
      };

globaldec:
      EXTERN type ID SEMICOLON {
        $$ = TBmakeGlobaldec($2, $3);
      };

exprs:
      expr COMMA exprs {
        $$ = TBmakeExprs($1, $3);
      }
      | expr {
        $$ = TBmakeExprs($1, NULL);
      }
      | %empty {
        $$ = NULL;
      };

expr:
      BRACKET_L expr BRACKET_R %prec PARENTHESISED {
        $$ = $2;
      }
      | binop {
        $$ = $1;
      }
      | constant {
        $$ = $1;
      }
      | monop {
        $$ = $1;
      }
      | BRACKET_L type BRACKET_R expr %prec CAST {
        $$ = TBmakeCast($2, $4);
      }
      | funcall {
        $$ = $1;
      }
      | ID {
        $$ = TBmakeId($1);
      };

funcall:
      ID BRACKET_L exprs BRACKET_R {
        $$ = TBmakeFuncall($1, $3);
      };

param:
      type ID COMMA param {
        $$ = TBmakeParam($1, $2, $4);
      }
      | type ID {
        $$ = TBmakeParam($1, $2, NULL);
      };

statements:
      statement statements {
        $$ = TBmakeStatements($1, $2);
      }
      | %empty {
        $$ = NULL;
      };

statement:
      assign {
        $$ = $1;
      }
      | funcall SEMICOLON {
        $$ = $1;
      }
      | IF BRACKET_L expr BRACKET_R block %prec IF_NO_ELSE {
        $$ = TBmakeIfelse($3, $5, NULL);
      }
      | IF BRACKET_L expr BRACKET_R block ELSE block {
         $$ = TBmakeIfelse($3, $5, $7);
      }
      | WHILE BRACKET_L expr BRACKET_R block {
        $$ = TBmakeWhile($3, $5);
      }
      | DO block WHILE BRACKET_L expr BRACKET_R SEMICOLON {
        $$ = TBmakeDowhile($5, $2);
      }
      | FOR BRACKET_L INT ID LET expr COMMA expr COMMA expr BRACKET_R block {
        $$ = TBmakeFor($4, $6, $8, $10, $12);
      }
      | FOR BRACKET_L INT ID LET expr COMMA expr BRACKET_R block {
        $$ = TBmakeFor($4, $6, $8, NULL, $10);
      }
      | RETURN expr SEMICOLON {
        $$ = TBmakeReturn($2);
      }
      | RETURN SEMICOLON {
        $$ = TBmakeReturn(NULL);
      };

assign:
      id LET expr SEMICOLON {
        $$ = TBmakeAssign($3, $1);
      };

id:
      ID {
        $$ = TBmakeId($1);
      };

block:
      CB_L statements CB_R {
        $$ = TBmakeBlock($2);
      }
      | statement {
        $$ = TBmakeBlock(TBmakeStatements($1, NULL));
      };

type:
      BOOL {
        $$ = BT_bool;
      }
      | INT {
        $$ = BT_int;
      }
      | FLOAT {
        $$ = BT_float;
      };

constant:
      FLOATVAL {
        $$ = TBmakeFloat($1);
      }
      | INTVAL {
        $$ = TBmakeInt($1);
      }
      | BOOLVAL {
        $$ = TBmakeBool($1);
      };

monop:
      MINUS expr   { $$ = TBmakeMonop(MO_neg, $2); }
    | NOT expr     { $$ = TBmakeMonop(MO_not, $2); }
    ;

binop:
       expr PLUS expr      { $$ = TBmakeBinop(BO_add, $1, $3); }
     | expr MINUS expr     { $$ = TBmakeBinop(BO_sub, $1, $3); }
     | expr STAR expr      { $$ = TBmakeBinop(BO_mul, $1, $3); }
     | expr SLASH expr     { $$ = TBmakeBinop(BO_div, $1, $3); }
     | expr PERCENT expr   { $$ = TBmakeBinop(BO_mod, $1, $3); }
     | expr LE expr        { $$ = TBmakeBinop(BO_le, $1, $3); }
     | expr LT expr        { $$ = TBmakeBinop(BO_lt, $1, $3); }
     | expr GE expr        { $$ = TBmakeBinop(BO_ge, $1, $3); }
     | expr GT expr        { $$ = TBmakeBinop(BO_gt, $1, $3); }
     | expr EQ expr        { $$ = TBmakeBinop(BO_eq, $1, $3); }
     | expr NE expr        { $$ = TBmakeBinop(BO_ne, $1, $3); }
     | expr OR expr        { $$ = TBmakeTernary($1, TBmakeBool(TRUE), $3); }
     | expr AND expr       { $$ = TBmakeTernary($1, $3, TBmakeBool(FALSE)); }
     ;
%%

static int yyerror(char *error){
  CTIabort("line %d, col %d\nError parsing source code: %s\n",
            global.line, global.col, error);
  return(0);
}

node *YYparseTree(void){
  DBUG_ENTER("YYparseTree");
  yyparse();
  DBUG_RETURN(parseresult);
}

