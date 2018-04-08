%{

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "types.h"
#include "node_basic.h"
#include "str.h"
#include "free.h"
#include "civic.tab.h"
#include "globals.h"
#include "ctinfo.h"
#include "errno.h"
#include "limits.h"


#define FILTER(token) \
  global.col += yyleng;  \
  return(token);

#define LINEBUF_SIZE 256
#define YY_NO_UNPUT
#define YY_NO_INPUT


%}

%p 6000 /* set positions to 6000 */
%e 2000 /* set nodes to 1100 */
%option noyywrap
%option nounput

%x PREPROCESSOR
%x IGNORELINE
DIGIT                       [0-9]
NEWLINE                     "\n"

%%

"\/\/".*                   { /* DO NOTHING */ }

"/*"((\*+[^/*])|([^*]))*\**"*/"     { /* DO NOTHING */ }

"#"                        {BEGIN(PREPROCESSOR);}

<PREPROCESSOR>{
    {DIGIT}+                    { global.line = atoi(yytext) - 1;}
    "\"[^\"]+\""                { free(global.currentfile);
                                  global.currentfile = malloc(strlen(yytext));
                                  memcpy(global.currentfile, yytext + 1, strlen(yytext) - 2);
                                  global.currentfile[strlen(yytext) - 2] = '\0';
                                  BEGIN(IGNORELINE);
                                }
    [\t]                        { }
    .                           {BEGIN(IGNORELINE); }
    \n                          { yyless(1); BEGIN(INITIAL); }
}

<IGNORELINE>{
    .                           { }
    \n                          { yyless(1); BEGIN(INITIAL); }
}


"("                        { FILTER(BRACKET_L); }
")"                        { FILTER(BRACKET_R); }
","                        { FILTER(COMMA); }
";"                        { FILTER(SEMICOLON); }

"{"                        { FILTER(CB_L); }
"}"                        { FILTER(CB_R); }

"["                        { FILTER(SQB_L); }
"]"                        { FILTER(SQB_R); }

"-"                        { FILTER(MINUS); }
"+"                        { FILTER(PLUS);  }
"*"                        { FILTER(STAR);  }
"/"                        { FILTER(SLASH);  }
"%"                        { FILTER(PERCENT);  }


"!"                        { FILTER(NOT); }

"<="                       { FILTER(LE);    }
"<"                        { FILTER(LT);    }
">="                       { FILTER(GE);    }
">"                        { FILTER(GT);    }
"=="                       { FILTER(EQ);    }
"!="                       { FILTER(NE);    }
"&&"                       { FILTER(AND);    }
"||"                       { FILTER(OR);    }

"="                        { FILTER(LET);    }

"if"                       { FILTER(IF); }
"else"                     { FILTER(ELSE); }
"do"                       { FILTER(DO); }
"while"                    { FILTER(WHILE); }
"for"                      { FILTER(FOR); }

"extern"                   { FILTER(EXTERN); }
"export"                   { FILTER(EXPORT); }
"void"                     { FILTER(VOID); }
"return"                   { FILTER(RETURN); }

"int"                      { FILTER(INT); }
"float"                    { FILTER(FLOAT); }
"bool"                     { FILTER(BOOL); }

"true"                     { yylval.cbool = TRUE;
                             FILTER(BOOLVAL);
                           }
"false"                    { yylval.cbool = FALSE;
                             FILTER(BOOLVAL);
                           }

[A-Za-z][A-Za-z0-9_]*      { yylval.id = STRcpy(yytext);
                             FILTER(ID);
                           }

[0-9]+                     { long ret = strtol(yytext, NULL, 10);
                             if (errno == ERANGE || ret > INT_MAX || ret < INT_MIN) {
                               CTIerror("Integer %s out of range", yytext);
                               errno = 0;
                             }
                             yylval.cint=(int)ret;
                             FILTER(INTVAL);
                           }

[0-9]*["."][0-9]+          { float ret = strtof(yytext, NULL);
                             if (errno == ERANGE) {
                               CTIerror("Float %s out of range", yytext);
                               errno = 0;
                             }
                             yylval.cflt=ret;
                             FILTER(FLOATVAL);
                           }

\n.*                       { global.line += 1;
                             global.col = 0;
                             yyless(1);
                           }

[ \t]                      { global.col += yyleng;
                           }

.                          { global.col += 1;
                             FILTER(OTHER);
                           }
