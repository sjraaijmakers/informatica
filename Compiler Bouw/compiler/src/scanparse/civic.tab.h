/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    BRACKET_L = 258,
    BRACKET_R = 259,
    COMMA = 260,
    SEMICOLON = 261,
    CB_L = 262,
    CB_R = 263,
    SQB_L = 264,
    SQB_R = 265,
    MINUS = 266,
    PLUS = 267,
    STAR = 268,
    SLASH = 269,
    PERCENT = 270,
    LE = 271,
    LT = 272,
    GE = 273,
    GT = 274,
    EQ = 275,
    NE = 276,
    OR = 277,
    AND = 278,
    NOT = 279,
    LET = 280,
    FLOAT = 281,
    INT = 282,
    BOOL = 283,
    IF = 284,
    ELSE = 285,
    DO = 286,
    WHILE = 287,
    FOR = 288,
    VOID = 289,
    RETURN = 290,
    EXTERN = 291,
    EXPORT = 292,
    OTHER = 293,
    IF_NO_ELSE = 294,
    CAST = 295,
    PARENTHESISED = 296,
    INTVAL = 297,
    FLOATVAL = 298,
    ID = 299,
    BOOLVAL = 300
  };
#endif
/* Tokens.  */
#define BRACKET_L 258
#define BRACKET_R 259
#define COMMA 260
#define SEMICOLON 261
#define CB_L 262
#define CB_R 263
#define SQB_L 264
#define SQB_R 265
#define MINUS 266
#define PLUS 267
#define STAR 268
#define SLASH 269
#define PERCENT 270
#define LE 271
#define LT 272
#define GE 273
#define GT 274
#define EQ 275
#define NE 276
#define OR 277
#define AND 278
#define NOT 279
#define LET 280
#define FLOAT 281
#define INT 282
#define BOOL 283
#define IF 284
#define ELSE 285
#define DO 286
#define WHILE 287
#define FOR 288
#define VOID 289
#define RETURN 290
#define EXTERN 291
#define EXPORT 292
#define OTHER 293
#define IF_NO_ELSE 294
#define CAST 295
#define PARENTHESISED 296
#define INTVAL 297
#define FLOATVAL 298
#define ID 299
#define BOOLVAL 300

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 22 "src/scanparse/civic.y" /* yacc.c:1909  */

  nodetype            nodetype;
  char               *id;
  int                 cint;
  float               cflt;
  bool                cbool;
  node               *node;
  basictype           basictype;

#line 154 "y.tab.h" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
