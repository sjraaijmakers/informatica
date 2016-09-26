/* Driver template for the LEMON parser generator.
** The author disclaims copyright to this source code.
*/
/* First off, code is included that follows the "include" declaration
** in the input grammar file. */
#include <stdio.h>
#line 12 "parser.y"

#include "shell.h"
#include "ast.h"
#include "lexer.h"
#include <assert.h>
#include <stdlib.h>
int echo = 0;
int parse_error = 0;
#pragma GCC diagnostic ignored "-Wunused-parameter"
#line 18 "parser.c"
/* Next is all token values, in a form suitable for use by makeheaders.
** This section will be null unless lemon is run with the -m switch.
*/
/* 
** These constants (all generated automatically by the parser generator)
** specify the various kinds of tokens (terminals) that the parser
** understands. 
**
** Each symbol here is a terminal symbol in the grammar.
*/
/* Make sure the INTERFACE macro is defined.
*/
#ifndef INTERFACE
# define INTERFACE 1
#endif
/* The next thing included is series of defines which control
** various aspects of the generated parser.
**    YYCODETYPE         is the data type used for storing terminal
**                       and nonterminal numbers.  "unsigned char" is
**                       used if there are fewer than 250 terminals
**                       and nonterminals.  "int" is used otherwise.
**    YYNOCODE           is a number of type YYCODETYPE which corresponds
**                       to no legal terminal or nonterminal number.  This
**                       number is used to fill in empty slots of the hash 
**                       table.
**    YYFALLBACK         If defined, this indicates that one or more tokens
**                       have fall-back values which should be used if the
**                       original value of the token will not parse.
**    YYACTIONTYPE       is the data type used for storing terminal
**                       and nonterminal numbers.  "unsigned char" is
**                       used if there are fewer than 250 rules and
**                       states combined.  "int" is used otherwise.
**    ParseTOKENTYPE     is the data type used for minor tokens given 
**                       directly to the parser from the tokenizer.
**    YYMINORTYPE        is the data type used for all minor tokens.
**                       This is typically a union of many types, one of
**                       which is ParseTOKENTYPE.  The entry in the union
**                       for base tokens is called "yy0".
**    YYSTACKDEPTH       is the maximum depth of the parser's stack.  If
**                       zero the stack is dynamically sized using realloc()
**    ParseARG_SDECL     A static variable declaration for the %extra_argument
**    ParseARG_PDECL     A parameter declaration for the %extra_argument
**    ParseARG_STORE     Code to store %extra_argument into yypParser
**    ParseARG_FETCH     Code to extract %extra_argument from yypParser
**    YYNSTATE           the combined number of states.
**    YYNRULE            the number of rules in the grammar
**    YYERRORSYMBOL      is the code number of the error symbol.  If not
**                       defined, then do no error processing.
*/
#define YYCODETYPE unsigned char
#define YYNOCODE 23
#define YYACTIONTYPE unsigned char
#define ParseTOKENTYPE  struct lex_token 
typedef union {
  int yyinit;
  ParseTOKENTYPE yy0;
  node_t* yy12;
  int yy20;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define ParseARG_SDECL
#define ParseARG_PDECL
#define ParseARG_FETCH
#define ParseARG_STORE
#define YYNSTATE 58
#define YYNRULE 29
#define YY_NO_ACTION      (YYNSTATE+YYNRULE+2)
#define YY_ACCEPT_ACTION  (YYNSTATE+YYNRULE+1)
#define YY_ERROR_ACTION   (YYNSTATE+YYNRULE)

/* The yyzerominor constant is used to initialize instances of
** YYMINORTYPE objects to zero. */
static const YYMINORTYPE yyzerominor = { 0 };

/* Define the yytestcase() macro to be a no-op if is not already defined
** otherwise.
**
** Applications can choose to define yytestcase() in the %include section
** to a macro that can assist in verifying code coverage.  For production
** code the yytestcase() macro should be turned off.  But it is useful
** for testing.
*/
#ifndef yytestcase
# define yytestcase(X)
#endif


/* Next are the tables used to determine what action to take based on the
** current state and lookahead token.  These tables are used to implement
** functions that take a state number and lookahead value and return an
** action integer.  
**
** Suppose the action integer is N.  Then the action is determined as
** follows
**
**   0 <= N < YYNSTATE                  Shift N.  That is, push the lookahead
**                                      token onto the stack and goto state N.
**
**   YYNSTATE <= N < YYNSTATE+YYNRULE   Reduce by rule N-YYNSTATE.
**
**   N == YYNSTATE+YYNRULE              A syntax error has occurred.
**
**   N == YYNSTATE+YYNRULE+1            The parser accepts its input.
**
**   N == YYNSTATE+YYNRULE+2            No such action.  Denotes unused
**                                      slots in the yy_action[] table.
**
** The action table is constructed as a single large table named yy_action[].
** Given state S and lookahead X, the action is computed as
**
**      yy_action[ yy_shift_ofst[S] + X ]
**
** If the index value yy_shift_ofst[S]+X is out of range or if the value
** yy_lookahead[yy_shift_ofst[S]+X] is not equal to X or if yy_shift_ofst[S]
** is equal to YY_SHIFT_USE_DFLT, it means that the action is not in the table
** and that yy_default[S] should be used instead.  
**
** The formula above is for computing the action when the lookahead is
** a terminal symbol.  If the lookahead is a non-terminal (as occurs after
** a reduce action) then the yy_reduce_ofst[] array is used in place of
** the yy_shift_ofst[] array and YY_REDUCE_USE_DFLT is used in place of
** YY_SHIFT_USE_DFLT.
**
** The following are the tables generated in this section:
**
**  yy_action[]        A single table containing all actions.
**  yy_lookahead[]     A table containing the lookahead for each entry in
**                     yy_action.  Used to detect hash collisions.
**  yy_shift_ofst[]    For each state, the offset into yy_action for
**                     shifting terminals.
**  yy_reduce_ofst[]   For each state, the offset into yy_action for
**                     shifting non-terminals after a reduce.
**  yy_default[]       Default action for each state.
*/
#define YY_ACTTAB_COUNT (111)
static const YYACTIONTYPE yy_action[] = {
 /*     0 */    37,   31,   18,   20,   46,   32,    3,   58,    2,   31,
 /*    10 */    18,   20,   46,   32,    3,   35,    2,   88,   36,   22,
 /*    20 */    34,   28,   55,   19,   59,   57,   22,   34,   28,   55,
 /*    30 */    19,   26,   22,   34,   28,   55,   19,   27,   22,   34,
 /*    40 */    28,   55,   19,   38,   22,   34,   28,   55,   19,   33,
 /*    50 */    23,   14,    6,   29,   25,   56,   10,   55,   19,   54,
 /*    60 */     4,   55,   19,    1,   53,   16,   55,   19,   52,   21,
 /*    70 */    55,   19,   51,   47,   55,   19,   48,    9,   55,   19,
 /*    80 */    44,   30,   55,   19,    7,   43,   15,   55,   19,   42,
 /*    90 */    13,   55,   19,   41,   12,   55,   19,   40,   11,   55,
 /*   100 */    19,   39,   17,   55,   19,   24,   49,   50,   45,    8,
 /*   110 */     5,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */     3,    4,    5,    6,    7,    8,    9,    0,   11,    4,
 /*    10 */     5,    6,    7,    8,    9,    3,   11,   15,   16,   17,
 /*    20 */    18,   19,   20,   21,    0,   16,   17,   18,   19,   20,
 /*    30 */    21,   16,   17,   18,   19,   20,   21,   16,   17,   18,
 /*    40 */    19,   20,   21,   16,   17,   18,   19,   20,   21,    4,
 /*    50 */     5,    7,    7,    4,    5,   18,    7,   20,   21,   18,
 /*    60 */     1,   20,   21,    4,   18,    2,   20,   21,   18,    5,
 /*    70 */    20,   21,   18,   12,   20,   21,   18,    7,   20,   21,
 /*    80 */    18,    4,   20,   21,    7,   18,    6,   20,   21,   18,
 /*    90 */     6,   20,   21,   18,    6,   20,   21,   18,    2,   20,
 /*   100 */    21,   18,    5,   20,   21,    8,    6,    7,   10,    7,
 /*   110 */     7,
};
#define YY_SHIFT_USE_DFLT (-4)
#define YY_SHIFT_COUNT (37)
#define YY_SHIFT_MIN   (-3)
#define YY_SHIFT_MAX   (103)
static const signed char yy_shift_ofst[] = {
 /*     0 */    -3,    5,    5,    5,    5,    5,    5,    5,    5,    5,
 /*    10 */     5,    5,    5,    5,    5,    5,    5,   49,   45,  100,
 /*    20 */    97,   77,   59,  103,  102,   70,   98,   61,   96,   88,
 /*    30 */    84,   64,   44,   80,   63,   24,   12,    7,
};
#define YY_REDUCE_USE_DFLT (-1)
#define YY_REDUCE_COUNT (16)
#define YY_REDUCE_MIN   (0)
#define YY_REDUCE_MAX   (83)
static const signed char yy_reduce_ofst[] = {
 /*     0 */     2,   27,   21,   15,    9,   83,   79,   75,   71,   67,
 /*    10 */    62,   58,   54,   50,   46,   41,   37,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */    87,   62,   87,   87,   61,   87,   87,   87,   87,   87,
 /*    10 */    87,   87,   87,   87,   87,   87,   87,   87,   87,   80,
 /*    20 */    84,   87,   60,   87,   87,   87,   87,   87,   66,   87,
 /*    30 */    87,   87,   87,   87,   65,   87,   87,   87,   64,   72,
 /*    40 */    71,   75,   79,   78,   77,   81,   83,   82,   68,   86,
 /*    50 */    85,   76,   74,   73,   70,   69,   67,   63,
};

/* The next table maps tokens into fallback tokens.  If a construct
** like the following:
** 
**      %fallback ID X Y Z.
**
** appears in the grammar, then ID becomes a fallback token for X, Y,
** and Z.  Whenever one of the tokens X, Y, or Z is input to the parser
** but it does not parse, the type of the token is changed to ID and
** the parse is retried before an error is thrown.
*/
#ifdef YYFALLBACK
static const YYCODETYPE yyFallback[] = {
};
#endif /* YYFALLBACK */

/* The following structure represents a single element of the
** parser's stack.  Information stored includes:
**
**   +  The state number for the parser at this level of the stack.
**
**   +  The value of the token stored at this level of the stack.
**      (In other words, the "major" token.)
**
**   +  The semantic value stored at this level of the stack.  This is
**      the information used by the action routines in the grammar.
**      It is sometimes called the "minor" token.
*/
struct yyStackEntry {
  YYACTIONTYPE stateno;  /* The state-number */
  YYCODETYPE major;      /* The major token value.  This is the code
                         ** number for the token at this stack level */
  YYMINORTYPE minor;     /* The user-supplied minor token value.  This
                         ** is the value of the token  */
};
typedef struct yyStackEntry yyStackEntry;

/* The state of the parser is completely contained in an instance of
** the following structure */
struct yyParser {
  int yyidx;                    /* Index of top element in stack */
#ifdef YYTRACKMAXSTACKDEPTH
  int yyidxMax;                 /* Maximum value of yyidx */
#endif
  int yyerrcnt;                 /* Shifts left before out of the error */
  ParseARG_SDECL                /* A place to hold %extra_argument */
#if YYSTACKDEPTH<=0
  int yystksz;                  /* Current side of the stack */
  yyStackEntry *yystack;        /* The parser's stack */
#else
  yyStackEntry yystack[YYSTACKDEPTH];  /* The parser's stack */
#endif
};
typedef struct yyParser yyParser;

#ifndef NDEBUG
#include <stdio.h>
static FILE *yyTraceFILE = 0;
static char *yyTracePrompt = 0;
#endif /* NDEBUG */

#ifndef NDEBUG
/* 
** Turn parser tracing on by giving a stream to which to write the trace
** and a prompt to preface each trace message.  Tracing is turned off
** by making either argument NULL 
**
** Inputs:
** <ul>
** <li> A FILE* to which trace output should be written.
**      If NULL, then tracing is turned off.
** <li> A prefix string written at the beginning of every
**      line of trace output.  If NULL, then tracing is
**      turned off.
** </ul>
**
** Outputs:
** None.
*/
void ParseTrace(FILE *TraceFILE, char *zTracePrompt){
  yyTraceFILE = TraceFILE;
  yyTracePrompt = zTracePrompt;
  if( yyTraceFILE==0 ) yyTracePrompt = 0;
  else if( yyTracePrompt==0 ) yyTraceFILE = 0;
}
#endif /* NDEBUG */

#ifndef NDEBUG
/* For tracing shifts, the names of all terminals and nonterminals
** are required.  The following table supplies these names */
static const char *const yyTokenName[] = { 
  "$",             "SEMI",          "PIPE",          "END",         
  "AMP",           "GT",            "NUMBER",        "WORD",        
  "LT",            "BRL",           "BRR",           "PL",          
  "PR",            "error",         "commands",      "top",         
  "seq",           "pipe",          "redir",         "pipe1",       
  "group",         "simple",      
};
#endif /* NDEBUG */

#ifndef NDEBUG
/* For tracing reduce actions, the names of all rules are required.
*/
static const char *const yyRuleName[] = {
 /*   0 */ "top ::= END",
 /*   1 */ "top ::= seq END",
 /*   2 */ "seq ::= pipe",
 /*   3 */ "seq ::= pipe SEMI",
 /*   4 */ "seq ::= pipe AMP",
 /*   5 */ "seq ::= pipe SEMI seq",
 /*   6 */ "seq ::= pipe AMP seq",
 /*   7 */ "pipe ::= redir",
 /*   8 */ "pipe ::= pipe1",
 /*   9 */ "pipe1 ::= redir PIPE redir",
 /*  10 */ "pipe1 ::= pipe1 PIPE redir",
 /*  11 */ "redir ::= group",
 /*  12 */ "redir ::= GT AMP NUMBER redir",
 /*  13 */ "redir ::= GT WORD redir",
 /*  14 */ "redir ::= GT GT WORD redir",
 /*  15 */ "redir ::= LT WORD redir",
 /*  16 */ "redir ::= AMP GT AMP NUMBER redir",
 /*  17 */ "redir ::= AMP GT WORD redir",
 /*  18 */ "redir ::= NUMBER GT AMP NUMBER redir",
 /*  19 */ "redir ::= NUMBER GT WORD redir",
 /*  20 */ "redir ::= NUMBER GT GT WORD redir",
 /*  21 */ "redir ::= NUMBER LT WORD redir",
 /*  22 */ "group ::= simple",
 /*  23 */ "group ::= BRL seq BRR",
 /*  24 */ "group ::= PL seq PR",
 /*  25 */ "simple ::= WORD",
 /*  26 */ "simple ::= NUMBER",
 /*  27 */ "simple ::= simple WORD",
 /*  28 */ "simple ::= simple NUMBER",
};
#endif /* NDEBUG */


#if YYSTACKDEPTH<=0
/*
** Try to increase the size of the parser stack.
*/
static void yyGrowStack(yyParser *p){
  int newSize;
  yyStackEntry *pNew;

  newSize = p->yystksz*2 + 100;
  pNew = realloc(p->yystack, newSize*sizeof(pNew[0]));
  if( pNew ){
    p->yystack = pNew;
    p->yystksz = newSize;
#ifndef NDEBUG
    if( yyTraceFILE ){
      fprintf(yyTraceFILE,"%sStack grows to %d entries!\n",
              yyTracePrompt, p->yystksz);
    }
#endif
  }
}
#endif

/* 
** This function allocates a new parser.
** The only argument is a pointer to a function which works like
** malloc.
**
** Inputs:
** A pointer to the function used to allocate memory.
**
** Outputs:
** A pointer to a parser.  This pointer is used in subsequent calls
** to Parse and ParseFree.
*/
void *ParseAlloc(void *(*mallocProc)(size_t)){
  yyParser *pParser;
  pParser = (yyParser*)(*mallocProc)( (size_t)sizeof(yyParser) );
  if( pParser ){
    pParser->yyidx = -1;
#ifdef YYTRACKMAXSTACKDEPTH
    pParser->yyidxMax = 0;
#endif
#if YYSTACKDEPTH<=0
    pParser->yystack = NULL;
    pParser->yystksz = 0;
    yyGrowStack(pParser);
#endif
  }
  return pParser;
}

/* The following function deletes the value associated with a
** symbol.  The symbol can be either a terminal or nonterminal.
** "yymajor" is the symbol code, and "yypminor" is a pointer to
** the value.
*/
static void yy_destructor(
  yyParser *yypParser,    /* The parser */
  YYCODETYPE yymajor,     /* Type code for object to destroy */
  YYMINORTYPE *yypminor   /* The object to be destroyed */
){
  ParseARG_FETCH;
  switch( yymajor ){
    /* Here is inserted the actions which take place when a
    ** terminal or non-terminal is destroyed.  This can happen
    ** when the symbol is popped from the stack during a
    ** reduce or during error processing or when a parser is 
    ** being destroyed before it is finished parsing.
    **
    ** Note: during a reduce, the only symbols destroyed are those
    ** which appear on the RHS of the rule, but which are not used
    ** inside the C code.
    */
      /* TERMINAL Destructor */
    case 1: /* SEMI */
    case 2: /* PIPE */
    case 3: /* END */
    case 4: /* AMP */
    case 5: /* GT */
    case 6: /* NUMBER */
    case 7: /* WORD */
    case 8: /* LT */
    case 9: /* BRL */
    case 10: /* BRR */
    case 11: /* PL */
    case 12: /* PR */
{
#line 2 "parser.y"
 if ((yypminor->yy0).text) free((yypminor->yy0).text); 
#line 437 "parser.c"
}
      break;
      /* Default NON-TERMINAL Destructor */
    case 13: /* error */
    case 14: /* commands */
    case 15: /* top */
    case 16: /* seq */
    case 17: /* pipe */
    case 18: /* redir */
    case 19: /* pipe1 */
    case 20: /* group */
    case 21: /* simple */
{
#line 4 "parser.y"
 free_tree((yypminor->yy12)); 
#line 453 "parser.c"
}
      break;
    default:  break;   /* If no destructor action specified: do nothing */
  }
}

/*
** Pop the parser's stack once.
**
** If there is a destructor routine associated with the token which
** is popped from the stack, then call it.
**
** Return the major token number for the symbol popped.
*/
static int yy_pop_parser_stack(yyParser *pParser){
  YYCODETYPE yymajor;
  yyStackEntry *yytos = &pParser->yystack[pParser->yyidx];

  if( pParser->yyidx<0 ) return 0;
#ifndef NDEBUG
  if( yyTraceFILE && pParser->yyidx>=0 ){
    fprintf(yyTraceFILE,"%sPopping %s\n",
      yyTracePrompt,
      yyTokenName[yytos->major]);
  }
#endif
  yymajor = yytos->major;
  yy_destructor(pParser, yymajor, &yytos->minor);
  pParser->yyidx--;
  return yymajor;
}

/* 
** Deallocate and destroy a parser.  Destructors are all called for
** all stack elements before shutting the parser down.
**
** Inputs:
** <ul>
** <li>  A pointer to the parser.  This should be a pointer
**       obtained from ParseAlloc.
** <li>  A pointer to a function used to reclaim memory obtained
**       from malloc.
** </ul>
*/
void ParseFree(
  void *p,                    /* The parser to be deleted */
  void (*freeProc)(void*)     /* Function used to reclaim memory */
){
  yyParser *pParser = (yyParser*)p;
  if( pParser==0 ) return;
  while( pParser->yyidx>=0 ) yy_pop_parser_stack(pParser);
#if YYSTACKDEPTH<=0
  free(pParser->yystack);
#endif
  (*freeProc)((void*)pParser);
}

/*
** Return the peak depth of the stack for a parser.
*/
#ifdef YYTRACKMAXSTACKDEPTH
int ParseStackPeak(void *p){
  yyParser *pParser = (yyParser*)p;
  return pParser->yyidxMax;
}
#endif

/*
** Find the appropriate action for a parser given the terminal
** look-ahead token iLookAhead.
**
** If the look-ahead token is YYNOCODE, then check to see if the action is
** independent of the look-ahead.  If it is, return the action, otherwise
** return YY_NO_ACTION.
*/
static int yy_find_shift_action(
  yyParser *pParser,        /* The parser */
  YYCODETYPE iLookAhead     /* The look-ahead token */
){
  int i;
  int stateno = pParser->yystack[pParser->yyidx].stateno;
 
  if( stateno>YY_SHIFT_COUNT
   || (i = yy_shift_ofst[stateno])==YY_SHIFT_USE_DFLT ){
    return yy_default[stateno];
  }
  assert( iLookAhead!=YYNOCODE );
  i += iLookAhead;
  if( i<0 || i>=YY_ACTTAB_COUNT || yy_lookahead[i]!=iLookAhead ){
    if( iLookAhead>0 ){
#ifdef YYFALLBACK
      YYCODETYPE iFallback;            /* Fallback token */
      if( iLookAhead<sizeof(yyFallback)/sizeof(yyFallback[0])
             && (iFallback = yyFallback[iLookAhead])!=0 ){
#ifndef NDEBUG
        if( yyTraceFILE ){
          fprintf(yyTraceFILE, "%sFALLBACK %s => %s\n",
             yyTracePrompt, yyTokenName[iLookAhead], yyTokenName[iFallback]);
        }
#endif
        return yy_find_shift_action(pParser, iFallback);
      }
#endif
#ifdef YYWILDCARD
      {
        int j = i - iLookAhead + YYWILDCARD;
        if( 
#if YY_SHIFT_MIN+YYWILDCARD<0
          j>=0 &&
#endif
#if YY_SHIFT_MAX+YYWILDCARD>=YY_ACTTAB_COUNT
          j<YY_ACTTAB_COUNT &&
#endif
          yy_lookahead[j]==YYWILDCARD
        ){
#ifndef NDEBUG
          if( yyTraceFILE ){
            fprintf(yyTraceFILE, "%sWILDCARD %s => %s\n",
               yyTracePrompt, yyTokenName[iLookAhead], yyTokenName[YYWILDCARD]);
          }
#endif /* NDEBUG */
          return yy_action[j];
        }
      }
#endif /* YYWILDCARD */
    }
    return yy_default[stateno];
  }else{
    return yy_action[i];
  }
}

/*
** Find the appropriate action for a parser given the non-terminal
** look-ahead token iLookAhead.
**
** If the look-ahead token is YYNOCODE, then check to see if the action is
** independent of the look-ahead.  If it is, return the action, otherwise
** return YY_NO_ACTION.
*/
static int yy_find_reduce_action(
  int stateno,              /* Current state number */
  YYCODETYPE iLookAhead     /* The look-ahead token */
){
  int i;
#ifdef YYERRORSYMBOL
  if( stateno>YY_REDUCE_COUNT ){
    return yy_default[stateno];
  }
#else
  assert( stateno<=YY_REDUCE_COUNT );
#endif
  i = yy_reduce_ofst[stateno];
  assert( i!=YY_REDUCE_USE_DFLT );
  assert( iLookAhead!=YYNOCODE );
  i += iLookAhead;
#ifdef YYERRORSYMBOL
  if( i<0 || i>=YY_ACTTAB_COUNT || yy_lookahead[i]!=iLookAhead ){
    return yy_default[stateno];
  }
#else
  assert( i>=0 && i<YY_ACTTAB_COUNT );
  assert( yy_lookahead[i]==iLookAhead );
#endif
  return yy_action[i];
}

/*
** The following routine is called if the stack overflows.
*/
static void yyStackOverflow(yyParser *yypParser, YYMINORTYPE *yypMinor){
   ParseARG_FETCH;
   yypParser->yyidx--;
#ifndef NDEBUG
   if( yyTraceFILE ){
     fprintf(yyTraceFILE,"%sStack Overflow!\n",yyTracePrompt);
   }
#endif
   while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
   /* Here code is inserted which will execute if the parser
   ** stack every overflows */
   ParseARG_STORE; /* Suppress warning about unused %extra_argument var */
}

/*
** Perform a shift action.
*/
static void yy_shift(
  yyParser *yypParser,          /* The parser to be shifted */
  int yyNewState,               /* The new state to shift in */
  int yyMajor,                  /* The major token to shift in */
  YYMINORTYPE *yypMinor         /* Pointer to the minor token to shift in */
){
  yyStackEntry *yytos;
  yypParser->yyidx++;
#ifdef YYTRACKMAXSTACKDEPTH
  if( yypParser->yyidx>yypParser->yyidxMax ){
    yypParser->yyidxMax = yypParser->yyidx;
  }
#endif
#if YYSTACKDEPTH>0 
  if( yypParser->yyidx>=YYSTACKDEPTH ){
    yyStackOverflow(yypParser, yypMinor);
    return;
  }
#else
  if( yypParser->yyidx>=yypParser->yystksz ){
    yyGrowStack(yypParser);
    if( yypParser->yyidx>=yypParser->yystksz ){
      yyStackOverflow(yypParser, yypMinor);
      return;
    }
  }
#endif
  yytos = &yypParser->yystack[yypParser->yyidx];
  yytos->stateno = (YYACTIONTYPE)yyNewState;
  yytos->major = (YYCODETYPE)yyMajor;
  yytos->minor = *yypMinor;
#ifndef NDEBUG
  if( yyTraceFILE && yypParser->yyidx>0 ){
    int i;
    fprintf(yyTraceFILE,"%sShift %d\n",yyTracePrompt,yyNewState);
    fprintf(yyTraceFILE,"%sStack:",yyTracePrompt);
    for(i=1; i<=yypParser->yyidx; i++)
      fprintf(yyTraceFILE," %s",yyTokenName[yypParser->yystack[i].major]);
    fprintf(yyTraceFILE,"\n");
  }
#endif
}

/* The following table contains information about every rule that
** is used during the reduce.
*/
static const struct {
  YYCODETYPE lhs;         /* Symbol on the left-hand side of the rule */
  unsigned char nrhs;     /* Number of right-hand side symbols in the rule */
} yyRuleInfo[] = {
  { 15, 1 },
  { 15, 2 },
  { 16, 1 },
  { 16, 2 },
  { 16, 2 },
  { 16, 3 },
  { 16, 3 },
  { 17, 1 },
  { 17, 1 },
  { 19, 3 },
  { 19, 3 },
  { 18, 1 },
  { 18, 4 },
  { 18, 3 },
  { 18, 4 },
  { 18, 3 },
  { 18, 5 },
  { 18, 4 },
  { 18, 5 },
  { 18, 4 },
  { 18, 5 },
  { 18, 4 },
  { 20, 1 },
  { 20, 3 },
  { 20, 3 },
  { 21, 1 },
  { 21, 1 },
  { 21, 2 },
  { 21, 2 },
};

static void yy_accept(yyParser*);  /* Forward Declaration */

/*
** Perform a reduce action and the shift that must immediately
** follow the reduce.
*/
static void yy_reduce(
  yyParser *yypParser,         /* The parser */
  int yyruleno                 /* Number of the rule by which to reduce */
){
  int yygoto;                     /* The next state */
  int yyact;                      /* The next action */
  YYMINORTYPE yygotominor;        /* The LHS of the rule reduced */
  yyStackEntry *yymsp;            /* The top of the parser's stack */
  int yysize;                     /* Amount to pop the stack */
  ParseARG_FETCH;
  yymsp = &yypParser->yystack[yypParser->yyidx];
#ifndef NDEBUG
  if( yyTraceFILE && yyruleno>=0 
        && yyruleno<(int)(sizeof(yyRuleName)/sizeof(yyRuleName[0])) ){
    fprintf(yyTraceFILE, "%sReduce [%s].\n", yyTracePrompt,
      yyRuleName[yyruleno]);
  }
#endif /* NDEBUG */

  /* Silence complaints from purify about yygotominor being uninitialized
  ** in some cases when it is copied into the stack after the following
  ** switch.  yygotominor is uninitialized when a rule reduces that does
  ** not set the value of its left-hand side nonterminal.  Leaving the
  ** value of the nonterminal uninitialized is utterly harmless as long
  ** as the value is never used.  So really the only thing this code
  ** accomplishes is to quieten purify.  
  **
  ** 2007-01-16:  The wireshark project (www.wireshark.org) reports that
  ** without this code, their parser segfaults.  I'm not sure what there
  ** parser is doing to make this happen.  This is the second bug report
  ** from wireshark this week.  Clearly they are stressing Lemon in ways
  ** that it has not been previously stressed...  (SQLite ticket #2172)
  */
  /*memset(&yygotominor, 0, sizeof(yygotominor));*/
  yygotominor = yyzerominor;


  switch( yyruleno ){
  /* Beginning here are the reduction cases.  A typical example
  ** follows:
  **   case 0:
  **  #line <lineno> <grammarfile>
  **     { ... }           // User supplied code
  **  #line <lineno> <thisfile>
  **     break;
  */
      case 0: /* top ::= END */
#line 23 "parser.y"
{   yy_destructor(yypParser,3,&yymsp[0].minor);
}
#line 778 "parser.c"
        break;
      case 1: /* top ::= seq END */
#line 24 "parser.y"
{ if (parse_error) free_tree(yymsp[-1].minor.yy12);
                      else { if (echo) print_tree_flat(yymsp[-1].minor.yy12, 1); run_command(yymsp[-1].minor.yy12); }   yy_destructor(yypParser,3,&yymsp[0].minor);
}
#line 785 "parser.c"
        break;
      case 2: /* seq ::= pipe */
      case 7: /* pipe ::= redir */ yytestcase(yyruleno==7);
      case 8: /* pipe ::= pipe1 */ yytestcase(yyruleno==8);
      case 11: /* redir ::= group */ yytestcase(yyruleno==11);
      case 22: /* group ::= simple */ yytestcase(yyruleno==22);
#line 27 "parser.y"
{ yygotominor.yy12 = yymsp[0].minor.yy12; }
#line 794 "parser.c"
        break;
      case 3: /* seq ::= pipe SEMI */
#line 28 "parser.y"
{ yygotominor.yy12 = yymsp[-1].minor.yy12;   yy_destructor(yypParser,1,&yymsp[0].minor);
}
#line 800 "parser.c"
        break;
      case 4: /* seq ::= pipe AMP */
#line 29 "parser.y"
{ yygotominor.yy12 = make_detach(yymsp[-1].minor.yy12);   yy_destructor(yypParser,4,&yymsp[0].minor);
}
#line 806 "parser.c"
        break;
      case 5: /* seq ::= pipe SEMI seq */
#line 30 "parser.y"
{ yygotominor.yy12 = make_seq(yymsp[-2].minor.yy12, yymsp[0].minor.yy12);   yy_destructor(yypParser,1,&yymsp[-1].minor);
}
#line 812 "parser.c"
        break;
      case 6: /* seq ::= pipe AMP seq */
#line 31 "parser.y"
{ yygotominor.yy12 = make_seq(make_detach(yymsp[-2].minor.yy12), yymsp[0].minor.yy12);   yy_destructor(yypParser,4,&yymsp[-1].minor);
}
#line 818 "parser.c"
        break;
      case 9: /* pipe1 ::= redir PIPE redir */
#line 35 "parser.y"
{ yygotominor.yy12 = make_pipe(yymsp[-2].minor.yy12, yymsp[0].minor.yy12);   yy_destructor(yypParser,2,&yymsp[-1].minor);
}
#line 824 "parser.c"
        break;
      case 10: /* pipe1 ::= pipe1 PIPE redir */
#line 36 "parser.y"
{ yygotominor.yy12 = extend_pipe(yymsp[-2].minor.yy12, yymsp[0].minor.yy12);   yy_destructor(yypParser,2,&yymsp[-1].minor);
}
#line 830 "parser.c"
        break;
      case 12: /* redir ::= GT AMP NUMBER redir */
#line 39 "parser.y"
{ yygotominor.yy12 = make_redir(yymsp[0].minor.yy12, 1, 0, yymsp[-1].minor.yy0.number, 0);   yy_destructor(yypParser,5,&yymsp[-3].minor);
  yy_destructor(yypParser,4,&yymsp[-2].minor);
}
#line 837 "parser.c"
        break;
      case 13: /* redir ::= GT WORD redir */
#line 40 "parser.y"
{ yygotominor.yy12 = make_redir(yymsp[0].minor.yy12, 1, 2, 0, yymsp[-1].minor.yy0.text);   yy_destructor(yypParser,5,&yymsp[-2].minor);
}
#line 843 "parser.c"
        break;
      case 14: /* redir ::= GT GT WORD redir */
#line 41 "parser.y"
{ yygotominor.yy12 = make_redir(yymsp[0].minor.yy12, 1, 3, 0, yymsp[-1].minor.yy0.text);   yy_destructor(yypParser,5,&yymsp[-3].minor);
  yy_destructor(yypParser,5,&yymsp[-2].minor);
}
#line 850 "parser.c"
        break;
      case 15: /* redir ::= LT WORD redir */
#line 42 "parser.y"
{ yygotominor.yy12 = make_redir(yymsp[0].minor.yy12, 0, 1, 0, yymsp[-1].minor.yy0.text);   yy_destructor(yypParser,8,&yymsp[-2].minor);
}
#line 856 "parser.c"
        break;
      case 16: /* redir ::= AMP GT AMP NUMBER redir */
#line 43 "parser.y"
{ yygotominor.yy12 = make_redir(yymsp[0].minor.yy12, -1, 0, yymsp[-1].minor.yy0.number, 0);   yy_destructor(yypParser,4,&yymsp[-4].minor);
  yy_destructor(yypParser,5,&yymsp[-3].minor);
  yy_destructor(yypParser,4,&yymsp[-2].minor);
}
#line 864 "parser.c"
        break;
      case 17: /* redir ::= AMP GT WORD redir */
#line 44 "parser.y"
{ yygotominor.yy12 = make_redir(yymsp[0].minor.yy12, -1, 2, 0, yymsp[-1].minor.yy0.text);   yy_destructor(yypParser,4,&yymsp[-3].minor);
  yy_destructor(yypParser,5,&yymsp[-2].minor);
}
#line 871 "parser.c"
        break;
      case 18: /* redir ::= NUMBER GT AMP NUMBER redir */
#line 45 "parser.y"
{ yygotominor.yy12 = make_redir(yymsp[0].minor.yy12, yymsp[-4].minor.yy0.number, 0, yymsp[-1].minor.yy0.number, 0);   yy_destructor(yypParser,5,&yymsp[-3].minor);
  yy_destructor(yypParser,4,&yymsp[-2].minor);
}
#line 878 "parser.c"
        break;
      case 19: /* redir ::= NUMBER GT WORD redir */
#line 46 "parser.y"
{ yygotominor.yy12 = make_redir(yymsp[0].minor.yy12, yymsp[-3].minor.yy0.number, 2, 0, yymsp[-1].minor.yy0.text);   yy_destructor(yypParser,5,&yymsp[-2].minor);
}
#line 884 "parser.c"
        break;
      case 20: /* redir ::= NUMBER GT GT WORD redir */
#line 47 "parser.y"
{ yygotominor.yy12 = make_redir(yymsp[0].minor.yy12, yymsp[-4].minor.yy0.number, 3, 0, yymsp[-1].minor.yy0.text);   yy_destructor(yypParser,5,&yymsp[-3].minor);
  yy_destructor(yypParser,5,&yymsp[-2].minor);
}
#line 891 "parser.c"
        break;
      case 21: /* redir ::= NUMBER LT WORD redir */
#line 48 "parser.y"
{ yygotominor.yy12 = make_redir(yymsp[0].minor.yy12, yymsp[-3].minor.yy0.number, 1, 0, yymsp[-1].minor.yy0.text);   yy_destructor(yypParser,8,&yymsp[-2].minor);
}
#line 897 "parser.c"
        break;
      case 23: /* group ::= BRL seq BRR */
#line 51 "parser.y"
{ yygotominor.yy12 = yymsp[-1].minor.yy12;   yy_destructor(yypParser,9,&yymsp[-2].minor);
  yy_destructor(yypParser,10,&yymsp[0].minor);
}
#line 904 "parser.c"
        break;
      case 24: /* group ::= PL seq PR */
#line 52 "parser.y"
{ yygotominor.yy12 = make_subshell(yymsp[-1].minor.yy12);   yy_destructor(yypParser,11,&yymsp[-2].minor);
  yy_destructor(yypParser,12,&yymsp[0].minor);
}
#line 911 "parser.c"
        break;
      case 25: /* simple ::= WORD */
      case 26: /* simple ::= NUMBER */ yytestcase(yyruleno==26);
#line 54 "parser.y"
{ yygotominor.yy12 = make_simple(yymsp[0].minor.yy0.text); }
#line 917 "parser.c"
        break;
      case 27: /* simple ::= simple WORD */
      case 28: /* simple ::= simple NUMBER */ yytestcase(yyruleno==28);
#line 56 "parser.y"
{ yygotominor.yy12 = extend_simple(yymsp[-1].minor.yy12, yymsp[0].minor.yy0.text); }
#line 923 "parser.c"
        break;
      default:
        break;
  };
  yygoto = yyRuleInfo[yyruleno].lhs;
  yysize = yyRuleInfo[yyruleno].nrhs;
  yypParser->yyidx -= yysize;
  yyact = yy_find_reduce_action(yymsp[-yysize].stateno,(YYCODETYPE)yygoto);
  if( yyact < YYNSTATE ){
#ifdef NDEBUG
    /* If we are not debugging and the reduce action popped at least
    ** one element off the stack, then we can push the new element back
    ** onto the stack here, and skip the stack overflow test in yy_shift().
    ** That gives a significant speed improvement. */
    if( yysize ){
      yypParser->yyidx++;
      yymsp -= yysize-1;
      yymsp->stateno = (YYACTIONTYPE)yyact;
      yymsp->major = (YYCODETYPE)yygoto;
      yymsp->minor = yygotominor;
    }else
#endif
    {
      yy_shift(yypParser,yyact,yygoto,&yygotominor);
    }
  }else{
    assert( yyact == YYNSTATE + YYNRULE + 1 );
    yy_accept(yypParser);
  }
}

/*
** The following code executes when the parse fails
*/
#ifndef YYNOERRORRECOVERY
static void yy_parse_failed(
  yyParser *yypParser           /* The parser */
){
  ParseARG_FETCH;
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sFail!\n",yyTracePrompt);
  }
#endif
  while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser fails */
  ParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}
#endif /* YYNOERRORRECOVERY */

/*
** The following code executes when a syntax error first occurs.
*/
static void yy_syntax_error(
  yyParser *yypParser,           /* The parser */
  int yymajor,                   /* The major type of the error token */
  YYMINORTYPE yyminor            /* The minor type of the error token */
){
  ParseARG_FETCH;
#define TOKEN (yyminor.yy0)
#line 7 "parser.y"
 fprintf(stderr, "42sh: syntax error\n"); parse_error = 1; 
#line 987 "parser.c"
  ParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/*
** The following is executed when the parser accepts
*/
static void yy_accept(
  yyParser *yypParser           /* The parser */
){
  ParseARG_FETCH;
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sAccept!\n",yyTracePrompt);
  }
#endif
  while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser accepts */
  ParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/* The main parser program.
** The first argument is a pointer to a structure obtained from
** "ParseAlloc" which describes the current state of the parser.
** The second argument is the major token number.  The third is
** the minor token.  The fourth optional argument is whatever the
** user wants (and specified in the grammar) and is available for
** use by the action routines.
**
** Inputs:
** <ul>
** <li> A pointer to the parser (an opaque structure.)
** <li> The major token number.
** <li> The minor token number.
** <li> An option argument of a grammar-specified type.
** </ul>
**
** Outputs:
** None.
*/
void Parse(
  void *yyp,                   /* The parser */
  int yymajor,                 /* The major token code number */
  ParseTOKENTYPE yyminor       /* The value for the token */
  ParseARG_PDECL               /* Optional %extra_argument parameter */
){
  YYMINORTYPE yyminorunion;
  int yyact;            /* The parser action. */
  int yyendofinput;     /* True if we are at the end of input */
#ifdef YYERRORSYMBOL
  int yyerrorhit = 0;   /* True if yymajor has invoked an error */
#endif
  yyParser *yypParser;  /* The parser */

  /* (re)initialize the parser, if necessary */
  yypParser = (yyParser*)yyp;
  if( yypParser->yyidx<0 ){
#if YYSTACKDEPTH<=0
    if( yypParser->yystksz <=0 ){
      /*memset(&yyminorunion, 0, sizeof(yyminorunion));*/
      yyminorunion = yyzerominor;
      yyStackOverflow(yypParser, &yyminorunion);
      return;
    }
#endif
    yypParser->yyidx = 0;
    yypParser->yyerrcnt = -1;
    yypParser->yystack[0].stateno = 0;
    yypParser->yystack[0].major = 0;
  }
  yyminorunion.yy0 = yyminor;
  yyendofinput = (yymajor==0);
  ParseARG_STORE;

#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sInput %s\n",yyTracePrompt,yyTokenName[yymajor]);
  }
#endif

  do{
    yyact = yy_find_shift_action(yypParser,(YYCODETYPE)yymajor);
    if( yyact<YYNSTATE ){
      assert( !yyendofinput );  /* Impossible to shift the $ token */
      yy_shift(yypParser,yyact,yymajor,&yyminorunion);
      yypParser->yyerrcnt--;
      yymajor = YYNOCODE;
    }else if( yyact < YYNSTATE + YYNRULE ){
      yy_reduce(yypParser,yyact-YYNSTATE);
    }else{
      assert( yyact == YY_ERROR_ACTION );
#ifdef YYERRORSYMBOL
      int yymx;
#endif
#ifndef NDEBUG
      if( yyTraceFILE ){
        fprintf(yyTraceFILE,"%sSyntax Error!\n",yyTracePrompt);
      }
#endif
#ifdef YYERRORSYMBOL
      /* A syntax error has occurred.
      ** The response to an error depends upon whether or not the
      ** grammar defines an error token "ERROR".  
      **
      ** This is what we do if the grammar does define ERROR:
      **
      **  * Call the %syntax_error function.
      **
      **  * Begin popping the stack until we enter a state where
      **    it is legal to shift the error symbol, then shift
      **    the error symbol.
      **
      **  * Set the error count to three.
      **
      **  * Begin accepting and shifting new tokens.  No new error
      **    processing will occur until three tokens have been
      **    shifted successfully.
      **
      */
      if( yypParser->yyerrcnt<0 ){
        yy_syntax_error(yypParser,yymajor,yyminorunion);
      }
      yymx = yypParser->yystack[yypParser->yyidx].major;
      if( yymx==YYERRORSYMBOL || yyerrorhit ){
#ifndef NDEBUG
        if( yyTraceFILE ){
          fprintf(yyTraceFILE,"%sDiscard input token %s\n",
             yyTracePrompt,yyTokenName[yymajor]);
        }
#endif
        yy_destructor(yypParser, (YYCODETYPE)yymajor,&yyminorunion);
        yymajor = YYNOCODE;
      }else{
         while(
          yypParser->yyidx >= 0 &&
          yymx != YYERRORSYMBOL &&
          (yyact = yy_find_reduce_action(
                        yypParser->yystack[yypParser->yyidx].stateno,
                        YYERRORSYMBOL)) >= YYNSTATE
        ){
          yy_pop_parser_stack(yypParser);
        }
        if( yypParser->yyidx < 0 || yymajor==0 ){
          yy_destructor(yypParser,(YYCODETYPE)yymajor,&yyminorunion);
          yy_parse_failed(yypParser);
          yymajor = YYNOCODE;
        }else if( yymx!=YYERRORSYMBOL ){
          YYMINORTYPE u2;
          u2.YYERRSYMDT = 0;
          yy_shift(yypParser,yyact,YYERRORSYMBOL,&u2);
        }
      }
      yypParser->yyerrcnt = 3;
      yyerrorhit = 1;
#elif defined(YYNOERRORRECOVERY)
      /* If the YYNOERRORRECOVERY macro is defined, then do not attempt to
      ** do any kind of error recovery.  Instead, simply invoke the syntax
      ** error routine and continue going as if nothing had happened.
      **
      ** Applications can set this macro (for example inside %include) if
      ** they intend to abandon the parse upon the first syntax error seen.
      */
      yy_syntax_error(yypParser,yymajor,yyminorunion);
      yy_destructor(yypParser,(YYCODETYPE)yymajor,&yyminorunion);
      yymajor = YYNOCODE;
      
#else  /* YYERRORSYMBOL is not defined */
      /* This is what we do if the grammar does not define ERROR:
      **
      **  * Report an error message, and throw away the input token.
      **
      **  * If the input token is $, then fail the parse.
      **
      ** As before, subsequent error messages are suppressed until
      ** three input tokens have been successfully shifted.
      */
      if( yypParser->yyerrcnt<=0 ){
        yy_syntax_error(yypParser,yymajor,yyminorunion);
      }
      yypParser->yyerrcnt = 3;
      yy_destructor(yypParser,(YYCODETYPE)yymajor,&yyminorunion);
      if( yyendofinput ){
        yy_parse_failed(yypParser);
      }
      yymajor = YYNOCODE;
#endif
    }
  }while( yymajor!=YYNOCODE && yypParser->yyidx>=0 );
  return;
}
