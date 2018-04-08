
/**
 * @file print.h
 *
 * Functions to print node structures
 *
 */

#ifndef _SAC_PRT_NODE_H_
#define _SAC_PRT_NODE_H_

#include "types.h"

extern void print_indentation(int indentation);

node *PRTstmts (node * arg_node, info * arg_info);
node *PRTmodule (node * arg_node, info * arg_info);
node *PRTassign (node * arg_node, info * arg_info);
node *PRTvar (node * arg_node, info * arg_info);
node *PRTvarlet (node * arg_node, info * arg_info);
node *PRTbinop (node * arg_node, info * arg_info);
node *PRTfloat (node * arg_node, info * arg_info);
node *PRTint (node * arg_node, info * arg_info);
node *PRTbool (node * arg_node, info * arg_info);
node *PRTerror (node * arg_node, info * arg_info);

/* ADDED */
node *PRTprogram(node *arg_node, info *arg_info);
node *PRTdeclarations(node *arg_node, info *arg_info);
node *PRTdeclaration(node *arg_node, info *arg_info);

node *PRTsymboltable(node *arg_node, info *arg_info);
node *PRTsymboltableentries(node *arg_node, info *arg_info);
node *PRTsymboltableentry(node *arg_node, info *arg_info);

node *PRTid(node *arg_node, info *arg_info);
node *PRTfundec(node *arg_node, info *arg_info);
node *PRTfundef(node *arg_node, info *arg_info);
node *PRTfunheader(node *arg_node, info *arg_info);
node *PRTfunbody(node *arg_node, info *arg_info);

node *PRTglobaldec(node *arg_node, info *arg_info);
node *PRTglobaldef(node *arg_node, info *arg_info);
node *PRTparam(node *arg_node, info *arg_info);

node *PRTternary(node *arg_node, info *arg_info);
node *PRTvardec(node *arg_node, info *arg_info);

node *PRTstatements(node *arg_node, info *arg_info);
node *PRTstatement(node *arg_node, info *arg_info);

node *PRTarrexpr(node *arg_node, info *arg_info);

node *PRTifelse(node *arg_node, info *arg_info);
node *PRTfor(node *arg_node, info *arg_info);
node *PRTwhile(node *arg_node, info *arg_info);
node *PRTdowhile(node *arg_node, info *arg_info);

node *PRTreturn(node *arg_node, info *arg_info);
node *PRTexprs(node *arg_node, info *arg_info);

node *PRTmonop(node *arg_node, info *arg_info);
node *PRTcast(node *arg_node, info *arg_info);

node *PRTfuncall(node *arg_node, info *arg_info);

node *PRTblock(node *arg_node, info *arg_info);

node *PRTdoPrint( node *syntaxtree);

#endif /* _SAC_PRT_NODE_H_ */
