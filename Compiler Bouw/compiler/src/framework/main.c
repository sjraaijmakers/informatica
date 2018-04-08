/* ---------------------------------------------------------------------------
 * 
 * SAC Compiler Construction Framework
 * 
 * ---------------------------------------------------------------------------
 * 
 * SAC COPYRIGHT NOTICE, LICENSE, AND DISCLAIMER
 * 
 * (c) Copyright 1994 - 2011 by
 * 
 *   SAC Development Team
 *   SAC Research Foundation
 * 
 *   http://www.sac-home.org
 *   email:info@sac-home.org
 * 
 *   All rights reserved
 * 
 * ---------------------------------------------------------------------------
 * 
 * The SAC compiler construction framework, all accompanying 
 * software and documentation (in the following named this software)
 * is developed by the SAC Development Team (in the following named
 * the developer) which reserves all rights on this software.
 * 
 * Permission to use this software is hereby granted free of charge
 * exclusively for the duration and purpose of the course 
 *   "Compilers and Operating Systems" 
 * of the MSc programme Grid Computing at the University of Amsterdam.
 * Redistribution of the software or any parts thereof as well as any
 * alteration  of the software or any parts thereof other than those 
 * required to use the compiler construction framework for the purpose
 * of the above mentioned course are not permitted.
 * 
 * The developer disclaims all warranties with regard to this software,
 * including all implied warranties of merchantability and fitness.  In no
 * event shall the developer be liable for any special, indirect or
 * consequential damages or any damages whatsoever resulting from loss of
 * use, data, or profits, whether in an action of contract, negligence, or
 * other tortuous action, arising out of or in connection with the use or
 * performance of this software. The entire risk as to the quality and
 * performance of this software is with you. Should this software prove
 * defective, you assume the cost of all servicing, repair, or correction.
 * 
 * ---------------------------------------------------------------------------
 */ 



#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <locale.h>

#include "phase_drivers.h"
#include "options.h"
#include "globals.h"
#include "myglobals.h"
#include "ctinfo.h"
#include "dbug.h"


static
node *SetupCompiler( int argc, char *argv[])
{
  node *syntax_tree = NULL;
  
  DBUG_ENTER("SetupCompiler");

  setlocale( LC_ALL, "en_US");
  CTIinstallInterruptHandlers();
  GLBinitializeGlobals( argc, argv);
  MYGLBinitializeGlobals( );
  OPTcheckOptions( argc, argv);

  DBUG_RETURN( syntax_tree);
}


/*
 *  And now, the main function which triggers the whole compilation.
 */

int main( int argc, char *argv[])
{
  node *syntax_tree;

  DBUG_ENTER("main");
  
  syntax_tree = SetupCompiler( argc, argv);

  syntax_tree = PHDdrive( syntax_tree);
  
  CTIterminateCompilation( syntax_tree);

  DBUG_RETURN( 0);
}


