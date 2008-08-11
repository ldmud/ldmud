/*------------------------------------------------------------------
 * mkfunc Wrapper
 *
 *------------------------------------------------------------------
 * This file serves as a wrapper for make_func.c (created from make_func.y)
 * in order to set up some macros required under some operating systems.
 *------------------------------------------------------------------
 */

/*--------------------------------------------------------------------*/

/* Darwin uses the NetBSD variant of byacc, which either requires the
 * parser to be compiled with -Dlint, or the existance of IDSTRING().
 */

#define lint

/*--------------------------------------------------------------------*/

#include "make_func.c"

/*====================================================================*/
