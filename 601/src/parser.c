/*------------------------------------------------------------------
 * LPC Parser Wrapper
 *
 *------------------------------------------------------------------
 * This file serves as a wrapper for lang.c (created from lang.y, which
 * in turn is created from prolang.y), in order to set up some macros
 * required under some operating systems.
 *------------------------------------------------------------------
 */

/*--------------------------------------------------------------------*/

/* Darwin uses the NetBSD variant of byacc, which either requires the
 * parser to be compiled with -Dlint, or the existance of IDSTRING().
 */

#define lint

/*--------------------------------------------------------------------*/

#include "lang.c"

/*====================================================================*/
