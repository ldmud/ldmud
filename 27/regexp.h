#ifndef _REGEXP_H_
#define _REGEXP_H_

#include "driver.h"

/*
 * Definitions etc. for regexp(3) routines.
 *
 * Caveat:  this is V8 regexp(3) [actually, a reimplementation thereof],
 * not the System V one.
 */

#define NSUBEXP  50                /* Number of allowed () expressions */

typedef struct regexp {
        char *startp[NSUBEXP];
        char *endp[NSUBEXP];
        char regstart;                /* Internal use only. */
        char reganch;                /* Internal use only. */
        char *regmust;                /* Internal use only. */
        int regmlen;                /* Internal use only. */
        long regalloc;                /* Allocated total length, used by rxcache */
        p_uint refs;            /* Number of refs, used+maintained by rxcache */
        char program[1];        /* Unwarranted chumminess with compiler. */
} regexp;


/*
 * The first byte of the regexp internal "program" is actually this magic
 * number; the start node begins in the second byte.
 */
#define        MAGIC        0234

extern regexp *regcomp PROT((char *expr, int excompat));
extern int regexec PROT((regexp *prog, char *string, char *start));
extern char *regsub PROT((regexp *prog, char *source, char *dest, int n, int quiet));
extern void regerror PROT((char *));  /* defined in ed.c! */

#endif /* _REGEXP_H_ */
