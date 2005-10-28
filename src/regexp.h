#ifndef REGEXP_H_
#define REGEXP_H_

#include "driver.h"

#include "pkg-pcre.h" /* for RE_ERROR_BACKTRACK */

#define NSUBEXP  50
  /* Number of allowed () expressions.
   */

/* --- struct regexp: regular expression basis ---
 *
 * This structure is used to hold a compiled regular expression as well
 * intermediate results between two match calls.
 *
 * The structure is allocated big enough to fit the whole program
 * starting with .program.
 */

typedef struct regexp
{
    char *startp[NSUBEXP];
    char *endp[NSUBEXP];
      /* After a match, the start and endpointers for the matched
       * () expressions.
       */
    unsigned char regstart;
      /* Internal use: char that must begin a match, '\0' if non obvious
       */
    unsigned char reganch;
      /* Internal use: is the match anchored (at beginning-of-line only)?
       */
    unsigned char *regmust;
      /* Internal use: string (pointer into program) that match must
       * include, or NULL.
       */
    int regmlen;
      /* Internal  use: length of regmust.
       */
    long regalloc;          /* Allocated total length, used by rxcache */
    unsigned char program[1];  /* The compiled regexp. */
} regexp;

/* --- Prototypes --- */
#ifdef DEBUG
extern Bool regnarrate;
void hs_regdump(regexp *rg);
#endif

extern regexp *hs_regcomp(unsigned char *expr, Bool excompat, char ** errmsg, int * erridx);
extern int hs_regexec(regexp *prog, char *string, char *start);

/* Return codes from regexec() */

#define RE_MATCH            (1)
#define RE_NOMATCH          (0)
#define RE_ERROR_NULL       (-1)
#define RE_ERROR_CORRUPT    (-2)
/* RE_ERROR_BACKTRACK is defined in pkg-pcre.h */

#endif /* REGEXP_H_ */
