/*---------------------------------------------------------------------------
 * Regular Expressions, implementing the minimal set required by P1003.
 *
 * Based on the code by Henry Spencer, and modified by Ian Phillipps,
 * Joern Rennecke and Lars Duening.
 *
 * The original copyright and authors notice:
 *
 * Copyright (c) 1986 by University of Toronto.
 * Written by Henry Spencer. Not derived from licensed software.
 *
 * Permission is granted to anyone to use this software for any
 * purpose on any computer system, and to redistribute it freely,
 * subject to the following restrictions:
 *
 * 1. The author is not responsible for the consequences of use of
 *    this software, no matter how awful, even if they arise
 *    from defects in it.
 *
 * 2. The origin of this software must not be misrepresented, either
 *    by explicit claim or by omission.
 *
 * 3. Altered versions must be plainly marked as such, and must not
 *    be misrepresented as being the original software.
 *
 * AUTHORS
 *
 *     Mark H. Colburn, NAPS International (mark@jhereg.mn.org)
 *     Henry Spencer, University of Torronto (henry@utzoo.edu)
 *
 * Sponsored by The USENIX Association for public distribution.
 *---------------------------------------------------------------------------
 * Beware that some of this code is subtly aware of the way operator
 * precedence is structured in regular expressions.  Serious changes in
 * regular-expression syntax might require a total rethink.
 * TODO: This could be documented better.
 */

/* Headers */

#include "driver.h"

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "regexp.h"
#include "simulate.h"
#include "xalloc.h"
#ifdef DEBUG
#include "main.h"
#endif

#include "i-eval_cost.h"

/*-------------------------------------------------------------------------*/

/* The "internal use only" fields in regexp.h are present to pass info from
 * compile to execute that permits the execute phase to run lots faster on
 * simple cases.  They are:
 *
 * regstart        char that must begin a match; '\0' if none obvious
 * reganch        is the match anchored (at beginning-of-line only)?
 * regmust        string (pointer into program) that match must include, or NULL
 * regmlen        length of regmust string
 *
 * Regstart and reganch permit very fast decisions on suitable starting points
 * for a match, cutting down the work a lot.  Regmust permits fast rejection
 * of lines that cannot possibly match.  The regmust tests are costly enough
 * that hs_regcomp() supplies a regmust only if the r.e. contains something
 * potentially expensive (at present, the only such thing detected is * or +
 * at the start of the r.e., which can involve a lot of backup).  Regmlen is
 * supplied because the test in hs_regexec() needs it and hs_regcomp() is
 * computing it anyway.
 *
 *----------------------------------------------------------------------------
 *
 * Structure for regexp "program".  This is essentially a linear encoding
 * of a nondeterministic finite-state machine (aka syntax charts or
 * "railroad normal form" in parsing technology).  Each node is an opcode
 * plus a "nxt" pointer, possibly plus an operand.  "Nxt" pointers of
 * all nodes except BRANCH implement concatenation; a "nxt" pointer with
 * a BRANCH on both ends of it is connecting two alternatives.  (Here we
 * have one of the subtle syntax dependencies:  an individual BRANCH (as
 * opposed to a collection of them) is never concatenated with anything
 * because of operator precedence.)  The operand of some types of node is
 * a literal string; for others, it is a node leading into a sub-FSM.  In
 * particular, the operand of a BRANCH node is the first node of the branch.
 * (NB this is *not* a tree structure:  the tail of the branch connects
 * to the thing following the set of BRANCHes.)  The opcodes are:
 */

/* definition    num      opnd?        meaning */
#define END        0   /* no    End of program. */
#define BOL        1   /* no    Match "" at beginning of line. */
#define EOL        2   /* no    Match "" at end of line. */
#define ANY        3   /* no    Match any one character. */
#define ANYOF      4   /* str   Match any character in this string. */
#define ANYBUT     5   /* str   Match any character not in this string. */
#define BRANCH     6   /* node  Match this alternative, or the nxt... */
#define BACK       7   /* no    Match "", "nxt" ptr points backward. */
#define EXACTLY    8   /* str   Match this string. */
#define NOTHING    9   /* no    Match empty string. */
#define STAR      10   /* node  Match this (simple) thing 0 or more times. */
#define PLUS      11   /* node  Match this (simple) thing 1 or more times */
#define WORDSTART 12   /* node  matching a start of a word          */
#define WORDEND   13   /* node  matching an end of a word           */
#define NOTEDGE   14   /* node  matching anything not at word edge  */
#define OPEN      20   /* no    Mark this point in input as start of #n. */
  /* OPEN+1 is number 1, etc. */
#define CLOSE (OPEN+NSUBEXP)    /* no        Analogous to OPEN. */

/* The first byte of the regexp internal "program" is actually this magic
 * number; the start node begins in the second byte.
 */
#define        MAGIC        0234

/* Opcode notes:
 *
 * BRANCH       The set of branches constituting a single choice are hooked
 *              together with their "nxt" pointers, since precedence prevents
 *              anything being concatenated to any individual branch.  The
 *              "nxt" pointer of the last BRANCH in a choice points to the
 *              thing following the whole choice.  This is also where the
 *              final "nxt" pointer of each individual branch points; each
 *              branch starts with the operand node of a BRANCH node.
 *
 * BACK         Normal "nxt" pointers all implicitly point forward; BACK
 *              exists to make loop structures possible.
 *
 * STAR,PLUS    complex '*' and '+', are implemented as circular BRANCH
 *              structures using BACK.  Simple cases (one character per match)
 *              are implemented with STAR for speed and to minimize recursive
 *              plunges.
 *
 * OPEN,CLOSE   ...are numbered at compile time.
 */

/* A node is one char of opcode followed by two chars of "nxt" pointer.
 * "Nxt" pointers are stored as two 8-bit pieces, high order first.  The
 * value is a positive offset from the opcode of the node containing it.
 * An operand, if any, simply follows the node.  (Note that much of the
 * code generation knows about this implicit relationship.)
 *
 * Using two bytes for the "nxt" pointer is vast overkill for most things,
 * but allows patterns to get big without disasters.
 */
#define OP(p)        (*(p))
#define NEXT(p)      (((*((p)+1)&0377)<<8) + (*((p)+2)&0377))
#define OPERAND(p)   ((p) + 3)

/*
 * Utility definitions.
 */

#define SPECIAL  0x100
#define LBRAC    ('('|SPECIAL)
#define RBRAC    (')'|SPECIAL)
#define ASTERIX  ('*'|SPECIAL)
#define CROSS    ('+'|SPECIAL)
#define OR_OP    ('|'|SPECIAL)
#define DOLLAR   ('$'|SPECIAL)
#define DOT      ('.'|SPECIAL)
#define CARET    ('^'|SPECIAL)
#define LSQBRAC  ('['|SPECIAL)
#define RSQBRAC  (']'|SPECIAL)
#define LSHBRAC  ('<'|SPECIAL)
#define RSHBRAC  ('>'|SPECIAL)
#define SLASHB   ('B'|SPECIAL)
#define FAIL(m)   { *ppErrMsg = m; return(NULL); }
#define XFREE(m)  { if (m) xfree(m); m = NULL; }
#define ISMULT(c) ((c) == ASTERIX || (c) == CROSS)
#define META      "^$.[()|+*\\"
#ifndef CHAR_BIT
#define UCHARAT(p) ((int)*(unsigned char *)(p))
#else
#define UCHARAT(p) ((int)*(p)&CHARBIT_MASK)
#endif
#define ISWORDPART(c) isalunum(c)

/*
 * Flags to be passed up and down.
 */
#define HASWIDTH 01   /* Known never to match null string. */
#define SIMPLE   02   /* Simple enough to be STAR or PLUS operand. */
#define SPSTART  04   /* Starts with * */
#define WORST     0   /* Worst case. */

/*-------------------------------------------------------------------------*/
/* Global work variables for hs_regcomp().
 */
static short   *regparse;       /* Input-scan pointer. */
static int      regnpar;        /* () count. */
static unsigned char regdummy;
static unsigned char *regcode;  /* Code-emit pointer; &regdummy = don't. */
static long     regsize;        /* Code size. */
static char   **ppErrMsg;       /* Place where to store an error message */

/* Global variables for hs_regexec().
 */
static unsigned char  *reginput;  /* String-input pointer. */
static unsigned char  *regbol;    /* Beginning of input, for ^ check. */
static char **regstartp;      /* Pointer to startp array. */
static char **regendp;        /* Ditto for endp. */
#ifdef DEBUG
Bool regnarrate = MY_FALSE;   /* TRUE: dump matching operations */
#endif

/*-------------------------------------------------------------------------*/
/* Forward declarations */

static unsigned char *regatom (int *flagp);
static void  regtail (unsigned char *p, unsigned char *val);
static void  regoptail (unsigned char *p, unsigned char *val);
static void  reginsert (char op, unsigned char *opnd);
static unsigned char *reg (Bool paren, int *flagp);

static int regtry(regexp*, char *);
static Bool regmatch(unsigned char *);
static int regrepeat(unsigned char *);

#ifdef DEBUG
static char *regprop(unsigned char *);
#endif

/*-------------------------------------------------------------------------*/
static void
regc (char b)

/* regc - emit (if appropriate) a byte of code
 */

{
    if (regcode != &regdummy)
        *regcode++ = b;
    else
        regsize++;
} /* regc() */

/*-------------------------------------------------------------------------*/
static unsigned char *
regnode (char op)

/* Emit a node, or at least count its size.
 */

{
    unsigned char *ret;
    unsigned char *ptr;

    ret = regcode;
    if (ret == &regdummy)
    {
        regsize += 3;
        return ret;
    }

    ptr = ret;
    *ptr++ = op;
    *ptr++ = '\0';    /* Null "nxt" pointer. */
    *ptr++ = '\0';
    regcode = ptr;

    return ret;
} /* regnode() */

/*-------------------------------------------------------------------------*/
static unsigned char *
regnext (unsigned char *p)

/* regnext - dig the "nxt" pointer out of a node
 */

{
    int offset;

    if (p == &regdummy)
        return NULL;

    offset = NEXT(p);
    if (offset == 0)
        return NULL;

    if (OP(p) == BACK)
        return (p - offset);
    else
        return (p + offset);
} /* regnext() */

/*-------------------------------------------------------------------------*/
static unsigned char *
regpiece (int *flagp)

/* regpiece - something followed by possible [*+]
 *
 * Note that the branching code sequence used for * and + is somewhat optimized:
 * they use the same NOTHING node as both the endmarker for their branch
 * list and the body of the last branch.  It might seem that this node could
 * be dispensed with entirely, but the endmarker role is not redundant.
 */

{
    unsigned char  *ret;
    unsigned char  *nxt;
    short  op;
    int    flags;

    ret = regatom(&flags);
    if (ret == NULL)
        return NULL;

    op = *regparse;
    if (!ISMULT(op))
    {
        *flagp = flags;
        return ret;
    }

    if (!(flags & HASWIDTH))
        FAIL("* operand could be empty");
    *flagp = (op != CROSS) ? (WORST | SPSTART) : (WORST | HASWIDTH);

    if (op == ASTERIX && (flags & SIMPLE))
        reginsert(STAR, ret);
    else if (op == ASTERIX)
    {
        /* Emit x* as (x&|), where & means "self". */
        reginsert(BRANCH, ret);         /* Either x */
        regoptail(ret, regnode(BACK));  /* and loop */
        regoptail(ret, ret);            /* back */
        regtail(ret, regnode(BRANCH));  /* or */
        regtail(ret, regnode(NOTHING)); /* null. */
    }
    else if (op == CROSS && (flags & SIMPLE))
        reginsert(PLUS, ret);
    else if (op == CROSS)
    {
        /* Emit x+ as x(&|), where & means "self" */
        nxt = regnode(BRANCH);          /* Either x */
        regtail(ret, nxt);
        regtail(regnode(BACK), ret);    /* and loop back */
        regtail(nxt, regnode(BRANCH));  /* or */
        regtail(nxt, regnode(NOTHING)); /* null. */
    }
    regparse++;
    if (ISMULT(*regparse))
        FAIL("nested */+");

    return ret;
} /* regpiece() */

/*-------------------------------------------------------------------------*/
static void
regtail (unsigned char *p, unsigned char *val)

/* regtail - set the next-pointer at the end of a node chain
 */

{
    unsigned char *scan;
    unsigned char *temp;
    int   offset;

    if (p == &regdummy)
        return;

    /* Find last node. */
    scan = p;
    for (;;)
    {
        temp = regnext(scan);
        if (temp == NULL)
            break;
        scan = temp;
    }

    if (OP(scan) == BACK)
        offset = scan - val;
    else
        offset = val - scan;
    *(scan + 1) = (offset >> 8) & 0377;
    *(scan + 2) = offset & 0377;
} /* regtail() */

/*-------------------------------------------------------------------------*/
static void
regoptail (unsigned char *p, unsigned char *val)

/* regoptail - regtail on operand of first argument; nop if operandless
 */

{
    /* "Operandless" and "op != BRANCH" are synonymous in practice. */
    if (p == NULL || p == &regdummy || OP(p) != BRANCH)
        return;
    regtail(OPERAND(p), val);
} /* regoptail() */

/*-------------------------------------------------------------------------*/
static unsigned char *
regbranch (int *flagp)

/* One alternative of an | operator.
 *
 * Implements the concatenation operator.
 */

{
    unsigned char *ret;
    unsigned char *chain;
    unsigned char *latest;
    int   flags;

    *flagp = WORST;   /* Tentatively. */

    ret = regnode(BRANCH);
    chain = NULL;
    while (*regparse != '\0' && *regparse != OR_OP && *regparse != RBRAC)
    {
        latest = regpiece(&flags);
        if (latest == NULL)
            return NULL;
        *flagp |= flags & HASWIDTH;
        if (chain == NULL)        /* First piece. */
            *flagp |= flags & SPSTART;
        else
            regtail(chain, latest);
        chain = latest;
    }

    if (chain == NULL)  /* Loop ran zero times. */
        regnode(NOTHING);

    return ret;
} /* regbranch() */

/*-------------------------------------------------------------------------*/
static unsigned char *
regatom (int *flagp)

/* regatom - the lowest level
 *
 * Optimization:  gobbles an entire sequence of ordinary characters so that
 * it can turn them into a single node, which is smaller to store and
 * faster to run.
 */

{
    unsigned char  *ret;
    int    flags;

    *flagp = WORST; /* Tentatively. */

    switch (*regparse++)
    {
    case CARET:
        ret = regnode(BOL);
        break;
    case DOLLAR:
        ret = regnode(EOL);
        break;
    case DOT:
        ret = regnode(ANY);
        *flagp |= HASWIDTH | SIMPLE;
        break;
    case LSHBRAC:
        ret = regnode(WORDSTART);
        break;
    case RSHBRAC:
        ret = regnode(WORDEND);
        break;
    case SLASHB:
        ret = regnode(NOTEDGE);
        break;
    case LSQBRAC:
      {
        int  class;
        int  classend;

        if (*regparse == CARET)
        {
            /* Complement of range. */
            ret = regnode(ANYBUT);
            regparse++;
        }
        else
            ret = regnode(ANYOF);

        if (*regparse == RSQBRAC || *regparse == '-')
            regc(*regparse++);

        while (*regparse != '\0' && *regparse != RSQBRAC)
        {
            if (*regparse == '-')
            {
                regparse++;
                if (*regparse == RSQBRAC || *regparse == '\0')
                    regc('-');
                else
                {
                    class = (CHARBIT_MASK & *(regparse - 2)) + 1;
                    classend = (CHARBIT_MASK & *(regparse));
                    if (class > classend + 1)
                        FAIL("invalid [] range");
                    for (; class <= classend; class++)
                        regc(class);
                    regparse++;
                }
            }
            else
                regc(*regparse++);
        }
        regc('\0');
        if (*regparse != RSQBRAC)
            FAIL("unmatched []");
        regparse++;
        *flagp |= HASWIDTH | SIMPLE;
        break;
      }

    case LBRAC:
        ret = reg(MY_TRUE, &flags);
        if (ret == NULL)
            return NULL;
        *flagp |= flags & (HASWIDTH | SPSTART);
        break;

    case '\0':
    case OR_OP:
    case RBRAC:
        FAIL("internal urp");        /* Supposed to be caught earlier. */
        break;

    case ASTERIX:
        FAIL("* follows nothing");
        break;

    case CROSS:
        FAIL("+ follows nothing");
        break;

    default:
      {
        int   len;
        short ender;

        regparse--;
        for ( len = 0
            ;    regparse[len]
              && !(regparse[len]&SPECIAL)
              && regparse[len] != RSQBRAC
            ; len++) NOOP;

        if (len <= 0)
        {
            if (len == 0 && regparse[len] == RSQBRAC)
            {
                FAIL("unmatched []");
            }
            FAIL("internal disaster");
        }
        ender = *(regparse + len);
        if (len > 1 && ISMULT(ender))
           len--;                /* Back off clear of * operand. */

        *flagp |= HASWIDTH;
        if (len == 1)
            *flagp |= SIMPLE;
        ret = regnode(EXACTLY);
        while (len > 0)
        {
            regc(*regparse++);
            len--;
        }
        regc('\0');
        break;
      }
    }

    return ret;
} /* regatom() */

/*-------------------------------------------------------------------------*/
static void
reginsert (char op, unsigned char *opnd)

/* reginsert - insert an operator in front of already-emitted operand
 *
 * Means relocating the operand.
 */

{
    unsigned char *src;
    unsigned char *dst;
    unsigned char *place;

    if (regcode == &regdummy)
    {
        regsize += 3;
        return;
    }
    src = regcode;
    regcode += 3;
    dst = regcode;
    while (src > opnd)
        *--dst = *--src;

    place = opnd;     /* Op node, where operand used to be. */
    *place++ = op;
    *place++ = '\0';
    *place++ = '\0';
} /* reginsert() */

/*-------------------------------------------------------------------------*/
static unsigned char *
reg (Bool paren, int *flagp)

/* Compile a top-level regular expression, ie. the main body or parenthesized
 * thing (<paren> is true in the latter case).
 *
 * Caller must absorb opening parenthesis.
 *
 * Combining parenthesis handling with the base level of regular expression
 * is a trifle forced, but the need to tie the tails of the branches to what
 * follows makes it hard to avoid.
 */

{
    unsigned char  *ret;
    unsigned char  *br;
    unsigned char  *ender;
    int    parno = 0;
    int    flags;

    *flagp = HASWIDTH;  /* Tentatively. */

    /* Make an OPEN node, if parenthesized. */
    if (paren)
    {
        if (regnpar >= NSUBEXP)
            FAIL("too many ()");
        parno = regnpar;
        regnpar++;
        ret = regnode(OPEN + parno);
    }
    else
        ret = NULL;

    /* Pick up the branches, linking them together. */
    br = regbranch(&flags);
    if (br == NULL)
        return NULL;
    if (ret != NULL)
        regtail(ret, br);  /* OPEN -> first. */
    else
        ret = br;

    if (!(flags & HASWIDTH))
        *flagp &= ~HASWIDTH;

    *flagp |= flags & SPSTART;
    while (*regparse == OR_OP)
    {
        regparse++;
        br = regbranch(&flags);
        if (br == NULL)
            return NULL;
        regtail(ret, br);        /* BRANCH -> BRANCH. */
        if (!(flags & HASWIDTH))
            *flagp &= ~HASWIDTH;
        *flagp |= flags & SPSTART;
    }

    /* Make a closing node, and hook it on the end. */
    ender = regnode((paren) ? CLOSE + parno : END);
    regtail(ret, ender);

    /* Hook the tails of the branches to the closing node. */
    for (br = ret; br != NULL; br = regnext(br))
        regoptail(br, ender);

    /* Check for proper termination. */
    if (paren && *regparse++ != RBRAC)
    {
        FAIL("unmatched ()");
    }
    else if (!paren && *regparse != '\0')
    {
        if (*regparse == RBRAC)
        {
            FAIL("unmatched ()");
        }
        else
            FAIL("junk on end");/* "Can't happen". */
        /* NOTREACHED */
    }
    return (ret);
} /* reg() */

/*-------------------------------------------------------------------------*/
regexp *
hs_regcomp (unsigned char *expr, Bool excompat
           , char ** errmsg, int * erridx)

/* Compile a regular expression in <expr> into internal code; if <excompat>
 * is TRUE; \( \) operators like in Unix ex are allowed. Result is the
 * regexp structure with the program.
 *
 * If an error occured during the compilation, *<errmsg> is set to the
 * static string with the error message, *<erridx> to the approximate
 * position in <expr> where the error happened, and the function returns NULL.
 *
 * We can't allocate space until we know how big the compiled form will be,
 * but we can't compile it (and thus know how big it is) until we've got a
 * place to put the code.  So we cheat:  we compile it twice, once with code
 * generation turned off and size counting turned on, and once "for real".
 * This also means that we don't allocate space until we are sure that the
 * thing really will compile successfully, and we never have to move the
 * code and thus invalidate pointers into it.  (Note that it has to be in
 * one piece because free() must be able to free it all.)
 *
 * Beware that the optimization-preparation code in here knows about some
 * of the structure of the compiled regexp.
 */

{
    unsigned char   *scan;
    unsigned char   *longest;
    int     len;
    int     flags;
    short  *dest, c;
    regexp *rc;       /* Holds the result to be returned */
    regexp  *r = NULL;       /* The resulting regexp */
    short   *expr2 = NULL;   /* Expression currently compiled */

    ppErrMsg = errmsg;
    *ppErrMsg = NULL;
    *erridx = 0;

#ifdef DEBUG
    /* Check the invariant for the global vars */

    if (r != NULL)
    {
        debug_message("DEBUG: (regexp:regcomp) r == %p\n", r);
        fprintf(stderr, "DEBUG: (regexp:regcomp) r == %p\n", r);
        r = NULL;
    }
    if (expr2 != NULL)
    {
        debug_message("DEBUG: (regexp:regcomp) expr2 == %p\n", expr2);
        fprintf(stderr, "DEBUG: (regexp:regcomp) expr2 == %p\n", expr2);
        expr2 = NULL;
    }
#endif

    if (expr == NULL)
        FAIL("NULL argument");

    /* Get some memory, maybe slightly more than needed */
    expr2 = xalloc( (strlen((char *)expr)+1) * (sizeof(short[8])/sizeof(char[8])) );
    if (expr2 == NULL)
        FAIL("out of space");

    /* Copy the expression into expr2 for compilation, marking
     * the special characters.
     */
    for (scan = expr, dest = expr2; '\0' != (c = *scan++); )
    {
        switch (c)
        {
        case '(':
        case ')':
            *dest++ = excompat ? c : c | SPECIAL;
            break;
        case '.':
        case '*':
        case '+':
        case '|':
        case '$':
        case '^':
        case '[':
        case ']':
            *dest++ =  c | SPECIAL;
            break;
        case '\\':
            switch ( c = *scan++ )
            {
            case '(':
            case ')':
                *dest++ = excompat ? c | SPECIAL : c;
                break;
            case '<':
            case '>':
            case 'B':
                *dest++ = c | SPECIAL;
                break;
            case '{':
            case '}':
                XFREE(expr2);
                FAIL("sorry, unimplemented operator {}");
            case 'b': *dest++ = '\b'; break;
            case 't': *dest++ = '\t'; break;
            case 'r': *dest++ = '\r'; break;
            case '\0': scan--;
            default:
                *dest++ = c;
            }
            break;
        default:
            *dest++ = c;
        }
    }
    *dest = '\0';

    /* First pass: determine size, legality. */
    regparse = expr2;
    regnpar = 1;
    regsize = 0L;
    regcode = &regdummy;
    regc(MAGIC);
    if (reg(MY_FALSE, &flags) == NULL)
    {
        *erridx = regparse - expr2;
        XFREE(expr2);
        return NULL;
    }

    /* Small enough for pointer-storage convention? */
    if (regsize >= 32767L)        /* TODO: Probably could be 65535L. */
    {
        /* Probably could be any amount of memory we could spare... */
        *erridx = regparse - expr2;
        XFREE(expr2);
        FAIL("regexp too big");
    }

    /* Allocate space. */
    r = xalloc(sizeof(regexp) + (size_t) regsize);
    if (r == NULL)
    {
        *erridx = regparse - expr2;
        XFREE(expr2);
        FAIL("out of space");
    }
    r->regalloc = sizeof(regexp)+regsize;

    /* Second pass: emit code. */
    regparse = expr2;
    regnpar = 1;
    regcode = r->program;
    regc(MAGIC);
    if (reg(MY_FALSE, &flags) == NULL)
    {
        *erridx = regparse - expr2;
        XFREE(r);
        XFREE(expr2);
        return NULL;
    }

    /* Dig out information for optimizations. */
    r->regstart = '\0';  /* Worst-case defaults. */
    r->reganch = 0;
    r->regmust = NULL;
    r->regmlen = 0;
    scan = r->program + 1; /* First BRANCH. */
    if (OP(regnext(scan)) == END)
    {
        /* Only one top-level choice. */
        scan = OPERAND(scan);

        /* Starting-point info. */
        if (OP(scan) == EXACTLY)
            r->regstart = *OPERAND(scan);
        else if (OP(scan) == BOL)
            r->reganch++;

        /* If there's something expensive in the r.e., find the longest
         * literal string that must appear and make it the regmust.  Resolve
         * ties in favor of later strings, since the regstart check works
         * with the beginning of the r.e. and avoiding duplication
         * strengthens checking.  Not a strong reason, but sufficient in the
         * absence of others.
         */
        if (flags & SPSTART)
        {
            longest = NULL;
            len = 0;
            for (; scan != NULL; scan = regnext(scan))
                if (OP(scan) == EXACTLY && (int)strlen((char *)OPERAND(scan)) >= len)
                {
                    longest = OPERAND(scan);
                    len = strlen((char *)OPERAND(scan));
                }
            r->regmust = longest;
            r->regmlen = len;
        }
    }

    /* Done: reset the global vars and return the result */
    XFREE(expr2);
    rc = r; r = NULL;
    return rc;
} /* hs_regcomp() */

/*-------------------------------------------------------------------------*/
int
hs_regexec (regexp *prog, char *string, char *start)

/* Match the regexp <prog> against the <string> starting at the
 * position <start>.
 *
 * Return one of the RE_ERROR_ codes depending on the result.
 */

{
    int rc;
    char  *s;

    /* Be paranoid... */
    if (prog == NULL || string == NULL)
        return RE_ERROR_NULL;

    /* Check validity of program. */
    if (UCHARAT(prog->program) != MAGIC)
    {
        return RE_ERROR_CORRUPT;
    }

    /* If there is a "must appear" string, look for it. */
    if (prog->regmust != NULL)
    {
        s = string;
        while ((s = strchr(s, prog->regmust[0])) != NULL)
        {
            if (strncmp(s, (char *)(prog->regmust), prog->regmlen) == 0)
                break;                /* Found it. */
            s++;
        }
        if (s == NULL)                /* Not present. */
            return RE_NOMATCH;
    }
    /* Mark beginning of line for ^ . */
    regbol = (unsigned char *)start;

    /* Simplest case:  anchored match need be tried only once. */
    if (prog->reganch)
        return regtry(prog, string);

    /* Messy cases:  unanchored match. */
    s = string;
    if (prog->regstart != '\0')
        /* We know what char it must start with. */
        while ((s = strchr(s, prog->regstart)) != NULL)
        {
            rc = regtry(prog,s);
            if (rc != RE_NOMATCH)
                return rc;
            s++;
        }
    else
        /* We don't -- general case. */
        do {
            rc = regtry(prog,s);
            if (rc != RE_NOMATCH)
                return rc;
        } while (*s++ != '\0');

    /* Failure. */
    return RE_NOMATCH;
} /* hs_regexec() */

/*-------------------------------------------------------------------------*/
static int
regtry (regexp *prog, char *string)

/* regtry - try match at specific point
 */

{
    int    i, rc;
    char **sp;
    char **ep;

    reginput = (unsigned char *)string;
    regstartp = prog->startp;
    regendp = prog->endp;

    sp = prog->startp;
    ep = prog->endp;
    for (i = NSUBEXP; i > 0; i--)
    {
        *sp++ = NULL;
        *ep++ = NULL;
    }
    rc = regmatch(prog->program + 1);
    if (rc == RE_MATCH)
    {
        prog->startp[0] = string;
        prog->endp[0] = (char *)reginput;
        return RE_MATCH;
    }
    else
        return rc;
} /* regtry() */

/*-------------------------------------------------------------------------*/
static Bool
regmatch (unsigned char *prog)

/* regmatch - main matching routine
 *
 * Conceptually the strategy is simple:  check to see whether the current
 * node matches, call self recursively to see whether the rest matches,
 * and then act accordingly.  In practice we make some effort to avoid
 * recursion, in particular by going through "ordinary" nodes (that don't
 * need to know whether the rest of the match failed) by a loop instead of
 * by recursion.
 */

{
    unsigned char  *scan;      /* Current node. */
    unsigned char  *nxt;       /* nxt node. */

    scan = prog;

#ifdef DEBUG
    if (scan != NULL && regnarrate)
        fprintf(stderr, "%s(\n", regprop(scan));
#endif

    while (scan != NULL)
    {
#ifdef DEBUG
        if (regnarrate)
            fprintf(stderr, "%s...\n", regprop(scan));
#endif
        nxt = regnext(scan);

        switch (OP(scan))
        {
        case BOL:
            if (reginput != regbol)
                return RE_NOMATCH;
            break;
        case EOL:
            if (*reginput != '\0')
                return RE_NOMATCH;
            break;
        case ANY:
            if (*reginput == '\0')
                return RE_NOMATCH;
            reginput++;
            break;
        case WORDSTART:
            if (reginput == regbol)
                break;
            if (*reginput == '\0'
             || ISWORDPART( *(reginput-1) ) || !ISWORDPART( *reginput ) )
                return RE_NOMATCH;
            break;
        case WORDEND:
            if (*reginput == '\0')
                break;
            if ( reginput == regbol
             || !ISWORDPART( *(reginput-1) ) || ISWORDPART( *reginput ) )
                return RE_NOMATCH;
            break;
        case NOTEDGE:
            if (reginput == regbol)
            {
                if (ISWORDPART(*reginput))
                    return RE_NOMATCH;
                break;
            }
            if ( ISWORDPART( *(reginput-1) ) != ISWORDPART( *reginput ) )
                return RE_NOMATCH;
            break;
        case EXACTLY:
          {
            int    len;
            unsigned char  *opnd;

            opnd = OPERAND(scan);
            /* Inline the first character, for speed. */
            if (*opnd != *reginput)
                return RE_NOMATCH;
            len = strlen((char *)opnd);
            if (len > 1 && strncmp((char *)opnd, (char *)reginput, len) != 0)
                return RE_NOMATCH;
            reginput += len;
            break;
          }
        case ANYOF:
            if (*reginput == '\0'
             || strchr((char *)OPERAND(scan), *reginput) == NULL)
                return RE_NOMATCH;
            reginput++;
            break;
        case ANYBUT:
            if (*reginput == '\0'
             || strchr((char *)OPERAND(scan), *reginput) != NULL)
                return RE_NOMATCH;
            reginput++;
            break;
        case NOTHING:
            break;
        case BACK:
            break;
        case BRANCH:
          {
            unsigned char  *save;

            if (OP(nxt) != BRANCH)        /* No choice. */
                nxt = OPERAND(scan);        /* Avoid recursion. */
            else
            {
                do {
                    int rc;
                    save = reginput;
                    rc = regmatch(OPERAND(scan));
                    if (rc != RE_NOMATCH)
                        return rc;
                    reginput = save;
                    scan = regnext(scan);
                } while (scan != NULL && OP(scan) == BRANCH);
                return RE_NOMATCH;
                /* NOTREACHED */
            }
            break;
          }
        case STAR:
        case PLUS:
          {
            unsigned char   nextch;
            int    no;
            unsigned char  *save;
            int    minimum;

            /* Lookahead to avoid useless match attempts when we know
             * what character comes next.
             */
            nextch = '\0';
            if (OP(nxt) == EXACTLY)
                nextch = *OPERAND(nxt);
            minimum = (OP(scan) == STAR) ? 0 : 1;
            save = reginput;
            no = regrepeat(OPERAND(scan));
            while (no >= minimum)
            {
                /* If it could work, try it. */
                if (nextch == '\0' || *reginput == nextch)
                {
                    int rc;
                    rc = regmatch(nxt);
                    if (rc != RE_NOMATCH)
                        return rc;
                }
                /* Couldn't or didn't -- back up. */
                no--;
                reginput = save + no;
                if (add_eval_cost(1))
                    return RE_ERROR_BACKTRACK;
            }
            return RE_NOMATCH;
            break;
          }
        case END:
            return MY_TRUE;                /* Success! */
            break;
        default:
            if (OP(scan) > OPEN && OP(scan) < OPEN+NSUBEXP)
            {
                int    no, rc;
                unsigned char  *save;

                no = OP(scan) - OPEN;
                save = reginput;

                rc = regmatch(nxt);
                if (rc == RE_MATCH)
                {
                    /* Don't set startp if some later invocation of the same
                     * parentheses already has.
                     */
                    if (regstartp[no] == NULL)
                        regstartp[no] = (char *)save;
                    return RE_MATCH;
                }
                else
                    return rc;
            }
            else if (OP(scan) > CLOSE && OP(scan) < CLOSE+NSUBEXP)
            {
                int    no, rc;
                unsigned char  *save;

                no = OP(scan) - CLOSE;
                save = reginput;

                rc = regmatch(nxt);
                if (rc == RE_MATCH)
                {
                    /*
                     * Don't set endp if some later invocation of the same
                     * parentheses already has.
                     */
                    if (regendp[no] == NULL)
                        regendp[no] = (char *)save;
                    return RE_MATCH;
                } else
                    return rc;
            }
            else
            {
                return RE_ERROR_CORRUPT;
            }
            break;
        }

        scan = nxt;
    }

    /* We get here only if there's trouble -- normally "case END" is the
     * terminating point.
     */
    return RE_ERROR_CORRUPT;
} /* regmatch() */

/*-------------------------------------------------------------------------*/
static int
regrepeat (unsigned char *p)

/* regrepeat - repeatedly match something simple, report how many
 */

{
    int    count = 0;
    unsigned char  *scan;
    unsigned char  *opnd;

    scan = reginput;
    opnd = OPERAND(p);
    switch (OP(p))
    {
    case ANY:
        count = strlen((char *)scan);
        scan += count;
        break;
    case EXACTLY:
        while (*opnd == *scan)
        {
            count++;
            scan++;
        }
        break;
    case ANYOF:
        while (*scan != '\0' && strchr((char *)opnd, *scan) != NULL)
        {
            count++;
            scan++;
        }
        break;
    case ANYBUT:
        while (*scan != '\0' && strchr((char *)opnd, *scan) == NULL)
        {
            count++;
            scan++;
        }
        break;
    default:                        /* Oh dear.  Called inappropriately. */
        fatal("regexp: internal foulup\n");
        count = 0;                /* Best compromise. */
        break;
    }
    reginput = scan;

    return count;
} /* regrepeat() */

/*-------------------------------------------------------------------------*/
#ifdef DEBUG

/*-------------------------------------------------------------------------*/
static char *
regprop (unsigned char *op)

/* regprop - printable representation of opcode
 */

{
    char  *p = NULL;
    static char     buf[50];

    strcpy(buf, ":");

    switch (OP(op))
    {
    case BOL:
        p = "BOL";
        break;
    case EOL:
        p = "EOL";
        break;
    case ANY:
        p = "ANY";
        break;
    case ANYOF:
        p = "ANYOF";
        break;
    case ANYBUT:
        p = "ANYBUT";
        break;
    case BRANCH:
        p = "BRANCH";
        break;
    case EXACTLY:
        p = "EXACTLY";
        break;
    case NOTHING:
        p = "NOTHING";
        break;
    case BACK:
        p = "BACK";
        break;
    case END:
        p = "END";
        break;
    case STAR:
        p = "STAR";
        break;
    case PLUS:
        p = "PLUS";
        break;
    case WORDSTART:
        p = "WORDSTART";
        break;
    case WORDEND:
        p = "WORDEND";
        break;
    case NOTEDGE:
        p = "NOTEDGE";
        break;
    default:
        if (OP(op) > OPEN && OP(op) < OPEN+NSUBEXP)
        {
            sprintf(buf + strlen(buf), "OPEN%d", OP(op) - OPEN);
            p = (char *)NULL;
        }
        else if (OP(op) > CLOSE && OP(op) < CLOSE+NSUBEXP)
        {
            sprintf(buf + strlen(buf), "CLOSE%d", OP(op) - CLOSE);
            p = (char *)NULL;
        }
        else
            errorf("corrupted opcode\n");
        break;
    }
    if (p != (char *)NULL)
        strcat(buf, p);
    return buf;
} /* regprop() */

/*-------------------------------------------------------------------------*/
void
hs_regdump (regexp *rg)

/* regdump - dump a regexp onto stdout in vaguely comprehensible form
 */

{
    unsigned char  *s;
    unsigned char   op = EXACTLY;        /* Arbitrary non-END op. */
    unsigned char  *nxt;

    s = rg->program + 1;
    while (op != END)   /* While that wasn't END last time... */
    {
        op = OP(s);
        printf("%2ld%s", (long)(s - rg->program), regprop(s));        /* Where, what. */
        nxt = regnext(s);
        if (nxt == NULL)        /* nxt ptr. */
            printf("(0)");
        else
            printf("(%ld)", (long)(s - rg->program) + (nxt - s));
        s += 3;
        if (op == ANYOF || op == ANYBUT || op == EXACTLY)
        {
            /* Literal string, where present. */
            while (*s != '\0')
            {
                putchar(*s);
                s++;
            }
            s++;
        }
        putchar('\n');
    }

    /* Header fields of interest. */
    if (rg->regstart != '\0')
        printf("start `%c' ", rg->regstart);
    if (rg->reganch)
        printf("anchored ");
    if (rg->regmust != NULL)
        printf("must have \"%s\"", rg->regmust);
    printf("\n");
} /* hs_regdump() */

#endif /* DEBUG */

/*-------------------------------------------------------------------------*/
#if 0
/*
 * The following is provided for those people who do not have strcspn() in
 * their C libraries.  They should get off their butts and do something
 * about it; at least one public-domain implementation of those (highly
 * useful) string routines has been published on Usenet.
 */
/*
 * strcspn - find length of initial segment of s1 consisting entirely
 * of characters not from s2
 */

#ifdef __STDC__

static int strcspn(char *s1, char *s2)

#else

static int strcspn(s1, s2)
char           *s1;
char           *s2;

#endif
{
    register char  *scan1;
    register char  *scan2;
    register int    count;

    count = 0;
    for (scan1 = s1; *scan1 != '\0'; scan1++) {
        for (scan2 = s2; *scan2 != '\0';)        /* ++ moved down. */
            if (*scan1 == *scan2++)
                return (count);
        count++;
    }
    return (count);
}
#endif

/***************************************************************************/
