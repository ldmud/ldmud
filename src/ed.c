/*---------------------------------------------------------------------------
 * Gamedriver - ed compatible editor
 *
 * Authors: Brian Beattie, Kees Bot, and others
 *
 * Copyright 1987 Brian Beattie Rights Reserved.
 * Permission to copy or distribute granted under the following conditions:
 *   1). No charge may be made other than reasonable charges for reproduction.
 *   2). This notice must remain intact.
 *   3). No further restrictions may be added.
 *   4). Except meaningless ones.
 *
 * TurboC mods and cleanup 8/17/88 RAMontante.
 *
 * Regexp stuff replaced with Spencerian version, sundry other bugfix+speedups
 *   by Ian Phillipps. Regexps replaced again to transparently use PCRE
 *   instead of Spencer's regexps by Lars Duening.
 *
 * help files and '^' added by Ted Gaunt.
 * Tab conversion added by Andreas Klauer.
 *
 * Original indentation algorithm replaced with adapted version from DGD
 *   editor by Dworkin (Felix A. Croes), 920510.
 *   The code may be used freely as long as its author and its origin from
 *   DGD are clearly stated.
 *---------------------------------------------------------------------------
 * Per interactive user there can be one active ed session; stored as
 * a pointer to the ed_buffer in the interactive structure. The lines of
 * the text are stored linewise in a double-linked ring.
 *
 * TODO: Make it possible to attach an editor to any object, and let
 * TODO:: the editor communicate via efuns - no direct io.
 * TODO: In the long run, maybe only offer primitives and let the command
 * TODO:: interpretation be done by the mudlib. This makes it easier
 * TODO:: to write non-ed like editors.
 *---------------------------------------------------------------------------
 */

#define ED_VERSION 6        /* used only in outputs for id */

#include "driver.h"
#include "typedefs.h"

#include <stdio.h>
#include <ctype.h>

#include "ed.h"
#include "actions.h"
#include "comm.h"
#include "filestat.h"
#include "gcollect.h"
#include "interpret.h"
#include "lex.h"
#include "main.h"
#include "mregex.h"
#include "mstrings.h"
#include "object.h"
#include "simulate.h"
#include "stdstrings.h"
#include "svalue.h"
#include "xalloc.h"

#include "../mudlib/sys/regexp.h"

/*-------------------------------------------------------------------------*/

/* Default TAB size */
# define DEFAULT_TABSIZE   8


/* #defines for non-printing ASCII characters */

#define NUL  0x00        /* ^@ */
#define EOS  0x00        /* end of string */
#define SOH  0x01        /* ^A */
#define STX  0x02        /* ^B */
#define ETX  0x03        /* ^C */
#define EOT  0x04        /* ^D */
#define ENQ  0x05        /* ^E */
#define ACK  0x06        /* ^F */
#define BEL  0x07        /* ^G */
#define BS   0x08        /* ^H */
#define HT   0x09        /* ^I */
#define LF   0x0a        /* ^J */
#define NL   '\n'
#define VT   0x0b        /* ^K */
#define FF   0x0c        /* ^L */
#define CR   0x0d        /* ^M */
#define SO   0x0e        /* ^N */
#define SI   0x0f        /* ^O */
#define DLE  0x10        /* ^P */
#define DC1  0x11        /* ^Q */
#define DC2  0x12        /* ^R */
#define DC3  0x13        /* ^S */
#define DC4  0x14        /* ^T */
#define NAK  0x15        /* ^U */
#define SYN  0x16        /* ^V */
#define ETB  0x17        /* ^W */
#define CAN  0x18        /* ^X */
#define EM   0x19        /* ^Y */
#define SUB  0x1a        /* ^Z */
#define ESC  0x1b        /* ^[ */
#define FS   0x1c        /* ^\ */
#define GS   0x1d        /* ^] */
/*#define RS   0x1e           ^^ */
#define US   0x1f        /* ^_ */
#define SP   0x20        /* space */
#define DEL  0x7f        /* DEL*/
#define ESCAPE  '\\'


/* Characters used in the indentation code */

#define TAB '\t'
#define LB  '{'
#define RB  '}'
#define LC  '('
#define RC  ')'
#define LS  '['
#define RS  ']'
#define PP  '\"'
#define EOL '\0'


/*-------------------------------------------------------------------------*/

#ifndef FALSE
#    define TRUE   1
#    define FALSE  0
#endif


/* Return codes */

#define ED_OK     FALSE
#define ERR       -2
#undef  FATAL     /* (ERR-1) */
#define CHANGED   (ERR-2)
#define SET_FAIL  (ERR-3)
#define SUB_FAIL  (ERR-4)
#define MEM_FAIL  (ERR-5)


/* Sizes and limits */

#define BUFFER_SIZE  2048   /* stream-buffer size, a multible of a disk block */

#define MAXLINE  2048         /* max number of chars per line */
#define MAXPAT    256         /* max number of chars per replacement pattern */
#define MAXFNAME  MAXPATHLEN  /* max file name size */


/*-------------------------------------------------------------------------*/

/* The whole text is stored linewise in a double-linked ring(!) of 'line'
 * structures. The text of every line is appended to the end of its
 * structure.
 */

struct line
{
    int          l_stat;    /* Status of the line */
    struct line *l_prev;    /* previous line */
    struct line *l_next;    /* next line */
    char         l_buff[1]; /* the line's text */
};
typedef struct line LINE;

/* Bitflags of line.l_stat */

#define LINFREE  0x01       /* entry not in use */
#define LGLOB    0x02       /* line marked global, e.g. by a match-pattern */

/*-------------------------------------------------------------------------*/

/* The ed_buffer holds all the information for one editor session.
 */
typedef struct ed_buffer_s ed_buffer_t;

struct ed_buffer_s
{
    input_t input;             /* It's an input handler. */

    Bool    diag;              /* True: diagnostic-output?*/
    Bool    truncflg;          /* True: truncate long line flag
                                * Note: not used anywhere */
    int     nonascii;          /* count of non-ascii chars read */
    int     nullchar;          /* count of null chars read */
    int     truncated;         /* count of lines truncated */
    string_t *fname;           /* name of the file */
    Bool    fchanged;          /* True: file-changed */
    int     nofname;
    int     mark['z'-'a'+1];
    regexp_t *oldpat;
    LINE    Line0;             /* anchor of the line buffer */
    int     CurLn;             /* number of current line */
    LINE   *CurPtr;            /* CurLn and CurPtr must be kept in sync */
    int     LastLn;            /* number of last line */
    int     Line1;             /* command linerange: first line */
    int     Line2;             /* command linerange: last line */
    int     nlines;
    int     flags;             /* flags */
    Bool    appending;
    int     moring;            /* used for the wait line of help */
    int     shiftwidth;        /* shiftwidth, in the range 0..15 */
    int     leading_blanks;    /* Current number of leading blanks when
                                  using autoindentation. */
    int     cur_autoindent;
    int     lastcmd;           /* The last command */
    string_t *exit_fn;         /* Function to be called when player exits */
                               /* TODO: Make this a callback */
    object_t *exit_ob;         /* Object holding <exit_fn> */
};

/* ed_buffer.flag values
 * The lower 4 bits are reserved to hold the shiftwidth value then
 * the settings are stored/retrieved.
 */

#define SHIFTWIDTH_MASK  0x000f

#define NFLG_MASK        0x0010  /* True: number lines */
#define LFLG_MASK        0x0020  /* True: mark tabs and line ends */
#define PFLG_MASK        0x0040
#define EIGHTBIT_MASK    0x0080  /* True: keep 8th bit */
#define AUTOINDFLG_MASK  0x0100  /* True: autoindent */
#define EXCOMPAT_MASK    0x0200
#define TABINDENT_MASK   0x0400
#define SMALLNUMBER_MASK 0x0800  /* True: short line numbers */
#define ALL_FLAGS_MASK   0x0ff0

/* Macros handling the current ed_buffer for less typing */

#define ED_BUFFER             (current_ed_buffer)
#define EXTERN_ED_BUFFER(ih)  ((ed_buffer_t*) (ih))

#define P_DIAG          (ED_BUFFER->diag)
#define P_TRUNCFLG      (ED_BUFFER->truncflg)
#define P_NONASCII      (ED_BUFFER->nonascii)
#define P_NULLCHAR      (ED_BUFFER->nullchar)
#define P_TRUNCATED     (ED_BUFFER->truncated)
#define P_FNAME         (ED_BUFFER->fname)
#define P_FCHANGED      (ED_BUFFER->fchanged)
#define P_NOFNAME       (ED_BUFFER->nofname)
#define P_MARK          (ED_BUFFER->mark)
#define P_OLDPAT        (ED_BUFFER->oldpat)
#define P_LINE0         (ED_BUFFER->Line0)
#define P_CURLN         (ED_BUFFER->CurLn)
#define P_CURPTR        (ED_BUFFER->CurPtr)
#define P_LASTLN        (ED_BUFFER->LastLn)
#define P_LINE1         (ED_BUFFER->Line1)
#define P_LINE2         (ED_BUFFER->Line2)
#define P_NLINES        (ED_BUFFER->nlines)
#define P_SHIFTWIDTH    (ED_BUFFER->shiftwidth)
#define P_FLAGS         (ED_BUFFER->flags)
#define P_NFLG          ( P_FLAGS & NFLG_MASK )
#define P_LFLG          ( P_FLAGS & LFLG_MASK )
#define P_PFLG          ( P_FLAGS & PFLG_MASK )
#define P_EIGHTBIT      ( P_FLAGS & EIGHTBIT_MASK )
#define P_AUTOINDFLG    ( P_FLAGS & AUTOINDFLG_MASK )
#define P_EXCOMPAT      ( P_FLAGS & EXCOMPAT_MASK )
#define P_TABINDENT     ( P_FLAGS & TABINDENT_MASK )
#define P_SMALLNUMBER   ( P_FLAGS & SMALLNUMBER_MASK )
#define P_APPENDING     (ED_BUFFER->appending)
#define P_MORE          (ED_BUFFER->moring)
#define P_LEADBLANKS    (ED_BUFFER->leading_blanks)
#define P_CUR_AUTOIND   (ED_BUFFER->cur_autoindent)
#define P_PROMPT        (ED_BUFFER->prompt)
#define P_LASTCMD       (ED_BUFFER->lastcmd)


/*-------------------------------------------------------------------------*/

/* The editor options are described by this structure: */

struct tbl
{
    char *t_str;       /* option name, NULL marks the table end */
    int   t_and_mask;  /* unset mask */
    int   t_or_mask;   /* set mask */
};

static struct tbl tbl[]
  = { {  "number",         ~FALSE,            NFLG_MASK        }
    , {  "nonumber",       ~NFLG_MASK,        FALSE            }
    , {  "list",           ~FALSE,            LFLG_MASK        }
    , {  "nolist",         ~LFLG_MASK,        FALSE            }
    , {  "print",          ~FALSE,            PFLG_MASK        }
    , {  "noprint",        ~PFLG_MASK,        FALSE            }
    , {  "eightbit",       ~FALSE,            EIGHTBIT_MASK    }
    , {  "noeightbit",     ~EIGHTBIT_MASK,    FALSE            }
    , {  "autoindent",     ~FALSE,            AUTOINDFLG_MASK  }
    , {  "noautoindent",   ~AUTOINDFLG_MASK,  FALSE            }
    , {  "excompatible",   ~FALSE,            EXCOMPAT_MASK    }
    , {  "noexcompatible", ~EXCOMPAT_MASK,    FALSE            }
    , {  "tabindent",      ~FALSE,            TABINDENT_MASK   }
    , {  "notabindent",    ~TABINDENT_MASK,   FALSE            }
    , {  "smallnumber",    ~FALSE,            SMALLNUMBER_MASK }
    , {  "nosmallnumber",  ~SMALLNUMBER_MASK, FALSE            }
    , {  NULL }
    };

/*-------------------------------------------------------------------------*/

static ed_buffer_t *current_ed_buffer;
  /* The current ed_buffer
   */


static char  inlin[MAXLINE];
static char *inptr;
  /* Command input buffer
   */

static int ed_tmp;
  /* Temporary used by some macros
   */

/*-------------------------------------------------------------------------*/

/* Some macros */

#ifndef max
#    define max(a,b)       ((a) > (b) ? (a) : (b))
#endif

#ifndef min
#    define min(a,b)       ((a) < (b) ? (a) : (b))
#endif

#define nextln(l)          ((l)+1 > P_LASTLN ? 0 : (l)+1)
#define prevln(l)          ((l)-1 < 0 ? P_LASTLN : (l)-1)

#define gettxtl(lin)       ((lin)->l_buff)
#define gettxt(num)        (gettxtl( getptr(num) ))

#define getnextptr(p)      ((p)->l_next)
#define getprevptr(p)      ((p)->l_prev)

#define _setCurLn( lin )   ( (P_CURPTR = getptr( (lin) )), P_CURLN = (lin) )
#define setCurLn( lin )    ( (P_CURPTR = getptr( ed_tmp = (lin) )), P_CURLN = ed_tmp )
#define nextCurLn()        ( P_CURLN = nextln(P_CURLN), P_CURPTR = getnextptr( P_CURPTR ) )
#define prevCurLn()        ( P_CURLN = prevln(P_CURLN), P_CURPTR = getprevptr( P_CURPTR ) )

#define clrbuf()           del(1, P_LASTLN)

#define Skip_White_Space   { while (*inptr==SP || *inptr==HT) inptr++; }

#define relink(a, x, y, b) { (x)->l_prev = (a); (y)->l_next = (b); }


/*-------------------------------------------------------------------------*/

/* Forward declarations */

static int doprnt(int, int);
static int ins(char *);
static int deflt(int, int);
static void print_help(char arg);
static void print_help2(void);
static void count_blanks(int line);
static void _count_blanks(char *str, int blanks);
static LINE *getptr(int num);
static void putcntl(char c);
static void prntln(char *str, Bool vflg, int lin);
static regexp_t *optpat(void);

/*-------------------------------------------------------------------------*/
size_t
ed_buffer_size (input_t *ih)

/* Return the size of the memory allocated for the <buffer>
 */

{
    ed_buffer_t *buffer = (ed_buffer_t*) ih;
    size_t sum;
    long line;
    LINE *pLine;

    if (!buffer)
        return 0;

    sum = sizeof(*buffer);
    for (line = 1, pLine = buffer->Line0.l_next
        ; line < buffer->LastLn
        ; line++, pLine = pLine->l_next)
        sum += sizeof(*pLine) + strlen(pLine->l_buff);

    return sum;
} /* ed_buffer_size() */

/*-------------------------------------------------------------------------*/
static INLINE void
set_ed_prompt (ed_buffer_t * ed_buffer, string_t * prompt)

/* Reference and set string <prompt> as new prompt in <ed_buffer>.
 * The prompt svalue must already have been initialized as T_STRING.
 */

{
    free_mstring(ed_buffer->input.prompt.u.str);
    ed_buffer->input.prompt.u.str = ref_mstring(prompt);
} /* set_ed_prompt() */

/*-------------------------------------------------------------------------*/
static int
append (int line, Bool glob)

/* Start appending (or inserting) after <line>: set the current line
 * and print/set the prompt.
 * Return success.
 */

{
    if (glob)
        return(ERR);
    _setCurLn( line );
    P_APPENDING = TRUE;
    if (P_NFLG)
        add_message(P_SMALLNUMBER ? "%3d " : "%6d. ",P_CURLN+1);
    if (P_CUR_AUTOIND)
        add_message("%*s", P_LEADBLANKS, "");
    set_ed_prompt(ED_BUFFER, STR_ED_APPEND_PROMPT);
    return ED_OK;
}

/*-------------------------------------------------------------------------*/
static INLINE int /* only used once */
more_append (char *str)

/* User entered a new line <str> in appending mode.
 * Terminate mode if it is just '.', else append it to the text.
 * In autoindentation mode, recompute the indentation. Outdentation can
 * be forced by entering Ctrl-D or CR as the first characters of <str>.
 * Return success code.
 */

{
    if(str[0] == '.' && str[1] == '\0')
    {
        P_APPENDING = FALSE;
        set_ed_prompt(ED_BUFFER, STR_ED_PROMPT);
        return ED_OK;
    }

    if (P_NFLG)
        add_message(P_SMALLNUMBER ? "%3d " : "%6d. ",P_CURLN+2);

    if (P_CUR_AUTOIND)
    {
        int i;
        int less_indent_flag = 0;

        while (*str=='\004' || *str == '\013' )
        {
            str++;
            P_LEADBLANKS -= P_SHIFTWIDTH;
            if (P_LEADBLANKS < 0)
                P_LEADBLANKS = 0;
            less_indent_flag = 1;
        }
        for (i = 0; i < P_LEADBLANKS; )
            inlin[i++]=' ';
        xstrncpy(inlin+P_LEADBLANKS, str, (size_t)(MAXLINE-P_LEADBLANKS));
        inlin[MAXLINE-1] = '\0';
        _count_blanks(inlin, 0);
        add_message("%*s", P_LEADBLANKS, "");
        if (!*str && less_indent_flag)
            return ED_OK;
        str = inlin;
    }
    if (ins(str) < 0)
        return MEM_FAIL;

    return ED_OK;
}

/*-------------------------------------------------------------------------*/
static void
count_blanks (int line)

/* Count the leading blanks in line <line> and set .leadingblanks to
 * the value.
 */
{
    _count_blanks(gettxtl(getptr(line)), 0);
}

/*-------------------------------------------------------------------------*/
static void
_count_blanks (char * str, int blanks)

/* Count the leading blanks of <str>, add them to <blanks> and set the
 * result as .leadingblanks.
 */

{
    for ( ; *str; str++ )
    {
        if ( *str == ' ' ) blanks++;
        else if ( *str == '\t' ) blanks += 8 - blanks % 8 ;
        else break;
    }
    P_LEADBLANKS = blanks < MAXLINE ? blanks : MAXLINE ;
}

/*-------------------------------------------------------------------------*/
static INLINE int /* only used once */
ckglob (void)

/* Check if the command starting at <inptr> has a global modifier of the
 * forms 'g/<expr>/' or 'v/<expr>/'.
 * If yes, mark all matching lines with LGLOB and return TRUE,
 * else return FALSE.
 * Return an error code on failure.
 */

{
    regexp_t      *glbpat;
    char        c, delim, *lin;
    int        num;
    LINE        *ptr;

    c = *inptr;

    if (c != 'g' && c != 'v')
        return FALSE;

    if (deflt(1, P_LASTLN) < 0)
        return ERR;

    delim = *++inptr;
    if (delim <= ' ')
        return ERR;

    glbpat = optpat();
    if (*inptr == delim)
        inptr++;

    ptr = getptr(1);
    for (num = 1; num <= P_LASTLN; num++)
    {
        ptr->l_stat &= ~LGLOB;
        if (P_LINE1 <= num && num <= P_LINE2)
        {
            /* we might have got a NULL pointer if the
             * supplied pattern was invalid
             */
            if (glbpat)
            {
                int rc;
                lin = gettxtl(ptr);
                rc = rx_exec_str(glbpat, lin, lin);
                if (rc < 0)
                {
                    add_message("ed: %s\n", rx_error_message(rc, glbpat) );
                    return ERR;
                }

                if (rc > 0)
                {
                    if (c=='g') ptr->l_stat |= LGLOB;
                }
                else
                {
                    if (c=='v') ptr->l_stat |= LGLOB;
                }
            }
        }
        ptr = getnextptr(ptr);
    }
    return TRUE;
}

/*-------------------------------------------------------------------------*/
static int
deflt (int def1, int def2)

/* Set P_LINE1 & P_LINE2 (the command-range delimiters) if none of them
 * was supplied with the command; test whether they have valid values.
 * Return success code.
 */
{
    if(P_NLINES == 0) {
        P_LINE1 = def1;
        P_LINE2 = def2;
    }
    return ( (P_LINE1 > P_LINE2 || P_LINE1 <= 0) ? ERR : ED_OK );
}


/*-------------------------------------------------------------------------*/
static int
del (int from, int to)

/* Delete the lines in the range <from> to <to> inclusive. The range is
 * automatically limited by the text start/end.
 * Always returns ED_OK.
 */

{
    LINE    *first, *last, *next, *tmp;

    if (from < 1)
        from = 1;
    first = getprevptr(getptr(from));
    P_CURLN = prevln(from);
    P_CURPTR = first;
    last = getnextptr(getptr(to));
    next = first->l_next;
    while(next != last && next != &P_LINE0)
    {
        tmp = next->l_next;
        xfree((char *)next);
        next = tmp;
    }
    relink(first, last, first, last);
    P_LASTLN -= (to - from)+1;
    return ED_OK;
}


/*-------------------------------------------------------------------------*/
static INLINE int /* only used once */
dolst (int line1, int line2)

/* Print the range <line1> to <line2> with tab characters printed
 * symbolically and line end marked with '$'.
 * Returns success (currently always ED_OK).
 */

{
    int oldflags = P_FLAGS;
    int p;

    P_FLAGS |= LFLG_MASK;
    p = doprnt(line1, line2);
    P_FLAGS = oldflags;
    return p;
}


/*-------------------------------------------------------------------------*/
#if 0 /* unused */

int
esc (char **s)

/* Map escape sequences into their equivalent symbols.  Returns the
 * correct ASCII character.  If no escape prefix is present then s
 * is untouched and *s is returned, otherwise **s is advanced to point
 * at the escaped character and the translated character is returned.
 */

{
    register int    rval;

    if (**s != ESCAPE)
    {
        rval = **s;
    }
    else
    {
        (*s)++;
        switch(islower(**s) ? toupper(**s) : **s)
        {
        case '\000':
            rval = ESCAPE;    break;
        case 'S':
            rval = ' ';    break;
        case 'N':
            rval = '\n';    break;
        case 'T':
            rval = '\t';    break;
        case 'B':
            rval = '\b';    break;
        case 'R':
            rval = '\r';    break;
        default:
            rval = **s;    break;
        }
    }
    return rval;
}
#endif


/*-------------------------------------------------------------------------*/
static int
doprnt (int from, int to)

/* Print the text in the range <from> to <to>, using the current
 * settings of P_LFLG and P_NFLG.
 * Return ED_OK.
 */

{
    from = (from < 1) ? 1 : from;
    to = (to > P_LASTLN) ? P_LASTLN : to;

    if (to != 0 && from <= P_LASTLN)
    {
        _setCurLn( from );
        while( P_CURLN <= to )
        {
            prntln( gettxtl( P_CURPTR ), P_LFLG, (P_NFLG ? P_CURLN : 0));
            if( P_CURLN == to )
                break;
            nextCurLn();
        }
    }
    return ED_OK;
}


/*-------------------------------------------------------------------------*/
static void
prntln (char *str, Bool vflg, int lin)

/* Print the line <str>, optionally prepended by the number <lin> (if not 0).
 * If <vflg> is true, tab characters are printed symbolically and line end
 * marked with '$'.
 */

{
    if (lin)
        add_message(P_SMALLNUMBER ? "%3d " : "%7d " ,lin);

    while(*str && *str != NL)
    {
        if (*str < ' ' || *str >= 0x7f)
        {
            switch(*str)
            {
            case '\t':
                if (vflg)
                    putcntl(*str);
                else
                    add_message("%c", *str);
                break;

            case DEL:
                add_message("^?");
                break;

            default:
                putcntl(*str);
                break;
            }
            str++;
        }
        else
        {
            char *start;

            start = str;
            do str++; while(*str >= ' ' && *str < 0x7f);
            if (*str)
                add_message("%.*s", (int)(str - start), start);
            else
                add_message("%s", start);
        }
    }
    if (vflg)
        add_message("$");
    add_message("\n");
}


/*-------------------------------------------------------------------------*/
static void
putcntl (char c)

/* Print the control character <c> symbolically (e.g. '^C' for a Control-C).
 */

{
    add_message("^%c",(c&31)|'@');
}


/*-------------------------------------------------------------------------*/
static INLINE int /* only used once */
egets (char *str, int size, FILE * stream)

/* Safe version of fgets(): get a line of text from <stream> into <str>,
 * but at max <size> characters, and return the number characters read.
 */

{
    int   c, count;
    char *cp;

    /* assert(size); if there is any chance of size == 0 */
    count = 0;
    cp = str;
    do {
        c = getc(stream);
        if(c == EOF)
        {
            *cp = EOS;
            if(count)
                add_message("[Incomplete last line]\n");
            return(count);
        }
        else if(c == NL) {
            *cp = EOS;
            return(++count);
        }
        else if (c == 0)
            P_NULLCHAR++;    /* count nulls */
        else
        {
            if (c > 127) {
                if (!P_EIGHTBIT) /* if not saving eighth bit */
                    c = c&127;   /* strip eigth bit */
                P_NONASCII++;    /* count it */
            }
            *cp++ = (char)c;    /* not null, keep it */
            count++;
        }
    } while (size > count);

    str[count-1] = EOS;
    if (c != NL) {
        add_message("truncating line\n");
        P_TRUNCATED++;
        while ((c = getc(stream)) != EOF)
            if (c == NL)
                break;
    }
    return count;
}  /* egets() */


/*-------------------------------------------------------------------------*/
static int
doread (int lin, string_t *fname)

/* Read the file <fname> and insert the lines at line <lin>
 * Return success code.
 */

{
    FILE          *fp;
    int            err;
    unsigned long  bytes;
    unsigned int   lines;
    char    str[MAXLINE];

    err = 0;
    P_NONASCII = P_NULLCHAR = P_TRUNCATED = 0;

    if (P_DIAG) add_message("\"%s\" ",get_txt(fname));
    if ((fp = fopen(get_txt(fname), "r")) == NULL )
    {
        if (!P_DIAG) add_message("\"%s\" ",get_txt(fname));
        add_message(" isn't readable.\n");
        return ERR ;
    }
    FCOUNT_READ(get_txt(fname));
    _setCurLn( lin );
    for(lines = 0, bytes = 0;(err = egets(str,MAXLINE,fp)) > 0;) {
        bytes += err;
        if (ins(str) < 0) {
            err = MEM_FAIL;
            break;
        }
        lines++;
    }
    fclose(fp);

    if(err < 0)
        return err;

    if (P_DIAG)
    {
        add_message("%u lines %lu bytes",lines,bytes);
        if(P_NONASCII)
            add_message(" [%d non-ascii]",P_NONASCII);
        if(P_NULLCHAR)
            add_message(" [%d nul]",P_NULLCHAR);
        if(P_TRUNCATED)
            add_message(" [%d lines truncated]",P_TRUNCATED);
        add_message("\n");
    }
    return err;
}  /* doread */


/*-------------------------------------------------------------------------*/
static int
dowrite (int from, int to, string_t *fname, Bool apflg)

/* Write the lines <from> to <to> into the file <fname>. If the <apflg>
 * is true, the file is opened for appending, else it is written
 * from scratch.
 * Return success code.
 */

{
    FILE          *fp;
    int            lin, err;
    unsigned int   lines;
    unsigned long  bytes;
    char          *str;
    LINE          *lptr;

    err = 0;
    lines = bytes = 0;

    add_message("\"%s\" ",get_txt(fname));
    if ((fp = fopen(get_txt(fname),(apflg?"a":"w"))) == NULL)
    {
        add_message(" can't be opened for writing!\n");
        return ERR;
    }
    FCOUNT_WRITE(get_txt(fname));

    lptr = getptr(from);
    for (lin = from; lin <= to; lin++)
    {
        str = lptr->l_buff;
        lines++;
        bytes += strlen(str) + 1;    /* str + '\n' */
        if(fputs(str, fp) == EOF)
        {
            add_message("file write error\n");
            err++;
            break;
        }
        fputc('\n', fp);
        lptr = lptr->l_next;
    }
    add_message("%u lines %lu bytes\n", lines, bytes);
    fclose(fp);
    return err;
}  /* dowrite */


/*-------------------------------------------------------------------------*/
static INLINE int /* only used once */
find (regexp_t *pat, Bool dir)

/* Find the <pat>tern in the text, starting 'after' the current line.
 * If <dir> is false, the search is carried out forward, else backwards.
 * Return the number of the line where the pattern is found first, else
 * the result code ERR.
 */

{
    int    i, num;
    LINE    *lin;

    dir ? nextCurLn() : prevCurLn() ;
    num = P_CURLN;
    lin = P_CURPTR;

    /* if the typed in pattern was invalid we have a NULL pointer! */
    if (!pat)
        return ERR;

    for (i = 0; i < P_LASTLN; i++ )
    {
        int rc;
        char *line_start = gettxtl(lin);
        rc = rx_exec_str(pat, line_start, line_start);
        if (rc < 0)
        {
            add_message("ed: %s\n", rx_error_message(rc, pat) );
            return ERR;
        }
        if (rc)
            return(num);
        if( dir )
            num = nextln(num), lin = getnextptr(lin);
        else
            num = prevln(num), lin = getprevptr(lin);
    }
    return ERR;
}

/*-------------------------------------------------------------------------*/
#if 0 /* unused */

static int
findg (regexp_t *pat, Bool dir)

/* Find the <pat>tern in the text, starting 'after' the current line
 * and print matching lines (like a grep).
 * If <dir> is false, the search is carried out forward, else backwards.
 * Return the numbers of matching lines, or ERR on failure.
 */

{
    int    i, num,count;
    LINE    *lin;

    count = 0;
    num = P_CURLN;
    lin = P_CURPTR;

    /* if the typed in pattern was invalid we have a NULL pointer! */
    if (!pat)
        return ERR;

    for (i = 0; i < P_LASTLN; i++ )
    {
        int rc;
        rc = rx_exec_str(pat, gettxtl(lin), gettxtl(lin)))
        if (rc < 0)
        {
            add_message("ed: %s\n", rx_error_message(rc, pat) );
            return ERR;
        }
        if (rc > 0)
        {
            prntln( gettxtl( lin ), P_LFLG, (P_NFLG ? P_CURLN : 0));
             count++;
        }
        if (dir)
            num = nextln(num), lin = getnextptr(lin);
        else
            num = prevln(num), lin = getprevptr(lin);
    }

    return count ? count : ERR;
}
#endif /* 0 */

/*-------------------------------------------------------------------------*/
static string_t *
getfn (Bool writeflg)

/* Get a filename from the input buffer, create a string from it and return
 * the pointer to it (counts as ref). Relative filenames are made absolute by
 * master->make_path_absolute(). If there is no filename, set P_NOFNAME to
 * true and set the filename to '/'+P_FNAME. In either case the filename
 * is validated by check_valid_path().
 *
 * Return NULL on an error.
 */

{
    string_t *file; /* TODO: make this ed_buffer based? */
    char     *cp;
    string_t *file2;
    svalue_t *ret;

    if (*inptr == NL)
    {
        P_NOFNAME = TRUE;

        if (!P_FNAME)
        {
            /* I have no idea how this can happen, but it did.
             * So be save.
             */
            add_message("bad file name\n");
            return NULL;
        }

        file = alloc_mstring(1+mstrsize(P_FNAME));
        if (!file)
        {
            add_message("Out of memory (%lu bytes) for filename.\n"
                       , (unsigned long) (1+mstrsize(P_FNAME)));
            return NULL;
        }
        get_txt(file)[0] = '/';
        memcpy(get_txt(file)+1, get_txt(P_FNAME), mstrsize(P_FNAME));
    }
    else
    {
        size_t len;
        char *tmp;

        P_NOFNAME = FALSE;
        Skip_White_Space;

        for (len = 0, tmp = inptr
            ; *tmp && *tmp != NL && *tmp != SP && *tmp != HT
            ; tmp++, len++) NOOP;

        file = alloc_mstring(len);
        if (!file)
        {
            add_message("Out of memory (%lu bytes) for filename.\n"
                       , (unsigned long)len);
            return NULL;
        }

        cp = get_txt(file);
        while (*inptr && *inptr != NL && *inptr != SP && *inptr != HT)
            *cp++ = *inptr++;
    }

    if (mstrsize(file) == 0)
    {
        add_message("bad file name\n");
        free_mstring(file);
        return NULL;
    }

    if (get_txt(file)[0] != '/')
    {
        push_ref_string(inter_sp, file);
        ret = apply_master(STR_ABS_PATH, 1);
        if (!ret || (ret->type == T_NUMBER && ret->u.number == 0))
        {
            free_mstring(file);
            return NULL;
        }

        if (ret->type == T_STRING)
        {
            free_mstring(file);
            file = ref_mstring(ret->u.str);
        }
    }

    /* add_message() / apply() might have nasty effects */
    if (!command_giver || command_giver->flags & O_DESTRUCTED)
    {
        free_mstring(file);
        return NULL;
    }

    file2 = check_valid_path(file, command_giver, STR_ED_START, writeflg);
    free_mstring(file);

    if (!file2)
    {
        return NULL;
    }

    if (mstrsize(file2) == 0) {
        add_message("no file name\n");
        free_mstring(file2);
        return NULL;
    }

    return file2;
}  /* getfn */

/*-------------------------------------------------------------------------*/
static int
getnum (int first)

/* Parse a line designation from the command input and return it.
 * If no line is given, return EOF or, if <first> is true, 1.
 * Allowed designations are:
 *   [0-9]*: line number
 *   .     : current line
 *   $     : last line
 *   /<pat>: line matching <pat>, searched forwards
 *   ?<pat>: line matching <pat>, searched backwards
 *   [+-]  : <first> true ? first line : cur. line; inptr is not incremented!
 *   \[a-z]: line of given mark
 */
{
    regexp_t *srchpat;
    int    num;
    char    c;

    Skip_White_Space;

    if (isdigit((unsigned char)*inptr)) /* line number */
    {
        for (num = 0; isdigit((unsigned char)*inptr); ++inptr) {
            num = (num * 10) + (*inptr - '0');
        }
        return num;
    }

    switch(c = *inptr)
    {
    case '.':
        inptr++;
        return P_CURLN;

    case '$':
        inptr++;
        return P_LASTLN;

    case '/':
    case '?':
        srchpat = optpat();
        if (*inptr == c)
            inptr++;
        return find(srchpat, (c == '/') );

#if 0
    case '^':            /* for grep-like searching */
    case '&':
        srchpat = optpat();
        if (*inptr == c)
            inptr++;
        return findg(srchpat, (c == '^'));
#endif

    case '-':
    case '+':
        return first ? P_CURLN : 1;

    case '\'':
        inptr++;
        if (*inptr < 'a' || *inptr > 'z')
            return(EOF);
        return P_MARK[ *inptr++ - 'a' ];

    default:
        return first ? EOF : 1;    /* unknown address */
    }
}  /* getnum */


/*-------------------------------------------------------------------------*/
static int
getone (void)

/* Parse a line number off the commandline. It can be any additive
 * expression of the numbers accepted by getnum().
 * Return the resulting line number, or EOF/ERR on failure.
 */

{
    int    c, i, num;

#   define FIRST 1
#   define NOTFIRST 0

    if ((num = getnum(FIRST)) >= 0)
    {
        for (;;)
        {
            Skip_White_Space;

            if (*inptr != '+' && *inptr != '-')
                break;    /* exit infinite loop */

            c = *inptr++;
            if ((i = getnum(NOTFIRST)) < 0)
                return ( i );
            if (c == '+')
                num += i;
            else
                num -= i;
        }
    }
    return num > P_LASTLN ? ERR : num;

#   undef FIRST
#   undef NOTFIRST
}  /* getone */


/*-------------------------------------------------------------------------*/
static
int getlst (void)

/* Get a range designation of lines and store it in P_LINE1 and P_LINE2
 * (defaults are 0). The number of designations (0, 1 or 2) is stored
 * in P_NLINES.
 * Accepted line designations are:
 *   <num>         : defines P_LINE1
 *   <num1>,<num2> : defines P_LINE1 and P_LINE2
 *   <num1>;<num2> : defines P_LINE1 and P_LINE2, current line is set
 *                   to <num2>
 * <num> is everything getone() accepts.
 *
 * Return the number of line designations, or ERR on failure.
 */

{
    int    num;

    P_LINE2 = 0;
    for(P_NLINES = 0; (num = getone()) >= 0;)
    {
        P_LINE1 = P_LINE2;
        P_LINE2 = num;
        P_NLINES++;
        if(*inptr != ',' && *inptr != ';')
            break;
        if(*inptr == ';')
            _setCurLn( num );
        inptr++;
    }

    P_NLINES = min(P_NLINES, 2);

    if (P_NLINES == 0)
        P_LINE2 = P_CURLN;
    if (P_NLINES <= 1)
        P_LINE1 = P_LINE2;

    return (num == ERR) ? num : P_NLINES;
}  /* getlst */


/*-------------------------------------------------------------------------*/
static LINE *
getptr (int num)

/* Find and return the structure for line <num>.
 * Simple partitioning gives the function a complexity of O(N/4).
 */

{
    LINE *ptr;
    int j, cur;

    cur = P_CURLN;
    if (num >= cur)
    {
        if (2*num - cur > P_LASTLN && num <= P_LASTLN)
        {
            /* high line numbers */
            ptr = P_LINE0.l_prev;
            for (j = P_LASTLN - num; --j >= 0; )
                ptr = ptr->l_prev;
        }
        else
        {
            ptr = P_CURPTR;
            for (j = num - cur; --j >= 0; )
                ptr = ptr->l_next;
        }
    }
    else
    {
        if (2*num <= cur) {
            /* low line numbers */
            ptr = &P_LINE0;
            for (j = num; --j >= 0; )
                ptr = ptr->l_next;
        }
        else
        {
            ptr = P_CURPTR;
            for (j = cur - num; --j >= 0; )
                ptr = ptr->l_prev;
        }
    }

    return ptr;
}


/*-------------------------------------------------------------------------*/
static INLINE int /* only used once */
getrhs (char *sub)

/* Parse the replacement pattern for a search-and-replace command and
 * put it into sub. inptr is expected to point to the leading delimiter
 * of the pattern. Special replacement designators are:
 *
 *   \r, \t, \n, \b: the ASCII characters CR, TAB, NL and BEL
 *   \\            : the '\' character itself
 *   \0xxx         : the ASCII character with octal code xxx
 *   \&, \[0-9]    : the matched (sub)pattern
 *
 * Return TRUE for a 'g'lobal replacement, and FALSE for a singular one.
 * Return ERR on failure.
 */
{
    char delim = *inptr++;
    char *outmax = sub + MAXPAT;

    if( delim == NL || *inptr == NL)    /* check for eol */
        return( ERR );

    while (*inptr != delim && *inptr != NL)
    {
        if ( sub > outmax )
            return ERR;
        if ( *inptr == ESCAPE )
        {
            switch ( *++inptr )
            {
            case 'r':
                *sub++ = '\r';
                inptr++;
                break;
#if 0
            case ESCAPE:
                *sub++ = ESCAPE;
                *sub++ = ESCAPE;
                inptr++;
#endif
            case 'n':
                *sub++ = '\n';
                inptr++;
                break;
            case 'b':
                *sub++ = '\b';
                inptr++;
                break;
            case 't':
                *sub++ = '\t';
                inptr++;
                break;
            case '0':
            {
                int i=3;
                *sub = 0;
                do {
                    if (*++inptr<'0' || *inptr >'7')
                        break;
                    *sub = (char)((*sub<<3) | (*inptr-'0'));
                } while (--i!=0);
                sub++;
                } break;
#if 0
            default:
                if ( *inptr != delim )
                    *sub++ = ESCAPE;
#else
            case '&':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
            case '\\':
                *sub++ = ESCAPE; /* fall through */
            default:
#endif
                *sub++ = *inptr;
                if ( *inptr != NL )
                    inptr++;
            }
        }
        else *sub++ = *inptr++;
    }
    *sub = '\0';

    inptr++;        /* skip over delimter */
    Skip_White_Space;
    if (*inptr == 'g')
    {
        inptr++;
        return TRUE;
    }
    return FALSE;
}

/*-------------------------------------------------------------------------*/
static int
ins (char *str)

/* Insert <str> as new line(s) after the current line. <str> can be
 * several lines, separated by \n.
 * Return TRUE on success, MEM_FAIL if out of memory.
 */

{
    char    *cp;
    LINE    *new, *nxt;
    size_t   len;

    do
    {
        for ( cp = str; *cp && *cp != NL; cp++ ) NOOP;
        len = (size_t)(cp - str);
        /* cp now points to end of first or only line */

        if ((new = (LINE *)xalloc(sizeof(LINE)+len)) == NULL)
            return( MEM_FAIL );     /* no memory */

        new->l_stat = 0;
        strncpy(new->l_buff, str, len);    /* build new line */
        new->l_buff[len] = EOS;
        nxt = getnextptr(P_CURPTR);    /* get next line */
        relink(P_CURPTR, new, new, nxt);    /* add to linked list */
        relink(new, nxt, P_CURPTR, new);
        P_LASTLN++;
        P_CURLN++;
        P_CURPTR = new;
        str = cp + 1;
    }
    while( *cp != EOS );

    return TRUE;
}


/*-------------------------------------------------------------------------*/
static INLINE int /* only used once */
join (int first, int last)

/* Join the lines <first> to <last> into one line and insert it at the
 * current line. The original lines are deleted.
 * Return success code.
 */

{
    char buf[MAXLINE];
    char *cp = buf, *str;
    LINE *lin;
    int num;

    if (first <= 0 || first > last || last > P_LASTLN)
        return ERR;

    if (first==last)
    {
        _setCurLn(first);
        return ED_OK;
    }

    lin = getptr(first);
    for (num=first; num<=last; num++)
    {
        str=gettxtl(lin);
        while (*str)
        {
            if (cp >= buf + MAXLINE-1 )
            {
                add_message("line too long\n");
                return ERR;
            }
            *cp++ = *str++;
        }
        lin = getnextptr(lin);
    }
    *cp = EOS;

    del(first, last);

    if( ins(buf) < 0 )
        return MEM_FAIL;

    P_FCHANGED = TRUE;
    return ED_OK;
}


/*-------------------------------------------------------------------------*/
static INLINE int /* only used once */
move (int num)

/* Move the block of lines P_LINE1 to P_LINE2 after the line <num> which
 * must not be in the moved range.
 * Return TRUE on success, else an error code.
 */

{
    LINE *dest, *before, *first, *last, *after;

    dest = getptr(num);
    if (num < P_LINE1)
        num += P_LINE2 - P_LINE1 + 1;
    else if (num <= P_LINE2)
        return ERR;

    first = getptr(P_LINE1);
    before = first->l_prev;
    last = getptr(P_LINE2);
    after = last->l_next;

    relink(before, after, before, after);

    before = dest;
    after = dest->l_next;
    relink(before, first, last, after);
    relink(last, after, before, first);
    P_CURPTR = last;
    P_CURLN = num;

    return TRUE;
}


/*-------------------------------------------------------------------------*/
static INLINE int /* only used once */
transfer (int num)

/* Copy the block of lines P_LINE1 to P_LINE2 after the line <num> which
 * may be in the moved range.
 * Return TRUE on success, else an error code.
 */

{
    int count1, count2;
    struct line *ptr;

    /* The caller made sure that (P_LINE1 > 0 && P_LINE1 <= P_LINE2)
     * by calling deflt()
     */

    if (num >= P_LINE1 && num < P_LINE2)
    {
        count1 = num - P_LINE1; /* loop has one iteration more */
        count2 = P_LINE2 - num;
    }
    else
    {
        count1 = P_LINE2 - P_LINE1;
        count2 = 0;
    }

    _setCurLn( num );
    ptr = getptr(P_LINE1);
    do
    {
        if (ins(gettxtl(ptr)) < 0)
            return MEM_FAIL;
        ptr = getnextptr(ptr);
    } while (--count1 >= 0);

    if (count2)
    {
        ptr = getnextptr(P_CURPTR);
        do
        {
            if (ins(gettxtl(ptr)) < 0)
                return MEM_FAIL;
            ptr = getnextptr(ptr);
        } while(--count2);
    }

    return TRUE;
}


/*-------------------------------------------------------------------------*/
static regexp_t *
optpat (void)

/* Parse a search- or replace-match pattern from the command input, compile
 * it and return the compiled regular expression. inptr is expected
 * to point to the leading delimiter, and is left pointing to the trailing
 * delimiter.
 *
 * The pattern is also stored as P_OLDPAT, so it can be reused by simply
 * entering a leading delimiter only.
 *
 * Return NULL on failure, else the pointer to the compiled pattern.
 */

{
    char delim, str[MAXPAT], *cp;
    string_t *buf;

    delim = *inptr;
    if (delim == NL)
        return P_OLDPAT;

    inptr++;

    cp = str;
    while (*inptr != delim
        && *inptr != NL
        && *inptr != EOS
        && cp < str + MAXPAT - 1)
    {
        if (*inptr == ESCAPE && inptr[1] != NL)
            *cp++ = *inptr++;
        *cp++ = *inptr++;
    }

    *cp = EOS;
    if (*str == EOS)
        return(P_OLDPAT);
    if(P_OLDPAT)
        free_regexp(P_OLDPAT);

    memsafe(buf = new_mstring(str), strlen(str), "regexp pattern string");
    P_OLDPAT = rx_compile(buf, P_EXCOMPAT ? RE_EXCOMPATIBLE : 0, MY_TRUE);
    free_mstring(buf);
    return P_OLDPAT;
}

/*-------------------------------------------------------------------------*/
static INLINE int /* only used once */
set (void)

/* Process the 'set' command and return the success code.
 * The options are defined in tbl[].
 */

{
    char word[16];
      /* the longest valid set keyword is 14 characters long. Add one char
       * for EOS, and another to get 4-byte-alignmnt
       */
    int    i;
    struct tbl *t;

    if (*(++inptr) != 't')
    {
        if(*inptr != SP && *inptr != HT && *inptr != NL)
            return ERR;
    } else
        inptr++;

    /* No arguments: print the settings */

    if ( *inptr == NL)
    {
        add_message("ed version %d.%d\n", ED_VERSION/100, ED_VERSION%100);
        for (t = tbl; t->t_str; t+=2)
        {
            add_message(    "%s:%s ",t->t_str,
                P_FLAGS & t->t_or_mask ?"ON":"OFF");
        }
        add_message("\nshiftwidth:%d\n",P_SHIFTWIDTH);
        return(0);
    }

    /* Parse the name of the option to be set */

    Skip_White_Space;
    for (i = 0; *inptr != SP && *inptr != HT && *inptr != NL;)
    {
        /* leave space for EOS too */
        if (i == sizeof word - 2) {
            add_message("Too long argument to 'set'!\n");
            return ED_OK;
        }
        word[i++] = *inptr++;
    }
    word[i] = EOS;

    /* Look for the option. If found, set the flag. */

    for(t = tbl; t->t_str; t++) {
        if (strcmp(word,t->t_str) == 0) {
            P_FLAGS = (P_FLAGS & t->t_and_mask) | t->t_or_mask;
            return ED_OK;
        }
    }

    /* Option not found in table, try the special ones. */

    if ( !strcmp(word,"save") ) {
        svalue_t *ret;
        push_ref_object(inter_sp, command_giver, "save ed");
        push_number(inter_sp, P_SHIFTWIDTH | P_FLAGS );
        ret = apply_master(STR_SAVE_ED,2);
        if ( ret && ret->type==T_NUMBER && ret->u.number > 0 )
            return ED_OK;
    }

    if ( !strcmp(word,"shiftwidth") ) {
        Skip_White_Space;
        if ( isdigit((unsigned char)*inptr) ) {
            P_SHIFTWIDTH = *inptr-'0';
            return ED_OK;
        }
    }

    /* Option not recognized */

    return SET_FAIL;
}

/*-------------------------------------------------------------------------*/
#ifndef relink
static void
relink (LINE *a, LINE *x, LINE *y, LINE *b)

{
    x->l_prev = a;
    y->l_next = b;
}
#endif


/*-------------------------------------------------------------------------*/
static INLINE void /* only used once */
set_ed_buf (void)

/* Initialize the line parameters in the ed buffer.
 */

{
    relink(&P_LINE0, &P_LINE0, &P_LINE0, &P_LINE0);
    P_CURLN = P_LASTLN = 0;
    P_CURPTR = &P_LINE0;
}


/*-------------------------------------------------------------------------*/
static INLINE int /* only used once */
subst (regexp_t *pat, char *sub, Bool gflg, Bool pflag)

/* Scan the range P_LINE1 to P_LINE2 and replace in every line matching <pat>
 * the matched pattern by <sub>. If <gflg> is true, all matching patterns
 * in the line are changed. If <pflg> is true, the changed lines are printed.
 * Return the number of changed lines, or a failure code.
 */

{
    int    nchngd = 0;
    char  *new, *old, buf[MAXLINE];
    Bool   still_running = TRUE;
    LINE  *lastline = getptr( P_LINE2 );

    if(P_LINE1 <= 0)
        return SUB_FAIL;

    nchngd = 0;        /* reset count of lines changed */

    for (setCurLn( prevln( P_LINE1 ) ); still_running; )
    {
        char    *start, *current;
        int    space, rc;

        nextCurLn();
        new = buf;
        if ( P_CURPTR == lastline )
            still_running = FALSE;

        current = start = gettxtl(P_CURPTR);
        rc = rx_exec_str(pat, current, start);
        if (rc < 0)
        {
            add_message("ed: %s\n", rx_error_message(rc, pat) );
            return SUB_FAIL;
        }
        if ( rc )
        {
            space = MAXLINE;
            do
            {
                /* Copy leading text */
                size_t mstart, mend;
                size_t diff;
                string_t * substr;

                rx_get_match_str(pat, start, &mstart, &mend);
                diff = start + mstart - current;
                if ( (space -= diff) < 0)
                    return SUB_FAIL;
                strncpy( new, current, diff );
                new += diff;
                /* Do substitution */
                old = new;
                substr = rx_sub_str( pat, current, sub);
                if (!substr)
                    return SUB_FAIL;
                if ((space-= mstrsize(substr)) < 0)
                {
                    free_mstring(substr);
                    return SUB_FAIL;
                }
                memcpy(new, get_txt(substr), mstrsize(substr));
                new += mstrsize(substr);
                free_mstring(substr);

                if (current == start + mend)
                {
                    /* prevent infinite loop */
                    if (!*current)
                        break;
                    if (--space < 0)
                        return SUB_FAIL;
                    *new++ = *current++;
                }
                else
                    current = start + mend;
            } while(gflg
                 && !rx_reganch(pat)
                 && (rc = rx_exec_str(pat, current, start)) > 0);

            if (rc < 0)
            {
                add_message("ed: %s\n", rx_error_message(rc, pat) );
                return SUB_FAIL;
            }

            /* Copy trailing chars */
            if ( (space -= strlen(current)+1 ) < 0)
                return SUB_FAIL;
            strcpy(new, current);
            del (P_CURLN,P_CURLN);
            if (ins(buf) < 0)
                return MEM_FAIL;
            nchngd++;
            if (pflag)
                doprnt(P_CURLN, P_CURLN);
        } /* if(rc) */
    } /* for() */
    return (( nchngd == 0 && !gflg ) ? SUB_FAIL : nchngd);
} /* subst() */

/*-------------------------------------------------------------------------*/
static void
detab_line (char *buf, int tabsize)

/* replace all possible '\t'ab characters with whitespace ' '
 * in the given string <buf> and replace the current line with
 * the result. <tabsize> is the desired tab spacing.
 */

{
    int i;                /* i: index of buffer */
    int h;                 /* h: index of result */
    int space;             /* counter for whitspace */
    char result[MAXLINE];  /* the detabbed result */

    for (i = 0, h = 0; buf[i] != '\0'; i++)
    {
        if (h == MAXLINE )
        {
            add_message("line too long.\n");
            return;
        }

        switch (buf[i])
        {
        case '\t':
            /* replace \t by up tu tabsize spaces, depending on position */

            for (space = tabsize - (h % tabsize); space--; h++)
            {
              if (h == MAXLINE)
              {
                  add_message("line too long.\n");
                  return;
              }

              result[h] = ' ';
            }

            break;

        default:
            result[h] = buf[i];
            h++;
            break;
        }
    }

    /* terminate result string */

    result[h] = '\0';

    /* replace current line by result */

    del(P_CURLN,P_CURLN);
    ins(result);
} /* detab_line() */

/*-------------------------------------------------------------------------*/
static void
tab_line (char *buf, int tabsize)

/* replace whitespace ' ' with '\t'ab-characters where it makes sense
 * in the given string <buf> and replace the current line with the result.
 * the result. <tabsize> is the desired tab spacing.
 *
 * TODO: whitespace to tab replacement makes only sense if the '\t'ab-char
 * TODO:: replaces more than one whitespace ' '. Not everyone may share
 * TODO:: this opinion, so it maybe this should be optional.
 */

{
    int i;                 /* i: index of buffer */
    int h;                 /* h: index of result */
    int space, pos;        /* whitespace & position counter */
    char result[MAXLINE];  /* the tabbed result */

    for (i = 0, h = 0, space = 0, pos = 0; buf[i] != '\0'; i++)
    {
        switch (buf[i])
        {
        case ' ':
            pos++;
            space++;

            if (! (pos % tabsize))
            {
                if (space == 1)
                {
                    /* makes no sense to replace 1 space by '\t'ab */
                    result[h] = ' ';
                    h++;
                }
                else
                {
                    result[h] = '\t';
                    h++;
                }

                pos = 0;
                space = 0;
            }
            break;

        case '\t':
            if (!space && (pos % tabsize) == tabsize - 1)
            {
                /* remove unnecessary tabs */
                result[h] = ' ';
                h++;
                pos++;
            }
            else
            {
                /* don't put unnecessary spaces in result */
                result[h] = '\t';
                h++;
                pos = 0;
                space = 0;
            }
            break;

        default:
            /* add spaces which couldn't be replaced */
            for (; space--; h++)
            {
                result[h] = ' ';
            }

            result[h] = buf[i];
            h++;

            pos++;
            space = 0;

            break;
        }
    }

    /* terminate result string */

    result[h] = '\0';

    /* replace current line by result */

    del(P_CURLN,P_CURLN);
    ins(result);
} /* tab_line() */

/*-------------------------------------------------------------------------*/
static int
tab_conversion (int from, int to, int tabsize, Bool do_detab)

/* Perform tab character conversion on the given range [<from>, <to>].
 * <tabsize> is the desired tab spacing, or 0 for the default.
 * <do_detab> is TRUE for the Tab->Whitespace conversion, and FALSE
 * for the Whitespace->Tab conversion.
 */

{
    from = (from < 1) ? 1 : from;
    to = (to > P_LASTLN) ? P_LASTLN : to;

    if (tabsize <= 0)
    {
        tabsize = DEFAULT_TABSIZE;
    }

    if (to != 0)
    {
        _setCurLn( from );
        while( P_CURLN <= to )
        {
            if (do_detab)
            {
                detab_line( gettxtl( P_CURPTR ), tabsize );
            }

            else
            {
                tab_line( gettxtl( P_CURPTR ), tabsize );
            }

            if( P_CURLN == to )
                break;

            nextCurLn();
        }
    }

    return ED_OK;
} /* tab_conversion() */

/*=========================================================================*/
/*
 * Adapted indent code from DGD editor (v0.1).
 * No attempt has been made to optimize for this editor.
 *  -- Dworkin 920510
 */

# define add_errorf(s)        { add_message(s, lineno); errs++; return; }

static int lineno, errs;
static int shi;        /* the current shift (negative for left shift) */
static int full_shift, small_shift;

# define STACKSZ    1024    /* size of indent stack */

/* token definitions in indent */
# define SEMICOLON   0
# define LBRACKET    1
# define RBRACKET    2
# define LOPERATOR   3
# define ROPERATOR   4
# define LHOOK       5
# define LHOOK2      6
# define RHOOK       7
# define TOKEN       8
# define ELSE        9
# define IF         10
# define FOR        11
# define WHILE      12
# define DO         13
# define XEOT       14

static char *stack, *stackbot;    /* token stack */
static int *ind, *indbot;    /* indent stack */
static char quote;        /* ' or " */
static Bool in_ppcontrol, in_comment, after_keyword;    /* status */

/*-------------------------------------------------------------------------*/
static void
shift (char *text)

/* Shift a line left or right according to "shi".
 */

{
    register int indent_index;

    /* first determine the number of leading spaces */
    indent_index = 0;
    while (*text == ' ' || *text == '\t')
    {
        if (*text++ == ' ')
        {
            indent_index++;
        }
        else
        {
            indent_index = (indent_index + 8) & ~7;
        }
    }

    if (*text != '\0') /* don't shift lines with ws only */
    {
        indent_index += shi;
        if (indent_index < MAXLINE)
        {
            char buffer[MAXLINE];
            register char *p;

            p = buffer;
            /* fill with leading ws */
            if (P_TABINDENT) while (indent_index >= 8)
            {
                *p++ = '\t';
                indent_index -= 8;
            }

            while (indent_index > 0)
            {
                *p++ = ' ';
                --indent_index;
            }

            if (p - buffer + strlen(text) < MAXLINE)
            {
                strcpy(p, text);
                del(lineno, lineno);
                ins(buffer);
                return;
            }
        }

        add_errorf("Result of shift would be too long, line %d\n");
    }
}

/*-------------------------------------------------------------------------*/
static void
indent (char *buf)

/* Parse and indent a line of text. This isn't perfect, as
 * keywords could be defined as macros, comments are very hard to
 * handle properly, (, [ and ({ will match any of ), ] and }),
 * and last but not least everyone has his own taste of
 * indentation.
 */

{
/*                      ;  {  }  (  )  [  ([ ]  tok el if fo whi do xe   */
/*                      (  ({ )  en  se    r  le     ot   */
    static char f[] = { 7, 1, 7, 1, 2, 1, 1, 6, 4,  2, 6, 7, 7,  2, 0, };
    static char g[] = { 2, 2, 1, 7, 1, 5, 5, 1, 3,  6, 2, 2, 2,  2, 0, };
    char text[MAXLINE], ident[MAXLINE];
    register char *p, *sp;
    register int *ip;
    register long indent_index;
    register int top, token;
    char *start;
    Bool do_indent;

    /*
     * Problem: in this editor memory for deleted lines is reclaimed. So
     * we cannot shift the line and then continue processing it, as in
     * DGD ed. Instead make a copy of the line, and process the copy.
     */
    strcpy(text, buf);

    do_indent = FALSE;
    indent_index = 0;
    p = text;

    /* process status vars */
    if (quote != '\0')
    {
        shi = 0;    /* in case a comment starts on this line */
    }
    else if (in_ppcontrol || (*p == '#' && p[1] != '\'') )
    {
        while (*p != '\0')
        {
            if (*p == '\\' && *++p == '\0')
            {
                in_ppcontrol = TRUE;
                return;
            }
            p++;
        }
        in_ppcontrol = FALSE;
        return;
    }
    else
    {
        /* count leading ws */
        while (*p == ' ' || *p == '\t')
        {
            if (*p++ == ' ')
            {
                indent_index++;
            }
            else
            {
                indent_index = (indent_index + 8) & ~7;
            }
        }
        if (*p == '\0')
        {
            del(lineno, lineno);
            ins(p);
            return;
        }
        else if (in_comment)
        {
            shift(text);    /* use previous shi */
        }
        else
        {
            do_indent = TRUE;
        }
    }

    /* process this line */
    start = p;
    while (*p != '\0') {

        /* lexical scanning: find the next token */
        ident[0] = '\0';
        if (in_comment)
        {
            /* comment */
            while (*p != '*')
            {
                if (*p == '\0')
                {
                    return;
                }
                p++;
            }
            while (*p == '*')
            {
                p++;
            }
            if (*p == '/')
            {
                in_comment = FALSE;
                p++;
            }
            continue;

        }
        else if (quote != '\0')
        {
            /* string or character constant */
            for (;;)
            {
                if (*p == quote)
                {
                    quote = '\0';
                    p++;
                    break;
                }
                else if (*p == '\0')
                {
                    add_errorf("Unterminated string in line %d\n");
                }
                else if (*p == '\\' && *++p == '\0')
                {
                    break;
                }
                p++;
            }
            token = TOKEN;
        }
        else
        {
            switch (*p++)
            {
            case ' ':    /* white space */
            case '\t':
                continue;

            case '\'':
                if (isalunum(*p) && p[1] && p[1] != '\'')
                {
                    do ++p; while (isalunum(*p));
                    token = TOKEN;
                    break;
                }
                if (*p == '(' && p[1] == '{')
                {
                    /* treat quoted array like an array */
                    token = TOKEN;
                    break;
                }
                /* FALLTHROUGH */
            case '"':    /* start of string */
                quote = p[-1];
                continue;

            case '/':
                if (*p == '*')     /* start of comment */
                {
                    in_comment = TRUE;
                    if (do_indent)
                    {
                        /* this line hasn't been indented yet */
                        shi = *ind - indent_index;
                        shift(text);
                        do_indent = FALSE;
                    }
                    else
                    {
                        register char *q;
                        register int index2;

                        /* find how much the comment has shifted, so the same
                         * shift can be used if the coment continues on the
                         * next line
                         */
                        index2 = *ind;
                        for (q = start; q < p - 1;)
                        {
                            if (*q++ == '\t')
                            {
                                indent_index = (indent_index + 8) & ~7;
                                index2 = (index2 + 8) & ~7;
                            }
                            else
                            {
                                indent_index++;
                                index2++;
                            }
                        }
                        shi = index2 - indent_index;
                    }
                    p++;
                    continue;
                }
                if (*p == '/')     /* start of C++ style comment */
                {
                    p = strchr(p, '\0');
                }
                token = TOKEN;
                break;

            case '{':
                token = LBRACKET;
                break;

            case '(':
                if (after_keyword)
                {
                    /* LOPERATOR & ROPERATOR are a kludge. The operator
                     * precedence parser that is used could not work if
                     * parenthesis after keywords was not treated specially.
                     */
                    token = LOPERATOR;
                    break;
                }
                if (*p == '{' || *p == '[')
                {
                    p++;    /* ({ , ([ each are one token */
                    token = LHOOK2;
                    break;
                }
                /* FALLTHROUGH */
            case '[':
                token = LHOOK;
                break;

            case '}':
                if (*p != ')')
                {
                    token = RBRACKET;
                    break;
                }
                /* }) is one token */
                p++;
                token = RHOOK;
                break;

            case ']':
                if (*p == ')'
                 && (*stack == LHOOK2 || (*stack != XEOT
                 && ( stack[1] == LHOOK2
                     || ( stack[1] == ROPERATOR && stack[2] == LHOOK2) ) ) ) )
                {
                    p++;
                }
                /* FALLTHROUGH */
            case ')':
                token = RHOOK;
                break;

            case ';':
                token = SEMICOLON;
                break;

            case '#':
                if (*p == '\'')
                {
                    ++p;
                    if (isalunum(*p))
                    {
                        do ++p; while (isalunum(*p));
                    }
                    else
                    {
                        const char *end;

                        if (symbol_operator(p, &end) < 0)
                        {
                            add_errorf("Missing function name after #' in line %d\n");
                        }
                        p = (char *)end;
                    }
                    token = TOKEN;
                    break;
                }
                /* FALLTHROUGH */

            default:
                if (isalpha((unsigned char)*--p) || *p == '_')
                {
                    register char *q;

                    /* Identifier. See if it's a keyword. */
                    q = ident;
                    do
                    {
                        *q++ = *p++;
                    } while (isalnum((unsigned char)*p) || *p == '_');
                    *q = '\0';

                    if      (strcmp(ident, "if"   ) == 0)    token = IF;
                    else if (strcmp(ident, "else" ) == 0)    token = ELSE;
                    else if (strcmp(ident, "for"  ) == 0)    token = FOR;
                    else if (strcmp(ident, "while") == 0)    token = WHILE;
                    else if (strcmp(ident, "do"   ) == 0)    token = DO;
                    else    /* not a keyword */              token = TOKEN;
                }
                else
                {
                    /* anything else is a "token" */
                    p++;
                    token = TOKEN;
                }
                break;
            }
        }

        /* parse */

        sp = stack;
        ip = ind;
        for (;;)
        {
            top = *sp;
            if (top == LOPERATOR && token == RHOOK)
            {
                /* ) after LOPERATOR is ROPERATOR */
                token = ROPERATOR;
            }

            if (f[top] <= g[token])     /* shift the token on the stack */
            {
                register int i;

                if (sp == stackbot)
                {
                    /* out of stack */
                    add_errorf("Nesting too deep in line %d\n");
                }

                /* handle indentation */
                i = *ip;
                /* if needed, reduce indentation prior to shift */
                if ((token == LBRACKET
                     && (*sp == ROPERATOR || *sp == ELSE || *sp == DO))
                 || token == RBRACKET || (token == IF && *sp == ELSE))
                {
                    /* back up */
                    i -= full_shift;
                }
                else if (token == RHOOK || token == ROPERATOR)
                {
                    i -= small_shift;
                }
                /* shift the current line, if appropriate */
                if (do_indent)
                {
                    shi = i - indent_index;
                    if (token == TOKEN && *sp == LBRACKET
                     && (   strcmp(ident, "case") == 0
                         || strcmp(ident, "default") == 0))
                    {
                        /* back up if this is a switch label */
                        shi -= full_shift;
                    }
                    shift(text);
                    do_indent = FALSE;
                }

                /* change indentation after current token */
                switch (token)
                {
                case LBRACKET: case ROPERATOR: case ELSE: case DO:
                    /* add indentation */
                    i += full_shift;
                    break;

                case LOPERATOR: case LHOOK: case LHOOK2:
                    /* half indent after ( [ ({ ([ */
                    i += small_shift;
                    break;

                case SEMICOLON:
                    /* in case it is followed by a comment */
                    if (*sp == ROPERATOR || *sp == ELSE)
                    {
                        i -= full_shift;
                    }
                    break;
                }

                *--sp = (char)token;
                *--ip = i;
                break;
            }

            /* reduce handle */
            do
            {
                top = *sp++;
                ip++;
            } while (f[(int)*sp] >= g[top]);
        }
        stack = sp;
        ind = ip;
        after_keyword = (token >= IF);    /* but not after ELSE */
    }
}

/*-------------------------------------------------------------------------*/
static int
indent_code (int from, int to)

/* Indent the code in the range <from> to <to>.
 * Return success code.
 */

{
    char s[STACKSZ];
    int i[STACKSZ];

    /* setup stacks */
    stackbot = s;
    indbot = i;
    stack = stackbot + STACKSZ - 1;
    *stack = XEOT;
    ind = indbot + STACKSZ - 1;
    *ind = 0;

    quote = '\0';
    in_ppcontrol = FALSE;
    in_comment = FALSE;

    P_FCHANGED = TRUE;
    errs = 0;
    full_shift = P_SHIFTWIDTH;
    small_shift = full_shift / 2;

    for (lineno = from; lineno <= to; lineno++)
    {
        _setCurLn(lineno);
        indent(gettxtl(P_CURPTR));
        if (errs != 0)
        {
            return ERR;
        }
    }

    return ED_OK;
}

# undef error

/* End of indent code */
/*=========================================================================*/

/*-------------------------------------------------------------------------*/
static int
docmd (Bool glob)

/* Read the command letter from the input buffer and execute the command.
 * All other command parameters are expected to be set up by now.
 * If <glob> is true and the line designation is a pattern, the command
 * is applied to all lines, else only to the first.
 * Return success code, with TRUE meaning 'just changed current line'.
 */

{
    static char  rhs[MAXPAT];
    regexp_t    *subpat;
    int          c, err, line3, lastcmd;
    int          apflg, pflag, gflag;
    int          nchng;
    string_t    *fptr;

    pflag = FALSE;
    Skip_White_Space;

    lastcmd = P_LASTCMD;

    P_LASTCMD = c = *inptr++;
    switch(c)
    {
    case NL:
        if (P_NLINES == 0 && (P_LINE2 = nextln(P_CURLN)) == 0 )
            return ERR;
        setCurLn( P_LINE2 );
        return TRUE;

    case '=':
        add_message("%d\n",P_LINE2);
        break;

    case 'a':
    case 'A':
        P_CUR_AUTOIND = c=='a' ? P_AUTOINDFLG : !P_AUTOINDFLG;
        if (*inptr != NL || P_NLINES > 1)
            return ERR;

        if ( P_CUR_AUTOIND ) count_blanks(P_LINE1);
        if (append(P_LINE1, glob) < 0)
            return ERR;
        P_FCHANGED = TRUE;
        break;

    case 'c':
        if (*inptr != NL)
            return ERR;

        if (deflt(P_CURLN, P_CURLN) < 0)
            return ERR;

        P_CUR_AUTOIND = P_AUTOINDFLG;
        if (P_AUTOINDFLG ) count_blanks(P_LINE1);
        if (del(P_LINE1, P_LINE2) < 0)
            return ERR;
        if (append(P_CURLN, glob) < 0)
            return ERR;
        P_FCHANGED = TRUE;
        break;

    case 'd':
        if (*inptr != NL)
            return ERR;

        if (deflt(P_CURLN, P_CURLN) < 0)
            return ERR;

        if (del(P_LINE1, P_LINE2) < 0)
            return ERR;
        if (nextln(P_CURLN) != 0)
            nextCurLn();
        if (P_PFLG)
            doprnt(P_CURLN, P_CURLN);
        P_FCHANGED = TRUE;
        break;

    case 'e':
        if (P_NLINES > 0)
            return ERR;
        if (P_FCHANGED)
            return CHANGED;
        /*FALL THROUGH*/
    case 'E':
        if (P_NLINES > 0)
            return ERR;

        if (*inptr != ' ' && *inptr != HT && *inptr != NL)
            return ERR;

        if ((fptr = getfn(MY_FALSE)) == NULL)
            return ERR;

        clrbuf();
        (void)doread(0, fptr);

        P_FNAME = fptr;
        P_FCHANGED = FALSE;
        break;

    case 'f':
        if (P_NLINES > 0)
            return ERR;

        if (*inptr != ' ' && *inptr != HT && *inptr != NL)
            return ERR;

        fptr = getfn(MY_FALSE);

        if (P_NOFNAME)
        {
            if (P_FNAME)
                add_message("%s\n", get_txt(P_FNAME));
            else
                add_message("<no file>\n");
        }
        else
        {
            if (fptr == NULL)
                return ERR;
            P_FNAME = fptr;
        }
        break;

    case 'i':
        if (*inptr != NL || P_NLINES > 1)
            return ERR;

        P_CUR_AUTOIND = P_AUTOINDFLG;
        if (P_AUTOINDFLG ) count_blanks(P_LINE1);
        if (append(prevln(P_LINE1), glob) < 0)
            return ERR;
        P_FCHANGED = TRUE;
        break;

    case 'j':
        if (*inptr != NL || deflt(P_CURLN, P_CURLN+1)<0)
            return(ERR);

        if (join(P_LINE1, P_LINE2) < 0)
            return ERR;
        break;

    case 'k':
        Skip_White_Space;

        if (*inptr < 'a' || *inptr > 'z')
            return ERR;
        c= *inptr++;

        if (*inptr != ' ' && *inptr != HT && *inptr != NL)
            return ERR;

        P_MARK[c-'a'] = P_LINE1;
        break;

    case 'l':
        if (*inptr != NL)
            return ERR;
        if (deflt(P_CURLN,P_CURLN) < 0)
            return ERR;
        if (dolst(P_LINE1,P_LINE2) < 0)
            return ERR;
        break;

    case 'm':
        if ((line3 = getone()) < 0)
            return ERR;
        if (deflt(P_CURLN,P_CURLN) < 0)
            return ERR;
        if (move(line3) < 0)
            return ERR;
        P_FCHANGED = TRUE;
        break;

    case 'M':
      {
        regexp_t * pat;

        if (deflt(1, P_LASTLN) < 0)
            return ERR;
        if (*inptr != NL)
            return ERR;


        pat = rx_compile(STR_CRPATTERN, P_EXCOMPAT ? RE_EXCOMPATIBLE : 0, MY_TRUE);
        nchng = subst(pat, "", 0, 0);
        free_regexp(pat);

        if (nchng < 0)
            return ERR;
        P_FCHANGED = TRUE;
        break;
      }

    case 'n':
        if (P_NFLG)
            P_FLAGS &= ~( NFLG_MASK | LFLG_MASK );
        else
            P_FLAGS |=  ( NFLG_MASK | LFLG_MASK );
        P_DIAG=!P_DIAG;
        add_message("number %s, list %s\n"
                   , P_NFLG?"ON":"OFF", P_LFLG?"ON":"OFF");
        break;

    case 'I':
        if (deflt(1, P_LASTLN) < 0)
            return ERR ;
        if (*inptr != NL)
            return ERR;
        if (!P_NLINES)
            add_message("Indenting entire code...\n");
        if (indent_code(P_LINE1, P_LINE2))
            add_message("Indention halted.\n");
        else
            add_message("Done indenting.\n");
        break;

    case 'H':
    case 'h':
        print_help(*(inptr++));
        break;

    case 'P':
    case 'p':
        if (*inptr != NL)
            return ERR;
        if (deflt(P_CURLN,P_CURLN) < 0)
            return ERR;
        if (doprnt(P_LINE1,P_LINE2) < 0)
            return ERR;
        break;

    case 'q':
        if (P_FCHANGED)
            return CHANGED;
        /*FALL THROUGH*/
    case 'Q':
        if (*inptr != NL || glob)
            return ERR;
        clrbuf();
        if (*inptr == NL && P_NLINES == 0 && !glob)
            return EOF;
        else /* Just in case clrbuf() fails */
            return ERR;

    case 'r':
        if (P_NLINES > 1)
            return ERR;

        if (P_NLINES == 0)
            P_LINE2 = P_LASTLN;

        if (*inptr != ' ' && *inptr != HT && *inptr != NL)
            return ERR;

        if ((fptr = getfn(MY_FALSE)) == NULL)
            return ERR;

        if ((err = doread(P_LINE2, fptr)) < 0)
        {
            free_mstring(fptr);
            return err;
        }
        free_mstring(fptr);
        P_FCHANGED = TRUE;
        break;

    case 's':
        if(*inptr == 'e')
            return set();
        Skip_White_Space;
        if((subpat = optpat()) == NULL)
            return ERR;
        if((gflag = getrhs(rhs)) < 0)
            return ERR;
        if(*inptr == 'p')
            pflag++;
        if(deflt(P_CURLN, P_CURLN) < 0)
            return ERR;
        if((nchng = subst(subpat, rhs, gflag, pflag)) < 0)
            return ERR;
        if(nchng)
            P_FCHANGED = TRUE;
        if (nchng == 1 && P_PFLG )
        {
            if(doprnt(P_CURLN, P_CURLN) < 0)
                return ERR;
        }
        break;

    case 't':
        if ((line3 = getone()) < 0)
            return ERR;
        if (deflt(P_CURLN,P_CURLN) < 0)
            return ERR;
        if (transfer(line3) < 0)
            return ERR;
        P_FCHANGED = TRUE;
        break;

    case 'T':
      {
        int tabsize;
        Bool do_detab;

        switch(*inptr)
        {
          case '+':
              do_detab = MY_FALSE;
              break;

          case '-':
              do_detab = MY_TRUE;
              break;

          default:
              return ERR;
        }

        inptr++;
        tabsize = atoi(inptr);

        if (deflt(P_CURLN,P_CURLN) < 0)
            return ERR;

        if (tab_conversion(P_LINE1, P_LINE2, tabsize, do_detab) < 0)
            return ERR;

        break;
      }

    case 'W':
    case 'w':
        apflg = (c=='W');

        if (*inptr != ' ' && *inptr != HT && *inptr != NL)
            return ERR;

        if ((fptr = getfn(MY_TRUE)) == NULL)
            return ERR;

        if (deflt(1, P_LASTLN) < 0)
        {
            free_mstring(fptr);
            return ERR;
        }
        if (dowrite(P_LINE1, P_LINE2, fptr, apflg) < 0)
        {
            free_mstring(fptr);
            return ERR;
        }
        free_mstring(fptr);
        P_FCHANGED = FALSE;
        break;

    case 'x':
        if (*inptr == NL && P_NLINES == 0 && !glob)
        {
            if ((fptr = getfn(MY_TRUE)) == NULL)
                return ERR;
            if (dowrite(1, P_LASTLN, fptr, 0) >= 0
             && command_giver && command_giver->flags & O_SHADOW)
            {
                free_mstring(fptr);
                return EOF;
            }
            free_mstring(fptr);
        }
        return ERR;

    case 'z':
      {
        int dfln;

        switch(*inptr)
        {
        case '-':
            dfln = P_CURLN;
            if (deflt(dfln,dfln) < 0)
                return ERR;
            if (doprnt(P_LINE1-21,P_LINE1) < 0)
                return ERR;
            break;

        case '.':
            dfln = P_CURLN;
            if (deflt(dfln,dfln) < 0)
                return ERR;
            if (doprnt(P_LINE1-11,P_LINE1+10) < 0)
                return ERR;
            break;

        case '+':
        case '\n':
            if (lastcmd == 'z' || lastcmd == 'Z')
                dfln = P_CURLN != 1 ? P_CURLN + 1 : 1;
            else
                dfln = P_CURLN;
            if (deflt(dfln,dfln) < 0)
                return ERR;
            if (doprnt(P_LINE1,P_LINE1+21) < 0)
                return ERR;
            break;
        }
        break;
      }

    case 'Z':
      {
        int dfln;

        switch(*inptr)
        {
        case '-':
            dfln = P_CURLN;
            if (deflt(dfln,dfln) < 0)
                return ERR;
            if (doprnt(P_LINE1-41,P_LINE1) < 0)
                return ERR;
            break;

        case '.':
            dfln = P_CURLN;
            if (deflt(dfln,dfln) < 0)
                return ERR;
            if (doprnt(P_LINE1-21,P_LINE1+20) < 0)
                return ERR;
            break;

        case '+':
        case '\n':
            if (lastcmd == 'z' || lastcmd == 'Z')
                dfln = P_CURLN != 1 ? P_CURLN + 1 : 1;
            else
                dfln = P_CURLN;
            if (deflt(dfln,dfln) < 0)
                return ERR;
            if (doprnt(P_LINE1,P_LINE1+41) < 0)
                return ERR;
            break;
        }
        break;
      }

    default:
        return ERR;
    }

    return ED_OK;
}  /* docmd */


/*-------------------------------------------------------------------------*/
static INLINE int /* only used once */
doglob (void)

/* Call docmd(TRUE) for every line marked with LGLOB, clearing that mark
 * in this.
 * Return the number of the last current line, or an error code on failure.
 */

{
    int    lin, status;
    char    *cmd;
    LINE    *ptr;

    cmd = inptr;

    for (;;)
    {
        ptr = getptr(1);
        for (lin=1; lin<=P_LASTLN; lin++)
        {
            if (ptr->l_stat & LGLOB)
                break;
            ptr = getnextptr(ptr);
        }
        if (lin > P_LASTLN)
            break;

        ptr->l_stat &= ~LGLOB;
        P_CURLN = lin; P_CURPTR = ptr;
        inptr = cmd;
        if ((status = getlst()) < 0)
            return status;
        if ((status = docmd(TRUE)) < 0)
            return status;
    }
    return P_CURLN;
}  /* doglob */


/*-------------------------------------------------------------------------*/
static void
ed_start (string_t *file_arg, string_t *exit_fn, object_t *exit_ob)

/* Start the editor on file <file_arg>. Because several players can edit
 * simultaneously, they will each need a separate editor data block.
 *
 * If an <exit_fn> and <exit_ob> is given, then call <exit_ob>-><exit_fn>()
 * at exit of editor. The purpose is to make it possible for external LPC
 * code to maintain a list of locked files.
 */

{
    string_t *new_path;
    svalue_t *setup;
    ed_buffer_t *old_ed_buffer;
    interactive_t *ip;

    if (!command_giver || !(O_SET_INTERACTIVE(ip, command_giver)))
        errorf("Tried to start an ed session on a non-interative player.\n");

    /* Check for read on startup, since the buffer is read in. But don't
     * check for write, since we may want to change the file name.
     */
    new_path = check_valid_path(file_arg, command_giver, STR_ED_START, MY_FALSE);
    if (!file_arg && !new_path)
        return;

    /* Never trust the master... it might be as paranoid as ourselves...
     */
    if (!command_giver
     || !(command_giver->flags & O_SHADOW)
     || command_giver->flags & O_DESTRUCTED
       )
    {
        return;
    }

    old_ed_buffer = ED_BUFFER;
    ED_BUFFER = (ed_buffer_t *)xalloc(sizeof (ed_buffer_t));

    memset(ED_BUFFER, '\0', sizeof (ed_buffer_t));

    ED_BUFFER->input.type = INPUT_ED;

    ED_BUFFER->truncflg = MY_TRUE;
    ED_BUFFER->flags |= EIGHTBIT_MASK | TABINDENT_MASK;
    ED_BUFFER->shiftwidth= 4;
    put_ref_string(&(ED_BUFFER->input.prompt), STR_ED_PROMPT);
    ED_BUFFER->CurPtr = &ED_BUFFER->Line0;

    add_input_handler(ip, &(ED_BUFFER->input), MY_FALSE);

    if (exit_fn)
    {
        ED_BUFFER->exit_fn = ref_mstring(exit_fn);
        ref_object(exit_ob, "ed_start");
    }
    else
    {
        ED_BUFFER->exit_fn = NULL;
    }
    ED_BUFFER->exit_ob = exit_ob;

    set_ed_buf();
    push_apply_value();
    push_ref_object(inter_sp, command_giver, "retr ed");

    setup = apply_master(STR_RETR_ED,1);
    if ( setup && setup->type==T_NUMBER && setup->u.number )
    {
        ED_BUFFER->flags      = setup->u.number & ALL_FLAGS_MASK;
        ED_BUFFER->shiftwidth = setup->u.number & SHIFTWIDTH_MASK;
    }

    /* It is possible to toggle P_DIAG in retrieve_ed_setup(), by issueing
     * an 'n' command(), which will cause add_message() to be called in
     * do_read(); add_message might in turn call apply() via
     * shadow_catch_message(), thus new_path needs to stay pushed.
     */

    if (new_path && !doread(0, new_path))
    {
        _setCurLn( 1 );
    }

    if (new_path)
    {
        P_FNAME = new_path;
        add_message("/%s, %d lines\n", get_txt(new_path), P_LASTLN);
    }
    else
    {
        add_message("No file.\n");
    }
    pop_apply_value();
    ED_BUFFER = old_ed_buffer;
}


#ifdef GC_SUPPORT

/*-------------------------------------------------------------------------*/
void
clear_ed_buffer_refs (input_t *ih)

/* GC Support: Clear all references from ed_buffer <b>.
 */

{
    ed_buffer_t *b = (ed_buffer_t*) ih;
    object_t *ob;

    if (b->fname)
        clear_string_ref(b->fname);

    if (b->exit_fn)
    {
        clear_string_ref(b->exit_fn);
        if ( NULL != (ob = b->exit_ob) )
        {
            clear_object_ref(ob);
        }
    }

    /* For the RE cache */
    clear_regexp_ref(b->oldpat);

    clear_ref_in_vector(&b->input.prompt, 1);
}

/*-------------------------------------------------------------------------*/
void
count_ed_buffer_refs (input_t *ih)

/* GC Support: Count all references from ed_buffer <b>.
 */

{
    ed_buffer_t *b = (ed_buffer_t*) ih;
    object_t *ob;
    LINE *line;

    if (b->LastLn)
    {
        line = b->Line0.l_next;
        while(line != &b->Line0)
        {
            note_malloced_block_ref((char *)line);
            line = line->l_next;
        }
    }

    if (b->fname)
        count_ref_from_string(b->fname);

    if (b->exit_fn)
    {
        count_ref_from_string(b->exit_fn);
        if ( NULL != (ob = b->exit_ob) )
        {
            if (ob->flags & O_DESTRUCTED)
            {
                reference_destructed_object(ob);
                b->exit_ob = NULL;
            }
            else
            {
                ob->ref++;
            }
        }
    }

    if (b->oldpat)
        count_regexp_ref(b->oldpat);
    count_ref_in_vector(&b->input.prompt, 1);
}

#endif /* GC_SUPPORT */

#ifdef DEBUG
/*-------------------------------------------------------------------------*/
void
count_ed_buffer_extra_refs (input_t *ih)

/* Count refs in ed_buffer <b> to debug refcounts.
 */

{
    ed_buffer_t *b = (ed_buffer_t*) ih;
    object_t *ob;

    if ( NULL != (ob = b->exit_ob) )
        ob->extra_ref++;
}

#endif /* DEBUG */

/*-------------------------------------------------------------------------*/
void
free_ed_buffer (input_t *ih)

/* Deallocate the ed_buffer of the command_giver and call the exit function
 * if set.
 *
 * ED_BUFFER won't be referenced unless set anew. There must be no errors
 * here, because there might be a call from remove_interactive().
 */

{
    string_t *name;
    object_t *ob;
    interactive_t *ip;

    ED_BUFFER = EXTERN_ED_BUFFER(ih);

    clrbuf();
    ob   = ED_BUFFER->exit_ob;
    name = ED_BUFFER->exit_fn;
    free_svalue(&ED_BUFFER->input.prompt);

    if(P_OLDPAT)
    {
        free_regexp(P_OLDPAT);
        P_OLDPAT = NULL;
    }

    if (P_FNAME)
        free_mstring(P_FNAME);
    if (O_SET_INTERACTIVE(ip, command_giver))
        remove_input_handler(ip, ih);
    xfree(ED_BUFFER);

    if (name)
    {
        if (!ob || ob->flags & O_DESTRUCTED)
        {
            debug_message("%s ed: exit_ob destructed at eof.\n", time_stamp());
        }
        else
        {
            object_t *save = current_object;

            current_object = ob;
            secure_apply(name, ob, 0);
            current_object = save;
        }
        if (ob)
            free_object(ob, "ed EOF");
        free_mstring(name);
    }
    else
    {
        add_message("Exit from ed.\n");
    }
}

/*-------------------------------------------------------------------------*/
void
ed_cmd (char *str, input_t *ih)

/* Called from the backend with a new line of player input in <str>.
 */

{
    int status;
    ed_buffer_t *old_ed_buffer;

    old_ed_buffer = ED_BUFFER;
    ED_BUFFER = EXTERN_ED_BUFFER(ih);
    if (P_MORE)
    {
        print_help2();
        ED_BUFFER = old_ed_buffer;
        return;
    }

    if (P_APPENDING)
    {
        more_append(str);
        ED_BUFFER = old_ed_buffer;
        return;
    }

    if (strlen(str) < MAXLINE)
        strcat(str, "\n");

    xstrncpy(inlin, str, MAXLINE-1);
    inlin[MAXLINE-1] = 0;
    inptr = inlin;

    if( (status = getlst()) >= 0)
    {
        if((status = ckglob()) != 0)
        {
            if(status >= 0 && (status = doglob()) >= 0)
            {
                _setCurLn( status );
                ED_BUFFER = old_ed_buffer;
                return;
            }
        }
        else
        {
            if((status = docmd(MY_FALSE)) >= 0)
            {
                if(status == 1)
                    doprnt(P_CURLN, P_CURLN);
                ED_BUFFER = old_ed_buffer;
                return;
            }
        }
    }

    switch (status)
    {
    case EOF:
        free_ed_buffer(&(ED_BUFFER->input));
        ED_BUFFER = old_ed_buffer;
        return;

#ifdef FATAL
    case FATAL:
        if (ED_BUFFER->exit_fn)
        {
            xfree(ED_BUFFER->exit_fn);
            free_object(ED_BUFFER->exit_ob, "ed FATAL");
        }
        xfree((char *)ED_BUFFER);
        EXTERN_ED_BUFFER = 0;
        add_message("FATAL ERROR\n");
        ED_BUFFER = old_ed_buffer;
        return;
#endif

    case CHANGED:
        add_message("File has been changed.\n");
        break;

    case SET_FAIL:
        add_message("`set' command failed.\n");
        break;

    case SUB_FAIL:
        add_message("string substitution failed.\n");
        break;

    case MEM_FAIL:
        add_message("Out of memory: text may have been lost.\n" );
        break;

    default:
        add_message("Unrecognized or failed command.\n");
        /*  Unrecognized or failed command (this is SOOOO much better
         * than "?" :-)
         */
    }
    ED_BUFFER = old_ed_buffer;
}

/*-------------------------------------------------------------------------*/
void
save_ed_buffer (input_t *ih)

/* Called when the command_giver is destructed in an edit session.
 * The function calls master->get_ed_buffer_save_file() to get a filename
 * to store the current buffer contents in. If the master doesn't return
 * a filename, the buffer contents are simply discarded.
 */

{
    svalue_t *stmp;
    string_t *fname;
    interactive_t *save;

    (void)O_SET_INTERACTIVE(save, command_giver);
    ED_BUFFER = EXTERN_ED_BUFFER(ih);
    push_ref_string(inter_sp, P_FNAME ? P_FNAME : STR_EMPTY);
    stmp = apply_master(STR_GET_ED_FNAME,1);
    if (save)
    {
        save->catch_tell_activ = MY_FALSE;
        command_giver = save->ob;
    }
    if (stmp && stmp->type == T_STRING) {
        fname = ref_mstring(stmp->u.str);
        if (*get_txt(fname) == '/')
        {
            string_t *tmp;
            tmp = new_n_mstring(get_txt(fname)+1, mstrsize(fname)-1);
            if (!tmp)
            {
                add_message("(ed) Out of memory (%lu bytes) for filename.\n"
                           , (unsigned long)(mstrsize(fname)-1));
                free_mstring(fname);
                return;
            }
            free_mstring(fname);
            fname = tmp;
        }
        dowrite(1, P_LASTLN, fname , MY_FALSE);
        free_mstring(fname);
    }
    free_ed_buffer(ih);
}

/*-------------------------------------------------------------------------*/
svalue_t *
v_ed (svalue_t *sp, int num_arg)

/* EFUN ed()
 *
 *   int ed()
 *   int ed(string file)
 *   int ed(string file, string func)
 *
 * Calling without arguments will start the editor ed with the
 * name of the error file, that was returned by
 * master->valid_read(0, geteuid(this_player()), "ed_start",
 * this_player()), usually something like ~/.err. If that file is
 * empty, ed will immediatly exit again.
 * Calling ed() with argument file will start the editor on the
 * file. If the optional argument func is given, this function
 * will be called after exiting the editor.
 *
 * Result is 1 if the editor could be started, else 0. TODO: ???
 */

{
    if (current_object->flags & O_DESTRUCTED)
    {
        /* could confuse the master... */
        errorf("Calling ed from destructed object.\n");
    }

    if (num_arg == 0)
    {
        ed_start(NULL, NULL, NULL);
        push_number(sp, 1);
    }
    else if (num_arg == 1)
    {
        ed_start(sp->u.str, NULL, NULL);
        free_svalue(sp);
        put_number(sp, 1);
    }
    else /* num_arg == 2 */
    {
        if (sp->type == T_STRING)
            ed_start((sp-1)->u.str, sp->u.str, current_object);
        else /* sp is number 0 */
            ed_start((sp-1)->u.str, NULL, NULL);
        free_svalue(sp--);
        free_svalue(sp);
        put_number(sp, 1);
    }

    return sp;
} /* f_ed() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_query_editing (svalue_t *sp)

/* EFUN: query_editing()
 *
 *   mixed query_editing (object ob)
 *
 * Returns 1 if the ob is interactive and currently editing
 * with ed(). If ed() was called with a function name as
 * second argument, the object where ed() was called is returned,
 * else 0.
 */

{
    object_t      *ob;
    interactive_t *ip;
    input_t       *ih;

    ob = sp->u.ob;
    deref_object(ob, "query_editing");

    if (O_SET_INTERACTIVE(ip, ob)
     && (ih = get_input_handler(ip, INPUT_ED)) != NULL)
    {
        if ( NULL != (ob = ((ed_buffer_t*) ih)->exit_ob) )
            sp->u.ob = ref_object(ob, "query_editing");
        else
            put_number(sp, 1);
    }
    else
    {
        put_number(sp, 0);
    }
    return sp;
}

/*-------------------------------------------------------------------------*/
static void
print_help (char arg)

/* Print the help for command 'arg'.
 */

{
    switch (arg)
    {
    case 'I':
        add_message(
"This command indents your entire file under the\n"
"assumption that it is LPC code.  It is only useful\n"
"for files that are not yet indented, since the\n"
"indentation style is unlikely to satisfy anyone.\n"
"Originally from DGD ed.\n"
               );
        break;

#if 0
    case '^':
        add_message(
"Command: ^   Usage: ^pattern\n"
"This command is similiar to grep, in that it searches the\n"
"entire file, printing every line that contains the specified\n"
"pattern.  To get the line numbers of found lines, turn on line\n"
"number printing with the 'n' command.\n"
               );
        break;
#endif

    case 'n':
        add_message(
"Command: n   Usage: n\n"
"This command toggles the internal flag which will cause line\n"
"numbers to be printed whenever a line is listed.\n"
                   );
        break;

    case 'a':
        add_message(
"Command: a   Usage: a\n"
"Append causes the editor to enter input mode, inserting all text\n"
"starting AFTER the current line. Use a '.' on a blank line to exit\n"
"this mode.\n"
                   );
        break;

    case 'A':
        add_message(
"Command: A   Usage: A\n"
"Like the 'a' command, but uses inverse autoindent mode.\n"
                   );
        break;

    case 'i':
        add_message(
"Command: i   Usage: i\n"
"Insert causes the editor to enter input mode, inserting all text\n"
"starting BEFORE the current line. Use a '.' on a blank line to exit\n"
"this mode.\n"
                   );
        break;

    case 'c':
        add_message(
"Command: c   Usage: c\n"
"Change command causes the current line to be wiped from memory.\n"
"The editor enters input mode and all text is inserted where the previous\n"
"line existed.\n"
                   );
        break;

    case 'd':
        add_message(
"Command: d   Usage: d  or [range]d\n"
"Deletes the current line unless preceeded with a range of lines,\n"
"then the entire range will be deleted.\n"
                   );
        break;

    case 'e':
        add_message(
"Commmand: e  Usage: e filename\n"
"Causes the current file to be wiped from memory, and the new file\n"
"to be loaded in.\n"
                   );
        break;

    case 'E':
        add_message(
"Commmand: E  Usage: E filename\n"
"Causes the current file to be wiped from memory, and the new file\n"
"to be loaded in.  Different from 'e' in the fact that it will wipe\n"
"the current file even if there are unsaved modifications.\n"
                   );
        break;

    case 'f':
        add_message(
"Command: f  Usage: f  or f filename\n"
"Display or set the current filename.   If  filename is given as\n"
"an argument, the file (f) command changes the current filename to\n"
"filename; otherwise, it prints  the current filename.\n"
                   );
        break;

    case 'g':
        add_message(
"Command: g  Usage: g/re/p\n"
"Search in all lines for expression 're', and print\n"
"every match. Command 'l' can also be given\n"
"Unlike in unix ed, you can also supply a range of lines\n"
"to search in\n"
"Compare with command 'v'.\n"
                   );
        break;

    case 'h':
        add_message("Command: h    Usage:  h  or hc (where c is a command)\n");
        break;

    case 'j':
        add_message(
"Command: j    Usage: j or [range]j\n"
"Join Lines. Remove the NEWLINE character  from  between the  two\n"
"addressed lines.  The defaults are the current line and the line\n"
"following.  If exactly one address is given,  this  command does\n"
"nothing.  The joined line is the resulting current line.\n"
                   );
        break;

    case 'k':
        add_message(
"Command: k   Usage: kc  (where c is a character)\n"
"Mark the addressed line with the name c,  a  lower-case\n"
"letter.   The  address-form,  'c,  addresses  the  line\n"
"marked by c.  k accepts one address; the default is the\n"
"current line.  The current line is left unchanged.\n"
                   );
        break;

    case 'l':
        add_message(
"Command: l   Usage: l  or  [range]l\n"
"List the current line or a range of lines in an unambiguous\n"
"way such that non-printing characters are represented as\n"
"symbols (specifically New-Lines).\n"
                   );
        break;

    case 'm':
        add_message(
"Command: m   Usage: mADDRESS or [range]mADDRESS\n"
"Move the current line (or range of lines if specified) to a\n"
"location just after the specified ADDRESS.  Address 0 is the\n"
"beginning of the file and the default destination is the\n"
"current line.\n"
                   );
        break;

    case 'M':
        add_message(
"Command: M   Usage: M or [range]M\n"
"The command removes in the whole file (or in the range of lines if\n"
"specified) any trailing ^M from the line end. This change converts MS-DOS\n"
"line ends into Unix-style lineends.\n"
                   );
        break;

    case 'p':
        add_message(
"Command: p    Usage: p  or  [range]p\n"
"Print the current line (or range of lines if specified) to the\n"
"screen. See the commands 'n' and 'set' if line numbering is desired.\n"
                   );
        break;

    case 'q':
        add_message(
"Command: q    Usage: q\n"
"Quit the editor. Note that you can't quit this way if there\n"
"are any unsaved changes.  See 'w' for writing changes to file.\n"
                   );
        break;

    case 'Q':
        add_message(
"Command: Q    Usage: Q\n"
"Force Quit.  Quit the editor even if the buffer contains unsaved\n"
"modifications.\n"
               );
        break;

    case 'r':
        add_message(
"Command: r    Usage: r filename\n"
"Reads the given filename into the current buffer starting\n"
"at the current line.\n"
                   );
        break;

    case 't':
        add_message(
"Command: t   Usage: tADDRESS or [range]tADDRESS\n"
"Transpose a copy of the current line (or range of lines if specified)\n"
"to a location just after the specified ADDRESS.  Address 0 is the\n"
"beginning of the file and the default destination\nis the current line.\n"
                   );
        break;

    case 'T':
        add_message(
"Command: T  Usage: T{+|-}[width] or [range]T{+|-}[width]\n"
"Replace whitespace or tabs in the current line or in the specified range.\n"
"T+ means that whitespace will be replaced by tabs,\n"
"T- means that tabs will be replaced by whitespace.\n"
"The width option specifies the number of spaces a tab character represents.\n"
"(Default value for width: 8)\n"
                   );
        break;

    case 'v':
        add_message(
"Command: v   Usage: v/re/p\n"
"Search in all lines without expression 're', and print\n"
"every match. Other commands than 'p' can also be given\n"
"Compare with command 'g'.\n"
                   );
        break;

    case 'z':
        add_message(
"Command: z   Usage: z  or  z-  or z.\n"
"Displays 20 lines starting at the current line.\n"
"If the command is 'z.' then 20 lines are displayed being\n"
"centered on the current line. The command 'z-' displays\n"
"the 20 lines before the current line.\n"
                   );
        break;

    case 'Z':
        add_message(
"Command: Z   Usage: Z  or  Z-  or Z.\n"
"Displays 40 lines starting at the current line.\n"
"If the command is 'Z.' then 40 lines are displayed being\n"
"centered on the current line. The command 'Z-' displays\n"
"the 40 lines before the current line.\n"
                   );
        break;

    case 'x':
        add_message(
"Command: x   Usage: x\n"
"Save file under the current name, and then exit from ed.\n"
                   );
        break;

    case 's':
        if ( *inptr=='e' && *(inptr+1)=='t' )
        {
            add_message(
"Without arguments: show current settings.\n"
"'set save' will preserve the current settings for subsequent invocations\n"
"of ed.\n"
"Options:\n"
"\n"
"number       will print line numbers before printing or inserting a lines\n"
"list         will print control characters in p(rint) and z command\n"
"               like in l(ist)\n"
"print        will show current line after a single substitution or deletion\n"
"eightbit     will preserve the highbit of characters\n"
"autoindent   will preserve current indentation while entering text,\n"
"               use ^D or ^K to get back one step back to the right.\n"
"excompatible will exchange the meaning of \\( and ( as well as \\) and )\n"
"\n"
"An option can be cleared by prepending it with 'no' in the set command, e.g.\n"
"'set nolist' to turn off the list option.\n"
"\n"
"set shiftwidth <digit> will store <digit> in the shiftwidth variable, which\n"
"determines how much blanks are removed from the current indentation when\n"
"typing ^D or ^K in the autoindent mode.\n"
                       );
            break;
        }
        else
        {
            add_message("TODO: document the 's' command\n"
"Command: s   Usage: s/pat/sub/[g]\n"
"Replace regular expression <pat> by the text <sub>. If 'g' is given, all\n"
"occurences of <pat> in a line are replaced, else just the first.\n"
"<sub> may reference subexpressions of <pat> with '\\0'..'\\9', or to the\n"
"whole matched pattern with '\\&'. The special characters '\\t', '\\b',\n"
"'\\r' and '\\n' are recognized, as is '\\0xxx' for arbitrary characters.\n"
"Any character but '/' may be used as delimiter as long as it is consistent\n"
"within one command.\n"
                       );
            break;
        }
    case 'w':
        add_message(
"Command: w   Usage: w filename\n"
"Write the text into the file <filename>, overwriting the old content.\n"
"If <filename> is omitted, the name of the originating file is used.\n"
                   );
        break;

    case 'W':
        add_message(
"Command: W   Usage: w filename\n"
"Appends the text to the file <filename>.\n"
"If <filename> is omitted, the name of the originating file is used.\n"
                   );
        break;

    case '/':
        add_message(
"Command: /   Usage: /pattern\n"
"Locates the first line matching <pattern>, searching forward from the\n"
"current line. If the pattern is omitted, the last pattern is reused.\n"
                   );
        break;

    case '?':
        add_message(
"Command: ?   Usage: ?pattern\n"
"Locates the first line matching <pattern>, searching backwards from the\n"
"current line. If the pattern is omitted, the last pattern is reused.\n"
                   );
        break;

    default:
        add_message("       Help for Ed  (V %d.%d)\n", ED_VERSION / 100, ED_VERSION % 100);
        add_message(
"---------------------------------\n"
"\n\nCommands\n--------\n"
"/\tsearch forward for pattern\n"
"?\tsearch backward for a pattern\n"
/* "^\tglobal search and print for pattern\n" */
"=\tshow current line number\n"
"a\tappend text starting after this line\n"
"A\tlike 'a' but with inverse autoindent mode\n"
"c\tchange current line, query for replacement text\n"
"d\tdelete line(s)\n"
"e\treplace this file with another file\n"
"E\tsame as 'e' but works if file has been modified\n"
"f\tshow/change current file name\n"
"g\tSearch and execute command on any matching line.\n"
"h\thelp file (display this message)\n"
"i\tinsert text starting before this line\n"
"I\tindent the entire file (from DGD ed v0.1)\n"
"\n--Return to continue--"
                   );
        P_MORE=1;
        break;
    }
}

/*-------------------------------------------------------------------------*/
static void
print_help2 (void)

/* Print the second page of the generic help.
 */
{
    P_MORE=0;
    add_message(
"j\tjoin lines together\n"
"k\tmark this line with a character - later referenced as 'a\n"
"l\tline line(s) with control characters displayed\n"
"m\tmove line(s) to specified line\n"
"M\tremove all ^M at lineends (DOS -> Unix lineconversion)\n"
"n\ttoggle line numbering\n"
"p\tprint line(s) in range\n"
"q\tquit editor\n"
"Q\tquit editor even if file modified and not saved\n"
"r\tread file into editor at end of file or behind the given line\n"
"s\tsearch and replace\n"
"set\tquery, change or save option settings\n"
"t\tmove copy of line(s) to specified line\n"
"T\ttab / detab line(s), see help\n"
"v\tSearch and execute command on any non-matching line.\n"
"x\tsave file and quit\n"
"w\twrite to current file (or specified file)\n"
"W\tlike the 'w' command but appends instead\n"
"z\tdisplay 20 lines, possible args are . + -\n"
"Z\tdisplay 40 lines, possible args are . + -\n"
"\n"
"For further information type 'hc' where c is the command\n"
"that help is desired for.\n"
               );
}

/***************************************************************************/

