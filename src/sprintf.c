/*---------------------------------------------------------------------------
 * sprintf for LPMud
 *
 * Implemented and put into the public domain by Lynscar (Sean A Reith).
 *---------------------------------------------------------------------------
 * An implementation of (s)printf() for LPC, with quite a few
 * extensions (some parameters have slightly different meaning or
 * restrictions to "standard" (s)printf.)
 *
 * The following are the possible type specifiers.
 *  "%"   in which case no arguments are interpreted, and a "%" is inserted, and
 *        all modifiers are ignored.
 *  "O"   the argument is an LPC datatype.
 *  "Q"   the argument is an LPC datatype, strings are printed in LPC notation.
 *  "s"   the argument is a string.
 *  "d"   the integer arg is printed in decimal.
 *  "i"   as d.
 *  "b"   the integer arg is printed in binary (negative numbers in 2's
 *        compliment)
 *  "c"   the integer arg is to be printed as a character.
 *  "o"   the integer arg is printed in octal.
 *  "x"   the integer arg is printed in hex.
 *  "X"   the integer arg is printed in hex (in capitals).
 * "e","E","f","F","g","G"
 *        floating point formatting like in C.
 *  "^"   prints "%^" for compatibility with terminal_colour() strings.
 *        No modifiers are allowed.
 *
 * This version supports the following as modifiers:
 *  " "   pad positive integers with a space.
 *  "+"   pad positive integers with a plus sign.
 *  "-"   left aligned within field size.
 *        NB: std (s)printf() defaults to right alignment, which is
 *            unnatural in the context of a mainly string based language
 *            but has been retained for "compatability" ;)
 *  "|"   centered within field size.
 *  "$"   justified to field size. Makes sense only for strings and columns
 *        of strings.
 *  "="   column mode if strings are greater than field size.  this is only
 *        meaningful with strings, all other types are ignored. The strings
 *        are broken into the size of 'precision', and the last line is
 *        padded to have a length of 'fs'.
 *  "#"   with strings: table mode - print a list of '\n' separated 'words' in a
 *          compact table within the field size.
 *        with %O/%Q: compact mode - omit most whitespace and shorten 
 *          identifiers
 *   n    specifies the field size, a '*' specifies to use the corresponding
 *        arg as the field size.  If n is prepended with a zero, then the field
 *        is printed with leading zeros.
 *  "."n  precision of n, simple strings truncate after this (if precision is
 *        greater than field size, then field size = precision), tables use
 *        precision to specify the number of columns (if precision not specified
 *        then tables calculate a best fit), all other types ignore this.
 *  ":"n  n specifies the fs _and_ the precision, if n is prepended by a zero
 *        then it is padded with zeros instead of spaces.
 *  "@"   the argument is an array.  the corresponding format_info (minus the
 *        "@") is applyed to each element of the array.
 *  "'X'" The char(s) between the single-quotes are used to pad to field
 *        size (defaults to space) (if both a zero (in front of field
 *        size) and a pad string are specified, the one specified second
 *        overrules).
 *        To include "'" in the pad string, you must use "\\'" (as the
 *        backslash has to be escaped past the interpreter), similarly, to
 *        include "\" requires "\\\\".
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
#include <stdio.h>
#include <setjmp.h>
#include <sys/types.h>

#include "sprintf.h"

#include "actions.h"
#include "array.h"
#include "closure.h"
#include "comm.h"
#include "interpret.h"
#include "main.h"
#include "mapping.h"
#include "mstrings.h"
#include "object.h"
#include "ptrtable.h"
#include "random.h"
#include "sent.h"
#include "simulate.h"
#include "simul_efun.h"
#include "stdstrings.h"
#ifdef USE_STRUCTS
#include "structs.h"
#endif /* USE_STRUCTS */
#include "svalue.h"
#include "swap.h"
#include "xalloc.h"

/* If this #define is defined then error messages are returned,
 * otherwise errorf() is called (ie: A "wrongness in the fabric...")
 */
#undef RETURN_ERROR_MESSAGES

/*-------------------------------------------------------------------------*/
/* Format of format_info:
 *
 *   00000000 0000xxxx : argument type INFO_T:
 *                                0000 : type not found yet;
 *                                0001 : error type not found;
 *                                0010 : percent sign, null argument;
 *                                0011 : LPC datatype;
 *                                0100 : LPC datatype, quoted;
 *                                0101 : string;
 *                                0110 : integer
 *                                0111 : float
 *   00000000 00xx0000 : alignment INFO_A:
 *                                00 : right;
 *                                01 : centre;
 *                                10 : left;
 *                                11 : justified;
 *   00000000 xx000000 : positive pad char INFO_PP:
 *                                00 : none;
 *                                01 : ' ';
 *                                10 : '+';
 *   0000000x 00000000 : array mode (INFO_ARRAY)?
 *   000000x0 00000000 : column mode (INFO_COLS)?
 *   00000x00 00000000 : table mode (INFO_TABLE)?
 *
 *   0000x000 00000000 : field is to be left-padded with zero
 *   000x0000 00000000 : pad-spaces before a newline are kept
 */

typedef unsigned int format_info;

enum format_info_t {
  INFO_T         =    0xF,
  INFO_T_ERROR   =    0x1,
  INFO_T_NULL    =    0x2,
  INFO_T_LPC     =    0x3,
  INFO_T_QLPC    =    0x4,
  INFO_T_STRING  =    0x5,
  INFO_T_INT     =    0x6,
  INFO_T_FLOAT   =    0x7,

  INFO_A         =   0x30, /* Right alignment */
  INFO_A_CENTRE  =   0x10,
  INFO_A_LEFT    =   0x20,
  INFO_A_JUSTIFY =   0x30,

  INFO_PP        =   0xC0,
  INFO_PP_SPACE  =   0x40,
  INFO_PP_PLUS   =   0x80,

  INFO_ARRAY     =  0x100,
  INFO_COLS      =  0x200,
  INFO_TABLE     =  0x400,

  INFO_PS_ZERO   =  0x800,
  INFO_PS_KEEP   = 0x1000,
};

/*-------------------------------------------------------------------------*/

#define BUFF_SIZE 0x20000  /* 128 KByte */
  /* Max size of returned string.
   */

/* The error handling */

enum format_err {
  ERR_ID_NUMBER          =     0xFFFF, /* Mask for the error number */
  ERR_ARGUMENT           = 0xFFFF0000, /* Mask for the arg number */

  ERR_BUFF_OVERFLOW      =        0x1, /* buffer overflowed */
  ERR_TO_FEW_ARGS        =        0x2, /* more arguments spec'ed than passed */
  ERR_INVALID_STAR       =        0x3, /* invalid arg to * */
  ERR_PREC_EXPECTED      =        0x4, /* expected precision not found */
  ERR_INVALID_FORMAT_STR =        0x5, /* error in format string */
  ERR_INCORRECT_ARG      =        0x6, /* invalid arg to %[idcxXs] */
  ERR_CST_REQUIRES_FS    =        0x7, /* field size not given for c/t */
  ERR_UNDEFINED_TYPE     =        0x8, /* undefined type found */
  ERR_QUOTE_EXPECTED     =        0x9, /* expected ' not found */
  ERR_UNEXPECTED_EOS     =        0xA, /* fs terminated unexpectedly */
  ERR_NULL_PS            =        0xB, /* pad string is null */
  ERR_ARRAY_EXPECTED     =        0xC, /* array expected */
  ERR_NOMEM              =        0xD, /* Out of memory */
  ERR_SIZE_OVERFLOW      =        0xE, /* Fieldsize/precision numeric overflow */
};

#define ERROR(x) (longjmp(st->error_jmp, (x)))

#define ERROR1(e,a)              ERROR((e) | (a)<<16)
#define EXTRACT_ERR_ARGUMENT(i)  ((i)>>16)

/*-------------------------------------------------------------------------*/
/* Types */

typedef struct SaveChars        savechars;
typedef struct ColumnSlashTable cst;
typedef struct sprintf_buffer   sprintf_buffer_t;
typedef struct stsf_locals      stsf_locals_t;
typedef struct fmt_state        fmt_state_t;


/* --- struct SaveChars: list of characters to restore before exiting.
 */
struct SaveChars
{
    char       what;   /* Saved character */
    char      *where;  /* Original position */
    savechars *next;
};


/* --- struct ColumnSlashTable: data for one column or table
 *
 * All tables and columns in one line are kept in a linked
 * list and added line-wise to the result string.
 */

struct ColumnSlashTable
{
    union CSTData {
        char  *col;  /* column data, possibly multiple lines */
        char **tab;  /* table data */
    } d;                     /* d == data */
    unsigned short  nocols;  /* number of columns in table *sigh* */
    char           *pad;     /* the pad string */
    size_t          start;   /* starting cursor position */
    size_t          size;    /* column/table width */
    int             pres;    /* precision */
    format_info     info;    /* formatting data */
    cst            *next;    /* next column structure */
};

/* --- struct sprintf_buffer: dynamic string buffer
 *
 * The structure implements a dynamic string buffer. The structure
 * sits at the end of the allocated memory and knows where the
 * char* pointing to the begin of the allocated memory is. This
 * way all the user sees is a char[] which automagically happens
 * to be of the right size.
 */

struct sprintf_buffer
{
    /* char text[.size - sizeof(sprintf_buffer_t)]; */
#define BUF_TEXT(b) ((char *)(b))
    int offset;    /* Offset from .size to the first free byte
                    * (ie. a negative number).
                    */
    int size;      /* Total size of the buffer */
    char **start;  /* Pointer to the string pointer */
};


/* --- struct stsf_locals: auxiliary structure
 *
 * The structure is used when printing a mapping to pass
 * the essential data.
 */

struct stsf_locals
{
    sprintf_buffer_t *spb;  /* Target buffer */
    int indent;             /* Indentation */
    int num_values;         /* Mapping width */
    Bool quote;             /* TRUE: Quote strings */
    Bool compact;           /* TRUE: Compact output */
    fmt_state_t      *st;   /* sprintf state */
};

/* --- struct fmt_state: status of the sprintf operation
 *
 * This structure contains several data items which the functions
 * here use and modify while creating the result string.
 * The structure is allocated by the string_print_formatted()
 * in order to achieve re-entrancy.
 *
 * The canonic declaration for this structure as parameter
 * is 'fmt_state_t *st', and is expected as such by some
 * macros.
 */

struct fmt_state
{
    savechars  * saves;            /* Characters to restore */
    cst        * csts;             /* list of columns/tables to be done */
    svalue_t     clean;            /* holds a temporary string */
    char       * tmp;              /* holds a temporary string buffer */
    char         buff[BUFF_SIZE];  /* Buffer for returned string. */
    size_t       bpos;             /* Position in buff. */
    ssize_t      sppos;
      /* -1, or the buffer position of the first character of
       * a trailing space padding. This is used to remove space
       * padding at the end of lines.
       */
    unsigned int line_start;       /* Position where to start a line. */
    jmp_buf      error_jmp;
      /* Error-exit context. In case of errors, the functions longjmp()
       * directly back to the top function which then handles the error.
       */
    struct pointer_table * ptable;
      /* When printing svalue, this keeps track of arrays and mappings
       * in order to catch recursions.
       */
    int32 pointer_id;
      /* Next ID to give to an array or mapping when printing svalues.
       */
};

/*-------------------------------------------------------------------------*/

static Bool static_fmt_used = MY_FALSE;
static fmt_state_t static_fmt;
  /* A reusable instance of fmt_state_t, to avoid repeated re-allocations
   * (and subsequent large block fragmentation).
   * Since sprintf() can be used recursively (through master::printf_obj_name)
   * static_fmt_used acts as mutex. A recursive sprintf() call will then
   * allocate a temporary fmt_state_t structure.
   */

/*-------------------------------------------------------------------------*/
/* Forward declarations */

static sprintf_buffer_t *svalue_to_string(fmt_state_t *
                                         , svalue_t *, sprintf_buffer_t *
                                         , int, Bool, Bool, Bool, Bool);

/*-------------------------------------------------------------------------*/
/* static helper functions */
static inline void ADD_CHAR(fmt_state_t *st, char x) {
    if (st->bpos >= BUFF_SIZE) ERROR(ERR_BUFF_OVERFLOW);
    if (x == '\n' && st->sppos != -1) st->bpos = st->sppos;
    st->sppos = -1;
    st->buff[st->bpos++] = x;
}

  /* Add character <x> to the buffer.
   */

/*-------------------------------------------------------------------------*/
static inline void ADD_STRN(fmt_state_t *st, const char *s, size_t n) {
    if (st->bpos + n > BUFF_SIZE) ERROR(ERR_BUFF_OVERFLOW);
    if (n >= 1 && s[0] == '\n' && st->sppos != -1) st->bpos = st->sppos;
    st->sppos = -1;
    memcpy(st->buff+st->bpos, s, n);
    st->bpos += n;
}

  /* Add the <n> characters from <s> to the buffer.
   */

/*-------------------------------------------------------------------------*/
static inline void ADD_CHARN(fmt_state_t *st, char c, size_t n) {
    /* n must not be negative! */
    if (st->bpos + n > BUFF_SIZE) ERROR(ERR_BUFF_OVERFLOW);
    if (n >= 1 && c == '\n' && st->sppos != -1) st->bpos = st->sppos;
    st->sppos = -1;
    memset(st->buff+st->bpos, c, n);
    st->bpos += n;
}

  /* Add character <c> <n>-times to the buffer.
   */

/*-------------------------------------------------------------------------*/
static inline void ADD_PADDING(fmt_state_t *st, const char *pad, size_t N) {
    int n = N;

    if (!pad[1]) {
        ADD_CHARN(st, *pad, n);
    } else {
        int i, l;

        l = strlen(pad);
        for (i=0; --n >= 0; ) {
            if (pad[i] == '\\')
                i++;
            ADD_CHAR(st, pad[i]);
            if (++i == l)
                i = 0;
        }
    }
}

  /* Add the padding string <pad> to the buffer, repeatedly if necessary,
   * yielding a total length of <N>.
   */

/*-------------------------------------------------------------------------*/
static sprintf_buffer_t *
realloc_sprintf_buffer (fmt_state_t *st, sprintf_buffer_t *b)

/* Increase the size of buffer <b> and return the new buffer.
 * At the time of call, a positive .offset determines how much more
 * data is needed. If it's negative to begin with, the size is just
 * doubled.
 */

{
    int offset   = b->offset;
    int size     = b->size;
    char **start = b->start;
    char *newstart;

    /* Get more memory */
    do {
        if (size > BUFF_SIZE)
            ERROR(ERR_BUFF_OVERFLOW);
        offset -= size;
        size *= 2;
        newstart = rexalloc(*start, size);
        if (!newstart)
            ERROR(ERR_NOMEM);
        *start = newstart;
    } while (offset >= 0);

    b = (sprintf_buffer_t*)(*start+size - sizeof(sprintf_buffer_t));
    b->offset = offset;
    b->size = size;
    b->start = start;
    return b;
} /* realloc_sprintf_buffer() */

/*-------------------------------------------------------------------------*/
static void
straddn (fmt_state_t *st, sprintf_buffer_t **buffer, char *add, int len)

/* Add string <add> of <len> characters to the <buffer>.
 */

{
    sprintf_buffer_t *b = *buffer;
    int o;

    o = b->offset;
    if ( (b->offset = o + len) >= 0)
    {
        *buffer = b = realloc_sprintf_buffer(st, b);
        o = b->offset - len;
    }
    memcpy(BUF_TEXT(b) + o, add, len);
} /* straddn() */

/*-------------------------------------------------------------------------*/
static void
stradd (fmt_state_t *st, sprintf_buffer_t **buffer, char *add)

/* Add string <add> to the <buffer>.
 * The function add_indent() intentionally matches our signature.
 */

{
    straddn(st, buffer, add, strlen(add));
} /* stradd() */

/*-------------------------------------------------------------------------*/
static void
numadd (fmt_state_t *st, sprintf_buffer_t **buffer, p_int num)

/* Add the <num>ber to the <buffer>.
 */

{
    sprintf_buffer_t *b = *buffer;
    int i, j;
    Bool nve;

    if (num < 0)
    {
        /* Negative number: remember that and make
         * the number positive.
         */
        if (num == PINT_MIN)
        {
            /* num == MININT: add <num>+1 to the buffer, the
             * increment the last digit by one.
             * Since all possible MIN values are powers of two,
             * the last digit can never be 9.
             */
            numadd(st, buffer, num+1);
            b = *buffer;
            BUF_TEXT(b)[b->offset - 1] += 1;
            return;
        }
        else
            num *= -1;
        nve = MY_TRUE;
    }
    else
        nve = MY_FALSE;

    /* Determine the number of digits required */
    if (num <= 1)
        j = 1;
    else
        j = (int) ceil(log10((double)num+1));
    if (nve)
        j++;

    /* Get the memory */
    i = b->offset;
    if ((b->offset = i + j) >= 0) {
        *buffer = b = realloc_sprintf_buffer(st, b);
        i = b->offset - j;
    }

    BUF_TEXT(b)[i+j] = '\0'; /* Add terminator in advance */

    /* '-' required? */
    if (nve)
    {
        BUF_TEXT(b)[i] = '-';
        j--;
    }
    else
        i--;

    /* Now store the number */
    for (; j; j--, num /= 10)
        BUF_TEXT(b)[i+j] = (num%10) + '0';
} /* num_add() */

/*-------------------------------------------------------------------------*/
static void
add_indent (fmt_state_t *st, sprintf_buffer_t **buffer, int indent)

/* Add <indent> characters indentation to <buffer>.
 * The function intentionally matches the signature of stradd().
 */

{
    int i;
    sprintf_buffer_t *b = *buffer;
    char *p;

    i = b->offset;
    if ( (b->offset = i + indent) >= 0)
    {
        *buffer = b = realloc_sprintf_buffer(st, b);
        i = b->offset - indent;
    }

    p = BUF_TEXT(b) + i;
    for (;indent;indent--)
        *p++ = ' ';
    *p = '\0';
} /* add_indent() */

/*-------------------------------------------------------------------------*/
static void
svalue_to_string_filter(svalue_t *key, svalue_t *data, void *extra)

/* Filter to add a mapping entry to the given sprintf_buffer.
 * <extra> is a stsf_locals*.
 */

{
    int i;
    stsf_locals_t *locals = (stsf_locals_t *)extra;
    char *delimiter = ":";

    i = locals->num_values;
    locals->spb =
      svalue_to_string(locals->st, key, locals->spb, locals->indent, !i, locals->quote, locals->compact, MY_FALSE);
    while (--i >= 0)
    {
        stradd(locals->st, &locals->spb, delimiter);
        locals->spb = svalue_to_string(locals->st, data++, locals->spb, 1, !i, locals->quote, locals->compact, MY_FALSE);
        delimiter = ";";
    }
} /* svalue_to_string_filter() */

/*-------------------------------------------------------------------------*/
static sprintf_buffer_t *
svalue_to_string ( fmt_state_t *st
                 , svalue_t *obj, sprintf_buffer_t *str
                 , int indent, Bool trailing, Bool quoteStrings
                 , Bool compact, Bool prefixed)

/* Print the value <obj> into the buffer <str> with indentation <indent>.
 * If <trailing> is true, add ",\n" after the printed value.
 * If <qoute> is true, special characters in strings are quoted LPC-style.
 * If <compact> is true, a short output format is used.
 * If <prefixed> is true, the caller has printed something ahead of this
 * value, meaning that for the first line no indentation is required.
 *
 * Result is the (updated) string buffer.
 * The function calls itself for recursive values.
 */

{
    mp_int i;

    if (!compact && !prefixed)
        add_indent(st, &str, indent);

    switch (obj->type)
    {
    case T_INVALID:
        stradd(st, &str, "T_INVALID");
        break;

    case T_LVALUE:
        stradd(st, &str, compact ? "l:" : "lvalue: ");
        str = svalue_to_string(st, obj->u.lvalue, str, indent+2, trailing, quoteStrings, compact, MY_FALSE);
        break;

    case T_NUMBER:
        numadd(st, &str, obj->u.number);
        break;

    case T_FLOAT:
      {
        char s[200]; /* TODO: Might be too small */
        double d;

        d = READ_DOUBLE(obj);
#ifdef HAVE_TRUNC
        if (trunc(d) == d)
#else
        if ((d >= 0.0 ? floor(d) : ceil(d)) == d)
#endif
            sprintf(s, "%g.0", d );
        else
            sprintf(s, "%g", d );
        stradd(st, &str, s);
        break;
      }

    case T_STRING:
        stradd(st, &str, "\"");

        if (!quoteStrings)
        {
            straddn(st, &str, get_txt(obj->u.str), mstrsize(obj->u.str));
        }
        else
        {
            size_t len;

            /* Compute the size of the result string */
            for (len = 0, i = mstrsize(obj->u.str); i > 0; --i)
            {
                unsigned char c = (unsigned char) get_txt(obj->u.str)[i];

                switch(c)
                {
                case '"':
                case '\n':
                case '\r':
                case '\t':
                case '\a':
                case 0x1b:
                case 0x08:
                case 0x00:
                case '\\':
                    len += 2; break;
                default:
                    if (c >= 0x20 && c < 0x7F)
                    {
                       len++;
                    }
                    else
                    {
                       len += 4;
                    }
                    break;
                }
            }

            if ( len == mstrsize(obj->u.str) )
            {
                /* No special characters found */
                stradd(st, &str, get_txt(obj->u.str));
            }
            else
            {
                char * tmpstr, *dest;
                unsigned char *src;

                /* Allocate the temporary string */
                tmpstr = alloca(len+1);

                src = (unsigned char *)get_txt(obj->u.str);
                dest = tmpstr;
                for (i = mstrsize(obj->u.str); i > 0; --i)
                {
                    unsigned char c = *src++;

                    switch(c)
                    {
                    case '"': strcpy(dest, "\\\""); dest += 2; break;
                    case '\n': strcpy(dest, "\\n"); dest += 2; break;
                    case '\r': strcpy(dest, "\\r"); dest += 2; break;
                    case '\t': strcpy(dest, "\\t"); dest += 2; break;
                    case '\a': strcpy(dest, "\\a"); dest += 2; break;
                    case 0x1b: strcpy(dest, "\\e"); dest += 2; break;
                    case 0x08: strcpy(dest, "\\b"); dest += 2; break;
                    case 0x00: strcpy(dest, "\\0"); dest += 2; break;
                    case '\\': strcpy(dest, "\\\\"); dest += 2; break;
                    default:
                        if (c >= 0x20 && c < 0x7F)
                        {
                           *dest++ = (char)c;
                        }
                        else
                        {
                           static char hex[] = "0123456789abcdef";
                           *dest++ = '\\';
                           *dest++ = 'x';
                           *dest++ = hex[c >> 4];
                           *dest++ = hex[c & 0xf];
                        }
                        break;
                    }
                } /* for() */
                *dest = '\0';

                stradd(st, &str, tmpstr);
            }

        }

        stradd(st, &str, "\"");
        break;

    case T_QUOTED_ARRAY:
      {
        i = obj->x.quotes;
        do {
            stradd(st, &str, "\'");
        } while (--i);
      }
      /* FALLTHROUGH */

    case T_POINTER:
      {
        size_t size;

        size = VEC_SIZE(obj->u.vec);
        if (!size)
        {
            stradd(st, &str, compact ? "({})" : "({ })");
        }
        else
        {
            struct pointer_record *prec;

            prec = find_add_pointer(st->ptable, obj->u.vec, MY_TRUE);
            if (!prec->id_number)
            {
                /* New array */
                prec->id_number = st->pointer_id++;

                if (compact)
                {
                    stradd(st, &str, "({#");
                    numadd(st, &str, prec->id_number);
                    stradd(st, &str, " ");
                }
                else
                {
                    stradd(st, &str, "({ /* #");
                    numadd(st, &str, prec->id_number);
                    stradd(st, &str, ", size: ");
                    numadd(st, &str, size);
                    stradd(st, &str, " */\n");
                }
                for (i = 0; (size_t)i < size-1; i++)
                {
                    str = svalue_to_string(st, &(obj->u.vec->item[i]), str, indent+2, MY_TRUE, quoteStrings, compact, MY_FALSE);
                }
                str = svalue_to_string(st, &(obj->u.vec->item[i]), str, indent+2, MY_FALSE, quoteStrings, compact, MY_FALSE);
                if (!compact)
                {
                    stradd(st, &str, "\n");
                    add_indent(st, &str, indent);
                }
                stradd(st, &str, "})");
            }
            else
            {
                /* Recursion! */
                stradd(st, &str, compact ? "({#" : "({ #");
                numadd(st, &str, prec->id_number);
                stradd(st, &str, compact ? "})" : " })");
            }
        }
        break;
      }

#ifdef USE_STRUCTS
    case T_STRUCT:
      {
        struct_t *strct = obj->u.strct;
        size_t size;
        struct pointer_record *prec;

        size = struct_size(strct);

        prec = find_add_pointer(st->ptable, strct, MY_TRUE);
        if (!prec->id_number)
        {
            /* New array */
            prec->id_number = st->pointer_id++;
            
            if (compact)
            {
                stradd(st, &str, "(<'");
                stradd(st, &str, get_txt(struct_unique_name(strct)));
                stradd(st, &str, "'#");
                numadd(st, &str, prec->id_number);
                stradd(st, &str, " ");
            }
            else
            {
                stradd(st, &str, "(<'");
                stradd(st, &str, get_txt(struct_unique_name(strct)));
                stradd(st, &str, "' /* #");
                numadd(st, &str, prec->id_number);
                stradd(st, &str, ", size: ");
                numadd(st, &str, size);
                stradd(st, &str, " */");
            }
            if (size)
            {
                if (!compact)
                    stradd(st, &str, "\n");
                for (i = 0; (size_t)i < size-1; i++)
                {
                    if (!compact)
                    {
                        add_indent(st, &str, indent+2);
                        stradd(st, &str, "/* ");
                        stradd(st, &str, get_txt(strct->type->member[i].name));
                        stradd(st, &str, ": */ ");
                    }
                    str = svalue_to_string(st, &(strct->member[i]), str, indent+2, MY_TRUE, quoteStrings, compact, !compact);
                }
                if (!compact)
                {
                    add_indent(st, &str, indent+2);
                    stradd(st, &str, "/* ");
                    stradd(st, &str, get_txt(strct->type->member[i].name));
                    stradd(st, &str, ": */ ");
                }
                str = svalue_to_string(st, &(strct->member[i]), str, indent+2, MY_FALSE, quoteStrings, compact, !compact);
                if (!compact)
                {
                    stradd(st, &str, "\n");
                    add_indent(st, &str, indent);
                }
            }
            stradd(st, &str, ">)");
        }
        else
        {
            /* Recursion! */
            stradd(st, &str, compact ? "(<#" : "(< #");
            numadd(st, &str, prec->id_number);
            stradd(st, &str, compact ? ">)" : " >)");
        }
        break;
      }
#endif /* USE_STRUCTS */

    case T_MAPPING:
      {
        struct stsf_locals locals;
        struct pointer_record *prec;

        prec = find_add_pointer(st->ptable, obj->u.map, MY_TRUE);
        if (!prec->id_number)
        {
            /* New mapping */
            prec->id_number = st->pointer_id++;

            stradd(st, &str, compact ? "([#" : "([ /* #");
            numadd(st, &str, prec->id_number);
            stradd(st, &str, compact ? " " : " */\n");

            locals.spb = str;
            locals.indent = indent + 2;
            locals.num_values = obj->u.map->num_values;
            locals.st = st;
            locals.quote = quoteStrings;
            locals.compact = compact;
            walk_mapping(obj->u.map, svalue_to_string_filter, &locals);
            str = locals.spb;
            if (!compact)
            {
                /* Remove the ',' from the trailing ',\n', if any */
                if (BUF_TEXT(str)[str->offset - 1] == '\n'
                 && BUF_TEXT(str)[str->offset - 2] == ','
                   )
                {
                    BUF_TEXT(str)[str->offset - 2] = '\n';
                    str->offset--;
                }
                add_indent(st, &str, indent);
            }
            else
            {
                /* Remove the trailing, if any */
                if (BUF_TEXT(str)[str->offset - 1] == ',')
                    str->offset--;
            }
            stradd(st, &str, "])");
        }
        else
        {
            /* Recursion! */
            stradd(st, &str, compact ? "([#" : "([ #");
            numadd(st, &str, prec->id_number);
            stradd(st, &str, compact ? "])" : " ])");
        }
        break;
      }

    case T_OBJECT:
      {
        svalue_t *temp;

        if (obj->u.ob->flags & O_DESTRUCTED)
        {
            /* *obj might be a mapping key, thus we mustn't change it. */
            stradd(st, &str,"0");
            break;
        }
        if (!compat_mode)
            stradd(st, &str, "/");
        stradd(st, &str, get_txt(obj->u.ob->name));
        if (!compact)
        {
            push_ref_object(inter_sp, obj->u.ob, "sprintf");
            temp = apply_master(STR_PRINTF_OBJ_NAME, 1);
            if (temp && (temp->type == T_STRING)) {
                stradd(st, &str, " (\"");
                stradd(st, &str, get_txt(temp->u.str));
                stradd(st, &str, "\")");
            }
        }
        break;
      }

    case T_SYMBOL:
        i = obj->x.quotes;
        do {
            stradd(st, &str, "\'");
        } while (--i);
        stradd(st, &str, get_txt(obj->u.str));
        break;

    case T_CLOSURE:
      {
        string_t * rc;

        rc = closure_to_string(obj, compact);
        stradd(st, &str, get_txt(rc));
        free_mstring(rc);

        break;
    } /* case T_CLOSURE */

  case T_CHAR_LVALUE:
    {
        char buf[2];

        buf[0] = *obj->u.charp;
        buf[1] = '\0';
        stradd(st, &str, "'");
        stradd(st, &str, buf);
        stradd(st, &str, "'");
        if (!compact)
        {
            stradd(st, &str, " (");
            numadd(st, &str, buf[0] & 0xff);
            stradd(st, &str, ")");
        }
        break;
    }

  case T_PROTECTED_CHAR_LVALUE:
    {
        stradd(st, &str, compact ? "p char:" : "prot char: ");
        str = svalue_to_string(st, obj->u.lvalue, str, indent+2, trailing, quoteStrings, compact, MY_FALSE);
        break;
    }

  case T_STRING_RANGE_LVALUE:
  case T_PROTECTED_STRING_RANGE_LVALUE:
    {
        if (obj->type == T_PROTECTED_STRING_RANGE_LVALUE)
            stradd(st, &str, compact ? "p:" : "prot: ");
        stradd(st, &str, "\"");
        stradd(st, &str, get_txt(obj->u.str));
        stradd(st, &str, "\"");
        break;
    }

  case T_POINTER_RANGE_LVALUE:
  case T_PROTECTED_POINTER_RANGE_LVALUE:
    {
        size_t size;

        if (obj->type == T_PROTECTED_POINTER_RANGE_LVALUE)
            stradd(st, &str, compact ? "p:" : "prot: ");

        size = VEC_SIZE(obj->u.vec);
        if (!size)
        {
            stradd(st, &str, compact ? "({})" : "({ })");
        }
        else
        {
            struct pointer_record *prec;

            prec = find_add_pointer(st->ptable, obj->u.vec, MY_TRUE);
            if (!prec->id_number)
            {
                /* New array */
                prec->id_number = st->pointer_id++;

                if (compact)
                {
                    stradd(st, &str, "({#");
                    numadd(st, &str, prec->id_number);
                    stradd(st, &str, " ");
                }
                else
                {
                    stradd(st, &str, "({ /* #");
                    numadd(st, &str, prec->id_number);
                    stradd(st, &str, ", size: ");
                    numadd(st, &str, size);
                    stradd(st, &str, " */\n");
                }
                for (i = 0; (size_t)i < size-1; i++)
                    str = svalue_to_string(st, &(obj->u.vec->item[i]), str, indent+2, MY_TRUE, quoteStrings, compact, MY_FALSE);
                str = svalue_to_string(st, &(obj->u.vec->item[i]), str, indent+2, MY_FALSE, quoteStrings, compact, MY_FALSE);
                if (!compact)
                {
                    stradd(st, &str, "\n");
                    add_indent(st, &str, indent);
                }
                stradd(st, &str, "})");
            }
            else
            {
                /* Recursion! */
                stradd(st, &str, compact ? "({#" : "({ #");
                numadd(st, &str, prec->id_number);
                stradd(st, &str, compact ? "})" : " })");
            }
        }
        break;
    }

  case T_PROTECTED_LVALUE:
      stradd(st, &str, compact ? "p l:" : "prot lvalue: ");
      str = svalue_to_string(st, obj->u.lvalue, str, indent+2, trailing, quoteStrings, compact, MY_FALSE);
      break;

  default:
      stradd(st, &str, "!ERROR: GARBAGE SVALUE (");
      numadd(st, &str, obj->type);
      stradd(st, &str, ")!");
  } /* end of switch (obj->type) */

  if (trailing)
      stradd(st, &str, compact ? "," : ",\n");

  return str;
} /* svalue_to_string() */

/*-------------------------------------------------------------------------*/
static void
add_justified ( fmt_state_t *st
              , char *str, size_t len, int fs
              )

/* Justify string <str> (length <len>) within the fieldsize <fs>.
 * After that, add it to the global buff[].
 */

{
    size_t sppos;
    int num_words;    /* Number of words in the input */
    int num_chars;    /* Number of non-space characters in the input */
    int num_spaces;   /* Number of spaces required */
    int min_spaces;   /* Min number of pad spaces */
    size_t pos;

    /* Check how much data we have */

    num_words = 0;
    num_chars = 0;

    /* Find the first non-space character.
     * If it's all spaces, return.
     */
    for (pos = 0; pos < len && *str == ' '; pos++, str++) NOOP;
    if (pos >= len)
        return;

    len -= (size_t)pos;
    pos = 0;
    num_words = 1;

    while (pos < len)
    {
        /* Find the end of the word */
        for ( ; pos < len && str[pos] != ' '; pos++, num_chars++) NOOP;
        if (pos >= len)
            break;

        /* Find the start of the next word */
        for ( ; pos < len && str[pos] == ' '; pos++) NOOP;
        if (pos >= len)
            break;

        /* We got a new word - count it */
        num_words++;
    }

#ifdef DEBUG
    if (fs < num_words - 1 + num_chars)
        fatal("add_justified(): fieldsize %d < data length %d\n", fs, num_words - 1 + num_chars);
#endif

    /* Compute the number of spaces we need to insert.
     * It is guaranteed here that we have enough space to insert
     * at least one space between each word.
     */
    num_spaces = fs - num_chars;
    if (num_words == 1)
        min_spaces = num_spaces;
    else
        min_spaces = num_spaces / (num_words-1);
        /* min_spaces * (num_words-1) <= num_spaces < min_spaces * num_words */

    /* Loop again over the data, now adding spaces as we go. */

    for (pos = 0; pos < len; )
    {
        int mark;
        int padlength;

        /* Find the end of the current word */
        for (mark = pos ; pos < len && str[pos] != ' '; pos++) NOOP;

        /* Add the word */
        ADD_STRN(st, str+mark, pos - mark);
        num_words--;

        if (pos >= len || num_words < 1)
            break;

        /* There is a word following - add spaces */
        if (num_words == 1) /* Last word: add all remaining padding */
            padlength = (int)num_spaces;
        else if (num_spaces < min_spaces * num_words) /* Space underrun */
            padlength = 1;
        else if (num_spaces == min_spaces * num_words)
            /* Exactly the min. padlength per word avail. */
            padlength = min_spaces;
        else if (num_spaces >= min_spaces * num_words + 2)
            /* Force an extra space */
            padlength = min_spaces+1;
        else /* Randomly add one space */
            padlength = min_spaces + (int)random_number(2);
        sppos = st->bpos;
        ADD_PADDING(st, " ", padlength);
        st->sppos = sppos;
        num_spaces -= padlength;

        /* Find the start of the next word */
        for ( ; pos < len && str[pos] == ' '; pos++) NOOP;
    }
} /* add_justified() */

/*-------------------------------------------------------------------------*/
static void
add_aligned ( fmt_state_t *st
            , char *str, size_t len, char *pad, int fs
            , format_info finfo)

/* Align string <str> (length <len>) within the fieldsize <fs> according
 * to the <finfo>. After that, add it to the global buff[].
 */

{
    size_t sppos;
    Bool is_space_pad;

    if ((size_t)fs < len)
        fs = len;

    sppos = 0;
    is_space_pad = MY_FALSE;
    if (pad[0] == ' ' && pad[1] == '\0' && !(finfo & INFO_PS_KEEP))
        is_space_pad = MY_TRUE;

    switch(finfo & INFO_A)
    {
    case INFO_A_JUSTIFY:
    case INFO_A_LEFT:
        /* Also called for the last line of a justified block */
        ADD_STRN(st, str, len);
        if (is_space_pad)
            sppos = st->bpos;
        ADD_PADDING(st, pad, fs - len);
        if (is_space_pad)
            st->sppos = sppos;
        break;

    case INFO_A_CENTRE:
        if (finfo & INFO_PS_ZERO)
        {
            ADD_PADDING(st, "0", (fs - len + 1) >> 1);
        }
        else
        {
            ADD_PADDING(st, pad, (fs - len + 1) >> 1);
        }
        ADD_STRN(st, str, len);
        if (is_space_pad)
            sppos = st->bpos;
        ADD_PADDING(st, pad, (fs - len) >> 1);
        if (is_space_pad)
            st->sppos = sppos;
        break;

    default:
      { /* std (s)printf defaults to right alignment.
         */
        if (finfo & INFO_PS_ZERO)
        {
            ADD_PADDING(st, "0", fs - len);
        }
        else
        {
            ADD_PADDING(st, pad, fs - len);
        }
        ADD_STRN(st, str, len);
      }
    }
} /* add_aligned() */

/*-------------------------------------------------------------------------*/
static int
add_column (fmt_state_t *st, cst **column)

/* Add the the next line from <column> to the buffer buff[].
 * Result 0: column not finished (more lines/data pending)
 *        1: column completed and removed from the list
 *        2: column completed with terminating \n, column removed from
 *           the list.
 */

{
#define COL (*column)

    unsigned int done;
    mp_int length;
    unsigned int save;
    char *COL_D = COL->d.col;
    char *p;

    /* Set done to the actual number of characters to copy.
     */
    length = COL->pres;
    if ((COL->info & INFO_A) == INFO_A_JUSTIFY && length > (mp_int)COL->size)
        length = COL->size;
    for (p = COL_D; length && *p && *p !='\n'; p++, length--) NOOP;
    done = p - COL_D;
    if (*p && *p !='\n')
    {
        /* Column data longer than the permitted size: find a
         * a space to do wordwrapping.
         */
        save = done;
        for (; ; done--,p--)
        {
            /* handle larger than column size words... */
            if (!done)
            {
                /* Sorry, it's one big word.
                 * Print the word over the fieldsize.
                 */
                done = save - 1;
                p += save;
                break;
            }
            if (*p == ' ')
            {
                /* If went more than one character back, check if
                 * the next word is longer than permitted. If that is
                 * the case we might as well start breaking it up right
                 * here.
                 */
                if (save-2 > done)
                {
                    char *p2;

                    length = COL->pres;
                    if ((COL->info & INFO_A) == INFO_A_JUSTIFY
                     && length > (mp_int)COL->size)
                        length = COL->size;
                    for ( p2 = p+1, length--
                        ; length && *p2 && *p2 !='\n' && *p2 != ' '
                        ; p2++, length--) NOOP;
                    if (*p2 && *p2 != '\n' && *p2 != ' ')
                    {
                        /* Yup, the next word is far too long. */
                        p += save - done;
                        done = save - 1;
                    }
                    /* else: the next word is not too long */
                }
                /* else: breaking too long word here would look silly anyway
                 */
                break;
            } /* if (p == ' ') */
        } /* for (done) */
    } /* if (breaking needed) */

    /* On justified formatting, don't format the last line that way, nor
     * justified lines ending in NL.
     */
    if ((COL->info & INFO_A) == INFO_A_JUSTIFY
     && *COL_D && *(COL_D+1)
     && *p != '\n' && *p != '\0'
       )
    {
        add_justified(st, COL_D, p - COL_D, COL->size);
    }
    else
    {
        add_aligned(st, COL_D, p - COL_D, COL->pad, COL->size, COL->info);
    }

    COL_D += done; /* inc'ed below ... */

    /* if this or the next character is a '\0' then take this column out
     * of the list.
     */
    if (!(*COL_D) || !(*(++COL_D)))
    {
        cst *temp;
        int ret;

        if (*(COL_D-1) == '\n')
            ret = 2;
        else
            ret = 1;

        temp = COL->next;
        xfree(COL);
        COL = temp;
        return ret;
    }

    /* Column not finished */
    COL->d.col = COL_D;
    return 0;

#undef COL

} /* add_column() */

/*-------------------------------------------------------------------------*/
static Bool
add_table (fmt_state_t *st, cst **table)

/* Add the next line of <table> to the buffer.
 * Return TRUE if the table was completed and removed from the list.
 */

{
    unsigned int done, i;

#define TAB (*table)
#define TAB_D (TAB->d.tab[i])

    /* Loop over all columns of the table */
    for (i = 0; i < TAB->nocols && TAB_D; i++)
    {
        /* Get the length to add */
        for (done = 0; (TAB_D[done]) && (TAB_D[done] != '\n'); done++) NOOP;

        add_aligned(st, TAB_D, done, TAB->pad, TAB->size, TAB->info);

        TAB_D += done; /* inc'ed next line ... */
        if (!(*TAB_D) || !(*(++TAB_D)))
            TAB_D = NULL;
    }

    /* Fill up the end of the table if required */
    if (i < TAB->nocols)
    {
        done = TAB->size;
        for (; i < TAB->nocols; i++)
        {
            /* TAB->size is not negative. */
            ADD_CHARN(st, ' ', done);
        }
    }

    if (!TAB->d.tab[0])
    {
        /* Table finished */

        cst *temp;

        temp = TAB->next;
        xfree(TAB->d.tab);
        xfree(TAB);
        TAB = temp;
        return MY_TRUE;
    }

    return MY_FALSE;

#undef TAB
#undef TAB_D

} /* add_table() */

/*-------------------------------------------------------------------------*/
static string_t *
string_print_formatted (char *format_str, int argc, svalue_t *argv)

/* The (s)printf() function: format <format_str> with the given arguments
 * and return a pointer to the result (a string with one reference).
 *
 * If an error occurs and RETURN_ERROR_MESSAGES is defined, an error
 * will return the error string as result; if R_E_M is undefined, an
 * true errorf() is raised.
 */

{
#ifndef RETURN_ERROR_MESSAGES
static char buff[BUFF_SIZE];         /* For error messages */
#endif
    string_t     *result;          /* The result string */
    fmt_state_t  *st;              /* The formatting state */
    volatile fmt_state_t  *vst;    /* A copy of st, stored in a volatile var
                                    * to survive a longjmp()
                                    */
    svalue_t     *carg;            /* current arg */
    int           arg;             /* current arg number */
    format_info   finfo;           /* parse formatting info */
    char          format_char;     /* format type */
    unsigned int  nelemno;         /* next offset into array */
    unsigned int  fpos;            /* position in format_str */
    p_uint        fs;              /* field size */
    int           pres;            /* precision */
    unsigned int  err_num;         /* error code */
    char         *pad;             /* fs pad string */
    int           column_stat;     /* Most recent column add status */

#   define GET_NEXT_ARG {\
        if (++arg >= argc) ERROR(ERR_TO_FEW_ARGS); \
        carg = (argv+arg);\
    }

#   define SAVE_CHAR(pointer) {\
        savechars *new;\
        new = xalloc(sizeof(savechars));\
        if (!new) \
            ERROR(ERR_NOMEM); \
        new->what = *(pointer);\
        new->where = pointer;\
        new->next = st->saves;\
        st->saves = new;\
    }

    result = NULL; /* To get rid of a warning */

    if (!static_fmt_used)
    {
        st = &static_fmt;
        static_fmt_used = MY_TRUE;
    }
    else
        xallocate(st, sizeof *st, "sprintf() context");

    st->saves = NULL;
    st->clean.u.str = NULL;
    st->tmp = NULL;
    st->csts = NULL;
    st->ptable = NULL;

    vst = st;

    if (0 != (err_num = setjmp(((fmt_state_t*)st)->error_jmp)))
    {
        /* error handling */
        char *err;
        cst  *tcst;

        st = (fmt_state_t*)vst;

        if (st->ptable)
            free_pointer_table(st->ptable);

        /* Restore the saved characters */
        while (st->saves)
        {
            savechars *tmp;
            *(st->saves->where) = st->saves->what;
            tmp = st->saves;
            st->saves = st->saves->next;
            xfree(tmp);
        }

        /* Get rid of a temp string */
        if (st->clean.u.str)
            free_mstring(st->clean.u.str);
        if (st->tmp)
            xfree(st->tmp);

        /* Free column and table data */
        while ( NULL != (tcst = st->csts) )
        {
            st->csts = tcst->next;
            if ((tcst->info & (INFO_COLS|INFO_TABLE)) == INFO_TABLE
             && tcst->d.tab)
                xfree(tcst->d.tab);
            xfree(tcst);
        }

        /* Select the error string */
        switch(err_num & ERR_ID_NUMBER)
        {
        default:
#ifdef DEBUG
            fatal("undefined (s)printf() error 0x%X\n", err_num);
#endif
        case ERR_BUFF_OVERFLOW:
            err = "BUFF_SIZE overflowed...";
            break;

        case ERR_TO_FEW_ARGS:
            err = "More arguments specified than passed.";
            break;

        case ERR_INVALID_STAR:
            err = "Incorrect argument type to *.";
            break;

        case ERR_PREC_EXPECTED:
            err = "Expected precision not found.";
            break;

        case ERR_INVALID_FORMAT_STR:
            err = "Error in format string.";
            break;

        case ERR_INCORRECT_ARG:
            err = "incorrect argument type to %%%c.";
            break;

        case ERR_CST_REQUIRES_FS:
            err = "Column/table mode requires a field size.";
            break;

        case ERR_UNDEFINED_TYPE:
            err = "!feature - undefined type!";
            break;

        case ERR_QUOTE_EXPECTED:
            err = "Quote expected in format string.";
            break;

        case ERR_UNEXPECTED_EOS:
            err = "Unexpected end of format string.";
            break;

        case ERR_NULL_PS:
            err = "Null pad string specified.";
            break;

        case ERR_ARRAY_EXPECTED:
            err = "Array expected.";
            break;

        case ERR_NOMEM:
            err = "Out of memory.";
            break;

        case ERR_SIZE_OVERFLOW:
            err = "Fieldsize or precision too large.";
            break;
        }

        /* Create the error message in buff[] */
        st->buff[0]='\0';
        if ((err_num & ERR_ID_NUMBER) != ERR_NOMEM)
        {
            int line;
            string_t *file;

            file = NULL;
            line = get_line_number_if_any(&file);
            sprintf(st->buff, "%s:%d: ", get_txt(file), line);

            if (file)
                free_mstring(file);
        }

#ifdef RETURN_ERROR_MESSAGES
        strcat(st->buff, "(s)printf() error: ");
#else
        strcat(st->buff, "(s)printf(): ");
#endif
        sprintf(st->buff + strlen(st->buff)
               , err, EXTRACT_ERR_ARGUMENT(err_num));
        strcat(st->buff, "\n");
#ifndef RETURN_ERROR_MESSAGES
        strcpy(buff, st->buff);
        if (st == &static_fmt)
            static_fmt_used = MY_FALSE;
        else
            xfree(st);
        errorf("%s", buff); /* buff may contain a '%' */
        /* NOTREACHED */
#else
        result = new_mstring(st->buff);
        if (!result)
            result = ref_mstring(STR_OUT_OF_MEMORY);
        xfree(st);
#endif /* RETURN_ERROR_MESSAGES */
        return result;
    }

    st = (fmt_state_t*)vst;

    format_char = 0;
    nelemno = 0;
    column_stat = 0;

    arg = -1;
    st->bpos = 0;
    st->sppos = -1;
    st->line_start = 0;

    /* Walk through the format string */
    for (fpos = 0; MY_TRUE; fpos++)
    {
        if ((format_str[fpos] == '\n') || (!format_str[fpos]))
        {
            /* Line- or Format end */

            if (!st->csts)
            {
                /* No columns/tables to resolve, but add a second
                 * newline if there is one pending from an added
                 * column
                 */

                if (column_stat == 2)
                    ADD_CHAR(st, '\n');
                column_stat = 0;
                if (!format_str[fpos])
                    break;
                ADD_CHAR(st, '\n');
                st->line_start = st->bpos;
                continue;
            }

            column_stat = 0; /* If there was a newline pending, it
                              * will be implicitely added now.
                              */
            ADD_CHAR(st, '\n');
            st->line_start = st->bpos;

            /* Handle pending columns and tables */
            while (st->csts)
            {
                cst **temp;

                /* Add one line from each column/table */
                temp = &st->csts;
                while (*temp)
                {
                    p_int i;
                    if ((*temp)->info & INFO_COLS)
                    {
                        if (*((*temp)->d.col-1) != '\n')
                            while (*((*temp)->d.col) == ' ')
                                (*temp)->d.col++;
                        i = (*temp)->start - (st->bpos - st->line_start);
                        ADD_CHARN(st, ' ', i);
                        column_stat = add_column(st, temp);
                        if (!column_stat)
                            temp = &((*temp)->next);
                    }
                    else
                    {
                        i = (*temp)->start - (st->bpos - st->line_start);
                        if (i > 0)
                            ADD_CHARN(st, ' ', i);
                        if (!add_table(st, temp))
                            temp = &((*temp)->next);
                    }
                } /* while (*temp) */

                if (st->csts || format_str[fpos] == '\n')
                    ADD_CHAR(st, '\n');
                st->line_start = st->bpos;
            } /* while (csts) */

            if (column_stat == 2 && format_str[fpos] != '\n')
                ADD_CHAR(st, '\n');

            if (!format_str[fpos])
                break;
            continue;
        } /* if newline or formatend */

        if (format_str[fpos] == '%')
        {
            /* Another format entry */

            if (format_str[fpos+1] == '%')
            {
                ADD_CHAR(st, '%');
                fpos++;
                continue;
            }

            if (format_str[fpos+1] == '^')
            {
                ADD_CHAR(st, '%');
                fpos++;
                ADD_CHAR(st, '^');
                continue;
            }

            GET_NEXT_ARG;
            fs = 0;
            pres = 0;
            pad = " ";
            finfo = 0;

            /* Parse the formatting entry */
            for (fpos++; !(finfo & INFO_T); fpos++)
            {
                if (!format_str[fpos])
                {
                    finfo |= INFO_T_ERROR;
                    break;
                }
                if ((format_str[fpos] >= '0' && format_str[fpos] <= '9')
                 || (format_str[fpos] == '*'))
                {
                    /* Precision resp. fieldwidth */
                    if (pres == -1) /* then looking for pres */
                    {
                        if (format_str[fpos] == '*')
                        {
                            /* Get the value from the args */
                            if (carg->type != T_NUMBER)
                                ERROR(ERR_INVALID_STAR);
                            pres = carg->u.number;
                            GET_NEXT_ARG;
                            continue;
                        }

                        /* Parse the number */
                        pres = format_str[fpos] - '0';
                        for ( fpos++
                            ; format_str[fpos]>='0' && format_str[fpos]<='9'
                            ; fpos++)
                        {
                            int new_pres = pres*10 + format_str[fpos] - '0';

                            if (new_pres < pres) /* Overflow */
                                ERROR(ERR_SIZE_OVERFLOW);

                            pres = new_pres;
                        }
                    }
                    else /* then is fs (and maybe pres) */
                    {
                        if (format_str[fpos] == '0'
                         && (   (   format_str[fpos+1] >= '1'
                                 && format_str[fpos+1] <= '9')
                             || format_str[fpos+1] == '*')
                           )
                            finfo |= INFO_PS_ZERO;
                        else
                        {
                            if (format_str[fpos] == '*')
                            {
                                if (carg->type != T_NUMBER)
                                    ERROR(ERR_INVALID_STAR);
                                if ((p_int)(fs = carg->u.number) < 0)
                                {
                                    if (fs == (p_uint)PINT_MIN)
                                        fs = PINT_MAX;
                                    else
                                        fs = -fs;
                                    finfo |= INFO_A_LEFT;
                                }

                                if (pres == -2)
                                    pres = fs; /* colon */
                                GET_NEXT_ARG;
                                continue;
                            }
                            fs = format_str[fpos] - '0';
                        }

                        for ( fpos++
                            ; format_str[fpos] >= '0' && format_str[fpos] <= '9'
                            ; fpos++)
                        {
                            int new_fs = fs*10 + format_str[fpos] - '0';

                            if (new_fs < fs) /* Overflow */
                                ERROR(ERR_SIZE_OVERFLOW);

                            fs = new_fs;
                        }

                        if (pres == -2) /* colon */
                            pres = fs;
                    }

                    fpos--; /* bout to get incremented */
                    continue;
                } /* if (precision/alignment field) */

                /* fpos now points to format type */

                switch (format_str[fpos])
                {
                case ' ': finfo |= INFO_PP_SPACE; break;
                case '+': finfo |= INFO_PP_PLUS; break;
                case '-': finfo |= INFO_A_LEFT; break;
                case '|': finfo |= INFO_A_CENTRE; break;
                case '$': finfo |= INFO_A_JUSTIFY; break;
                case '@': finfo |= INFO_ARRAY; break;
                case '=': finfo |= INFO_COLS; break;
                case '#': finfo |= INFO_TABLE; break;
                case '.': pres = -1; break;
                case ':': pres = -2; break;
                case '%': finfo |= INFO_T_NULL; break; /* never reached */
                case 'O': finfo |= INFO_T_LPC; break;
                case 'Q': finfo |= INFO_T_QLPC; break;
                case 's': finfo |= INFO_T_STRING; break;
                case 'i': finfo |= INFO_T_INT; format_char = 'd'; break;
                case 'd':
                case 'c':
                case 'o':
                case 'b':
                case 'B':
                case 'x':
                case 'X':
                    format_char = format_str[fpos];
                    finfo |= INFO_T_INT;
                    break;
                case 'f':
                case 'F':
                case 'g':
                case 'G':
                case 'e':
                case 'E':
                    format_char = format_str[fpos];
                    finfo |= INFO_T_FLOAT;
                    break;
                case '\'':
                    pad = &(format_str[++fpos]);
                    finfo |= INFO_PS_KEEP;
                    while (1)
                    {
                        if (!format_str[fpos])
                            ERROR(ERR_UNEXPECTED_EOS);

                        if (format_str[fpos] == '\\')
                        {
                            if (!format_str[fpos+1])
                                ERROR(ERR_UNEXPECTED_EOS);
                            fpos += 2;
                            continue;
                        }

                        if (format_str[fpos] == '\'')
                        {
                            if (format_str+fpos == pad)
                                ERROR(ERR_NULL_PS);
                            SAVE_CHAR(format_str+fpos);
                            format_str[fpos] = '\0';
                            break;
                        }
                        fpos++;
                    }
                    break;
                default:
                    finfo |= INFO_T_ERROR;
                    break;
                }
            } /* for(format parsing) */

            if (pres < 0)
                ERROR(ERR_PREC_EXPECTED);

            /* Now handle the different arg types...
             */
            if (finfo & INFO_ARRAY)
            {
                if (carg->type != T_POINTER)
                    ERROR(ERR_ARRAY_EXPECTED);
                if (carg->u.vec == &null_vector)
                {
                    fpos--; /* 'bout to get incremented */
                    continue;
                }
                carg = (argv+arg)->u.vec->item;
                nelemno = 1; /* next element number */
            }

            while(1)
            {
                switch(finfo & INFO_T)
                {
                case INFO_T_ERROR:
                    ERROR(ERR_INVALID_FORMAT_STR);

                case INFO_T_NULL:
                  {
                    /* never reached... */
                    fprintf(stderr, "%s: (s)printf: INFO_T_NULL.... found.\n"
                                  , get_txt(current_object->name));
                    ADD_CHAR(st, '%');
                    break;
                  }

                case INFO_T_LPC:
                case INFO_T_QLPC:
                  {
                    sprintf_buffer_t *b;
#                   define CLEANSIZ 0x200

                    if (st->clean.u.str)
                        free_mstring(st->clean.u.str);
                    st->clean.u.str = NULL;
                    if (st->tmp)
                        xfree(st->tmp);

                    st->tmp = xalloc(CLEANSIZ);
                    if (!st->tmp)
                        ERROR(ERR_NOMEM);
                    st->tmp[0] = '\0';

                    st->ptable = new_pointer_table();
                    if (!st->ptable)
                        ERROR(ERR_NOMEM);
                    st->pointer_id = 1;

                    b = (sprintf_buffer_t *)
                        ( st->tmp+CLEANSIZ-sizeof(sprintf_buffer_t) );
                    b->offset = -CLEANSIZ+(p_int)sizeof(sprintf_buffer_t);
                    b->size = CLEANSIZ;
                    b->start = &st->tmp;
                    b = svalue_to_string(st, carg, b, 0, MY_FALSE
                                        , (finfo & INFO_T) == INFO_T_QLPC
                                        , (finfo & INFO_TABLE)
                                        , MY_FALSE
                                        );
                    finfo &= ~INFO_TABLE; /* since we fall through */

                    /* Store the created result in .clean and pass it
                     * to case INFO_T_STRING is 'the' carg.
                     */
                    put_string(&st->clean, new_n_mstring(st->tmp, b->size + b->offset - (p_int)sizeof(sprintf_buffer_t) ));
                    carg = &st->clean;

                    free_pointer_table(st->ptable);
                    st->ptable = NULL;
                    /* FALLTHROUGH */
                  }

                case INFO_T_STRING:
                  {
                    mp_int slen;

                    if (carg->type != T_STRING)
                        ERROR1(ERR_INCORRECT_ARG, 's');
                    slen = mstrsize(carg->u.str);

                    if (finfo & (INFO_COLS | INFO_TABLE) )
                    {
                        /* Add a new column/table info
                         * ... this is complicated ...
                         */
                        cst **temp;

                        if (!fs)
                            ERROR(ERR_CST_REQUIRES_FS);

                        if ((finfo & (INFO_COLS | INFO_TABLE))
                          == (INFO_COLS | INFO_TABLE)
                           )
                            ERROR(ERR_INVALID_FORMAT_STR);

                        /* Find the end of the list of columns/tables */
                        temp = &st->csts;
                        while (*temp)
                            temp = &((*temp)->next);

                        if (finfo & INFO_COLS)
                        {
                            /* Create a new columns structure */
                            *temp = xalloc(sizeof(cst));
                            if (!*temp)
                                ERROR(ERR_NOMEM);
                            (*temp)->next = NULL;
                            (*temp)->d.col = get_txt(carg->u.str);
                            (*temp)->pad = pad;
                            (*temp)->size = fs;
                            (*temp)->pres = (pres) ? (int)pres : (int)fs;
                            (*temp)->info = finfo;
                            (*temp)->start = st->bpos - st->line_start;

                            /* Format the first line from the column */
                            column_stat = add_column(st, temp);
                        }
                        else
                        {
                            /* (finfo & INFO_TABLE) */
                            unsigned int n, len, max;
                            int tpres;
                            char c, *s, *start;
                            p_uint i;

#                    define TABLE get_txt(carg->u.str)

                            /* Create the new table structure */
                            (*temp) = (cst *)xalloc(sizeof(cst));
                            if (!*temp)
                                ERROR(ERR_NOMEM);
                            (*temp)->pad = pad;
                            (*temp)->info = finfo;
                            (*temp)->start = st->bpos - st->line_start;
                            (*temp)->next = NULL;

                            /* Determine the size of the table */
                            max = len = 0;
                            n = 1;
                            s = TABLE;
                            if ( '\0' != (c = *(start = s)) ) for (;;)
                            {
                                if (c != '\n')
                                {
                                    if ( '\0' != (c = *++s) )
                                        continue;
                                    else
                                        break;
                                }
                                len = s - start;
                                if (len > max)
                                    max = len;
                                n++;
                                if ( '\0' != (c = *(start = ++s)) )
                                {
                                    continue;
                                }
                                n--;
                                break;
                            } /* if() for() */

                            /* Now: n = number of lines
                             *      max = max length of the lines
                             */

                            tpres = pres;
                            if (tpres)
                            {
                                (*temp)->size = fs/tpres;
                            }
                            else
                            {
                                len = s - start;
                                if (len > max)
                                    max = len; /* the null terminated word */
                                tpres = fs/(max+2);
                                  /* at least two separating spaces */
                                if (!tpres)
                                    tpres = 1;
                                (*temp)->size = fs/tpres;
                            }

                            len = n/tpres; /* length of average column */

                            if (n < (unsigned int)tpres)
                                tpres = n;
                            if (len*tpres < n)
                                len++;
                            /* Since the table will be filled by column,
                             * the result will be a rectangle with as little
                             * as possible empty space. This means we have
                             * to adjust the no of columns (tpres) to
                             * what the fill algorithm will actually
                             * produce.
                             */
                            if (len > 1 && n%tpres)
                                tpres -= (tpres - n%tpres)/len;

                            (*temp)->d.tab = xalloc(tpres*sizeof(char *));
                            if (!(*temp)->d.tab)
                                ERROR(ERR_NOMEM);
                            (*temp)->nocols = tpres; /* heavy sigh */
                            (*temp)->d.tab[0] = TABLE;

                            if (tpres == 1)
                                goto add_table_now;

                            /* Subformat the table, replacing some characters
                             * in the given string.
                             * The original chars are saved for later restoring.
                             */
                            i = 1; /* the next column number */
                            n = 0; /* the current "word" number in this column */
                            for (fs = 0; TABLE[fs]; fs++)
                            {
                                /* throwing away fs... */
                                if (TABLE[fs] == '\n')
                                {
                                    if (++n >= len)
                                    {
                                        SAVE_CHAR(((TABLE)+fs));
                                        TABLE[fs] = '\0';
                                        (*temp)->d.tab[i++] = TABLE+fs+1;
                                        if (i >= (unsigned int)tpres)
                                            goto add_table_now;
                                        n = 0;
                                    }
                                }
                            } /* for (fs) */

add_table_now:
                            /* Now add the table (at least the first line) */
                            add_table(st, temp);
                        }
                    }
                    else
                    {
                        Bool justifyString;

                        /* not column or table */
                        if (pres && pres < slen)
                        {
                            slen = pres;
                        }

                        /* Determine whether to print this string
                         * justified, or not.
                         */
                        justifyString = MY_FALSE;
                        if (fs && (finfo & INFO_A) == INFO_A_JUSTIFY)
                        {
                            /* Flush the string only if it doesn't
                             * end in a NL.
                             * Also, strip off trailing spaces.
                             */
                            for ( ;    slen > 0
                                    && get_txt(carg->u.str)[slen-1] == ' '
                                  ; slen--
                               ) NOOP;

                            if ( slen != 0
                              && (unsigned int)slen <= fs
                              && get_txt(carg->u.str)[slen-1] != '\n'
                               )
                                justifyString = MY_TRUE;
                        }

                        /* not column or table */

                        if (justifyString)
                        {
                            add_justified(st, get_txt(carg->u.str), slen, fs);
                        }
                        else if (fs && fs > (unsigned int)slen)
                        {
                            add_aligned(st, get_txt(carg->u.str)
                                       , slen, pad, fs, finfo);
                        }
                        else
                        {
                            ADD_STRN(st, get_txt(carg->u.str), slen);
                        }
                    }
                    break;
                  }

                case INFO_T_INT:
                case INFO_T_FLOAT:
                    /* We 'cheat' by using the systems sprintf() to format
                     * the number in most of the formats.
                     */
                  if ((finfo & INFO_T ) == INFO_T_INT
                   && (format_char == 'b' || format_char == 'B')
                     )
                  {
                    /* Dummy for finding most significant bit */
                    int signi = 0;
                    int isize = sizeof(carg->u.number) * CHAR_BIT;
                    char temp[sizeof(carg->u.number) * CHAR_BIT + 1];
                    int tmpl;
                    
                    memset(temp, '\0', sizeof(temp));
                    
                    if (carg->type != T_NUMBER)
                    {
                        ERROR1(ERR_INCORRECT_ARG, format_char);
                    }
                    /* Calculate binary representation (2's compliment) */
                    while ( --isize > -1 )
                    {
                       if ( ( carg->u.number & ( 1 << isize ) ) != 0 )
                       {
                          signi = 1;
                          strcat( temp, "1" );
                       }
                       else if ( signi )
                          strcat( temp, "0" );
                    }
                    tmpl = strlen(temp);
                    if ((size_t)tmpl >= sizeof(temp))
                        fatal("Local buffer overflow in sprintf() for int.\n");
                    if (pres && tmpl > pres)
                        tmpl = pres; /* well.... */
                    if ((unsigned int)tmpl < fs)
                        add_aligned(st, temp, tmpl, pad, fs, finfo);
                    else
                        ADD_STRN(st, temp, tmpl);
                    break;
                  }
                  else
                  {
                    /* Synthesized format for sprintf() */
                    char cheat[6+sizeof(PRI_PINT_PREFIX)-1];
                    char temp[1024];
                      /* The buffer must be big enough to hold the biggest float
                       * in non-exponential representation. 1 KByte is hopefully
                       * far on the safe side.
                       * TODO: Allocate it dynamically?
                       */
                    double value;   /* The value to print */
                    int    numdig;  /* (Estimated) number of digits before the '.' */
                    Bool zeroCharHack = MY_FALSE;
                    char *p = cheat; /* pointer to the format buffer */
                    int tmpl;

                    *(p++) = '%';
                    switch (finfo & INFO_PP)
                    {
                        case INFO_PP_SPACE: *(p++) = ' '; break;
                        case INFO_PP_PLUS:  *(p++) = '+'; break;
                    }
                    if ((finfo & INFO_T) == INFO_T_FLOAT)
                    {
                        if (carg->type != T_FLOAT) /* sigh... */
                        {
                            ERROR1(ERR_INCORRECT_ARG, format_char);
                        }
                        *(p++) = '.';
                        *(p++) = '*';
                        *(p++) = format_char;
                        *p = '\0';

                        value = READ_DOUBLE(carg);

                        if ('e' == format_char
                         || 'E' == format_char
                         || fabs(value) < 1.0)
                            numdig = 1;
                        else
                            numdig = (int) ceil(log10(fabs(value)));
                        if (value < 0.0)
                            numdig++;

                        if ((size_t)pres > (sizeof(temp) - 12 - numdig))
                        {
                            pres = sizeof(temp) - 12 - numdig;
                        }
                        sprintf(temp, cheat, pres, READ_DOUBLE(carg));
                        tmpl = strlen(temp);
                        if ((size_t)tmpl >= sizeof(temp))
                            fatal("Local buffer overflow in sprintf() for float.\n");
                    }
                    else
                    {
                        if (carg->type != T_NUMBER) /* sigh... */
                        {
                            ERROR1(ERR_INCORRECT_ARG, format_char);
                        }

                        /* System sprintf() can't handle ("%c", 0), but LDMud
                         * strings can. So in that case we format with
                         * character 0x01 and convert to 0 afterwards.
                         */
                        if (format_char == 'c') {
                            if (carg->u.number == 0)
                            {
                            carg->u.number = 1;
                            zeroCharHack = MY_TRUE;
                            }
                        }
                        /* insert the correct length modifier for a p_int
                         * type. (If the prefix is an empty string, this will
                         * be probably optimized by the compiler anyway. */
                        else {
                            p = memcpy(p, PRI_PINT_PREFIX,
                                          strlen(PRI_PINT_PREFIX));
                            p += strlen(PRI_PINT_PREFIX);
                        }
                        *(p++) = format_char;
                        *p = '\0';
                        sprintf(temp, cheat, carg->u.number);
                        tmpl = strlen(temp);
                        if ((size_t)tmpl >= sizeof(temp))
                            fatal("Local buffer overflow in sprintf() for int.\n");
                        if (pres && tmpl > pres)
                            tmpl = pres; /* well.... */

                        if (zeroCharHack)
                        {
                            int pos;
                            for (pos = 0; pos < tmpl; ++pos)
                            {
                                if (temp[pos] == 0x01)
                                    temp[pos] = 0x00;
                            }
                        }
                    }
                    if ((unsigned int)tmpl < fs)
                    {
                        if ((finfo & INFO_PS_ZERO) != 0
                         && (   temp[0] == ' '
                             || temp[0] == '+'
                             || temp[0] == '-'
                            )
                         && (finfo & INFO_A) != INFO_A_LEFT
                           )
                        {
                            /* Non-left alignment and we're printing
                             * with leading zeroes: preserve the sign
                             * character in the right place.
                             */
                            ADD_STRN(st, temp, 1);
                            add_aligned(st, temp+1, tmpl-1, pad, fs-1, finfo);
                        }
                        else
                            add_aligned(st, temp, tmpl, pad, fs, finfo);
                    }
                    else
                        ADD_STRN(st, temp, tmpl);
                    break;
                  }
                default:        /* type not found */
                    ERROR(ERR_UNDEFINED_TYPE);
                }

                if (!(finfo & INFO_ARRAY))
                    break;

                if (nelemno >= (size_t)VEC_SIZE((argv+arg)->u.vec))
                    break;

                carg = (argv+arg)->u.vec->item+nelemno++;
            } /* end of while (1) */
            fpos--; /* bout to get incremented */
            continue;
        } /* if format entry */

        /* Nothing to format: just copy the character */
        ADD_CHAR(st, format_str[fpos]);
    } /* for (fpos=0; 1; fpos++) */

    ADD_CHAR(st, '\0'); /* Terminate the formatted string */

    /* Restore characters */
    while (st->saves)
    {
        savechars *tmp;
        *(st->saves->where) = st->saves->what;
        tmp = st->saves;
        st->saves = st->saves->next;
        xfree(tmp);
    }

    /* Free the temp string */
    if (st->clean.u.str)
        free_mstring(st->clean.u.str);
    if (st->tmp)
        xfree(st->tmp);

    /* Copy over the result */
    if  (st->bpos > 1)
        result = new_n_mstring(st->buff, st->bpos-1);
    else
        result = ref_mstring(STR_EMPTY);
    if (!result)
        result = ref_mstring(STR_OUT_OF_MEMORY);

    if (st == &static_fmt)
        static_fmt_used = MY_FALSE;
    else
        xfree(st);

    /* Done */
    return result;

#undef GET_NEXT_ARG
#undef SAVE_CHAR

} /* string_print_formatted() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_printf (svalue_t *sp, int num_arg)

/* EFUN printf()
 *
 *   void printf(string format, ...)
 *
 * A cross between sprintf() and write(). Returns void and prints
 * the result string to the user.
 */

{
    string_t *str;

    str = string_print_formatted(get_txt((sp-num_arg+1)->u.str)
                                , num_arg-1, sp-num_arg+2);
    if (command_giver)
        tell_object(command_giver, str);
    else
        add_message(FMT_STRING, str);
    free_mstring(str);
    sp = pop_n_elems(num_arg, sp);

    return sp;
} /* v_printf() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_sprintf (svalue_t *sp, int num_arg)

/* EFUN sprintf()
 *
 *   string sprintf(string fmt, ...)
 *
 * Generate a string according to the <fmt> and the following
 * arguments and put it onto the stack.
 *
 * <fmt> follows the C-style sprintf-format string in style,
 * and partly in meaning, too.
 */

{
    string_t *s;

    s = string_print_formatted(get_txt((sp-num_arg+1)->u.str),
                               num_arg-1, sp-num_arg+2);
    sp = pop_n_elems(num_arg, sp);
    if (!s)
        push_number(sp, 0);
    else
        push_string(sp, s);

    return sp;
} /* v_sprintf() */

/***************************************************************************/

