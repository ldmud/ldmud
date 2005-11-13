/*
 * sprintf.c v1.05 for LPMud 3.0.52
 *
 * An implementation of (s)printf() for LPC, with quite a few
 * extensions (note that as no floating point exists, some parameters
 * have slightly different meaning or restrictions to "standard"
 * (s)printf.)
 * Implemented and put into the public domain by Lynscar (Sean A Reith).
 *
 * This version supports the following as modifiers:
 *  " "   pad positive integers with a space.
 *  "+"   pad positive integers with a plus sign.
 *  "-"   left adjusted within field size.
 *        NB: std (s)printf() defaults to right justification, which is
 *            unnatural in the context of a mainly string based language
 *            but has been retained for "compatability" ;)
 *  "|"   centered within field size.
 *  "="   column mode if strings are greater than field size.  this is only
 *        meaningful with strings, all other types ignore
 *        this.  columns are auto-magically word wrapped.
 *  "#"   table mode, print a list of '\n' separated 'words' in a
 *        table within the field size.  only meaningful with strings.
 *   n    specifies the field size, a '*' specifies to use the corresponding
 *        arg as the field size.  if n is prepended with a zero, then is padded
 *        zeros, else it is padded with spaces (or specified pad string).
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
 *        overrules).  NOTE:  to include "'" in the pad string, you must
 *        use "\\'" (as the backslash has to be escaped past the
 *        interpreter), similarly, to include "\" requires "\\\\".
 * The following are the possible type specifiers.
 *  "%"   in which case no arguments are interpreted, and a "%" is inserted, and
 *        all modifiers are ignored.
 *  "O"   the argument is an LPC datatype.
 *  "s"   the argument is a string.
 *  "d"   the integer arg is printed in decimal.
 *  "i"   as d.
 *  "c"   the integer arg is to be printed as a character.
 *  "o"   the integer arg is printed in octal.
 *  "x"   the integer arg is printed in hex.
 *  "X"   the integer arg is printed in hex (in capitals).
 * e,E,f,F,g,G like in c.
 *
 * TODO: Improve the printing of endless loops in arrays/mappings, maybe
 * TODO:: in a way similar to save_object(). For example:
 * TODO::   ({ / * <1> * / 1, -1, ({ / * <2> * / 3, <1> }) })
 * TODO:: (the space between *s and /s keeps the compiler from misparsing
 * TODO::  the example).
 */

#include "driver.h"

#include <stdio.h>
#include <setjmp.h>
#include <sys/types.h>

#define NO_INCREMENT_STRING_REF
#include "sprintf.h"

#include "array.h"
#include "exec.h"
#include "interpret.h"
#include "instrs.h"
#include "main.h"
#include "object.h"
#include "prolang.h"
#include "sent.h"
#include "simulate.h"
#include "simul_efun.h"
#include "stralloc.h"
#include "swap.h"

/*
 * If this #define is defined then error messages are returned,
 * otherwise error() is called (ie: A "wrongness in the fabric...")
 */
#define RETURN_ERROR_MESSAGES

#if defined(F_SPRINTF) || defined(F_PRINTF)

typedef unsigned int format_info;
/*
 * Format of format_info:
 *   00000000 0000xxxx : argument type:
 *                                0000 : type not found yet;
 *                                0001 : error type not found;
 *                                0010 : percent sign, null argument;
 *                                0011 : LPC datatype;
 *                                0100 : string;
 *                                0101 : integer;
 *                                0110 : float
 *   00000000 00xx0000 : justification:
 *                                00 : right;
 *                                01 : centre;
 *                                10 : left;
 *   00000000 xx000000 : positive pad char:
 *                                00 : none;
 *                                01 : ' ';
 *                                10 : '+';
 *   0000000x 00000000 : array mode?
 *   000000x0 00000000 : column mode?
 *   00000x00 00000000 : table mode?
 */

#define INFO_T 0xF
#define INFO_T_ERROR 0x1
#define INFO_T_NULL 0x2
#define INFO_T_LPC 0x3
#define INFO_T_STRING 0x4
#define INFO_T_INT 0x5
#define INFO_T_FLOAT 0x6

#define INFO_J 0x30
#define INFO_J_CENTRE 0x10
#define INFO_J_LEFT 0x20

#define INFO_PP 0xC0
#define INFO_PP_SPACE 0x40
#define INFO_PP_PLUS 0x80

#define INFO_ARRAY 0x100
#define INFO_COLS 0x200
#define INFO_TABLE 0x400

#define BUFF_SIZE 10000

#define ERROR(x) (longjmp(error_jmp, (x)))
#define ERR_ID_NUMBER             0xFFFF
#define ERR_BUFF_OVERFLOW        0x1        /* buffer overflowed */
#define ERR_TO_FEW_ARGS                0x2        /* more arguments spec'ed than passed */
#define ERR_INVALID_STAR        0x3        /* invalid arg to * */
#define ERR_PREC_EXPECTED        0x4        /* expected precision not found */
#define ERR_INVALID_FORMAT_STR        0x5        /* error in format string */
#define ERR_INCORRECT_ARG        0x6        /* invalid arg to %[idcxXs] */
#define ERR_CST_REQUIRES_FS        0x7        /* field size not given for c/t */
#define ERR_UNDEFINED_TYPE        0x8        /* undefined type found */
#define ERR_QUOTE_EXPECTED        0x9        /* expected ' not found */
#define ERR_UNEXPECTED_EOS        0xA        /* fs terminated unexpectedly */
#define ERR_NULL_PS                0xB        /* pad string is null */
#define ERR_ARRAY_EXPECTED        0xC        /* array expected */
#define ERR_NOMEM                0xD        /* Out of memory */

#define ERR_ARGUMENT         0xFFFF0000

#define ERROR1(e,a) ERROR((e) | (a)<<16)
#define EXTRACT_ERR_ARGUMENT(i) ((i)>>16)

#define ADD_CHAR(x) {\
    if (bpos >= BUFF_SIZE) ERROR(ERR_BUFF_OVERFLOW); \
    buff[bpos++] = x;\
}

#define ADD_STRN(s, n) { \
    if (bpos + n > BUFF_SIZE) ERROR(ERR_BUFF_OVERFLOW); \
    strncpy(buff+bpos, s, n); \
    bpos += n; \
}

#define ADD_CHARN(c, n) { \
    /* n must not be negative! */ \
    if (bpos + n > BUFF_SIZE) ERROR(ERR_BUFF_OVERFLOW); \
    memset(buff+bpos, c, n); \
    bpos += n; \
}

#define ADD_PADDING(pad, N) { \
    int n = (N); \
\
    if (!pad[1]) { \
        ADD_CHARN(*pad, n) \
    } else { \
        int l; \
\
        l = strlen(pad); \
        for (i=0; --n >= 0; ) { \
            if (pad[i] == '\\') \
                i++; \
            ADD_CHAR(pad[i]); \
            if (++i == l) \
                i = 0; \
        } \
    } \
}

#define GET_NEXT_ARG {\
  if (++arg >= argc) ERROR(ERR_TO_FEW_ARGS); \
  carg = (argv+arg);\
}

#define SAVE_CHAR(pointer) {\
  savechars *new;\
  new = (savechars *)xalloc(sizeof(savechars));\
  if (!new) \
    ERROR(ERR_NOMEM); \
  new->what = *(pointer);\
  new->where = pointer;\
  new->next = saves;\
  saves = new;\
}

/*
 * list of characters to restore before exiting.
 */
typedef struct SaveChars {
  char what;
  char *where;
  struct SaveChars *next;
} savechars;

typedef struct ColumnSlashTable {
  union CSTData {
    char *col;                        /* column data */
    char **tab;                        /* table data */
  } d;                                /* d == data */
  unsigned short int nocols;        /* number of columns in table *sigh* */
  char *pad;
  unsigned int start;                /* starting cursor position */
  unsigned int size;                /* column/table width */
  int pres;                        /* precision */
  format_info info;                /* formatting data */
  struct ColumnSlashTable *next;
} cst;                                /* Columns Slash Tables */

struct sprintf_buffer {
    /* char text[0]; */
#define BUF_TEXT(b) ((char *)(b))
    int offset;
    int size;
    char **start;
};

static char buff[BUFF_SIZE];        /* buffer for returned string */
static unsigned int bpos;                /* position in buff */
static unsigned int line_start;
static jmp_buf error_jmp;                /* for error longjmp()s */

static struct sprintf_buffer *realloc_sprintf_buffer(b)
    struct sprintf_buffer *b;
{
    int offset   = b->offset;
    int size     = b->size;
    char **start = b->start;
    char *newstart;

    do {
        if (size > BUFF_SIZE)
            ERROR(ERR_BUFF_OVERFLOW);
        offset -= size;
        size <<= 1;
        newstart = rexalloc(*start, size);
        if (!newstart)
            ERROR(ERR_NOMEM);
        *start = newstart;
    } while (offset >= 0);
    b = (struct sprintf_buffer *)(*start+size - sizeof(struct sprintf_buffer));
    b->offset = offset;
    b->size = size;
    b->start = start;
    return b;
}

static void stradd(buffer, add)
    struct sprintf_buffer **buffer;
    char *add;
{
    register struct sprintf_buffer *b = *buffer;
    register int o;
    int len;

    len = strlen(add);
    o = b->offset;
    if ( (b->offset = o + len) >= 0) {
        *buffer = b = realloc_sprintf_buffer(b);
        o = b->offset - len;
    }
    strcpy(BUF_TEXT(b) + o, add);
}

static void numadd(buffer, num)
    struct sprintf_buffer **buffer;
    int num;
{
    struct sprintf_buffer *b = *buffer;
    int i,j,nve;

    if (num < 0) {
        if ( (num *= -1) < 0) {
            numadd(buffer, num+1);
            b = *buffer;
            BUF_TEXT(b)[b->offset - 1] += 1;
            return;
        }
        nve=1;
    } else nve=0;
    if (num >= 1000000000)
        j = nve + 10;
    else
        for (i=10, j=nve+1; num >= i; i*=10, j++) ;
    i = b->offset;
    if ((b->offset = i + j) >= 0) {
        *buffer = b = realloc_sprintf_buffer(b);
        i = b->offset - j;
    }
    BUF_TEXT(b)[i+j] = '\0';
    if (nve) {
        BUF_TEXT(b)[i] = '-';
        j--;
    } else
        i--;
    for (; j; j--, num /= 10)
        BUF_TEXT(b)[i+j] = (num%10) + '0';
} /* end of num_add() */

/*
 * This is a function purely because stradd() is, to keep same param
 * passing...
 */
static void add_indent(buffer, indent)
    struct sprintf_buffer **buffer;
    int indent;
{
    int i;
    struct sprintf_buffer *b = *buffer;
    char *p;

    i = b->offset;
    if ( (b->offset = i + indent) >= 0) {
        *buffer = b = realloc_sprintf_buffer(b);
        i = b->offset - indent;
    }
    p = BUF_TEXT(b) + i;
    for (;indent;indent--)
        *p++ = ' ';
    *p = '\0';
}

static struct sprintf_buffer *svalue_to_string
  PROT((struct svalue *, struct sprintf_buffer *, int, int));

#ifdef MAPPINGS
struct stsf_locals {
    struct sprintf_buffer *spb;
    int indent;
    int num_values;
};

static void svalue_to_string_filter(key, data, extra)
    struct svalue *key;
    struct svalue *data;
    char *extra;
{
    int i;
    struct stsf_locals *locals = (struct stsf_locals *)extra;
    char *delimiter = ":";

    i = locals->num_values;
    locals->spb =
      svalue_to_string(key, locals->spb, locals->indent, !i);
    while (--i >= 0) {
        stradd(&locals->spb, delimiter);
        locals->spb =
          svalue_to_string(data++, locals->spb, 1, !i);
        delimiter = ";";
    }
}
#endif /* MAPPINGS */

/*
 * Converts any LPC datatype into an arbitrary string format
 * and returns a pointer to this string.
 * Scary number of parameters for a recursive function.
 */
static struct sprintf_buffer *svalue_to_string(obj, str, indent, trailing)
  struct svalue *obj;
  struct sprintf_buffer *str;
  int indent, trailing;
{
  int i;

  add_indent(&str, indent);
  switch (obj->type) {
    case T_INVALID:
      stradd(&str, "T_INVALID");
      break;
    case T_LVALUE:
      stradd(&str, "lvalue: ");
      str = svalue_to_string(obj->u.lvalue, str, indent+2, trailing);
      break;
    case T_NUMBER:
      numadd(&str, obj->u.number);
      break;
#ifdef FLOATS
    case T_FLOAT:
    {
      char s[80];

      sprintf(s, "%g", READ_DOUBLE(obj) );
      stradd(&str, s);
      break;
    }
#endif
    case T_STRING:
      stradd(&str, "\"");
      stradd(&str, obj->u.string);
      stradd(&str, "\"");
      break;
    case T_QUOTED_ARRAY:
    {
      i = obj->x.quotes;
      do {
        stradd(&str, "\'");
      } while (--i);
    }
    /* fall through */
    case T_POINTER:
    {
      mp_int size;

      size = VEC_SIZE(obj->u.vec);
      if (!size) {
        stradd(&str, "({ })");
      } else {
        stradd(&str, "({ /* sizeof() == ");
        numadd(&str, size);
        stradd(&str, " */\n");
        for (i=0; i<size-1; i++)
          str = svalue_to_string(&(obj->u.vec->item[i]), str, indent+2, 1);
        str = svalue_to_string(&(obj->u.vec->item[i]), str, indent+2, 0);
        stradd(&str, "\n");
        add_indent(&str, indent);
        stradd(&str, "})");
      }
      break;
    }
#ifdef MAPPINGS
    case T_MAPPING:
    {
        void check_map_for_destr PROT((struct mapping *));
        void walk_mapping PROT((
            struct mapping *,
            void (*)(struct svalue *, struct svalue *, char *),
            char *
        ));

      struct stsf_locals locals;

      stradd(&str, "([\n");
      locals.spb = str;
      locals.indent = indent + 2;
      locals.num_values = obj->u.map->num_values;
      check_map_for_destr(obj->u.map);
      walk_mapping(obj->u.map, svalue_to_string_filter, (char*)&locals);
      str = locals.spb;
      add_indent(&str, indent);
      stradd(&str, "])");
      break;
    }
#endif /* MAPPINGS */
    case T_OBJECT:
    {
      struct svalue *temp;

      if (obj->u.ob->flags & O_DESTRUCTED)
      {
        /* *obj might be a mapping key, thus we mustn't change it. */
        stradd(&str,"0");
        break;
      }
      stradd(&str, obj->u.ob->name);
      push_object(obj->u.ob);
      temp = apply_master_ob(STR_OBJ_NAME, 1);
      if (temp && (temp->type == T_STRING)) {
        stradd(&str, " (\"");
        stradd(&str, temp->u.string);
        stradd(&str, "\")");
      }
      /*
       * These flags aren't that useful...
       *
      if (obj->u.ob->flags & O_HEART_BEAT)        stradd(&str, " (hb)");
      if (obj->u.ob->flags & O_IS_WIZARD)        stradd(&str, " (wiz)");
      if (obj->u.ob->flags & O_ENABLE_COMMANDS)        stradd(&str, " (enabled)");
      if (obj->u.ob->flags & O_CLONE)                stradd(&str, " (clone)");
      if (obj->u.ob->flags & O_DESTRUCTED)        stradd(&str, " (destructed)");
      if (obj->u.ob->flags & O_SWAPPED)                stradd(&str, " (swapped)");
      if (obj->u.ob->flags & O_ONCE_INTERACTIVE) stradd(&str, " (x-activ)");
      if (obj->u.ob->flags & O_APPROVED)        stradd(&str, " (ok)");
      if (obj->u.ob->flags & O_RESET_STATE)        stradd(&str, " (reset)");
      if (obj->u.ob->flags & O_WILL_CLEAN_UP)        stradd(&str, " (clean up)");
       */
      break;
    }
    case T_SYMBOL:
      i = obj->x.quotes;
      do {
        stradd(&str, "\'");
      } while (--i);
      stradd(&str, obj->u.string);
      break;
    case T_CLOSURE:
    {
      int type;

      switch(type = obj->x.closure_type) {
        case CLOSURE_LFUN:
        case CLOSURE_ALIEN_LFUN:
        {
          struct lambda *l;
          struct program *prog;
          int ix;
          uint32 flags;
          char *function_name;
          struct object *ob;

          l = obj->u.lambda;
          if (type == CLOSURE_LFUN) {
            ob = l->ob;
            ix = l->function.index;
          } else {
            ob = l->function.alien.ob;
            ix = l->function.alien.index;
          }
          if (ob->flags & O_DESTRUCTED) {
            stradd(&str, "<local function in destructed object>");
            break;
          }
          if (O_PROG_SWAPPED(ob))
            load_ob_from_swap(ob);
          stradd(&str, "#'");
          stradd(&str, ob->name);
          stradd(&str, "->");
          prog = ob->prog;
          flags = prog->functions[ix];
          while (flags & NAME_INHERITED) {
            struct inherit *inheritp;

            inheritp = &prog->inherit[flags & INHERIT_MASK];
            ix -= inheritp->function_index_offset;
            prog = inheritp->prog;
            flags = prog->functions[ix];
          }
          memcpy(
            (char *)&function_name,
            prog->program + (flags & FUNSTART_MASK) - 1 - sizeof function_name,
            sizeof function_name
          );
          stradd(&str, function_name);
          break;
        }
        case CLOSURE_IDENTIFIER:
        {
          struct lambda *l;

          l = obj->u.lambda;
          if (l->ob->flags & O_DESTRUCTED) {
            stradd(&str, "<local variable in destructed object>");
            break;
          }
          if (O_PROG_SWAPPED(l->ob))
            load_ob_from_swap(l->ob);
          stradd(&str, "#'");
          stradd(&str, l->ob->name);
          stradd(&str, "->");
          stradd(&str, l->ob->prog->variable_names[l->function.index].name);
          break;
        }
        default:
          if (type < 0) {
            switch(type & -0x0800) {
              case CLOSURE_OPERATOR:
              {
                char *s = 0;
                switch(type - CLOSURE_OPERATOR) {
                  case F_POP_VALUE-F_OFFSET:
                    s = ",";
                    break;
                  case F_BBRANCH_WHEN_NON_ZERO-F_OFFSET:
                    s = "do";
                    break;
                  case F_BBRANCH_WHEN_ZERO-F_OFFSET:
                    s = "while";
                    break;
                  case F_BRANCH-F_OFFSET:
                    s = "continue";
                    break;
                  case F_CSTRING0-F_OFFSET:
                    s = "default";
                    break;
                  case F_BRANCH_WHEN_ZERO-F_OFFSET:
                    s = "?";
                    break;
                  case F_BRANCH_WHEN_NON_ZERO-F_OFFSET:
                    s = "?!";
                    break;
                  case F_RANGE-F_OFFSET:
                    s = "[..]";
                    break;
                  case F_NR_RANGE-F_OFFSET:
                    s = "[..<]";
                    break;
                  case F_RR_RANGE-F_OFFSET:
                    s = "[<..<]";
                    break;
                  case F_RN_RANGE-F_OFFSET:
                    s = "[<..]";
                    break;
                  case F_MAP_INDEX-F_OFFSET:
                    s = "[,]";
                    break;
                  case F_EXTRACT2-F_OFFSET:
                    s = "[..";
                    break;
                  case F_EXTRACT_LVALUE-F_OFFSET:
                    s = "[<..";
                    break;
                }
                if (s) {
                    stradd(&str, "#'");
                    stradd(&str, s);
                    break;
                }
                type += CLOSURE_EFUN - CLOSURE_OPERATOR;
              }
              /* default action for operators: fall through to efun case . */
              case CLOSURE_EFUN:
                stradd(&str, "#'");
                stradd(&str, instrs[type - CLOSURE_EFUN].name);
                break;
              case CLOSURE_SIMUL_EFUN:
              {
                stradd(&str, "#'");
                stradd(&str, simul_efunp[type - CLOSURE_SIMUL_EFUN].name);
                break;
              }
            }
            break;
          }
        case CLOSURE_LAMBDA:
        case CLOSURE_BOUND_LAMBDA:
        case CLOSURE_UNBOUND_LAMBDA:
        case CLOSURE_PRELIMINARY:
        {
          static char s[] = "<lambda 0x012345>6789abcdef";

          sprintf(s+10, "%lx>", (long)obj->u.lambda);
          stradd(&str, s);
          break;
        }
      }
      break;
    }
    default:
      stradd(&str, "!ERROR: GARBAGE SVALUE!");
  } /* end of switch (obj->type) */
  if (trailing) stradd(&str, ",\n");
  return str;
} /* end of svalue_to_string() */

/*
 * Adds the string "str" to the buff after justifying it within "fs".
 * "trailing" is a flag which is set if trailing justification is to be done.
 * "str" is unmodified.  trailing is, of course, ignored in the case
 * of right justification.
 */
static void add_justified(str, len, pad, fs, finfo, trailing)
  char *str, *pad;
  int len;
  int fs;
  format_info finfo;
  short int trailing;
{
  int i;

  if (fs < len)
    fs = len;
  switch(finfo & INFO_J) {
    case INFO_J_LEFT:
      ADD_STRN(str, len)
      if (trailing)
        ADD_PADDING(pad, fs - len)
      break;
    case INFO_J_CENTRE:
      ADD_PADDING(pad, (fs - len + 1) >> 1)
      ADD_STRN(str, len)
      if (trailing)
        ADD_PADDING(pad, (fs - len) >> 1)
      break;
    default: { /* std (s)printf defaults to right justification */
      ADD_PADDING(pad, fs - len)
      ADD_STRN(str, len)
    }
  }
} /* end of add_justified() */

/*
 * Adds "column" to the buffer.
 * Returns 0 is column not finished.
 * Returns 1 if column completed.
 * Returns 2 if column completed has a \n at the end.
 */
static int add_column(column, trailing)
  cst **column;
  short int trailing;
{
  register unsigned int done;
  mp_int length;
  unsigned int save;
#define COL (*column)
  char *COL_D = COL->d.col;
  char *p;

  length = COL->pres;
  for (p = COL_D; length && *p && *p !='\n'; p++, length--) NOOP;
  done = p - COL_D;
  if (*p && *p !='\n') {
    save = done;
    for (; ; done--,p--) {
        /* handle larger than column size words... */
        if (!done) {
            done = save - 1;
            p += save;
            break;
        }
        if (*p == ' ')
            break;
     }
  }
  add_justified(COL_D, p - COL_D, COL->pad, COL->size, COL->info,
                (trailing || (COL->next)));
  COL_D += done; /* inc'ed below ... */
  /*
   * if this or the next character is a NULL then take this column out
   * of the list.
   */
  if (!(*COL_D) || !(*(++COL_D))) {
    cst *temp;
    int ret;

    if (*(COL_D-1) == '\n') ret = 2; else ret = 1;
    temp = COL->next;
    xfree((char *)COL);
    COL = temp;
    return ret;
  }
  COL->d.col = COL_D;
  return 0;
} /* end of add_column() */

/*
 * Adds "table" to the buffer.
 * Returns 0 if table not completed.
 * Returns 1 if table completed.
 */
static int add_table(table, trailing)
  cst **table;
  short int trailing;
{
  register unsigned int done, i;
#define TAB (*table)
#define TAB_D (TAB->d.tab[i])

  for (i=0; i < TAB->nocols && TAB_D; i++) {
    for (done=0;(TAB_D[done])&&(TAB_D[done] != '\n');done++) NOOP;
    add_justified(TAB_D, done, TAB->pad, TAB->size, TAB->info,
                  (trailing || ((unsigned short int)i < TAB->nocols-1) || (TAB->next)));
    TAB_D += done; /* inc'ed next line ... */
    if (!(*TAB_D) || !(*(++TAB_D))) TAB_D = 0;
  }
  if (trailing && i < TAB->nocols) {
    done = TAB->size;
    for (; i < TAB->nocols; i++) {
      /* TAB->size is not negative. */
      ADD_CHARN(' ', done)
    }
  }
  if (!TAB->d.tab[0]) {
    cst *temp;

    temp = TAB->next;
    xfree((char *)TAB->d.tab);
    xfree((char *)TAB);
    TAB = temp;
    return 1;
  }
  return 0;
} /* end of add_table() */

/*
 * THE (s)printf() function.
 * It returns a pointer to it's internal buffer (or a string in the text
 * segment) thus, the string must be copied if it has to survive after
 * this function is called again, or if it's going to be modified (esp.
 * if it risks being free()ed).
 */
char *string_print_formatted(format_str, argc, argv)
  char *format_str;
  int argc;
  struct svalue *argv;
{
  format_info finfo;
  char format_char = 0;
  savechars *saves = 0;        /* chars to restore */
  cst *csts;                /* list of columns/tables to be done */
  /* It is important that the address of csts is taken at least once, so
   * that it will have the correct value at error recovery.
   */
  struct svalue *carg;        /* current arg */
  unsigned int nelemno = 0;        /* next offset into array */
  unsigned int fpos;        /* position in format_str */
  int arg;        /* current arg number */
  unsigned int fs;        /* field size */
  int pres;                /* precision */
  unsigned int err_num;
  char *pad;                /* fs pad string */
  struct svalue clean;  /* the address of clean is taken somewhere below. This
                           is essential, to prevent putting it into a register,
                           which would make longjmp() dangerous. */

  clean.u.string = 0;
  if (0 != (err_num = setjmp(error_jmp))) { /* error handling */
    static char error_prefix[] = "ERROR: (s)printf(): ";
    char *err;
    cst *tcst;

    /* Amylaar: restore also in case of errors.... */
    /* Use the address of saves to be sure it is not put into a register.
     * This must be done in a way that no optimizing compiler will drop.
     */
    if (const1.u.number == 0) {
        xfree( (char *)&saves );
    }
    while (saves) {
      savechars *tmp;
      *(saves->where) = saves->what;
      tmp = saves;
      saves = saves->next;
      xfree((char *)tmp);
    }
    if (clean.u.string)
      xfree(clean.u.string);
    while ( NULL != (tcst = csts) ) {
        csts = tcst->next;
        if (tcst->info & INFO_TABLE && tcst->d.tab)
            xfree((char *)tcst->d.tab);
        xfree((char *)tcst);
    }
    switch(err_num & ERR_ID_NUMBER) {
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
    }
    strcpy(buff, error_prefix);
    sprintf(buff + sizeof(error_prefix) - 1, err,
      EXTRACT_ERR_ARGUMENT(err_num) );
    strcat(buff, "\n");
#ifdef RETURN_ERROR_MESSAGES
    if ((err_num & ERR_ID_NUMBER) != ERR_NOMEM) {
        int line;
        char *file;

        line = get_line_number_if_any(&file);
        fprintf(stderr, "%s:%d: %s", file,
                                 line, buff);
        return buff;
    } else
#endif /* RETURN_ERROR_MESSAGES */
        error(buff);
  }
  arg = -1;
  bpos = 0;
  line_start = 0;
  csts = 0;
  for (fpos=0; 1; fpos++) {
    if ((format_str[fpos] == '\n') || (!format_str[fpos])) {
      int column_stat = 0;

      if (!csts) {
        if (!format_str[fpos]) break;
        ADD_CHAR('\n');
        line_start = bpos;
        continue;
      }
      ADD_CHAR('\n');
      line_start = bpos;
      while (csts) {
        cst **temp;

        temp = &csts;
        while (*temp) {
          p_int i;
          if ((*temp)->info & INFO_COLS) {
            if (*((*temp)->d.col-1) != '\n')
              while (*((*temp)->d.col) == ' ') (*temp)->d.col++;
            i = (*temp)->start - (bpos - line_start);
            ADD_CHARN(' ', i);
            column_stat = add_column(temp, 0);
            if (!column_stat) temp = &((*temp)->next);
          } else {
            i = (*temp)->start - (bpos - line_start);
            if (i > 0)
                ADD_CHARN(' ', i);
            if (!add_table(temp, 0)) temp = &((*temp)->next);
          }
        } /* of while (*temp) */
        if (csts || format_str[fpos] == '\n')
          ADD_CHAR('\n');
        line_start = bpos;
      } /* of while (csts) */
      if (column_stat == 2) ADD_CHAR('\n');
      if (!format_str[fpos]) break;
      continue;
    }
    if (format_str[fpos] == '%') {
      if (format_str[fpos+1] == '%') {
        ADD_CHAR('%');
        fpos++;
        continue;
      }
      GET_NEXT_ARG;
      fs = 0;
      pres = 0;
      pad = " ";
      finfo = 0;
      for (fpos++; !(finfo & INFO_T); fpos++) {
        if (!format_str[fpos]) {
          finfo |= INFO_T_ERROR;
          break;
        }
        if (((format_str[fpos] >= '0') && (format_str[fpos] <= '9'))
            || (format_str[fpos] == '*')) {
          if (pres == -1) { /* then looking for pres */
            if (format_str[fpos] == '*') {
              if (carg->type != T_NUMBER)
                ERROR(ERR_INVALID_STAR);
              pres = carg->u.number;
              GET_NEXT_ARG;
              continue;
            }
            pres = format_str[fpos] - '0';
            for (fpos++;
                 (format_str[fpos]>='0')&&(format_str[fpos]<='9'); fpos++) {
              pres = pres*10 + format_str[fpos] - '0';
            }
          } else { /* then is fs (and maybe pres) */
            if ((format_str[fpos] == '0') && (((format_str[fpos+1] >= '1')
                && (format_str[fpos+1] <= '9')) || (format_str[fpos+1] == '*')))
              pad = "0";
            else {
              if (format_str[fpos] == '*') {
                if (carg->type != T_NUMBER)
                  ERROR(ERR_INVALID_STAR);
                if ((int)(fs = carg->u.number) < 0) {
                    fs = -fs;
                    finfo |= INFO_J_LEFT;
                }
                if (pres == -2) pres = fs; /* colon */
                GET_NEXT_ARG;
                continue;
              }
              fs = format_str[fpos] - '0';
            }
            for (fpos++;
                 (format_str[fpos]>='0')&&(format_str[fpos]<='9'); fpos++) {
              fs = fs*10 + format_str[fpos] - '0';
            }
            if (pres == -2) { /* colon */
              pres = fs;
            }
          }
          fpos--; /* bout to get incremented */
          continue;
        }
        switch (format_str[fpos]) {
          case ' ': finfo |= INFO_PP_SPACE; break;
          case '+': finfo |= INFO_PP_PLUS; break;
          case '-': finfo |= INFO_J_LEFT; break;
          case '|': finfo |= INFO_J_CENTRE; break;
          case '@': finfo |= INFO_ARRAY; break;
          case '=': finfo |= INFO_COLS; break;
          case '#': finfo |= INFO_TABLE; break;
          case '.': pres = -1; break;
          case ':': pres = -2; break;
          case '%': finfo |= INFO_T_NULL; break; /* never reached */
          case 'O': finfo |= INFO_T_LPC; break;
          case 's': finfo |= INFO_T_STRING; break;
          case 'i': finfo |= INFO_T_INT; format_char = 'd'; break;
          case 'd':
          case 'c':
          case 'o':
          case 'x':
          case 'X':
            format_char = format_str[fpos]; finfo |= INFO_T_INT; break;
          case 'f':
          case 'F':
          case 'g':
          case 'G':
          case 'e':
          case 'E':
            format_char = format_str[fpos]; finfo |= INFO_T_FLOAT; break;
          case '\'':
            pad = &(format_str[++fpos]);
            while (1) {
              if (!format_str[fpos]) ERROR(ERR_UNEXPECTED_EOS);
              if (format_str[fpos] == '\\') {
                if (!format_str[fpos+1]) ERROR(ERR_UNEXPECTED_EOS);
                fpos += 2;
                continue;
              }
              if (format_str[fpos] == '\'') {
                if (format_str+fpos == pad) ERROR(ERR_NULL_PS);
                SAVE_CHAR(format_str+fpos);
                format_str[fpos] = '\0';
                break;
              }
              fpos++;
            }
            break;
          default: finfo |= INFO_T_ERROR;
        }
      } /* end of for () */
      if (pres < 0)
        ERROR(ERR_PREC_EXPECTED);
      /*
       * now handle the different arg types...
       */
      if (finfo & INFO_ARRAY) {
        if (carg->type != T_POINTER)
          ERROR(ERR_ARRAY_EXPECTED);
        if (carg->u.vec == &null_vector) {
          fpos--; /* 'bout to get incremented */
          continue;
        }
        carg = (argv+arg)->u.vec->item;
        nelemno = 1; /* next element number */
      }
      while (1) {
        switch(finfo & INFO_T) {
          case INFO_T_ERROR:
            ERROR(ERR_INVALID_FORMAT_STR);
          case INFO_T_NULL:
          {
            /* never reached... */
            fprintf(stderr, "%s: (s)printf: INFO_T_NULL.... found.\n",
                  current_object->name);
            ADD_CHAR('%');
            break;
          }
          case INFO_T_LPC:
          {
            struct sprintf_buffer *b;
#define CLEANSIZ 0x200

            if (clean.u.string)
              xfree(clean.u.string);
            clean.type = T_STRING;
            clean.x.string_type = STRING_MALLOC;
            clean.u.string = (char *)xalloc(CLEANSIZ);
            if (!clean.u.string)
                ERROR(ERR_NOMEM);
            clean.u.string[0] = '\0';
            b = (struct sprintf_buffer *)
                 ( clean.u.string+CLEANSIZ-sizeof(struct sprintf_buffer) );
            b->offset = -CLEANSIZ+(p_int)sizeof(struct sprintf_buffer);
            b->size = CLEANSIZ;
            b->start = &clean.u.string;
            svalue_to_string(carg, b, 0, 0);
            carg = &clean;                        /* be sure not to remove the
                                                   last address reference to
                                                   clean */
            /* fall through */
         }
         case INFO_T_STRING:
         {
          int slen;

          if (carg->type != T_STRING)
            ERROR1(ERR_INCORRECT_ARG, 's');
          slen = strlen(carg->u.string);
          if (finfo & (INFO_COLS | INFO_TABLE) ) {
            cst **temp;

            if (!fs)
              ERROR(ERR_CST_REQUIRES_FS);

            temp = &csts;
            while (*temp) temp = &((*temp)->next);
            if (finfo & INFO_COLS) {
              *temp = (cst *)xalloc(sizeof(cst));
              if (!*temp)
                ERROR(ERR_NOMEM);
              (*temp)->next = 0;
              (*temp)->d.col = carg->u.string;
              (*temp)->pad = pad;
              (*temp)->size = fs;
              (*temp)->pres = (pres) ? pres : fs;
              (*temp)->info = finfo;
              (*temp)->start = bpos - line_start;
              if ((add_column(temp, (((format_str[fpos] != '\n')
                    && (format_str[fpos] != '\0')) || ((finfo & INFO_ARRAY)
                      && (nelemno < VEC_SIZE((argv+arg)->u.vec))))) == 2)
                  && !format_str[fpos])
              {
                ADD_CHAR('\n');
              }
            } else { /* (finfo & INFO_TABLE) */
              unsigned int n, len, max;
              char c, *s, *start;
              p_uint i;

#define TABLE carg->u.string
              (*temp) = (cst *)xalloc(sizeof(cst));
              if (!*temp)
                ERROR(ERR_NOMEM);
              (*temp)->pad = pad;
              (*temp)->info = finfo;
              (*temp)->start = bpos - line_start;
              (*temp)->next = 0;
              max = len = 0;
              n = 1;
              s = TABLE;
              if ( '\0' != (c = *(start = s)) ) for (;;) {
                if (c != '\n') {
                  if ( '\0' != (c = *++s) )
                    continue;
                  else
                    break;
                }
                len = s - start;
                if (len > max) max = len;
                n++;
                if ( '\0' != (c = *(start = ++s)) ) {
                  continue;
                }
                n--;
                break;
              }
              if (pres) {
                (*temp)->size = fs/pres;
              } else {
                len = s - start;
                if (len > max) max = len; /* the null terminated word */
                pres = fs/(max+2); /* at least two separating spaces */
                if (!pres) pres = 1;
                (*temp)->size = fs/pres;
              }
              len = n/pres; /* length of average column */
              if (n < (unsigned int)pres) pres = n;
              if (len*pres < n) len++;
              if (len > 1 && n%pres) pres -= (pres - n%pres)/len;
              (*temp)->d.tab = (char **)xalloc(pres*sizeof(char *));
              if (!(*temp)->d.tab)
                ERROR(ERR_NOMEM);
              (*temp)->nocols = pres; /* heavy sigh */
              (*temp)->d.tab[0] = TABLE;
              if (pres == 1) goto add_table_now;
              i = 1; /* the next column number */
              n = 0; /* the current "word" number in this column */
              for (fs = 0; TABLE[fs]; fs++) { /* throwing away fs... */
                if (TABLE[fs] == '\n') {
                  if (++n >= len) {
                    SAVE_CHAR(((TABLE)+fs));
                    TABLE[fs] = '\0';
                    (*temp)->d.tab[i++] = TABLE+fs+1;
                    if (i >= (unsigned int)pres) goto add_table_now;
                    n = 0;
                  }
                }
              }
add_table_now:
              add_table(temp, (((format_str[fpos] != '\n')
                && (format_str[fpos] != '\0')) || ((finfo & INFO_ARRAY)
                  && (nelemno < VEC_SIZE((argv+arg)->u.vec)))));
            }
          } else { /* not column or table */
            if (pres && pres<slen) {
              slen = pres;
            }
            if (fs && fs > (unsigned int)slen) {
              add_justified(carg->u.string, slen, pad, fs, finfo,
          (((format_str[fpos] != '\n') && (format_str[fpos] != '\0'))
          || ((finfo & INFO_ARRAY) && (nelemno < VEC_SIZE((argv+arg)->u.vec))))
          || carg->u.string[slen-1] != '\n');
            } else {
              ADD_STRN(carg->u.string, slen)
            }
          }
          break;
         }
         case INFO_T_INT:
         case INFO_T_FLOAT:
         {
            char cheat[6];
            char temp[1024];
              /* The buffer must be big enough to hold the biggest float
               * in non-exponential representation. 1 KByte is hopefully
               * far on the safe side.
               * TODO: Allocate it dynamically?
               */
            double value;   /* The value to print */
            int    numdig;  /* (Estimated) number of digits before the '.' */
            int tmpl;
            p_uint i = 1;

            *cheat = '%';
            switch (finfo & INFO_PP) {
              case INFO_PP_SPACE: cheat[i++] = ' '; break;
              case INFO_PP_PLUS:  cheat[i++] = '+'; break;
            }
            if ((finfo & INFO_T) == INFO_T_FLOAT) {
              if (carg->type != T_FLOAT) { /* sigh... */
                ERROR1(ERR_INCORRECT_ARG, format_char);
              }
              cheat[i++] = '.';
              cheat[i++] = '*';
              cheat[i++] = format_char;
              cheat[i] = '\0';
              value = READ_DOUBLE(carg);
              if ('e' == format_char || 'E' == format_char || fabs(value) < 1.0)
                  numdig = 1;
              else
                  numdig = (int) ceil(log10(fabs(value)));
              if (value < 0.0)
                  numdig++;
              if ((size_t)pres > (sizeof(temp) - 12 - numdig)) {
                pres = sizeof(temp) - 12 - numdig;
              }
              sprintf(temp, cheat, pres, READ_DOUBLE(carg));
              tmpl = strlen(temp);
              if ((size_t)tmpl >= sizeof(temp))
                fatal("Local buffer overflow in sprintf() for float.\n");
            } else {
              if (carg->type != T_NUMBER) { /* sigh... */
                ERROR1(ERR_INCORRECT_ARG, format_char);
              }
              cheat[i++] = format_char;
              cheat[i] = '\0';
              sprintf(temp, cheat, carg->u.number);
              tmpl = strlen(temp);
              if ((size_t)tmpl >= sizeof(temp))
                fatal("Local buffer overflow in sprintf() for int.\n");
              if (pres && tmpl > pres) tmpl = pres; /* well.... */
            }
            if ((unsigned int)tmpl < fs)
              add_justified(temp, tmpl, pad, fs, finfo,
                (((format_str[fpos] != '\n') && (format_str[fpos] != '\0')) ||
                 ((finfo & INFO_ARRAY) &&
                  (nelemno < VEC_SIZE((argv+arg)->u.vec))))
              );
            else
              ADD_STRN(temp, tmpl)
            break;
          }
          default:        /* type not found */
            ERROR(ERR_UNDEFINED_TYPE);
        }
        if (!(finfo & INFO_ARRAY)) break;
        if (nelemno >= VEC_SIZE((argv+arg)->u.vec)) break;
        carg = (argv+arg)->u.vec->item+nelemno++;
      } /* end of while (1) */
      fpos--; /* bout to get incremented */
      continue;
    }
    ADD_CHAR(format_str[fpos]);
  } /* end of for (fpos=0; 1; fpos++) */
  ADD_CHAR('\0');
  while (saves) {
    savechars *tmp;
    *(saves->where) = saves->what;
    tmp = saves;
    saves = saves->next;
    xfree((char *)tmp);
  }
  if (clean.u.string)
    xfree(clean.u.string);
  return buff;
} /* end of string_print_formatted() */

#endif /* defined(F_SPRINTF) || defined(F_PRINTF) */
