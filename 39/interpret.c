#include "driver.h"

#include "my-alloca.h"
#include "my-rusage.h"
#ifndef __STDC__
#include <varargs.h>
#else
#include <stdarg.h>
#endif
#include <stdio.h>
#include <setjmp.h>
#include <ctype.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef MARK
#include <prof.h>
#endif

#ifdef HAVE_CRYPT_H
     /* solaris defines crypt() here */
#    include <crypt.h>
#endif
#if !defined(HAVE_CRYPT) && defined(HAVE__CRYPT)
#    define crypt(pass, salt) _crypt(pass, salt)
#endif

#ifdef MARK
#define CASE(x) case (x)-F_OFFSET: MARK(x);
#else
#define CASE(x) case (x)-F_OFFSET:
#endif

#define INTERPRET
#define USES_SVALUE_STRLEN
#include "interpret.h"
#include "array.h"
#include "backend.h"
#include "call_out.h"
#include "closure.h"
#include "comm.h"
#include "ed.h"
#include "exec.h"
#include "gcollect.h"
#include "instrs.h"
#include "lex.h"
#include "main.h"
#include "mapping.h"
#include "object.h"
#include "otable.h"
#include "parse.h"
#include "prolang.h"
#include "random.h"
#include "sent.h"
#include "simulate.h"
#include "simul_efun.h"
#include "stralloc.h"
#include "smalloc.h"
#include "sprintf.h"
#include "swap.h"
#include "switch.h"
#include "wiz_list.h"



static void free_protector_svalue PROT((struct svalue *));
static void do_trace PROT((char *, char *, char *));
static int apply_low PROT((char *, struct object *, int, short /* TODO: BOOL */));
static int inter_sscanf PROT((int, struct svalue *));
static struct vector *inter_add_array PROT((struct vector *, struct vector**));
static int strpref PROT((char *, char *));
static void transfer_pointer_range PROT((struct svalue *source));
static void transfer_protected_pointer_range
  PROT((struct protected_range_lvalue *dest, struct svalue *source));
static void assign_string_range PROT((struct svalue *source, int do_free));
static void assign_protected_string_range
  PROT((struct protected_range_lvalue *dest,struct svalue *source, int do_free));
static void call_simul_efun PROT((int code, struct object *ob, int num_arg));

struct program *current_prog;

/* A function call can cause an eval_cost overflow linear to the number of
 * shadows. Well, adding more than a million is likely to cause memory
 * trouble anyway.
 */
#define MIN_TRACE_COST (0x100000 + \
        CATCH_RESERVED_COST + 2 * MASTER_RESERVED_COST + 2 * MAX_TRACE)
#define MAX_TRACE_COST ((int32)(0x80000000 - MIN_TRACE_COST))
int tracedepth;
int trace_level;
static int traceing_recursion = -1;
static int trace_test PROT((int));
#define TRACE_CALL 1
#define TRACE_CALL_OTHER 2
#define TRACE_RETURN 4
#define TRACE_ARGS 8
#define TRACE_EXEC 16
#define TRACE_HEART_BEAT 32
#define TRACE_APPLY 64
#define TRACE_OBJNAME 128
#define TRACETST(b) (O_GET_INTERACTIVE(command_giver)->trace_level & (b))
#define TRACEP(b) (trace_level & (b) && trace_test(b))
#define SET_TRACE_EXEC (trace_level & TRACE_EXEC && eval_cost < 0 && \
        (eval_cost += MAX_TRACE_COST, assigned_eval_cost += MAX_TRACE_COST))
#define TRACE_EXEC_P \
 ( TRACEP(TRACE_EXEC) || \
   (eval_cost -= MAX_TRACE_COST, assigned_eval_cost -= MAX_TRACE_COST, \
   MY_FALSE) )

#define TRACEHB \
  ( current_heart_beat == 0 || \
    (O_GET_INTERACTIVE(command_giver)->trace_level & TRACE_HEART_BEAT))

/*
 * Inheritance:
 * An object X can inherit from another object Y. This is done with
 * the statement 'inherit "file";'
 * The inherit statement will clone a copy of that file, call reset
 * in it, and set a pointer to Y from X.
 * Y has to be removed from the linked list of all objects.
 * All variables declared by Y will be copied to X, so that X has access
 * to them.
 *
 * If Y isn't loaded when it is needed, X will be discarded, and Y will be
 * loaded separetly. X will then be reloaded again.
 */

/*
 * These are the registers used at runtime.
 * The control stack saves registers to be restored when a function
 * will return. That means that control_stack[0] will have almost no
 * interesting values, as it will terminate execution.
 */
#ifndef SMALLOC_LPC_TRACE
static
#endif
char *inter_pc;                        /* Program pointer. */
static struct svalue *inter_fp;        /* Pointer to first argument. */
struct svalue *inter_sp;        /* Points to value of last push. */
static char **break_sp;                /* Points to address to branch to
                                 * at next F_BREAK                        */
int function_index_offset; /* Needed for inheritance */
static int variable_index_offset; /* Needed for inheritance */
struct svalue *current_variables;
static char **current_strings;

/* reserve space to store values + overflow space */
static struct svalue start_of_stack[EVALUATOR_STACK_SIZE<<1];

struct svalue catch_value = { T_INVALID } ;
                                /* Used to throw an error to a catch */

static struct control_stack control_stack[MAX_TRACE];
static struct control_stack *csp;        /* Points to last element pushed */

#define ERRORF(s) {inter_pc = pc; inter_sp = sp; error s ;}
#define ERROR(s) ERRORF((s))
#define STACK_OVERFLOW(sp, fp, pc) stack_overflow(sp, fp, pc)

/* defines and a helpfun for F_TERMINAL_COLOUR */

#define CALLOCATE(num, type) ((type *)xalloc(sizeof(type[1]) * (num) ))
#define RESIZE(ptr, num, type) ((type *)rexalloc((void *)ptr, sizeof(type) * (num)))

#define NSTRSEGS 32
#define TC_FIRST_CHAR '%'
#define TC_SECOND_CHAR '^'

static int at_end(int i, int imax, int z, int *lens) {
    if (z + 1 != lens[i])
        return 0;
    for (i++; i < imax; i++) {
        if (lens[i] > 0)
            return 0;
    }
    return 1;
}

/* end of F_TERMINAL_COLOUR addons */

#define pop_n_elems(n) (sp = _pop_n_elems((n), sp))
INLINE static struct svalue *_pop_n_elems PROT((int, struct svalue *));
static void stack_overflow(sp, fp, pc)
    struct svalue *sp;
    struct svalue *fp;
    char *pc;
{
    pop_n_elems(sp-fp);
    ERROR("stack overflow\n")
}

#define ASSIGN_EVAL_COST \
    if (current_object->user)\
        current_object->user->cost += eval_cost - assigned_eval_cost;\
    assigned_eval_cost = eval_cost;

void assign_eval_cost() { ASSIGN_EVAL_COST }

/*
 * Information about assignments of values:
 *
 * There are three types of l-values: Local variables, global variables
 * and vector elements.
 *
 * The local variables are allocated on the stack together with the arguments.
 * the register 'frame_pointer' points to the first argument.
 *
 * The global variables must keep their values between executions, and
 * have space allocated at the creation of the object.
 *
 * Elements in vectors are similar to global variables. There is a reference
 * count to the whole vector, that states when to deallocate the vector.
 * The elements consists of 'struct svalue's, and will thus have to be freed
 * immediately when over written.
 */

/*
 * Push an object pointer on the stack. Note that the reference count is
 * incremented.
 * A destructed object must never be pushed onto the stack, use
 * push_valid_ob() to take care of that. push_object() is the faster variant,
 * to be used in contexes where the object is guaranteed to be valid.
 */
INLINE
static struct svalue *_push_object(ob, sp)
    struct object *ob;
    struct svalue *sp;
{
    sp++;
    sp->type = T_OBJECT;
    sp->u.ob = ob;
    add_ref(ob, "push_object");
    return sp;
}

void push_object(ob)
    struct object *ob;
{
    inter_sp++;
    inter_sp->type = T_OBJECT;
    inter_sp->u.ob = ob;
    add_ref(ob, "push_object");
}
#define push_object(ob) (sp = _push_object((ob), sp))

INLINE
static struct svalue *_push_valid_ob(ob, sp)
    struct object *ob;
    struct svalue *sp;
{
    sp++;
    if (ob->flags & O_DESTRUCTED)
    {
        sp->type = T_NUMBER;
        sp->u.number = 0;
    } else
    {
        sp->type = T_OBJECT;
        sp->u.ob = ob;
        add_ref(ob, "push_valid_ob");
    }
    return sp;
}

void push_valid_ob(ob)
    struct object *ob;
{
    inter_sp++;
    if (ob->flags & O_DESTRUCTED)
    {
        inter_sp->type = T_NUMBER;
        inter_sp->u.number = 0;
    } else
    {
        inter_sp->type = T_OBJECT;
        inter_sp->u.ob = ob;
        add_ref(ob, "push_valid_ob");
    }
}
#define push_valid_ob(ob) (sp = _push_valid_ob((ob), sp))

static INLINE
void _put_object(ob, sp)
    struct object *ob;
    struct svalue *sp;
{
    sp->type = T_OBJECT;
    sp->u.ob = ob;
    add_ref(ob, "push_object");
}
#define put_object(ob) _put_object(ob, sp)

/*
 * Push a number on the value stack.
 */
INLINE
static struct svalue *_push_number(n, sp)
    p_int n;
    struct svalue *sp;
{
    sp++;
    sp->type = T_NUMBER;
    sp->u.number = n;
    return sp;
}

#define put_number(n) (sp->type = T_NUMBER, sp->u.number = n)

void push_number(n)
    p_int n;
{
    inter_sp++;
    inter_sp->type = T_NUMBER;
    inter_sp->u.number = n;
}
#define push_number(n) (sp = _push_number(n, sp))

/*
 * Push a string on the stack that is already shared.
 */
INLINE
struct svalue *_push_shared_string(p, sp)
    char *p;
    struct svalue *sp;
{
    sp++;
    sp->type = T_STRING;
    sp->x.string_type = STRING_SHARED;
    increment_string_ref( sp->u.string = p );
    return sp;
}
void push_shared_string(p) char *p; {
    inter_sp = _push_shared_string(p, inter_sp);
}
#define push_shared_string(p) (sp = _push_shared_string((p), sp))

void push_referenced_shared_string(p)
    char *p;
{
    struct svalue *sp = inter_sp;

    sp++;
    sp->type = T_STRING;
    sp->x.string_type = STRING_SHARED;
    sp->u.string = p;
    inter_sp = sp;
}

/*
 * Get address to a valid global variable.
 */
#ifdef DEBUG
static INLINE struct svalue *find_value(num)
    int num;
{
    if (num >= current_object->prog->num_variables) {
        fatal("Illegal variable access %d(%d). See trace above.\n",
            num, current_object->prog->num_variables);
    }
    return &current_variables[num];
}
#else
#define find_value(num) (&current_variables[(num)])
#endif

static INLINE struct svalue *find_virtual_value(num)
    int num;
{
    struct inherit *inheritp;
    struct program *progp;
    char *progpp; /* points to a struct program *, but some compilers... */

    inheritp = current_prog->inherit;
    while
      (inheritp->variable_index_offset + inheritp->prog->num_variables <= num ||
       inheritp->variable_index_offset > num)
    {
        inheritp++;
    }
    num -= inheritp->variable_index_offset;
    progp = inheritp->prog;
    progpp = (char *)&current_object->prog->inherit->prog;
    while (*(struct program **)progpp != progp)
        progpp += sizeof(struct inherit);
    num += (
          (struct inherit *)(
            ((PTRTYPE)(progpp))-
            ((PTRTYPE)(&((struct inherit *)0)->prog)-(PTRTYPE) 0)
          )
        )->variable_index_offset;
    return &current_object->variables[num];
}

LOCAL_INLINE void free_string_svalue(v)
    struct svalue *v;
{
    switch(v->x.string_type) {
    case STRING_MALLOC:
        xfree(v->u.string);
        break;
    case STRING_SHARED:
        free_string(v->u.string);
        break;
    }
}

void free_object_svalue(v)
    struct svalue *v;
{
    struct object *ob = v->u.ob;
    free_object(ob, "free_object_svalue");
}

void zero_object_svalue(v)
    struct svalue *v;
{
    struct object *ob = v->u.ob;
    free_object(ob, "zero_object_svalue");
    v->type = T_NUMBER;
    v->u.number = 0;
}


struct protected_lvalue {
    struct svalue v; /* this must come first, to simulate superclass */
    struct svalue protector; /* for v.u.lvalue */
};
struct protected_char_lvalue {
    struct svalue v; /* this must come first, to simulate superclass */
                     /* *v.u.string is the character to access       */
    struct svalue protector; /* for the following lvalue... */
    struct svalue *lvalue;
    char *start;
/* this lvalue is invalid when start != lvalue->u.string */
};
struct protected_range_lvalue {
    struct svalue v; /* this must come first, to simulate superclass */
    struct svalue protector; /* for the following lvalue... */
    struct svalue *lvalue;
    int index1, index2, size;
};

/*
 * Free the data that an svalue is pointing to. Not the svalue
 * itself.
 */
void free_svalue(v)
    struct svalue *v;
{
    switch(v->type) {
      case T_STRING:
        switch(v->x.string_type) {
          case STRING_MALLOC:
            xfree(v->u.string);
            break;
          case STRING_SHARED:
            free_string(v->u.string);
            break;
        }
        break;
      case T_OBJECT:
      {
        struct object *ob = v->u.ob;
        free_object(ob, "free_svalue");
        break;
      }
      case T_QUOTED_ARRAY:
      case T_POINTER:
        free_vector(v->u.vec);
        break;
      case T_SYMBOL:
        free_string(v->u.string);
        break;
      case T_CLOSURE:
        free_closure(v);
        break;
      case T_LVALUE:
#ifdef DEBUG
if (d_flag > 2) {
fprintf(stderr, "free_svalue called for T_LVALUE\n");
dump_trace(1);
}
#endif
        switch (v->u.lvalue->type) {
            case T_PROTECTED_LVALUE:
            {
                struct protected_lvalue *p;

                p = v->u.protected_lvalue;
                free_protector_svalue(&p->protector);
                xfree((char*)p);
                break;
            }
            case T_PROTECTED_CHAR_LVALUE:
            {
                struct protected_char_lvalue *p;

                p = v->u.protected_char_lvalue;
                if (p->lvalue->type == T_STRING &&
                    p->lvalue->u.string == p->start)
                {
                    p->lvalue->x.string_type = STRING_MALLOC;
                } else {
                    xfree(p->start);
                }
                free_protector_svalue(&p->protector);
                xfree((char*)p);
                break;
            }
            case T_PROTECTED_STRING_RANGE_LVALUE:
            {
                struct protected_range_lvalue *p;

                p = v->u.protected_range_lvalue;
                if (p->lvalue->type == T_STRING &&
                    p->lvalue->u.string == p->v.u.string)
                {
                    p->lvalue->x.string_type = STRING_MALLOC;
                } else {
                    xfree(p->v.u.string);
                }
                free_protector_svalue(&p->protector);
                xfree((char*)p);
                break;
            }
            case T_PROTECTED_POINTER_RANGE_LVALUE:
            {
                struct protected_range_lvalue *p;

                p = v->u.protected_range_lvalue;
                free_vector(p->v.u.vec);
                free_protector_svalue(&p->protector);
                xfree((char*)p);
                break;
            }
            case T_ERROR_HANDLER:
            /* this is nested here only to save overhead in the general case.
             * It has actually little connection to lvalues.
             */
            {
                struct svalue *p;

                p = v->u.lvalue;
                (*p->u.error_handler)(p);
                break;
            }
        }
        break;
#ifdef MAPPINGS
      case T_MAPPING:
        free_mapping(v->u.map);
        break;
#endif
    }
#if 0
    *v = const0; /* marion - clear this value all away */
#endif
}

static void free_protector_svalue(v)
    struct svalue *v;
{
    switch(v->type) {
      case T_POINTER:
        free_vector(v->u.vec);
        break;
#ifdef MAPPINGS
      case T_MAPPING:
        free_mapping(v->u.map);
        break;
      case T_PROTECTOR_MAPPING:
        free_protector_mapping(v->u.map);
        break;
#endif
    }
}

/*
 * Prepend a slash in front of a string.
 */
static char *add_slash(str)
    char *str;
{
    char *tmp;

    tmp = xalloc(strlen(str)+2);
    if (tmp) {
        *tmp = '/';
        strcpy(tmp+1,str);
    }
    return tmp;
}

/*
 * Assign to a svalue.
 * This is done either when element in vector, or when to an identifier
 * (as all identifiers are kept in a vector pointed to by the object).
 */

static void malloced_string_copy(svp, str)
    struct svalue *svp;
    char *str;
{
    char *p;

    p = xalloc(malloced_strlen(str));
    if (!p) {
        svp->type = T_NUMBER;
        error("Out of memory\n");
    }
    (void)strcpy(p, str);
    svp->u.string = p;
}

static struct {
    struct svalue v;
    int index1, index2, size;
} special_lvalue;

LOCAL_INLINE void assign_svalue_no_free(to, from)
    struct svalue *to;
    struct svalue *from;
{
#ifdef DEBUG
    if (from == 0)
        fatal("Null pointer to assign_svalue().\n");
#endif
    *to = *from;
    switch(from->type) {
      case T_STRING:
        switch(from->x.string_type) {
          case STRING_MALLOC:        /* No idea to make the string shared */
            malloced_string_copy(to, from->u.string);
            break;
          case STRING_VOLATILE:        /* Good idea to make it shared */
            to->x.string_type = STRING_SHARED;
            if ( !(to->u.string = make_shared_string(from->u.string)) ) {
                to->type = T_NUMBER;
                error("Out of memory\n");
            }
            break;
          case STRING_SHARED:        /* It already is shared */
            increment_string_ref(from->u.string);
            break;
#ifdef DEBUG
          default:
            fatal("Bad string type %d\n", from->x.string_type);
#endif
        }
        break;
      case T_OBJECT:
        add_ref(to->u.ob, "ass to var");
        break;
      case T_QUOTED_ARRAY:
      case T_POINTER:
        to->u.vec->ref++;
        break;
      case T_SYMBOL:
        increment_string_ref(to->u.string);
        break;
      case T_CLOSURE:
        if (CLOSURE_MALLOCED(to->x.closure_type))
            to->u.lambda->ref++;
        else
            add_ref(to->u.ob, "ass to var");
        break;
#ifdef MAPPINGS
      case T_MAPPING:
        to->u.map->ref++;
        break;
#endif
    }
}

static INLINE void assign_lrvalue_no_free(to, from)
    struct svalue *to;
    struct svalue *from;
{
#ifdef DEBUG
    if (from == 0)
        fatal("Null pointer to assign_lrvalue_no_free().\n");
#endif
    *to = *from;
    switch(from->type) {
      case T_STRING:
        if (to->x.string_type != STRING_SHARED) {
            to->x.string_type = STRING_SHARED;
            to->u.string = make_shared_string(from->u.string);
            if (from->x.string_type == STRING_MALLOC) {
                xfree(from->u.string);
            }
            *from = *to;
        }
        increment_string_ref(from->u.string);
        break;
      case T_OBJECT:
        add_ref(to->u.ob, "ass to var");
        break;
      case T_QUOTED_ARRAY:
      case T_POINTER:
        to->u.vec->ref++;
        break;
      case T_SYMBOL:
        increment_string_ref(to->u.string);
        break;
      case T_CLOSURE:
        if (CLOSURE_MALLOCED(to->x.closure_type))
            to->u.lambda->ref++;
        else
            add_ref(to->u.ob, "ass to var");
        break;
#ifdef MAPPINGS
      case T_MAPPING:
        to->u.map->ref++;
        break;
#endif
      case T_LVALUE:
        to->u.lvalue = from;
        break;
    }
}

void assign_svalue(dest, v)
    struct svalue *dest;
    struct svalue *v;
{
    for (;;) {
        /* Deallocate the previous value. Structured types will necessiate to
         * do the assignment first, then deallocate, lest recursive structures
         * could cause crashs.
         */
        switch(dest->type) {
          case T_LVALUE:
          case T_PROTECTED_LVALUE:
            dest = dest->u.lvalue;
            continue;
          case T_STRING:
            switch(dest->x.string_type) {
              case STRING_MALLOC:
                xfree(dest->u.string);
                break;
              case STRING_SHARED:
                free_string(dest->u.string);
                break;
            }
            break;
          case T_OBJECT:
          {
            struct object *ob = dest->u.ob;
            free_object(ob, "assign_svalue");
            break;
          }
          case T_QUOTED_ARRAY:
          case T_POINTER:
          {
            struct vector *vec = dest->u.vec;
            assign_svalue_no_free(dest, v); /* leaks vec if out fo memory */
            free_vector(vec);
            return;
          }
          case T_SYMBOL:
            free_string(dest->u.string);
            break;
          case T_CLOSURE:
            free_closure(dest);
            break;
          case T_CHAR_LVALUE:
            if (v->type == T_NUMBER)
                *dest->u.string = v->u.number;
            return;
          case T_PROTECTED_CHAR_LVALUE:
          {
            struct protected_char_lvalue *p;

            p = (struct protected_char_lvalue *)dest;
            if (p->lvalue->type == T_STRING &&
                p->lvalue->u.string == p->start)
            {
                if (v->type == T_NUMBER)
                    *p->v.u.string = v->u.number;
            }
            return;
          }
          case T_POINTER_RANGE_LVALUE:
            if (v->type == T_POINTER) {
                v->u.vec->ref++;
                transfer_pointer_range(v);
            }
            return;
          case T_PROTECTED_POINTER_RANGE_LVALUE:
            if (v->type == T_POINTER) {
                v->u.vec->ref++;
                transfer_protected_pointer_range(
                  (struct protected_range_lvalue *)dest, v
                );
            }
            return;
          case T_STRING_RANGE_LVALUE:
            assign_string_range(v, 0);
            return;
          case T_PROTECTED_STRING_RANGE_LVALUE:
            assign_protected_string_range(
                  (struct protected_range_lvalue *)dest, v, 0
            );
            return;
#ifdef MAPPINGS
          case T_MAPPING:
          {
            struct mapping *map = dest->u.map;
            assign_svalue_no_free(dest, v); /* leaks map if out of memory */
            free_mapping(map);
            return;
          }
#endif
        }
        break;
    } /* end for */
    assign_svalue_no_free(dest, v);
    return;
}

LOCAL_INLINE void transfer_svalue_no_free(dest, v)
    struct svalue *dest;
    struct svalue *v;
{
    if (v->type == T_STRING && v->x.string_type == STRING_VOLATILE) {
        dest->type = T_STRING;
        dest->x.string_type = STRING_SHARED;
        if ( !(dest->u.string = make_shared_string(v->u.string)) ) {
            dest->type = T_NUMBER;
            error("Out of memory\n");
        }
    } else {
        *dest = *v;
    }
}

INLINE static void transfer_svalue_no_free_spc(dest, v, sp, pc)
    struct svalue *dest;
    struct svalue *v;
    struct svalue *sp; char *pc;
{
    if (v->type == T_STRING && v->x.string_type == STRING_VOLATILE) {
        dest->type = T_STRING;
        dest->x.string_type = STRING_SHARED;
        if ( !(dest->u.string = make_shared_string(v->u.string)) ) {
            dest->type = T_NUMBER;
            inter_sp = sp;
            inter_pc = pc;
            error("Out of memory\n");
        }
    } else {
        *dest = *v;
    }
}

void transfer_svalue(dest, v)
    struct svalue *dest;
    struct svalue *v;
{

    for(;;) {
        /* First deallocate the previous value. */
        switch(dest->type) {
          case T_LVALUE:
          case T_PROTECTED_LVALUE:
            dest = dest->u.lvalue;
            continue;
          case T_STRING:
            switch(dest->x.string_type) {
              case STRING_MALLOC:
                xfree(dest->u.string);
                break;
              case STRING_SHARED:
                free_string(dest->u.string);
                break;
            }
            break;
          case T_OBJECT:
          {
            struct object *ob = dest->u.ob;
            free_object(ob, "transfer_svalue");
            break;
          }
          case T_QUOTED_ARRAY:
          case T_POINTER:
            free_vector(dest->u.vec);
            break;
          case T_SYMBOL:
            free_string(dest->u.string);
            break;
          case T_CLOSURE:
            free_closure(dest);
            break;
          case T_CHAR_LVALUE:
            if (v->type == T_NUMBER) {
                *dest->u.string = v->u.number;
            } else
                free_svalue(v);
            return;
          case T_PROTECTED_CHAR_LVALUE:
          {
            struct protected_char_lvalue *p;

            p = (struct protected_char_lvalue *)dest;
            if (p->lvalue->type == T_STRING &&
                p->lvalue->u.string == p->start)
            {
                if (v->type == T_NUMBER) {
                    *p->v.u.string = v->u.number;
                    return;
                }
            }
            free_svalue(v);
            return;
          }
          case T_POINTER_RANGE_LVALUE:
            transfer_pointer_range(v);
            return;
          case T_PROTECTED_POINTER_RANGE_LVALUE:
            transfer_protected_pointer_range(
              (struct protected_range_lvalue *)dest, v
            );
            return;
          case T_STRING_RANGE_LVALUE:
            assign_string_range(v, 1);
            return;
          case T_PROTECTED_STRING_RANGE_LVALUE:
            assign_protected_string_range(
              (struct protected_range_lvalue *)dest, v, 1
            );
            return;
#ifdef MAPPINGS
          case T_MAPPING:
            free_mapping(dest->u.map);
            break;
#endif
        } /* end switch */
        break;
    } /* end for */
    if (v->type == T_STRING && v->x.string_type == STRING_VOLATILE) {
        dest->type = T_STRING;
        dest->x.string_type = STRING_SHARED;
        dest->u.string = make_shared_string(v->u.string);
    } else {
        *dest = *v;
    }
}

static void transfer_pointer_range(source)
    struct svalue *source;
{
    if (source->type == T_POINTER) {
        struct vector *sv, *dv, *rv;
        int dsize, ssize, index1, index2;
        int i;

        dsize = special_lvalue.size;
        index1 = special_lvalue.index1;
        index2 = special_lvalue.index2;
        dv = special_lvalue.v.u.lvalue->u.vec;
        sv = source->u.vec;
        ssize = VEC_SIZE(sv);
        if (ssize + index1 - index2 == 0) {
            struct svalue *s, *d;

            s = sv->item;
            d = dv->item + index1;
            dv->ref++; /* This way, it won't be freed while we are
                        * using it */
            if (!--sv->ref) {
                sv->ref++;
                for (i = ssize; --i >= 0; ) {
                    transfer_svalue(d++, s++);
                }
                free_empty_vector(sv);
            } else {
                for (i = ssize; --i >= 0; ) {
                    assign_svalue(d++, s++);
                }
            }
            free_vector(dv);
        } else {
            struct svalue *s, *d;

            rv = allocate_array(dsize + ssize + index1 - index2);
            special_lvalue.v.u.lvalue->u.vec = rv;
            s = dv->item;
            d = rv->item;
            for (i = index1; --i >= 0; ) {
                assign_svalue_no_free(d++, s++);
            }
            s = sv->item;
            for (i = ssize; --i >= 0; ) {
                assign_svalue_no_free(d++, s++);
            }
            free_vector(sv);
            s = dv->item + index2;
            for (i = dsize - index2; --i >= 0; ) {
                assign_svalue_no_free(d++, s++);
            }
            free_vector(dv); /* this can make the lvalue invalid to use */
        }
    } else
        free_svalue(source);
    return;
}

static void transfer_protected_pointer_range(dest, source)
    struct protected_range_lvalue *dest;
    struct svalue *source;
{
    if (source->type == T_POINTER && dest->v.u.vec == dest->lvalue->u.vec) {
        struct vector *sv, *dv, *rv;
        int dsize, ssize, index1, index2;
        int i;

        dsize = dest->size;
        index1 = dest->index1;
        index2 = dest->index2;
        dv = dest->v.u.vec;
        sv = source->u.vec;
        ssize = VEC_SIZE(sv);
        if (ssize + index1 - index2 == 0) {
            struct svalue *s, *d;

            s = sv->item;
            d = dv->item + index1;
            if (!--sv->ref) {
                sv->ref++;
                for (i = ssize; --i >= 0; ) {
                    transfer_svalue(d++, s++);
                }
                free_empty_vector(sv);
            } else {
                for (i = ssize; --i >= 0; ) {
                    assign_svalue(d++, s++);
                }
            }
        } else {
            struct svalue *s, *d;

            rv = allocate_array(dsize + ssize + index1 - index2);
            dest->lvalue->u.vec = rv;
            s = dv->item;
            d = rv->item;
            for (i = index1; --i >= 0; ) {
                assign_svalue_no_free(d++, s++);
            }
            s = sv->item;
            for (i = ssize; --i >= 0; ) {
                assign_svalue_no_free(d++, s++);
            }
            free_vector(sv);
            s = dv->item + index2;
            for (i = dsize - index2; --i >= 0; ) {
                assign_svalue_no_free(d++, s++);
            }
            free_vector(dv); /* this can make the lvalue invalid to use */
        }
    } else
        free_svalue(source);
    return;
}

static void assign_string_range(source, do_free)
    struct svalue *source;
    int do_free;
{
    if (source->type == T_STRING) {
        struct svalue *dsvp;
        char *ss, *ds, *rs;
        int dsize, ssize, index1, index2;

        dsize = special_lvalue.size;
        index1 = special_lvalue.index1;
        index2 = special_lvalue.index2;
        dsvp = special_lvalue.v.u.lvalue;
        ds = dsvp->u.string;
        ss = source->u.string;
        ssize = svalue_strlen(source);
        rs = xalloc(dsize + ssize + index1 - index2 + 1);
        if (!rs) {
            /* We don't pop the stack here --> don't free source */
            error("Out of memory\n");
        }
        if (index1) memcpy(rs, ds, index1);
        if (ssize) memcpy(rs + index1, ss, ssize);
        strcpy(rs + index1 + ssize, ds + index2);
        free_string_svalue(dsvp);
        dsvp->x.string_type = STRING_MALLOC;
        dsvp->u.string = rs;
        if (do_free)
            free_string_svalue(source);
    } else {
        if (do_free)
            free_svalue(source);
    }
    return;
}

static void assign_protected_string_range(dest, source, do_free)
    struct protected_range_lvalue *dest;
    struct svalue *source;
    int do_free;
{
    if (source->type == T_STRING) {
        struct svalue *dsvp;
        char *ss, *ds, *rs;
        int dsize, ssize, index1, index2;

        dsize = dest->size;
        index1 = dest->index1;
        index2 = dest->index2;
        dsvp = dest->lvalue;
        ds = dest->v.u.string;
        if (dsvp->u.string != ds) {
            if (do_free) {
                free_svalue(source);
                xfree(dest->v.u.string);
                xfree((char *)dest);
            }
            return;
        }
        ss = source->u.string;
        ssize = svalue_strlen(source);
        rs = xalloc(dsize + ssize + index1 - index2 + 1);
        if (!rs) {
            error("Out of memory\n");
        }
        if (index1) memcpy(rs, ds, index1);
        if (ssize) memcpy(rs + index1, ss, ssize);
        strcpy(rs + (dest->index2 = index1 + ssize), ds + index2);
        xfree(ds);
        dest->v.u.string = dsvp->u.string = rs;
        if (do_free) {
            free_string_svalue(source);
            dest->v.x.string_type = STRING_MALLOC;
            free_protector_svalue(&dest->protector);
            xfree((char *)dest);
        }
    } else {
        if (do_free) {
            free_svalue(source);
            dest->v.x.string_type = STRING_MALLOC;
            free_protector_svalue(&dest->protector);
            xfree((char *)dest);
        }
    }
    return;
}

static int add_number_to_svalue(dest, i)
struct svalue *dest; /* dest->type == T_LVALUE, or should be treated as such */
int i;
{
    do
        dest = dest->u.lvalue;
    while(dest->type == T_LVALUE);
    switch (dest->type) {
      default:
        error("Reference to bad type to ++/--\n");
        return i;
      case T_NUMBER:
        return dest->u.number += i;
      case T_PROTECTED_LVALUE:
        return add_number_to_svalue(dest, i);
      case T_CHAR_LVALUE:
        return (*dest->u.string) += i;
      case T_PROTECTED_CHAR_LVALUE:
      {
        struct protected_char_lvalue *p;

        p = (struct protected_char_lvalue *)dest;
        if (p->lvalue->type == T_STRING &&
            p->lvalue->u.string == p->start)
        {
            i = *p->v.u.string += i;
        }
        return i;
      }
    }
}

void push_svalue(v)
    struct svalue *v;
{
    assign_svalue_no_free(++inter_sp, v);
}

static void push_lrvalue PROT((struct svalue *v)) UNUSED;

static void push_lrvalue(v)
    struct svalue *v;
{
    assign_lrvalue_no_free(++inter_sp, v);
}

void push_svalue_block(num, v)
    int num;
    struct svalue *v;
{
    struct svalue *w;

    for (w = inter_sp; --num >= 0; v++) {
        w++;
        assign_lrvalue_no_free(w, v);
    }
    inter_sp = w;
}

/* 'from' is meant to point to a variable or vector element, so it might
 *  contain a destructed object.
 */
static INLINE void assign_checked_svalue_no_free(to, from, sp, pc)
    struct svalue *to, *from;
    struct svalue *sp; char *pc;
{
    switch (from->type) {
      case T_STRING:
        switch(from->x.string_type) {
          case STRING_MALLOC:        /* No idea to make the string shared */
          {
            char *p;
            char *str;

            p = xalloc(malloced_strlen(str = from->u.string));
            if (!p) {
                to->type = T_NUMBER;
                inter_sp = sp;
                inter_pc = pc;
                error("Out of memory\n");
            }
            (void)strcpy(p, str);
            to->type = T_STRING;
            to->x.string_type = STRING_MALLOC;
            to->u.string = p;
            return;
          }
          case STRING_SHARED:        /* It already is shared */
            increment_string_ref( to->u.string = from->u.string );
            break;
#ifdef DEBUG
          default:
            fatal("Bad string type %d\n", from->x.string_type);
#endif
        }
        to->type = T_STRING;
        to->x.string_type = STRING_SHARED;
        return;
      case T_OBJECT:
      {
        struct object *ob = from->u.ob;
        if ( !(ob->flags & O_DESTRUCTED) ) {
            add_ref(ob, "ass to var");
            break;
        }
        zero_object_svalue(from);
        break;
      }
      case T_QUOTED_ARRAY:
      case T_POINTER:
        from->u.vec->ref++;
        break;
      case T_SYMBOL:
        increment_string_ref(from->u.string);
        break;
      case T_CLOSURE:
        if (CLOSURE_MALLOCED(from->x.closure_type))
            from->u.lambda->ref++;
        else
            add_ref(from->u.ob, "ass to var");
        break;
#ifdef MAPPINGS
      case T_MAPPING:
        from->u.map->ref++;
        break;
#endif
    }
    *to = *from;
}

/* 'from' is meant to point to a local variable.
 * if 'from' is an argument to the current lfun, it might be of type
 * volatile.
 */
static INLINE void assign_local_svalue_no_free(to, from, sp, pc)
    struct svalue *to, *from;
    struct svalue *sp; char *pc;
{
assign_from_lvalue:
    switch (from->type) {
      case T_STRING:
        switch(from->x.string_type) {
          case STRING_MALLOC:        /* No idea to make the string shared */
          {
            char *p;
            char *str;

            p = xalloc(malloced_strlen(str = from->u.string));
            if (!p) {
                to->type = T_NUMBER;
                inter_sp = sp;
                inter_pc = pc;
                error("Out of memory\n");
            }
            (void)strcpy(p, str);
            to->type = T_STRING;
            to->x.string_type = STRING_MALLOC;
            to->u.string = p;
            return;
          }
          case STRING_SHARED:        /* It already is shared */
            increment_string_ref( to->u.string = from->u.string );
            break;
          case STRING_VOLATILE:
            if ( !(to->u.string = make_shared_string(from->u.string)) ) {
                to->type = T_STRING;
                to->x.string_type = STRING_SHARED;
                increment_string_ref(to->u.string = STR_DEFAULT);
                inter_sp = sp;
                inter_pc = pc;
                error("Out of memory\n");
            }
            break;
#ifdef DEBUG
          default:
            fatal("Bad string type %d\n", from->x.string_type);
#endif
        }
        to->type = T_STRING;
        to->x.string_type = STRING_SHARED;
        return;
      case T_OBJECT:
        add_ref(from->u.ob, "assign_local_lvalue_no_free");
        break;
      case T_QUOTED_ARRAY:
      case T_POINTER:
        from->u.vec->ref++;
        break;
      case T_SYMBOL:
        increment_string_ref(from->u.string);
        break;
      case T_CLOSURE:
        if (CLOSURE_MALLOCED(from->x.closure_type))
            from->u.lambda->ref++;
        else
            add_ref(from->u.ob, "ass to var");
        break;
#ifdef MAPPINGS
      case T_MAPPING:
        from->u.map->ref++;
        break;
#endif
      case T_LVALUE:
      case T_PROTECTED_LVALUE:
        from = from->u.lvalue;
        if (from->type == T_OBJECT && from->u.ob->flags & O_DESTRUCTED) {
            zero_object_svalue(from);
            break;
        }
        goto assign_from_lvalue;
      case T_PROTECTED_CHAR_LVALUE:
        to->type = T_NUMBER;
        to->u.number = *from->u.string;
        return;
    }
    *to = *from;
}

/*
 * Pop the top-most value of the stack.
 * Don't do this if it is a value that will be used afterwards, as the
 * data may be sent to xfree(), and destroyed.
 */
LOCAL_INLINE void pop_stack() {
#ifdef DEBUG
    if (inter_sp < start_of_stack)
        fatal("Stack underflow.\n");
#endif
    free_svalue(inter_sp--);
}
#define pop_stack() free_svalue(sp--)

void drop_stack() {
    inter_sp--;
}

static struct svalue indexing_quickfix = { T_NUMBER };

/*
 * Compute the address of an array element.
 */
static INLINE struct svalue *push_indexed_lvalue(sp, pc)
    struct svalue *sp;
    char *pc;
{
    struct svalue *i, *vec, *item;
    int ind;

    i = sp;
    vec = sp - 1;
    if (vec->type == T_POINTER) {
        if (i->type != T_NUMBER || (ind = i->u.number) < 0) {
            ERROR("Illegal index\n")
            return sp;
        }
        sp = vec;
        if ((size_t)ind >= VEC_SIZE(vec->u.vec)) ERROR ("Index out of bounds\n")
        item = &vec->u.vec->item[ind];
        if (vec->u.vec->ref == 1) {
            /* marion says: but this is crude too */
            /* marion blushes. */
            assign_svalue (&indexing_quickfix, item);
            item = &indexing_quickfix;
        }
        free_vector(vec->u.vec);  /* This will make 'vec' invalid to use */
        vec->type = T_LVALUE;
        vec->u.lvalue = item;
        return sp;
    }
#ifdef MAPPINGS
    if (vec->type == T_MAPPING) {
        struct mapping *m;

        m = vec->u.map;
        if (m->num_values) {
            item = get_map_lvalue(m, i, 1);
            free_svalue(sp--);
            if (m->ref == 1) {
                assign_svalue (&indexing_quickfix, item);
                item = &indexing_quickfix;
            }
            free_mapping(m);  /* This will make 'vec' invalid to use */
            vec->type = T_LVALUE;
            vec->u.lvalue = item;
            return sp;
        }
    }
#endif /* MAPPINGS */
    inter_sp = sp;
    inter_pc = pc;
    dump_trace(1);
    error("(lvalue)Indexing on illegal type.\n");
    return sp;
}

static INLINE struct svalue *push_rindexed_lvalue(sp, pc)
    struct svalue *sp;
    char *pc;
{
    struct svalue *i, *vec, *item;
    int ind;

    i = sp;
    vec = sp - 1;
    if (vec->type == T_POINTER) {
        if (i->type != T_NUMBER || (ind = i->u.number) <= 0) {
            ERROR("Illegal index\n")
            return sp;
        }
        sp = vec;
        if ( (ind = VEC_SIZE(vec->u.vec) - ind) < 0) {
            ERROR ("Index out of bounds\n")
            return sp;
        }
        item = &vec->u.vec->item[ind];
        if (vec->u.vec->ref == 1) {
            /* marion says: but this is crude too */
            /* marion blushes. */
            assign_svalue (&indexing_quickfix, item);
            item = &indexing_quickfix;
        }
        free_vector(vec->u.vec);  /* This will make 'vec' invalid to use */
        vec->type = T_LVALUE;
        vec->u.lvalue = item;
        return sp;
    }
    inter_sp = sp;
    inter_pc = pc;
    dump_trace(1);
    error("(lvalue)Indexing on illegal type.\n");
    return NULL;
}

/* NB: the mapping as a whole still needs an ref count increment to
 * protect the lvalue.
 */
#define BUILD_MAP_PROTECTOR(dest, m)        \
{                                        \
    struct hash_mapping *hm;                \
                                        \
    if ( NULL != (hm = (m)->hash) ) {        \
        if (!hm->ref++)                 \
            hm->deleted = 0;                \
        dest.type = T_PROTECTOR_MAPPING;\
    } else {                                \
        dest.type = T_MAPPING;                \
    }                                        \
    dest.u.map = m;                        \
}

static INLINE struct svalue *push_protected_indexed_lvalue(sp, pc)
    struct svalue *sp;
    char *pc;
{
    struct svalue *i, *vec, *item;
    struct protected_lvalue *lvalue;
    int ind;

    i = sp;
    vec = sp - 1;
    if (vec->type == T_POINTER) {
        if (i->type != T_NUMBER || (ind = i->u.number) < 0) {
            ERROR("Illegal index\n")
            return sp;
        }
        sp = vec;
        if ((size_t)ind >= VEC_SIZE(vec->u.vec)) ERROR ("Index out of bounds\n")
        item = &vec->u.vec->item[ind];
        lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
        lvalue->v.type = T_PROTECTED_LVALUE;
        lvalue->v.u.lvalue = item;
        lvalue->protector.type = T_POINTER;
        lvalue->protector.u.vec = vec->u.vec;
        vec->type = T_LVALUE;
        vec->u.lvalue = &lvalue->v;
        return sp;
    }
#ifdef MAPPINGS
    if (vec->type == T_MAPPING) {
        struct mapping *m;

        m = vec->u.map;
        if (m->num_values) {
            item = get_map_lvalue(m, i, 1);
            pop_stack();
            lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
            lvalue->v.type = T_PROTECTED_LVALUE;
            lvalue->v.u.lvalue = item;
            BUILD_MAP_PROTECTOR(lvalue->protector, m)
            vec->type = T_LVALUE;
            vec->u.lvalue = &lvalue->v;
            return sp;
        }
    }
#endif /* MAPPINGS */
    inter_sp = sp;
    inter_pc = pc;
    dump_trace(1);
    error("(lvalue)Indexing on illegal type.\n");
    return NULL;
}

static INLINE struct svalue *push_protected_rindexed_lvalue(sp, pc)
    struct svalue *sp;
    char *pc;
{
    struct svalue *i, *vec, *item;
    struct protected_lvalue *lvalue;
    int ind;

    i = sp;
    vec = sp - 1;
    if (vec->type == T_POINTER) {
        if (i->type != T_NUMBER || (ind = i->u.number) <= 0) {
            ERROR("Illegal index\n")
            return sp;
        }
        sp = vec;
        if ( (ind = VEC_SIZE(vec->u.vec) - ind) < 0) {
            ERROR ("Index out of bounds\n")
            return sp;
        }
        item = &vec->u.vec->item[ind];
        lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
        lvalue->v.type = T_PROTECTED_LVALUE;
        lvalue->v.u.lvalue = item;
        lvalue->protector.type = T_POINTER;
        lvalue->protector.u.vec = vec->u.vec;
        vec->type = T_LVALUE;
        vec->u.lvalue = &lvalue->v;
        return sp;
    }
    inter_sp = sp;
    inter_pc = pc;
    dump_trace(1);
    error("(lvalue)Indexing on illegal type.\n");
    return NULL;
}

#ifdef MAPPINGS
static INLINE struct svalue *push_protected_indexed_map_lvalue(sp, pc)
    struct svalue *sp;
    char *pc;
{
    struct svalue *i, *vec, *item;
    struct protected_lvalue *lvalue;

    i = sp - 1;
    vec = sp - 2;
    if (vec->type == T_MAPPING && sp->type == T_NUMBER) {
        struct mapping *m;

        m = vec->u.map;
        if ((p_uint)sp->u.number >= (p_uint)m->num_values) {
            /* using uints automagically checks for negative indices */
            ERROR("Illegal index\n")
            return sp;
        }
        item = get_map_lvalue(m, i, 1) + sp->u.number;
        sp--;
        pop_stack();
        lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
        lvalue->v.type = T_PROTECTED_LVALUE;
        lvalue->v.u.lvalue = item;
        BUILD_MAP_PROTECTOR(lvalue->protector, m)
        vec->type = T_LVALUE;
        vec->u.lvalue = &lvalue->v;
        return sp;
    }
    inter_sp = sp;
    inter_pc = pc;
    dump_trace(1);
    error("(lvalue)Indexing on illegal type.\n");
    return NULL;
}
#endif /* MAPPINGS */

/*
 * Compute the address of an array element.
 */
static INLINE struct svalue *index_lvalue(sp, pc)
    struct svalue *sp;
    char *pc;
{
    struct svalue *i, *vec, *vecp;
    int ind;
    short type;

    vecp = sp;
    i = sp -1;
    vec = vecp->u.lvalue;
    for(;;) {
        type = vec->type;
        if (type == T_POINTER) {
            struct vector *v = vec->u.vec;

            if (i->type != T_NUMBER || (ind = i->u.number) < 0) {
                ERROR("Illegal index\n")
                return sp;
            }
            sp = i;
            if ((size_t)ind >= VEC_SIZE(v)) {
                ERROR ("Index out of bounds\n")
                return sp;
            }
            i->type = T_LVALUE;
            i->u.lvalue = &v->item[ind];
            return sp;
        }
        if (type == T_STRING) {
            if (i->type != T_NUMBER || (ind = i->u.number) < 0) {
                ERROR("Illegal index\n")
                return sp; /* flow control hint... */
            }
            if (vec->x.string_type != STRING_MALLOC) {
                char *p = string_copy(vec->u.string);
                if (vec->x.string_type == STRING_SHARED)
                    free_string(vec->u.string);
                vec->x.string_type = STRING_MALLOC;
                vec->u.string = p;
            }
            if (ind >= _svalue_strlen(vec) ) {
                ERROR ("Index out of bounds\n")
                return sp;
            }
            sp = i;
            i->type = T_LVALUE;
            i->u.lvalue = &special_lvalue.v;
            special_lvalue.v.type = T_CHAR_LVALUE;
            special_lvalue.v.u.string = &vec->u.string[ind];
            return sp;
        }
#ifdef MAPPINGS
        if (type == T_MAPPING) {
            struct svalue *item;
            struct mapping *m;

            m = vec->u.map;
            if (m->num_values) {
                item = get_map_lvalue(m, i, 1);
                sp = i;
                free_svalue(i);
                i->type = T_LVALUE;
                i->u.lvalue = item;
                return sp;
            }
        }
#endif /* MAPPINGS */
        if (type == T_LVALUE || type == T_PROTECTED_LVALUE) {
            vec = vec->u.lvalue;
            continue;
        }
        inter_sp = sp;
        inter_pc = pc;
        dump_trace(1);
        error("(lvalue)Indexing on illegal type.\n");
    } /* end for */
    return sp;
}

static INLINE struct svalue *rindex_lvalue(sp, pc)
    struct svalue *sp;
    char *pc;
{
    struct svalue *i, *vec, *vecp;
    int ind;
    short type;

    vecp = sp;
    i = sp -1;
    vec = vecp->u.lvalue;
    if (i->type != T_NUMBER || (ind = i->u.number) <= 0) {
        ERROR("Illegal index\n")
        return sp; /* flow control hint... */
    }
    for(;;) {
        type = vec->type;
        if (type == T_POINTER) {
            struct vector *v = vec->u.vec;

            sp = i;
            if ( (ind = VEC_SIZE(v) - ind) < 0) {
                ERROR ("Index out of bounds\n")
                return sp;
            }
            i->type = T_LVALUE;
            i->u.lvalue = &v->item[ind];
            return sp;
        }
        if (type == T_STRING) {
            if (vec->x.string_type != STRING_MALLOC) {
                char *p = string_copy(vec->u.string);
                if (vec->x.string_type == STRING_SHARED)
                    free_string(vec->u.string);
                vec->x.string_type = STRING_MALLOC;
                vec->u.string = p;
            }
            if ( (ind = _svalue_strlen(vec) - ind) < 0) {
                ERROR ("Index out of bounds\n")
                return sp;
            }
            sp = i;
            i->type = T_LVALUE;
            i->u.lvalue = &special_lvalue.v;
            special_lvalue.v.type = T_CHAR_LVALUE;
            special_lvalue.v.u.string = &vec->u.string[ind];
            return sp;
        }
        if (type == T_LVALUE || type == T_PROTECTED_LVALUE) {
            vec = vec->u.lvalue;
            continue;
        }
        inter_sp = sp;
        inter_pc = pc;
        dump_trace(1);
        error("(lvalue)Indexing on illegal type.\n");
        return sp;
    } /* end for */
}

struct svalue last_indexing_protector = { T_NUMBER };

/*
 * Compute the address of an array element.
 */
static INLINE struct svalue *protected_index_lvalue(sp, pc)
    struct svalue *sp;
    char *pc;
{
    struct svalue *i, *vec, *vecp;
    int ind;
    short type;

    vecp = sp;
    i = sp -1;
    vec = vecp->u.lvalue;
    for (;;) {
        type = vec->type;
        if (type == T_POINTER) {
            struct vector *v = vec->u.vec;
            struct protected_lvalue *lvalue;

            if (i->type != T_NUMBER || (ind = i->u.number) < 0) {
                ERROR("Illegal index\n")
                return sp;
            }
            sp = i;
            if ((size_t)ind >= VEC_SIZE(v)) {
                ERROR ("Index out of bounds\n")
                return sp;
            }

            v->ref++;
            lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
            lvalue->v.type = T_PROTECTED_LVALUE;
            lvalue->v.u.lvalue = &v->item[ind];
            lvalue->protector.type = T_POINTER;
            lvalue->protector.u.vec = v;
            i->type = T_LVALUE;
            i->u.lvalue = &lvalue->v;
            return sp;
        }
        if (type == T_STRING) {
            struct protected_char_lvalue *val;

            if (i->type != T_NUMBER || (ind = i->u.number) < 0) {
                ERROR("Illegal index\n")
                return sp; /* flow control hint... */
            }
            if (vec->x.string_type != STRING_MALLOC) {
                char *p = string_copy(vec->u.string);
                if (vec->x.string_type == STRING_SHARED)
                    free_string(vec->u.string);
                vec->u.string = p;
                /* string_type is used by svalue_strlen */
                vec->x.string_type = STRING_MALLOC;
            }
            if (ind > svalue_strlen(vec) ) {
                ERROR ("Index out of bounds\n")
                return sp;
            }
            vec->x.string_type = STRING_VOLATILE;
            val = (struct protected_char_lvalue *)xalloc(sizeof *val);
            i->type = T_LVALUE;
            i->u.protected_char_lvalue = val;
            val->v.type = T_PROTECTED_CHAR_LVALUE;
            val->v.u.string = &vec->u.string[ind];
            val->lvalue = vec;
            val->start = vec->u.string;
            val->protector.type = T_INVALID;
            sp = i;
            return sp;
        }
#ifdef MAPPINGS
        if (type == T_MAPPING) {
            struct svalue *item;
            struct protected_lvalue *lvalue;
            struct mapping *m;

            m = vec->u.map;
            if (m->num_values) {
                item = get_map_lvalue(m, i, 1);

                m->ref++;
                lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
                lvalue->v.type = T_PROTECTED_LVALUE;
                lvalue->v.u.lvalue = item;
                BUILD_MAP_PROTECTOR(lvalue->protector, m)

                sp = i;
                free_svalue(i);
                i->type = T_LVALUE;
                i->u.lvalue = &lvalue->v;
                return sp;
            }
        }
#endif /* MAPPINGS */
        if (type == T_LVALUE) {
            vec = vec->u.lvalue;
            continue;
        }
        if (type == T_PROTECTED_LVALUE) {
            struct protected_lvalue *lvalue;
            struct protected_char_lvalue *val;

            lvalue = (struct protected_lvalue *)vec;
            if (lvalue->v.u.lvalue->type != T_STRING) {
                if (vec == sp->u.lvalue) {
                    free_protector_svalue(&last_indexing_protector);
                    last_indexing_protector = lvalue->protector;
                    vec = lvalue->v.u.lvalue;
                    xfree((char*)lvalue);
                    continue;
                }
                vec = lvalue->v.u.lvalue;
                continue;
            }
            if (i->type != T_NUMBER || (ind = i->u.number) < 0) {
                ERROR("Illegal index\n")
                return sp; /* flow control hint... */
            }
            vec = lvalue->v.u.lvalue;
            if (vec->x.string_type != STRING_MALLOC) {
                char *p = string_copy(vec->u.string);
                if (vec->x.string_type == STRING_SHARED)
                    free_string(vec->u.string);
                vec->u.string = p;
            }
            vec->x.string_type = STRING_VOLATILE;
            if (ind > svalue_strlen(vec) ) {
                ERROR ("Index out of bounds\n")
                return sp;
            }
            val = (struct protected_char_lvalue *)xalloc(sizeof *val);
            i->type = T_LVALUE;
            i->u.protected_char_lvalue = val;
            val->v.type = T_PROTECTED_CHAR_LVALUE;
            val->v.u.string = &vec->u.string[ind];
            val->lvalue = vec;
            val->start = vec->u.string;
            if (lvalue == sp->u.protected_lvalue) {
                sp = i;
                val->protector = lvalue->protector;
                xfree((char*)lvalue);
            } else {
                sp = i;
                val->protector.type = T_INVALID;
            }
            return sp;
        }
        inter_sp = sp;
        inter_pc = pc;
        dump_trace(1);
        error("(lvalue)Indexing on illegal type.\n");
        return sp;
    }
    return sp;
}

static INLINE struct svalue *protected_rindex_lvalue(sp, pc)
    struct svalue *sp;
    char *pc;
{
    struct svalue *i, *vec, *vecp;
    int ind;
    short type;

    vecp = sp;
    i = sp -1;
    if (i->type != T_NUMBER || (ind = i->u.number) <= 0) {
        ERROR("Illegal index\n")
        return sp; /* flow control hint... */
    }
    vec = vecp->u.lvalue;
    for (;;) {
        type = vec->type;
        if (type == T_POINTER) {
            struct vector *v = vec->u.vec;
            struct protected_lvalue *lvalue;

            sp = i;
            if ( (ind = VEC_SIZE(v) - ind) < 0) {
                ERROR ("Index out of bounds\n")
                return sp;
            }
            v->ref++;
            lvalue = (struct protected_lvalue *)xalloc(sizeof *lvalue);
            lvalue->v.type = T_PROTECTED_LVALUE;
            lvalue->v.u.lvalue = &v->item[ind];
            lvalue->protector.type = T_POINTER;
            lvalue->protector.u.vec = v;
            i->type = T_LVALUE;
            i->u.lvalue = &lvalue->v;
            return sp;
        }
        if (type == T_STRING) {
            struct protected_char_lvalue *val;

            if (vec->x.string_type != STRING_MALLOC) {
                char *p = string_copy(vec->u.string);
                if (vec->x.string_type == STRING_SHARED)
                    free_string(vec->u.string);
                vec->u.string = p;
            }
            if ( (ind = svalue_strlen(vec)  - ind) < 0) {
                vec->x.string_type = STRING_VOLATILE;
                ERROR ("Index out of bounds\n")
                return sp;
            }
            vec->x.string_type = STRING_VOLATILE;
            val = (struct protected_char_lvalue *)xalloc(sizeof *val);
            i->type = T_LVALUE;
            i->u.protected_char_lvalue = val;
            val->v.type = T_PROTECTED_CHAR_LVALUE;
            val->v.u.string = &vec->u.string[ind];
            val->lvalue = vec;
            val->start = vec->u.string;
            val->protector.type = T_INVALID;
            sp = i;
            return sp;
        }
        if (type == T_LVALUE) {
            vec = vec->u.lvalue;
            continue;
        }
        if (type == T_PROTECTED_LVALUE) {
            struct protected_lvalue *lvalue;
            struct protected_char_lvalue *val;

            lvalue = (struct protected_lvalue *)vec;
            if (lvalue->v.u.lvalue->type != T_STRING) {
                if (vec == sp->u.lvalue) {
                    free_protector_svalue(&last_indexing_protector);
                    last_indexing_protector = lvalue->protector;
                    vec = lvalue->v.u.lvalue;
                    xfree((char*)lvalue);
                    continue;
                }
                vec = lvalue->v.u.lvalue;
                continue;
            }
            if (i->type != T_NUMBER || (ind = i->u.number) < 0) {
                ERROR("Illegal index\n")
                return sp; /* flow control hint... */
            }
            vec = lvalue->v.u.lvalue;
            if (vec->x.string_type != STRING_MALLOC) {
                char *p = string_copy(vec->u.string);
                if (vec->x.string_type == STRING_SHARED)
                    free_string(vec->u.string);
                vec->u.string = p;
            }
            vec->x.string_type = STRING_VOLATILE;
            if ( (ind = svalue_strlen(vec)  - ind) < 0) {
                ERROR ("Index out of bounds\n")
                return sp;
            }
            val = (struct protected_char_lvalue *)xalloc(sizeof *val);
            i->type = T_LVALUE;
            i->u.protected_char_lvalue = val;
            val->v.type = T_PROTECTED_CHAR_LVALUE;
            val->v.u.string = &vec->u.string[ind];
            val->lvalue = vec;
            val->start = vec->u.string;
            if (lvalue == sp->u.protected_lvalue) {
                sp = i;
                val->protector = lvalue->protector;
                xfree((char*)lvalue);
            } else {
                sp = i;
                val->protector.type = T_INVALID;
            }
            return sp;
        }
        inter_sp = sp;
        inter_pc = pc;
        dump_trace(1);
        error("(lvalue)Indexing on illegal type.\n");
        return sp;
    }
}

/*
 * Compile lvalue information for a range.
 */
static struct svalue *range_lvalue(code, sp)
    int code;
    struct svalue *sp;
{
    struct svalue *i, *vec, *vecp;
    int ind1, ind2;
    short type;
    int size;

    vecp = sp;
#ifdef DEBUG
    if (vecp->type != T_LVALUE) {
        inter_sp = sp;
        error("wrong type to range_lvalue\n");
    }
#endif
    vec = vecp->u.lvalue;
    for (;;) {
        type = vec->type;
        if (type == T_POINTER) {
            special_lvalue.v.type = T_POINTER_RANGE_LVALUE;
            size = VEC_SIZE(vec->u.vec);
            break;
        } else if (type == T_STRING) {
            special_lvalue.v.type = T_STRING_RANGE_LVALUE;
            size = svalue_strlen(vec);
            break;
        } else if (type == T_LVALUE || type == T_PROTECTED_LVALUE) {
            vec = vec->u.lvalue;
            continue;
        } else {
            inter_sp = sp;
            dump_trace(1);
            error("(lvalue)range lvalue on illegal type.\n");
            return sp;
        }
    }
    special_lvalue.v.u.lvalue = vec;
    special_lvalue.size = size;
    if ((i = vecp-1)->type != T_NUMBER)
    {
        inter_sp = sp;
        error("Illegal index\n");
        return sp; /* flow control hint... */
    }
    if (code & 0xff) {
        ind2 = size - i->u.number;
    } else {
        ind2 = i->u.number;
    }
    if (++ind2 < 0 || ind2 > size) {
        inter_sp = sp;
        error ("Index out of bounds\n");
        return sp;
    }
    special_lvalue.index2 = ind2;
    if ((--i)->type != T_NUMBER)
    {
        inter_sp = sp;
        error("Illegal index\n");
        return sp;
    }
    if (code & 0xff00) {
        ind1 = size - i->u.number;
    } else {
        ind1 = i->u.number;
    }
    if (ind1 < 0 || ind1 > size) { /* Appending is allowed */
        inter_sp = sp;
        error ("Index out of bounds\n");
        return sp;
    }
    special_lvalue.index1 = ind1;
    sp = i;
    i->type = T_LVALUE;
    i->u.lvalue = &special_lvalue.v;
    return sp;
}

/*
 * Compile lvalue information for an unterminated range.
 */
struct svalue *f_extract_lvalue(sp)
    struct svalue *sp;
{
    struct svalue *vec, *vecp;
    int ind1;
    short type;
    int size;

    vecp = sp;
#ifdef DEBUG
    if (vecp->type != T_LVALUE) {
        inter_sp = sp;
        error("wrong type to range_lvalue\n");
    }
#endif
    vec = vecp->u.lvalue;
    for (;;) {
        type = vec->type;
        if (type == T_POINTER) {
            special_lvalue.v.type = T_POINTER_RANGE_LVALUE;
            size = VEC_SIZE(vec->u.vec);
            break;
        } else if (type == T_STRING) {
            special_lvalue.v.type = T_STRING_RANGE_LVALUE;
            size = svalue_strlen(vec);
            break;
        } else if (type == T_LVALUE || type == T_PROTECTED_LVALUE) {
            vec = vec->u.lvalue;
            continue;
        } else {
            inter_sp = sp;
            dump_trace(1);
            error("(lvalue)range lvalue on illegal type.\n");
            return sp;
        }
    }
    special_lvalue.v.u.lvalue = vec;
    special_lvalue.size = size;
    sp = vecp-1;
    special_lvalue.index2 = size;
    if (sp->type != T_NUMBER)
    {
        inter_sp = sp+1;
        error("Illegal index\n");
        return sp;
    }
    if (sp->u.number < 0) {
        ind1 = size + sp->u.number;
    } else {
        ind1 = sp->u.number;
    }
    if (ind1 < 0 || ind1 > size) { /* Appending is allowed */
        inter_sp = sp+1;
        error ("Index out of bounds\n");
        return sp;
    }
    special_lvalue.index1 = ind1;
    sp->type = T_LVALUE;
    sp->u.lvalue = &special_lvalue.v;
    return sp;
}

static struct svalue *protected_range_lvalue(code, sp)
    int code;
    struct svalue *sp;
{
    struct svalue *i, *vec, *vecp;
    int ind1, ind2;
    short type;
    int size;
    short lvalue_type;
    struct svalue protector;
    struct protected_range_lvalue *new_lvalue;

    vecp = sp;
#ifdef DEBUG
    if (vecp->type != T_LVALUE) {
        inter_sp = sp;
        error("wrong type to range_lvalue\n");
    }
#endif
    vec = vecp->u.lvalue;
    protector.type = T_INVALID;
    for (;;) {
        type = vec->type;
        if (type == T_POINTER) {
            vec->u.vec->ref++;
            lvalue_type = T_PROTECTED_POINTER_RANGE_LVALUE;
            size = VEC_SIZE(vec->u.vec);
            break;
        } else if (type == T_STRING) {
            if (vec->x.string_type != STRING_MALLOC) {
                char *p = string_copy(vec->u.string);
                if (vec->x.string_type == STRING_SHARED)
                    free_string(vec->u.string);
                vec->u.string = p;
            }
            vec->x.string_type = STRING_VOLATILE;
            lvalue_type = T_PROTECTED_STRING_RANGE_LVALUE;
            size = svalue_strlen(vec);
            break;
        } else if (type == T_LVALUE) {
            vec = vec->u.lvalue;
            continue;
        } else if (type == T_PROTECTED_LVALUE) {
            if (vec == vecp->u.lvalue) {
                protector = ((struct protected_lvalue*)vec)->protector;
            }
            vec = vec->u.lvalue;
            continue;
        } else {
            inter_sp = sp;
            dump_trace(1);
            error("(lvalue)range lvalue on illegal type.\n");
            return sp;
        }
    }
    if ((i = vecp-1)->type != T_NUMBER)
    {
        inter_sp = sp;
        error("Illegal index\n");
        return sp; /* flow control hint... */
    }
    if (code & 0xff) {
        ind2 = size - i->u.number;
    } else {
        ind2 = i->u.number;
    }
    if (++ind2 < 0 || ind2 > size) {
        inter_sp = sp;
        error ("Index out of bounds\n");
        return sp;
    }
    if ((--i)->type != T_NUMBER)
    {
        inter_sp = sp;
        error("Illegal index\n");
        return sp;
    }
    if (code & 0xff00) {
        ind1 = size - i->u.number;
    } else {
        ind1 = i->u.number;
    }
    if (ind1 < 0 || ind1 > size) { /* Appending is allowed */
        inter_sp = sp;
        error ("Index out of bounds\n");
        return sp;
    }
    new_lvalue = (struct protected_range_lvalue *)xalloc(sizeof *new_lvalue);
    new_lvalue->v.type = lvalue_type;
    new_lvalue->v.u = vec->u;
    new_lvalue->protector = protector;
    new_lvalue->lvalue = vec;
    new_lvalue->index2 = ind2;
    new_lvalue->index1 = ind1;
    new_lvalue->size = size;
    sp = i;
    i->type = T_LVALUE;
    i->u.protected_range_lvalue = new_lvalue;
    return sp;
}

/*
 * Compute an array element. If it is an destructed object, replace it by 0.
 */
static INLINE struct svalue *push_indexed_value(sp, pc)
    struct svalue *sp;
    char *pc;
{
    struct svalue *i, *vec;
    int ind;

    i = sp;
    vec = sp - 1;
    switch (vec->type) {
      case T_STRING:
      {
        if (i->type != T_NUMBER || (ind = i->u.number) < 0) {
            ERROR("Illegal index\n")
            return sp; /* flow control hint... */
        }
        if (ind > _svalue_strlen(vec))
            ind = 0;
        else
            ind = vec->u.string[ind];
        free_string_svalue(vec);
        vec->type = T_NUMBER;
        vec->u.number = ind;
        sp = vec;
        return sp;
      }
      case T_POINTER:
        if (i->type != T_NUMBER || (ind = i->u.number) < 0)
            { ERROR("Illegal index\n") return sp; }
        sp = vec;
        if ((size_t)ind >= VEC_SIZE(vec->u.vec)) ERROR ("Index out of bounds\n")
        if (!--vec->u.vec->ref) {
            struct svalue *p, tmp;

            vec->u.vec->ref++;
            p = &vec->u.vec->item[ind];
            if (p->type == T_OBJECT && p->u.ob->flags & O_DESTRUCTED) {
                free_object_svalue(p);
                tmp.type = T_NUMBER;
                tmp.u.number = 0;
            } else {
                tmp = *p;
            }
            p->type = T_NUMBER;
            free_vector(vec->u.vec);
            *vec = tmp;
            return sp;
        }
        assign_checked_svalue_no_free(vec, &vec->u.vec->item[ind], sp, pc);
        return sp;
#ifdef MAPPINGS
      case T_MAPPING:
      {
        struct svalue *item;
        struct mapping *m;

        m = vec->u.map;
        if (!m->num_values) {
            inter_sp = sp;
            inter_pc = pc;
            dump_trace(1);
            error("(value)Indexing on illegal type.\n");
            return sp;
        }
        item = get_map_lvalue(m, i, 0);
        free_svalue(i); /* must be here: consider i == vec->u.map not in i */
        if (m->ref == 1) {
            assign_svalue (&indexing_quickfix, item);
            item = &indexing_quickfix;
        }
        free_mapping(m);  /* This will make 'vec' invalid to use */
        assign_checked_svalue_no_free(vec, item, sp, pc);
        sp = vec;
        return sp;
      }
#endif /* MAPPINGS */
      default:
        inter_sp = sp;
        inter_pc = pc;
        dump_trace(1);
        error("(value)Indexing on illegal type.\n");
        return sp;
    }
    return sp;
}

static INLINE struct svalue *push_rindexed_value(sp, pc)
    struct svalue *sp;
    char *pc;
{
    struct svalue *i, *vec;
    int ind;

    i = sp;
    vec = sp - 1;
    if (i->type != T_NUMBER || (ind = i->u.number) <= 0) {
        ERROR("Illegal index\n")
        return sp; /* flow control hint... */
    }
    switch (vec->type) {
      case T_STRING:
      {
        if ( (ind = _svalue_strlen(vec) - ind) < 0 )
            ind = 0;
        else
            ind = vec->u.string[ind];
        free_string_svalue(vec);
        vec->type = T_NUMBER;
        vec->u.number = ind;
        sp = vec;
        return sp;
      }
      case T_POINTER:
        sp = vec;
        if ( (ind = VEC_SIZE(vec->u.vec) - ind) < 0) {
            ERROR ("Index out of bounds\n")
            return sp;
        }
        if (!--vec->u.vec->ref) {
            struct svalue *p, tmp;

            vec->u.vec->ref++;
            p = &vec->u.vec->item[ind];
            if (p->type == T_OBJECT && p->u.ob->flags & O_DESTRUCTED) {
                free_object_svalue(p);
                tmp.type = T_NUMBER;
                tmp.u.number = 0;
            } else {
                tmp = *p;
            }
            p->type = T_NUMBER;
            free_vector(vec->u.vec);
            *vec = tmp;
            return sp;
        }
        assign_checked_svalue_no_free(vec, &vec->u.vec->item[ind], sp, pc);
        return sp;
      default:
        inter_sp = sp;
        inter_pc = pc;
        dump_trace(1);
        error("(value)Indexing on illegal type.\n");
        return sp;
    }
    return sp;
}

#ifdef OPCPROF
#define MAXOPC 0x280
static int opcount[MAXOPC];
#endif

/*
 * Deallocate 'n' values from the stack.
 */
INLINE
static struct svalue *_pop_n_elems(n, sp)
    int n;
    struct svalue *sp;
{
#ifdef DEBUG
    if (n < 0)
        fatal("pop_n_elems: %d elements.\n", n);
#endif
    for (; --n >= 0; )
        pop_stack();
    return sp;
}

static void bad_arg_pc PROT((int arg, int instr, struct svalue *sp, char *pc))
        NORETURN;

static void bad_arg_pc(arg, instr, sp, pc)
    int arg, instr;
    struct svalue *sp;
    char *pc;
{
    ERRORF(("Bad argument %d to %s()\n", arg, get_f_name(instr)))
}

void bad_efun_arg(arg, instr, sp)
    int arg, instr;
    struct svalue *sp;
{
    inter_sp = sp;
    if (instr < 0) {
        unsigned char *pc = inter_pc + instr;
        instr = *pc;
        if (instr <= F_VEFUN - F_OFFSET)
            instr = pc[1] | (instr << F_ESCAPE_BITS);
    }
    error("Bad argument %d to %s()\n", arg, get_f_name(instr));
}

void bad_xefun_arg(arg, sp)
    int arg; struct svalue *sp;
{
    bad_efun_arg(arg, -2, sp);
}

void bad_xefun_vararg(arg, sp)
    int arg; struct svalue *sp;
{
    bad_efun_arg(arg, -3, sp);
}

INLINE
static void push_control_stack(sp, pc, fp)
    struct svalue *sp;
    char *pc;
    struct svalue *fp;
{
    if (csp >= &control_stack[MAX_USER_TRACE-1]) {
        if (!num_error || csp == &control_stack[MAX_TRACE-1]) {
            ERROR("Too deep recursion.\n")
        }
    }
    csp++;
    /* csp->funstart  has to be set later, it is used only for tracebacks. */
    csp->fp = fp;
    csp->prog = current_prog;
    /* csp->extern_call = 0; It is set by eval_instruction() */
    csp->pc = pc;
    csp->function_index_offset = function_index_offset;
    csp->current_variables = current_variables;
    csp->break_sp = break_sp;
}

/*
 * Pop the control stack one element, and restore registers.
 * extern_call must not be modified here, as it is used imediately after pop.
 */
static void pop_control_stack() {
#ifdef DEBUG
    if (csp == control_stack - 1)
        fatal("Popped out of the control stack");
#endif
    if ( NULL != (current_prog = csp->prog) ) /* is 0 when we reach the bottom */
        current_strings = current_prog->strings;
    inter_pc = csp->pc;
    inter_fp = csp->fp;
    function_index_offset = csp->function_index_offset;
    current_variables     = csp->current_variables;
    break_sp = csp->break_sp;
    csp--;
}

/*
 * Push a pointer to a vector on the stack. Note that the reference count
 * is incremented. Newly created vectors normally have a reference count
 * initialized to 1.
 */
void push_vector(v)
    struct vector *v;
{
    v->ref++;
    inter_sp++;
    inter_sp->type = T_POINTER;
    inter_sp->u.vec = v;
}

void push_referenced_vector(v)
    struct vector *v;
{
    inter_sp++;
    inter_sp->type = T_POINTER;
    inter_sp->u.vec = v;
}

#define push_vector(v) ( \
        sp++,\
        sp->type = T_POINTER,\
        (sp->u.vec = (v))->ref++\
)

#define push_referenced_vector(v) ( \
        sp++,\
        sp->type = T_POINTER,\
        sp->u.vec = (v)\
)

#define put_vector(v) ( \
        sp->type = T_POINTER,\
        (sp->u.vec = (v))->ref++\
)

#define put_referenced_vector(v) ( \
        sp->type = T_POINTER,\
        sp->u.vec = (v)\
)

#ifdef MAPPINGS
void push_referenced_mapping(m)
    struct mapping *m;
{
    inter_sp++;
    inter_sp->type = T_MAPPING;
    inter_sp->u.map = m;
}

#define push_mapping(m) ( \
    sp++,\
    sp->type = T_MAPPING,\
    (sp->u.map = (m))->ref++\
)

void m_indices_filter(key, data, extra)
    struct svalue *key;
    struct svalue *data UNUSED;
    char *extra;
{
    struct svalue **svpp = (struct svalue **)extra;

    assign_svalue_no_free( (*svpp)++, key );
}

static void m_values_filter(key, data, extra)
    struct svalue *key UNUSED;
    struct svalue *data;
    char *extra;
{
    struct svalue **svpp = (struct svalue **)extra;

    assign_svalue_no_free( (*svpp)++, data );
}
#endif /* MAPPINGS */

/*
 * Push a string on the stack that is to be malloced.
 */
void push_string_malloced(p)
    char *p;
{
    struct svalue *sp;
    char *s;

    s = xalloc(strlen(p)+1);
    strcpy(s, p);
    sp = ++inter_sp;
    sp->type = T_STRING;
    sp->x.string_type = STRING_MALLOC;
    sp->u.string = s;
}

static struct svalue *_push_string_malloced(p, sp)
    char *p;
    struct svalue *sp;
{
    char *s;

    s = xalloc(strlen(p)+1);
    strcpy(s, p);
    sp++;
    sp->type = T_STRING;
    sp->x.string_type = STRING_MALLOC;
    sp->u.string = s;
    return sp;
}
#define push_string_malloced(p) (sp = _push_string_malloced(p, sp))

#if 0
static struct svalue *_put_string_malloced(p, sp)
    char *p;
    struct svalue *sp;
{
    char *s;

    s = xalloc(strlen(p)+1);
    strcpy(s, p);
    sp->type = T_STRING;
    sp->x.string_type = STRING_MALLOC;
    sp->u.string = s;
    return sp;
}
#define put_string_malloced(p) _put_string_malloced(p, sp)
#endif

/*
 * Push a string on the stack that is to be shared.
 */

void push_string_shared(p)
    char *p;
{
    inter_sp++;
    inter_sp->type = T_STRING;
    inter_sp->x.string_type = STRING_SHARED;
    inter_sp->u.string = make_shared_string(p);
}

/*
 * Push a string on the stack that is already malloced.
 */
INLINE static struct svalue *_push_malloced_string(p, sp)
    char *p;
    struct svalue *sp;
{
    sp++;
    sp->type = T_STRING;
    sp->x.string_type = STRING_MALLOC;
    sp->u.string = p;
    return sp;
}


LOCAL_INLINE void push_malloced_string(p)
    char *p;
{
    inter_sp++;
    inter_sp->type = T_STRING;
    inter_sp->x.string_type = STRING_MALLOC;
    inter_sp->u.string = p;
}

static INLINE void put_malloced_string(p, sp)
    char *p;
    struct svalue *sp;
{
    sp->type = T_STRING;
    sp->x.string_type = STRING_MALLOC;
    sp->u.string = p;
}

/*
 * Push a string on the stack that is constant in the current stack frame.
 */
static INLINE
struct svalue *_push_volatile_string(p, sp)
    char *p;
    struct svalue *sp;
{
    sp++;
    sp->type = T_STRING;
    sp->x.string_type = STRING_VOLATILE;
    sp->u.string = p;
    return sp;
}

void push_volatile_string(p)
    char *p;
{
    inter_sp++;
    inter_sp->type = T_STRING;
    inter_sp->x.string_type = STRING_VOLATILE;
    inter_sp->u.string = p;
}
#define push_volatile_string(s) (sp = _push_volatile_string((s), sp))

#if 0
/*
 * Push a string on the stack that is already constant.
 */
INLINE
void push_constant_string(p)
    char *p;
{
    sp++;
    sp->type = T_STRING;
    sp->x.string_type = STRING_CONSTANT;
    sp->u.string = p;
}
#endif
#define _push_constant_string _push_volatile_string

int _privilege_violation(what, where, sp)
char *what;
struct svalue *where;
struct svalue *sp;
{
    struct svalue *svp;

    if (current_object == master_ob) return 1;
    if (current_object == simul_efun_object) return 1;
    push_volatile_string(what);
    push_valid_ob(current_object);
    sp++;
    assign_svalue_no_free(sp, where);
    inter_sp = sp;
    svp = apply_master_ob(STR_PRIVILEGE, 3);
    if (!svp || svp->type != T_NUMBER || svp->u.number < 0) {
        inter_sp = sp-3;
        error("privilege violation : %s\n", what);
    }
    return svp->u.number;
}

int privilege_violation4(what, whom, how_str, how_num, sp)
    char *what;
    struct object *whom;
    char *how_str;
    int how_num;
    struct svalue *sp;
{
    struct svalue *svp;

    if (current_object == master_ob) return 1;
    if (current_object == simul_efun_object) return 1;
    push_volatile_string(what);
    push_valid_ob(current_object);
    if (!whom) {
        push_volatile_string(how_str);
        push_number(how_num);
    } else {
        push_object(whom);
        if (how_str)
            push_volatile_string(how_str);
        else
            push_number(how_num);
    }
    inter_sp = sp;
    svp = apply_master_ob(STR_PRIVILEGE, 4);
    if (!svp || svp->type != T_NUMBER || svp->u.number < 0) {
        inter_sp = sp-4;
        error("privilege violation : %s\n", what);
    }
    return svp->u.number;
}

static void do_trace_call(funstart)
    unsigned char *funstart;
{
    char *name;

    if (!++traceing_recursion) {
        int save_var_ix_offset = variable_index_offset; /* Sunblood */
        memcpy((char *)&name, funstart - 1 - sizeof name, sizeof name);
        do_trace("Call direct ", name, " ");
        if (TRACEHB) {
            if (TRACETST(TRACE_ARGS)) {
                int i;
                struct svalue *svp;

            add_message(" with %d arguments: ", (funstart[0] & 0x7f));
            svp = inter_fp;
            for(i = (funstart[0] & 0x7f); --i >= 0; ) {
                    print_svalue(svp++);
                    add_message(" ");
                }
            }
            add_message("\n");
        }
        variable_index_offset = save_var_ix_offset; /* Sunblood */
    }
    traceing_recursion--;
}

static void do_trace_return(sp)
    struct svalue *sp;
{
    tracedepth--;
    if (!++traceing_recursion) {
        if (trace_test(TRACE_RETURN)) {
            inter_sp = sp;
            do_trace("Return", "", "");
            if (TRACEHB) {
                if (TRACETST(TRACE_ARGS)) {
                    add_message(" with value: ");
                    print_svalue(sp);
                }
                add_message("\n");
            }
        }
    }
    traceing_recursion--;
    (void)SET_TRACE_EXEC;
}

/*
 * Argument is the function to execute. If it is defined by inheritance,
 * then search for the real definition, and return it.
 * There is a number of arguments on the stack. Normalize them and initialize
 * local variables, so that the called function is pleased.
 */
static INLINE uint32 setup_new_frame1(fx, fun_ix_offs, var_ix_offs)
    int fx;
    int fun_ix_offs, var_ix_offs;
{
    struct program *progp;
    uint32 flags;

    progp = current_prog;
    flags = progp->functions[fx];
    fun_ix_offs += fx;
    while(flags & NAME_INHERITED) {
        struct inherit *inheritp;

        inheritp = &progp->inherit[flags & INHERIT_MASK];
        progp = inheritp->prog;
        fx -= inheritp->function_index_offset;
        var_ix_offs += inheritp->variable_index_offset;
        flags = progp->functions[fx];
    }
    current_prog = progp;
    function_index_offset = fun_ix_offs - fx;
    variable_index_offset = var_ix_offs;
    return flags;
}

static INLINE struct svalue *setup_new_frame2(funstart, sp)
    unsigned char *funstart;
    struct svalue *sp;
{
    short i;
    int num_arg;

    inter_fp = sp - csp->num_local_variables + 1;
    num_arg = EXTRACT_SCHAR(&funstart[0]);
    /* (Re)move excessive arguments */
    if ((i = csp->num_local_variables - num_arg) > 0)
    {
#ifndef NO_XVARARGS
      if (num_arg < 0)
      {
        num_arg &= 0x7f;
        if ((i = csp->num_local_variables - num_arg + 1) < 0)
        {
          /* More formal than actual parameters */
          csp->num_local_variables = num_arg;
          /* First, fill in zero for the rest */
          do {
            *++sp = const0;
          } while (++i);
          /* And an empty array for the varargs portion */
          (++sp)->type = T_POINTER;
          sp->u.vec = allocate_uninit_array(0);
        }
        else
        {
          /* More actual than formal parameters */
          struct vector *v;
          char *pc = funstart;

          csp->num_local_variables = num_arg;
          v = allocate_uninit_array(i);
          while (--i>=0)
          {
            if (sp->type == T_LVALUE)
              num_arg = -1; /* mark error condition */
            v->item[i] = *sp--;
          }
          (++sp)->type = T_POINTER;
          sp->u.vec = v;
          if (num_arg < 0)
            ERROR("Varargs argument passed by reference.\n");
        }
        /* Clear the local variables */
        if ( 0 != (i = funstart[1]) )
        {
          csp->num_local_variables += i;
          do {
            *++sp = const0;
          } while (--i > 0);
        }
      }
      else
#endif /* NO_XVARARGS */
      {
        do {
            free_svalue(sp--);
            csp->num_local_variables--;
        } while(--i);
        if ( 0 != (i = funstart[1]) )
        {
          csp->num_local_variables += i;
          do {
              *++sp = const0;
          } while (--i);
        }
      }
    }
    else
    {
        /* Correct number of arguments and local variables */
        if ( 0 != (i = funstart[1] - i) ) {
#if defined(__GNUC__) && __GNUC__ >= 2
            /* there seems to be a problem with gcc 1.36 */
            /* local structs are handled more efficiently. */
            struct svalue const_0 = {T_NUMBER, { 0 }, { 0 } };
#else
#define const_0 const0
#endif
            csp->num_local_variables += i;
            do {
                *++sp = const_0;
            } while (--i);
#undef const_0
        }
    }
    if ( sp >= &start_of_stack[EVALUATOR_STACK_SIZE] )
      STACK_OVERFLOW(sp, csp->fp, funstart);
    tracedepth++;
    if (TRACEP(TRACE_CALL)) {
      inter_sp = sp;
      do_trace_call(funstart);
    }
    break_sp = &sp[1].u.string;
    return sp;
}

static uint32 setup_new_frame(fx)
    int fx;
{
    uint32 flags;

    flags = setup_new_frame1(fx, 0, 0);
    inter_sp = setup_new_frame2(
      current_prog->program + (flags & FUNSTART_MASK), inter_sp
    );
    current_variables = current_object->variables + variable_index_offset;
    current_strings = current_prog->strings;
    return flags;
}

#ifdef F_BREAK_POINT
static void break_point(sp, fp)
    struct svalue *sp, *fp;
{
    if (sp - fp - csp->num_local_variables + 1 != 0)
        fatal("Bad stack pointer.\n");
}
#endif

/* marion
 * maintain a small and inefficient stack of error recovery context
 * data structures.
 * This routine is called in three different ways:
 * push=-1        Pop the stack.
 * push=1        push the stack.
 * push=0        No error occured, so the pushed value does not have to be
 *                restored. The pushed value can simply be popped into the void.
 *
 * The stack is implemented as a linked list of stack-objects, allocated
 * from the heap, and deallocated when popped.
 */
/* Amylaar:        I like it a bit less inefficient. Besides, the value of
 *                error_recovery_pointer (as a pointer to an opaque structure)
 *                is needed to cure an old bug in the player_parser.
 */

struct catch_context {
        struct error_recovery_info recovery_info; /* must be first */
        struct control_stack *save_csp;
        struct object *save_command_giver;
        struct svalue *save_sp;
};

static INLINE struct con_struct *push_error_context (sp)
    struct svalue *sp;
{
    struct catch_context *p;

        /*
         * Save some global variables that must be restored separately
         * after a longjmp. The stack will have to be manually popped all
         * the way.
         */
        p = (struct catch_context *)xalloc (sizeof *p);
        p->save_sp = sp;
        p->save_csp = csp;
        p->save_command_giver = command_giver;
        p->recovery_info.last = error_recovery_pointer;
        p->recovery_info.type = ERROR_RECOVERY_CATCH;
        error_recovery_pointer = &p->recovery_info;
        return &p->recovery_info.con;
}

static INLINE void pop_error_context ()
{
    struct catch_context *p;

    p = (struct catch_context *)error_recovery_pointer;
#ifdef DEBUG
    if (p->recovery_info.type != ERROR_RECOVERY_CATCH)
        fatal("Catch: error context stack underflow");
    if (csp != p->save_csp-1)
        fatal("Catch: Lost track of csp");
#if 0
    /*
     * This test is not valid! The statement catch(exec("...")) will
     * change the value of command_giver.
     */
    if (command_giver != p->save_command_giver)
        fatal("Catch: Lost track of command_giver");
#endif
#endif
    error_recovery_pointer = p->recovery_info.last;
    xfree ((char *)p);
}

static struct svalue *pull_error_context (sp)
    struct svalue *sp;
{
    /* They did a throw() or error. That means that the control
     * stack must be restored manually here.
     */
    struct catch_context *p;
    struct control_stack *csp2;

    p = (struct catch_context *)error_recovery_pointer;
    if (p->recovery_info.type != ERROR_RECOVERY_CATCH)
        fatal("Catch: error context stack underflow");
    csp2 = p->save_csp;
    while (++csp2 <= csp) {
        if (csp2->extern_call) {
            previous_ob = csp2->prev_ob;
            current_object = csp2->ob;
            break;
        }
    }
    csp = p->save_csp;
    pop_n_elems (sp - p->save_sp);
    command_giver = p->save_command_giver;
    error_recovery_pointer = p->recovery_info.last;
    xfree ((char *)p);
    return sp;
}

static struct program *search_inherited(str, prg, offpnt)
    char *str;
    struct program *prg;
    int *offpnt;
{
    struct program *tmp;
    int i;

#ifdef DEBUG
    if (d_flag) {
        debug_message("search_inherited started\n");
        debug_message("searching for PRG(%s) in PRG(%s)\n",str,prg->name);
        debug_message("num_inherited=%d\n", prg->num_inherited);
    }
#endif
    for (i=0; i < prg->num_inherited; i++) {
#ifdef DEBUG
        if (d_flag) {
            debug_message("index %d:\n",i);
            debug_message("checking PRG(%s)\n", prg->inherit[i].prog->name);
        }
#endif
        if ( strcmp(str, prg->inherit[i].prog->name ) == 0 ) {
#ifdef DEBUG
            if (d_flag)
                debug_message("match found\n");
#endif
            offpnt[0] = prg->inherit[i].variable_index_offset;
            offpnt[1] = prg->inherit[i].function_index_offset;
            return prg->inherit[i].prog;
        } else if ( NULL != (tmp = search_inherited(str, prg->inherit[i].prog,offpnt)) )
        {
#ifdef DEBUG
            if (d_flag)
                debug_message("deferred match found\n");
#endif
            offpnt[0] += prg->inherit[i].variable_index_offset;
            offpnt[1] += prg->inherit[i].function_index_offset;
            return tmp;
        }
    }
#ifdef DEBUG
        if (d_flag)
            debug_message("search_inherited failed\n");
#endif
    return (struct program *)0;
}

struct replace_ob *retrieve_replace_program_entry() {
    struct replace_ob *r_ob;

    for (r_ob = obj_list_replace; r_ob; r_ob = r_ob->next) {
        if (r_ob->ob == current_object) {
            return r_ob;
        }
    }
    return 0;
}

/*
 * When a vector is given as argument to an efun, all items has to be
 * checked if there would be an destructed object.
 * A bad problem currently is that a vector can contain another vector, so this
 * should be tested too. But, there is currently no prevention against
 * recursive vectors, which means that this can not be tested. Thus, the game
 * may crash if a vector contains a vector that contains a destructed object
 * and this top-most vector is used as an argument to an efun.
 */
/* The game won't crash when doing simple operations like assign_svalue
 * on a destructed object. You have to watch out, of course, that you don't
 * apply a function to it.
 * to save space it is preferable that destructed objects are freed soon.
 *   amylaar
 */
void check_for_destr(v)
    struct vector *v;
{
    int i;
    struct svalue *p;

    for (p = v->item, i = VEC_SIZE(v); --i >= 0 ; p++ ) {
        if (p->type != T_OBJECT)
            continue;
        if (!(p->u.ob->flags & O_DESTRUCTED))
            continue;
        zero_object_svalue(p);
    }
}

#ifdef DEBUG
#define GET_NUM_ARG if (num_arg != EXTRACT_UCHAR(pc-1)) {\
                        fprintf(stderr, "%d vs. %d\n", num_arg, EXTRACT_UCHAR(pc-1));\
                        fatal("argument count error\n");}
#else /* DEBUG */
#define GET_NUM_ARG num_arg = EXTRACT_UCHAR(pc); pc++;
#endif /* DEBUG */
#define TYPE_TEST1(arg, t) if ( (arg)->type != t ) goto bad_arg_1;
#define TYPE_TEST2(arg, t) if ( (arg)->type != t ) goto bad_arg_2;
#define TYPE_TEST3(arg, t) if ( (arg)->type != t ) goto bad_arg_3;
#define TYPE_TEST4(arg, t) if ( (arg)->type != t ) goto bad_arg_4;

/*
 * Evaluate instructions at address 'p'. All program offsets are
 * to current_prog->program. 'current_prog' must be setup before
 * call of this function.
 *
 * There must not be destructed objects on the stack. The destruct_object()
 * function will automatically remove all occurences. The effect is that
 * all called efuns knows that they won't have destructed objects as
 * arguments.
 */
#define push_malloced_string(s) (sp = _push_malloced_string((s), sp))
#define privilege_violation(what, where) (\
        inter_pc = pc,\
        _privilege_violation(what, where, sp)\
)

#ifdef TRACE_CODE
/* keep some more information in the core than we show... */
static int previous_instruction[TOTAL_TRACE_LENGTH];
static int stack_size[TOTAL_TRACE_LENGTH];
static char *previous_pc[TOTAL_TRACE_LENGTH];
static struct program *previous_programs[TOTAL_TRACE_LENGTH];
static struct object *previous_objects[TOTAL_TRACE_LENGTH];
static int last = TOTAL_TRACE_LENGTH - 1;
#endif
static void eval_instruction(first_instruction, sp)
    char *first_instruction;
    register struct svalue *sp;
    /* gcc feels better about setjmp() when variables are declared register.
     * Still we might get 'variable foo might be clobbered' warnings, but
     * declaring them as volatile would degrade optimization, so we don't. */
{
    register char *pc;
    register struct svalue *fp;
    int num_arg;
    int instruction;
#ifdef DEBUG
    struct svalue *expected_stack;
#endif

    /* Next F_RETURN at this level will return out of eval_instruction() */
    csp->extern_call = 1;
    pc = first_instruction;
    fp = inter_fp;
    (void)SET_TRACE_EXEC;
again:
    instruction = EXTRACT_UCHAR(pc);
#ifdef TRACE_CODE
#if TOTAL_TRACE_LENGTH & TOTAL_TRACE_LENGTH-1
    if (++last == TOTAL_TRACE_LENGTH)
        last = 0;
#else
    last = (last+1) & (TOTAL_TRACE_LENGTH-1);
#endif
    previous_instruction[last] = instruction;
    previous_pc[last] = pc;
    stack_size[last] = sp - fp - csp->num_local_variables;
    if (previous_objects[last]) {
        if (!--previous_objects[last]->ref) {
            previous_objects[last]->ref++;
            free_object(previous_objects[last], "TRACE_CODE");
        }
    }
    previous_objects[last] = current_object;
    current_object->ref++;
    previous_programs[last] = current_prog;
#endif
    pc++;
#ifdef SMALLOC_LPC_TRACE
    inter_pc = pc;
#endif
    if (++eval_cost >= 0) {
        if (eval_cost >= MIN_TRACE_COST && eval_cost < MAX_TRACE_COST) {
            if (TRACE_EXEC_P) {
                if (!++traceing_recursion) {
                    inter_sp = sp;
                    do_trace("Exec ", get_f_name(instruction), "\n");
                    instruction = EXTRACT_UCHAR(pc-1);
                }
                traceing_recursion--;
            }
        } else {
            if (eval_cost >= MAX_TRACE_COST) {
                eval_cost -= MAX_TRACE_COST;
                assigned_eval_cost -= MAX_TRACE_COST;
            }
            printf("eval_cost too big %ld\n", eval_cost - initial_eval_cost);
            assign_eval_cost();
            if (error_recovery_pointer->type <= ERROR_RECOVERY_BACKEND) {
                CLEAR_EVAL_COST;
            }
            inter_pc = pc;
            inter_fp = fp;
            ERROR("Too long evaluation. Execution aborted.\n")
        }
    }
    /*
     * Execute current instruction. Note that all functions callable
     * from LPC must return a value or be declared void. This does not apply
     * to control instructions, like F_JUMP.
     */
#if defined( DEBUG )
    if (instrs[instruction].min_arg != instrs[instruction].max_arg) {
        num_arg = EXTRACT_UCHAR(pc);
        pc++;
    } else {
        /*
         * Safety measure. It is supposed that the evaluator knows
         * the number of arguments.
         */
        num_arg = -1;
    }
    if (num_arg != -1) {
        expected_stack = sp - num_arg +
            ( instrs[instruction].ret_type == TYPE_VOID ? 0 : 1 );
    } else {
        expected_stack = 0;
    }
#endif /* DEBUG */
#ifdef OPCPROF
    opcount[instruction]++;
#endif
    /*
     * Execute the instructions. The number of arguments are correct,
     * and the type of the two first arguments are also correct.
     */
    switch(instruction) {
    default:
        fatal("Undefined instruction %s (%d)\n", get_f_name(instruction),
              instruction);
        /*NOTREACHED*/
bad_arg_1: bad_arg_pc(1, instruction, sp, pc);
bad_arg_2: bad_arg_pc(2, instruction, sp, pc);
bad_arg_3: bad_arg_pc(3, instruction, sp, pc);
bad_arg_4: bad_arg_pc(4, instruction, sp, pc);
bad_left:  ERRORF(("Bad left type to %s.\n",  get_f_name(instruction)))
bad_right: ERRORF(("Bad right type to %s.\n", get_f_name(instruction)))
        /*NOTREACHED*/
        return; /* hint for data flow analysis */
#ifdef F_ILLEGAL
case 255:
    CASE(F_ILLEGAL);
        inter_pc = pc;
        fatal("Illegal instruction\n");
        /*NOTREACHED*/
#endif /* F_ILLEGAL */
#ifdef F_SPRINTF
    CASE(F_SPRINTF);
    {
        char *s;

        /*
         * string_print_formatted() returns a pointer to it's internal
         * buffer, or to an internal constant...  Either way, it must
         * be copied before it's returned as a string.
         */

        assign_eval_cost();
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        TYPE_TEST1(sp - num_arg + 1, T_STRING)
        s = string_print_formatted((sp-num_arg+1)->u.string,
                                   num_arg-1, sp-num_arg+2);
        pop_n_elems(num_arg);
        if (!s) push_number(0);
        else push_malloced_string(string_copy(s));
        break;
    }
#endif /* F_SPRINTF */
#ifdef F_PRINTF
    CASE(F_PRINTF);
        assign_eval_cost();
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        TYPE_TEST1(sp - num_arg + 1, T_STRING)
        add_message("%s", string_print_formatted((sp-num_arg+1)->u.string,
                                                 num_arg-1, sp-num_arg+2));
        pop_n_elems(num_arg);
        break;
#endif /* F_PRINTF */
    CASE(F_REGEXP);
    {
        struct vector *v;
        TYPE_TEST1(sp-1, T_POINTER)
        TYPE_TEST2(sp,   T_STRING)
        v = match_regexp((sp-1)->u.vec, sp->u.string);
        pop_stack();
        free_svalue(sp);
        if (v == 0)
            put_number(0);
        else {
            put_referenced_vector(v);
        }
        break;
    }
    CASE(F_POP_VALUE);
        pop_stack();
        break;
    CASE(F_DUP);
        sp++;
        assign_svalue_no_free(sp, sp-1);
        break;
#ifdef F_JUMP_WHEN_ZERO
    CASE(F_JUMP_WHEN_ZERO);
    {
        unsigned short offset[2];

        ((char *)offset)[0] = pc[0];
        ((char *)offset)[1] = pc[1];
        if (sp->type == T_NUMBER && sp->u.number == 0)
            pc = current_prog->program + offset[0];
        else
            pc += 2;
        pop_stack();
        break;
    }
#endif /* F_JUMP_WHEN_ZERO */
#ifdef F_JUMP
    CASE(F_JUMP);
    {
        unsigned short offset[2];

        ((char *)offset)[0] = pc[0];
        ((char *)offset)[1] = pc[1];
        pc = current_prog->program + offset[0];
        break;
    }
#endif /* F_JUMP */
#ifdef F_JUMP_WHEN_NON_ZERO
    CASE(F_JUMP_WHEN_NON_ZERO);
    {
        unsigned short offset[2];

        ((char *)offset)[0] = pc[0];
        ((char *)offset)[1] = pc[1];
        if (sp->type == T_NUMBER && sp->u.number == 0)
            pc += 2;
        else
            pc = current_prog->program + offset[0];
        pop_stack();
        break;
    }
#endif /* F_JUMP_WHEN_NON_ZERO */
    CASE(F_LBRANCH);
    {
        short offset[2];

        ((char *)offset)[0] = pc[0];
        ((char *)offset)[1] = pc[1];
        pc += offset[0];
        break;
    }
    CASE(F_LBRANCH_WHEN_ZERO);
    {
        short offset[2];

        if (sp->type == T_NUMBER && sp->u.number == 0) {
            ((char *)offset)[0] = pc[0];
            ((char *)offset)[1] = pc[1];
            pc += offset[0];
            sp--;
            break;
        }
        pc += 2;
        pop_stack();
        break;
    }
    CASE(F_LBRANCH_WHEN_NON_ZERO);
    {
        short offset[2];

        if (sp->type != T_NUMBER || sp->u.number != 0) {
            ((char *)offset)[0] = pc[0];
            ((char *)offset)[1] = pc[1];
            pc += offset[0];
            pop_stack();
            break;
        }
        pc += 2;
        sp--;
        break;
    }
    CASE(F_BRANCH);
    {
        pc += EXTRACT_UCHAR(pc)+1;
        break;
    }
    CASE(F_BRANCH_WHEN_ZERO);
    {
        if (sp->type == T_NUMBER) {
            if (sp->u.number == 0) {
                sp--;
                pc += EXTRACT_UCHAR(pc) + 1;
                break;
            }
            sp--;
            pc++;
            break;
        } else {
            free_svalue(sp);
            sp--;
            pc++;
            break;
        }
    }
    CASE(F_BRANCH_WHEN_NON_ZERO);
    {
        if (sp->type == T_NUMBER) {
            if (sp->u.number == 0) {
                sp--;
                pc++;
                break;
            }
        } else {
            free_svalue(sp);
        }
        sp--;
        pc += EXTRACT_UCHAR(pc) + 1;
        break;
    }
    CASE(F_BBRANCH_WHEN_ZERO);
    {

        if (sp->type == T_NUMBER && sp->u.number == 0) {
            sp--;
            pc -= EXTRACT_UCHAR(pc);
            break;
        }
        pc += 1;
        pop_stack();
        break;
    }
    CASE(F_BBRANCH_WHEN_NON_ZERO);
    {

        if (sp->type == T_NUMBER) {
            if (sp->u.number == 0) {
                pc += 1;
                sp--;
                break;
            }
        } else free_svalue(sp);
        sp--;
        pc -= EXTRACT_UCHAR(pc);
        break;
    }
    CASE(F_LOR);
    {
        if (sp->type == T_NUMBER && sp->u.number == 0)
            sp--;
        else
            pc += EXTRACT_UCHAR(pc);
        pc++;
        break;
    }
    CASE(F_LAND);
    {
        if (sp->type == T_NUMBER) {
            if (sp->u.number == 0) {
                pc += EXTRACT_UCHAR(pc) + 1;
                break;
            }
        } else {
            free_svalue(sp);
        }
        sp--;
        pc++;
        break;
    }
#ifdef F_INDIRECT
    CASE(F_INDIRECT);
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            fatal("Bad type to F_INDIRECT\n");
#endif
        assign_svalue(sp, sp->u.lvalue);
        /*
         * Fetch value of a variable. It is possible that it is a variable
         * that points to a destructed object. In that case, it has to
         * be replaced by 0.
         */
        if (sp->type == T_OBJECT && (sp->u.ob->flags & O_DESTRUCTED)) {
            free_svalue(sp);
            *sp = const0;
        }
        break;
#endif /* F_INDIRECT */
    CASE(F_IDENTIFIER);
        /*
         * Fetch value of a variable. It is possible that it is a variable
         * that points to a destructed object. In that case, it has to
         * be replaced by 0.
         */
        sp++;
        assign_checked_svalue_no_free(sp, find_value((int)(EXTRACT_UCHAR(pc)
                                                )), sp, pc);
        pc++;
        break;
    CASE(F_PUSH_IDENTIFIER_LVALUE);
        sp++;
        sp->type = T_LVALUE;
        sp->u.lvalue = find_value((int)(EXTRACT_UCHAR(pc) ));
        pc++;
        break;
    CASE(F_VIRTUAL_VARIABLE);
        sp++;
        assign_checked_svalue_no_free(
          sp,
          find_virtual_value((int)(EXTRACT_UCHAR(pc))),
          sp, pc
        );
        pc++;
        break;
    CASE(F_PUSH_VIRTUAL_VARIABLE_LVALUE);
        sp++;
        sp->type = T_LVALUE;
        sp->u.lvalue = find_virtual_value((int)(EXTRACT_UCHAR(pc) ));
        pc++;
        break;
#ifdef F_IDENTIFIER16
    CASE(F_IDENTIFIER16);
    {
        unsigned short var_index[2];

        ((char *)var_index)[0] = pc[0];
        ((char *)var_index)[1] = pc[1];
        sp++;
        assign_checked_svalue_no_free(sp, find_value((int)(var_index[0])), sp, pc);
        pc += 2;
        break;
    }
    CASE(F_PUSH_IDENTIFIER16_LVALUE);
    {
        unsigned short var_index[2];

        ((char *)var_index)[0] = pc[0];
        ((char *)var_index)[1] = pc[1];
        sp++;
        sp->type = T_LVALUE;
        sp->u.lvalue = find_value((int)(var_index[0]));
        pc += 2;
        break;
    }
#endif /* F_IDENTIFIER16 */
    CASE(F_PUSH_INDEXED_LVALUE);
        sp = push_indexed_lvalue(sp, pc);
        break;
    CASE(F_PUSH_RINDEXED_LVALUE);
        sp = push_rindexed_lvalue(sp, pc);
        break;
    CASE(F_INDEX_LVALUE);
        sp = index_lvalue(sp, pc);
        break;
    CASE(F_RINDEX_LVALUE);
        sp = rindex_lvalue(sp, pc);
        break;
    CASE(F_INDEX);
        /*
         * Fetch value of a variable. It is possible that it is a variable
         * that points to a destructed object. In that case,
         * push_indexed_value will replace it by 0 .
         */
        sp = push_indexed_value(sp, pc);
        break;
    CASE(F_RINDEX);
        sp = push_rindexed_value(sp, pc);
        break;
    CASE(F_LOCAL);
        /*
         * Fetch value of a local variable. Unlike other variables,
         * it might be an argument to the current lfun, and thus contain
         * a volatile string or a reference.
         */
        sp++;
        assign_local_svalue_no_free(sp, fp + EXTRACT_UCHAR(pc), sp, pc);
        pc++;
        break;
    CASE(F_PUSH_LOCAL_VARIABLE_LVALUE);
        sp++;
        sp->type = T_LVALUE;
        sp->u.lvalue = fp + EXTRACT_UCHAR(pc);
        pc++;
        break;
    CASE(F_RETURN0);
        push_number(0);
    /* fall through */
    CASE(F_RETURN);
    {
        struct svalue *svp;

        svp = sp;
        /*
         * Deallocate frame and return.
         */
#ifdef DEBUG
        if (fp + csp->num_local_variables != sp)
            fatal("Bad stack at F_RETURN\n");
#endif
        while (sp != fp)
            free_svalue(--sp);
        *sp = *svp;        /* This way, the same ref counts are maintained */
        if ( NULL != (current_prog = csp->prog) ) /* is 0 when we reach the bottom */
            current_strings = current_prog->strings;
        function_index_offset = csp->function_index_offset;
        current_variables     = csp->current_variables;
        break_sp = csp->break_sp;
        if (csp->extern_call) {
            ASSIGN_EVAL_COST
            current_object = csp->ob;
            previous_ob = csp->prev_ob;
            inter_pc = csp->pc;
            inter_fp = csp->fp;
            if (trace_level) {
                do_trace_return(sp);
                if (csp == control_stack - 2)
                    traceing_recursion = -1;
            }
            csp--;
            inter_sp = sp;
            return;
        }
        if (trace_level)
            do_trace_return(sp);
        pc = csp->pc;
        fp = csp->fp;
        csp--;
        break;
    }
#ifdef F_BREAK_POINT
    CASE(F_BREAK_POINT);
        break_point(sp, fp);/* generated by lang.y when -d. Will check stack. */
        break;
#endif
#ifdef F_CLONE_OBJECT
    CASE(F_CLONE_OBJECT);
    {
        struct object *ob;

        assign_eval_cost();
        TYPE_TEST1(sp, T_STRING)
        inter_sp = sp;
        inter_pc = pc;
        ob = clone_object(sp->u.string);
        free_svalue(sp);
        if (ob) {
            sp->type = T_OBJECT;
            sp->u.ob = ob;
            add_ref(ob, "F_CLONE_OBJECT");
        } else {
            put_number(0);
        }
        break;
    }
#endif /* F_CLONE_OBJECT */
    CASE(F_AGGREGATE);
    {
        int i;
        struct vector *v;
        unsigned short num[2];
        struct svalue *value, *item;

        ((char *)num)[0] = pc[0];
        ((char *)num)[1] = pc[1];
        pc += 2;
        i = num[0];
        v = allocate_uninit_array(i);
        sp = value = sp - i + 1;
        item = v->item;
        while (--i >= 0)
            transfer_svalue_no_free_spc(item++, value++, sp, pc);
        sp->type = T_POINTER;
        sp->u.vec = v;                /* Ref count already initialized */
        break;
    }
#ifdef MAPPINGS
    CASE(F_M_AGGREGATE);
    {
        int i, j;
        struct mapping *m;
        struct svalue *data;
        int num_values;
        struct svalue *value;
        {
            unsigned short num[2];
            ((char *)num)[0] = pc[0];
            ((char *)num)[1] = pc[1];
            ((char *)num)[2] = pc[2];
            ((char *)num)[3] = pc[3];
            pc += 4;
            i = num[0];
            num_values = num[1];
        }
#ifndef _DCC /* DICE understands only a subset of C . */
        if (0) {
    CASE(F_M_CAGGREGATE);
            i = EXTRACT_UCHAR(pc);
            num_values = EXTRACT_UCHAR(pc+1);
            pc += 2;
        }
#endif /* _DCC */
        m = allocate_mapping(i, num_values);
        if (!m)
            ERROR("Out of memory\n")
        sp = value = sp - (i * (num_values+1)) + 1;
        while (--i >= 0) {
            data = get_map_lvalue(m, value, 1);
            free_svalue(value++);
            for (j = num_values; --j >= 0;) {
                if (data->type != T_NUMBER)
                    free_svalue(data);
                transfer_svalue_no_free_spc(data++, value++, sp, pc);
            }
        }
        sp->type = T_MAPPING;
        sp->u.map = m;
        break;
    }
#ifdef _DCC
    CASE(F_M_CAGGREGATE);
    {
        int i, j;
        struct mapping *m;
        struct svalue *data;
        int num_values;
        struct svalue *value;

        i = EXTRACT_UCHAR(pc);
        num_values = EXTRACT_UCHAR(pc+1);
        pc += 2;
        m = allocate_mapping(i, num_values);
        if (!m)
            ERROR("Out of memory\n")
        sp = value = sp - (i * (num_values+1)) + 1;
        while (--i >= 0) {
            data = get_map_lvalue(m, value, 1);
            free_svalue(value++);
            for (j = num_values; --j >= 0;) {
                if (data->type != T_NUMBER)
                    free_svalue(data);
                transfer_svalue_no_free_spc(data++, value++, sp, pc);
            }
        }
        sp->type = T_MAPPING;
        sp->u.map = m;
        break;
    }
#endif /* _DCC */
    CASE(F_FILTER_MAPPING);
        GET_NUM_ARG
        inter_pc = pc; /* apply_low() needs this */
        sp = filter_mapping(sp, num_arg);
        break;
    CASE(F_MAPPINGP);
    {
        int i;

        i = sp->type == T_MAPPING;
        free_svalue(sp);
        put_number(i);
        break;
    }
    CASE(F_MKMAPPING);
    {
        int i, length, num_values;
        struct mapping *m;
        struct svalue *key;

        GET_NUM_ARG
        length = MAX_ARRAY_SIZE;
        for (i = -num_arg; ++i <= 0; ) {
            if ( sp[i].type != T_POINTER )
                bad_arg_pc(i+num_arg, instruction, sp, pc);
            if (length > (int)VEC_SIZE(sp[i].u.vec))
                length = VEC_SIZE(sp[i].u.vec);
        }
        num_values = num_arg - 1;
        m = allocate_mapping(length, num_values);
        if (!m)
            ERROR("Out of memory\n")
        key = &(sp-num_values)->u.vec->item[length];
        while (--length >= 0) {
            struct svalue *dest;

            dest = get_map_lvalue(m, --key, 1);
            for (i = -num_values; ++i <= 0; ) {
                /* If a key value appears multiple times, we have to free
                 * a previous assigned value to avoid a memory leak
                 */
                assign_svalue(dest++, &sp[i].u.vec->item[length]);
            }
        }
        pop_n_elems(num_arg);
        push_mapping(m);
        sp->u.map->ref--; /* This will make ref count == 1 */
        break;
    }
    CASE(F_M_INDICES);
    {
        struct mapping *m;
        struct vector *v;

        TYPE_TEST1(sp, T_MAPPING)
        m = sp->u.map;
        check_map_for_destr(m);
        inter_pc = pc;
        inter_sp = sp;
        v = m_indices(m);
        free_mapping(m);
        put_referenced_vector(v);
        break;
    }
    CASE(F_M_VALUES);
    {
        struct mapping *m;
        struct vector *v;
        struct svalue *svp;
        mp_int size;

        if (sp->type != T_MAPPING || (m = sp->u.map)->num_values < 1)
            goto bad_arg_1;
        check_map_for_destr(m);
        size = m->condensed->string_size / sizeof(char *) +
               m->condensed->misc_size   / sizeof(struct svalue) +
              (m->hash ? m->hash->used - m->hash->condensed_deleted : 0);
        v = allocate_array(size);
        svp = v->item;
        walk_mapping(m, m_values_filter, (char *)&svp);
        free_mapping(m);
        put_referenced_vector(v);
        break;
    }
    CASE(F_M_DELETE);
    {
        struct mapping *m;

        TYPE_TEST1(sp-1, T_MAPPING)
        m = (sp-1)->u.map;
        remove_mapping(m, sp);
        pop_stack();
        /* leave the mapping unaltered on the stack */
        break;
    }
    CASE(F_MAP_MAPPING);
        GET_NUM_ARG
        inter_pc = pc; /* apply_low() needs this */
        sp = map_mapping(sp, num_arg);
        break;
#endif /* MAPPINGS */
    CASE(F_TAIL);
        assign_eval_cost();
        TYPE_TEST1(sp, T_STRING)
        inter_sp = sp;
        inter_pc = pc;
        if (tail(sp->u.string))
            assign_svalue(sp, &const1);
        else
            assign_svalue(sp, &const0);
        break;
    CASE(F_CALL_FUNCTION_BY_ADDRESS);
    {
        unsigned short func_index[2];
        unsigned short func_offset;
        uint32 flags;
        unsigned char *funstart;

        ((char *)func_index)[0] = pc[0];
        ((char *)func_index)[1] = pc[1];
        pc += 2;
        func_offset = func_index[0] + function_index_offset;
        /*
         * Find the function in the function table. As the function may have
         * been redefined by inheritance, we must look in the last table,
         * which is pointed to by current_object.
         */
#ifdef DEBUG
        if (func_offset >= current_object->prog->num_functions * sizeof(struct function))
            fatal("Illegal function index\n");
#endif

        /* NOT current_prog, which can be an inherited object. */
        flags = current_object->prog->functions[func_offset];
        if (flags & NAME_CROSS_DEFINED) {
            func_offset += (flags & INHERIT_MASK) - ( (INHERIT_MASK + 1) >> 1);
        }
        /* Save all important global stack machine registers */
        push_control_stack(sp, pc+1, fp);

        /* This assigment must be done after push_control_stack() */
        current_prog = current_object->prog;
        /*
         * If it is an inherited function, search for the real
         * definition.
         */
        csp->num_local_variables = EXTRACT_UCHAR(pc);
        flags = setup_new_frame1(func_offset, 0, 0);
        funstart = current_prog->program + (flags & FUNSTART_MASK);
        csp->funstart = funstart;
        sp = setup_new_frame2(funstart, sp);
        current_variables = current_object->variables + variable_index_offset;
        current_strings = current_prog->strings;
        fp = inter_fp;
        pc = funstart + 2;
        csp->extern_call = 0;
        break;
    }
    CASE(F_CALL_EXPLICIT_INHERITED);
    {
        unsigned short tmp_ushort[2];
        unsigned short prog_index, func_index;
        uint32 flags;
        unsigned char *funstart;
        struct inherit *inheritp;

        ((char *)tmp_ushort)[0] = pc[0];
        ((char *)tmp_ushort)[1] = pc[1];
        ((char *)tmp_ushort)[2] = pc[2];
        ((char *)tmp_ushort)[3] = pc[3];
        pc += 4;
        prog_index = tmp_ushort[0];
        func_index = tmp_ushort[1];
        inheritp = &current_prog->inherit[prog_index];
#ifdef DEBUG
        if (func_index >= inheritp->prog->num_functions) {
            fprintf(stderr, "program index : %d\n", prog_index);
            fprintf(stderr, "function index: %d\n", func_index);
            fprintf(stderr, "#functions    : %d\n", inheritp->prog->num_functions);
            fatal("Illegal function index\n");
        }
#endif

        /* Save all important global stack machine registers */
        push_control_stack(sp, pc+1, fp);

        /* if we do an explicit call into a virtually inherited base class we
         * have to look up the position of the virtual variables anew, this
         * cannot be done at compile time because it depends on the _object_
         * (i.e. the runtime environment) in which current_prog is running
         * TODO: A better compiler might do some backpatching and at least
         * TODO:: leave hints where the variables are, so that we can omit
         * TODO:: the explicite search.
         */
        if (current_prog != current_object->prog
         && inheritp->prog->num_variables
         && (current_prog->variable_names[inheritp->variable_index_offset 
                                          +inheritp->prog->num_variables-1
                                         ].flags & TYPE_MOD_VIRTUAL)
         && !(inheritp->prog->variable_names[inheritp->prog->num_variables-1
                                            ].flags & TYPE_MOD_VIRTUAL)
           ) 
        {
            /* now lookup the inheritp of the virtually inherited program
             * in the inherit list of the topmost program.
             */
            int i = current_object->prog->num_inherited;
            struct inherit *inh = current_object->prog->inherit;

            while (i)
            {
                if (inh->prog == inheritp->prog
                 && current_object->prog
                                  ->variable_names[inh->variable_index_offset
                                                  +inh->prog->num_variables-1
                                                  ].flags&TYPE_MOD_VIRTUAL
                   )
                    break;
                inh++;
                i--;
            }
            if (i)
            { 
                /* found, so adjust the inheritp and the offsets 
                 * to start with 
                 */
                inheritp = inh;
                current_variables = current_object->variables;
                function_index_offset = 0;
            }
#ifdef DEBUG
            else { /* this shouldn't happen! */
                fprintf(stderr, 
                        "Adjusting variable offsets because of virtual "
                        "inheritance for call from %s into %s (topmost "
                        "program %s) FAILED, please check the inherit "
                        "tree and report it.\n",
                        current_prog->name, inheritp->prog->name,
                        current_object->prog->name);
            }
#endif
        }

        /* This assigment must be done after push_control_stack() */
        current_prog = inheritp->prog;
        /*
         * If it is an inherited function, search for the real
         * definition.
         */
        csp->num_local_variables = EXTRACT_UCHAR(pc);
        flags = setup_new_frame1(
          func_index,
          function_index_offset + inheritp->function_index_offset,
          inheritp->variable_index_offset
        );
        funstart = current_prog->program + (flags & FUNSTART_MASK);
        csp->funstart = funstart;
        sp = setup_new_frame2(funstart, sp);
        fp = inter_fp;
        current_variables += variable_index_offset;
        pc = funstart + 2;
        current_strings = current_prog->strings;
        csp->extern_call = 0;
        break;
    }
    CASE(F_SAVE_OBJECT);
        assign_eval_cost();
        TYPE_TEST1(sp, T_STRING)
        inter_sp = sp;
        inter_pc = pc;
        save_object(current_object, sp->u.string);
        pop_stack();
        break;
    CASE(F_FIND_OBJECT);
    {
        struct object *ob;

        TYPE_TEST1(sp, T_STRING)
        ob = find_object(sp->u.string);
        free_svalue(sp);
        if (ob)
            put_object(ob);
        else
            put_number(0);
        break;
    }
#ifdef F_TERMINAL_COLOUR
    CASE(F_TERMINAL_COLOUR);
    {
      char *instr, *cp, *savestr = NULL, *deststr, **parts;
      int num, i, j, k, col, start, space, *lens, maybe_at_end;
      int space_garbage = 0;
      int wrap = 0;
      int indent = 0;
      int max_string_length = 200000; /* TODO: Make this a define */
      struct svalue * mdata;
      struct svalue mkey;

      GET_NUM_ARG;
      if ( num_arg >= 3 ) {
        if ( num_arg == 4 ) {
          TYPE_TEST4(sp, T_NUMBER );
          indent = (sp--)->u.number;
        }
        TYPE_TEST3(sp,T_NUMBER);
        wrap = (sp--)->u.number;
      }

      TYPE_TEST2(sp-1, T_STRING);
      TYPE_TEST1(sp, T_MAPPING);

      /* now tested for all types, stored wrap and indent and have the
       * string to process and the color data mapping at sp and sp-1
       */

      cp = instr = (sp-1)->u.string;
      do {
        cp = strchr(cp, TC_FIRST_CHAR);
        if (cp) {
          if (cp[1] == TC_SECOND_CHAR) {
            savestr = string_copy( instr );
            cp = savestr + ( cp - instr );
            instr = savestr;
            break;
          }
          cp++;
        }
      } while (cp);

      if (cp == NULL) {
        if (wrap) {
          num = 1;
          parts = CALLOCATE(1, char *);
          parts[0] = instr;
          savestr = NULL;
        } else {
          pop_stack(); 
            /* no delimiter in string and no wrapping, so return the original */
          break;
        }
      } else {
        /* here we have something to parse */

        parts = CALLOCATE( NSTRSEGS, char * );
        if (cp - instr) {       /* starting seg, if not delimiter */
          num = 1;
          parts[0] = instr;
          *cp = 0;
        } else
          num = 0;
        while (cp) {
          cp += 2;
          instr = cp;
          do {
            cp = strchr(cp,TC_FIRST_CHAR);
            if (cp) {
              if (cp[1] == TC_SECOND_CHAR)
                break;
              cp++;
            }
          } while (cp);
          if (cp) {
            *cp = 0;
            if (cp > instr) {
              parts[num] = instr;
              num++;
              if (num % NSTRSEGS == 0)
                parts = RESIZE(parts, num + NSTRSEGS, char * );
            }
          }
        }
        if (*instr)     /* trailing seg, if not delimiter */
          parts[num++] = instr;
      }

      /* Could keep track of the lens as we create parts, removing the need
       for a strlen() below */
      if ( num )
        lens = CALLOCATE(num, int);
      else
        lens = NULL;

      /* Do the the pointer replacement and calculate the lengths */
      col = 0;
      start = -1;
      space = 0;
      maybe_at_end = 0;
      for (j = i = 0; i < num; i++) {
        int len;

        /* Make an svalue from parts[i] (and get_map_lvalue() would make
         * it a shared string anyway).
         */
        mkey.type = T_STRING;
        mkey.x.string_type = STRING_SHARED;
        mkey.u.string = make_shared_string( parts[i] );

        /* now look for mapping data */
        mdata = get_map_lvalue( sp->u.map, & mkey, 0 );

        free_string(mkey.u.string);

        /* if it is a string, use it as part, if not, go on with the old */
        if ( mdata && mdata->type == T_STRING ) {
          parts[i] = mdata->u.string;
          len = svalue_strlen( mdata );
          if (wrap) len = -len;
        }
        else
          len = strlen( parts[i] );

        lens[i] = len;
        if (len > 0) {
          if (maybe_at_end) {
            if (j + indent > max_string_length) {
              /* this string no longer counts, so we are still in
                 a maybe_at_end condition.  This means we will end
                 up truncating the rest of the fragments too, since
                 the indent will never fit. */
              lens[i] = 0;
              len = 0;
            } else {
              j += indent;
              col += indent;
              maybe_at_end = 0;
            }
          }
          j += len;
          if (j > max_string_length) {
            lens[i] -= j - max_string_length;
            j = max_string_length;
          }
          if (wrap) {
            int z;
            char *p = parts[i];
            for (z = 0; z < lens[i]; z++) {
              char c = p[z];
              if (c == '\n') {
                col = 0;
                start = -1;
              } else {
                if (col > start || c != ' ')
                  col++;
                else
                  j--;

                if (c == ' ')
                  space = col;
                if (col == wrap+1) {
                  if (space) {
                    col -= space;
                    space = 0;
                  } else {
                    j++;
                    col = 1;
                  }
                  start = indent;
                } else
                  continue;
              }
              /* If we get here, we ended a line */
              if (col || z + 1 != lens[i]) {
                j += indent;
                col += indent;
              } else
                maybe_at_end = 1;

              if (j > max_string_length) {
                lens[i] -= (j - max_string_length);
                j = max_string_length;
                if (lens[i] < z) {
                  /* must have been ok
                     or we wouldn't be here */
                  lens[i] = z;
                  break;
                }
              }
            }
          }
        } else {
          j += -len;
          if (j > max_string_length) {
            lens[i] = -(-(lens[i]) - (j - max_string_length));
            j = max_string_length;
          }
        }
      }

      /* now we have the final string in parts and length in j.
         let's compose it, wrapping if necessary */
      cp = deststr = xalloc(j+1);
      if (wrap) {
        /* TODO: Fixed buffer size *ugh* */
        char *tmpmem = xalloc(8192);
        char *pt = tmpmem;

        col = 0;
        start = -1;
        space = 0;
        for (i = 0; i < num; i++) {
          int kind;
          int len;
          int l = lens[i];
          char *p = parts[i];
          if (l < 0) {
            memcpy(pt, p, -l);
            pt += -l;
            space_garbage += -l; /* Number of chars due to ignored junk
                                    since last space */
            continue;
          }
          for (k = 0; k < lens[i]; k++) {
            int n;
            char c = p[k];
            *pt++ = c;
            if (c == '\n') {
              col = 0;
              kind = 0;
              start = -1;
            } else {
              if (col > start || c != ' ')
                col++;
              else
                pt--;

              if (c == ' ') {
                space = col;
                space_garbage = 0;
              }
              if (col == wrap+1) {
                if (space) {
                  col -= space;
                  space = 0;
                  kind = 1;
                } else {
                  col = 1;
                  kind = 2;
                }
                start = indent;
              } else
                continue;
            }
            /* If we get here, we ended a line */
            len = (kind == 1 ? col + space_garbage : col);
            n = (pt - tmpmem) - len;
            memcpy(cp, tmpmem, n);
            cp += n;
            if (kind == 1) {
              /* replace the space */
              cp[-1] = '\n';
            }
            if (kind == 2) {
              /* need to insert a newline */
              *cp++ = '\n';
            }
            move_memory(tmpmem, tmpmem + n, len);
            pt = tmpmem + len;
            if (len > space_garbage || !at_end(i, num, k, lens)) {
              memset(cp, ' ', indent);
              cp += indent;
              col += indent;
            }
          }
        }
        memcpy(cp, tmpmem, pt - tmpmem);
        cp += pt - tmpmem;
        xfree(tmpmem);
      } else {
        for (i = 0; i < num; i++) {
          memcpy(cp, parts[i], lens[i]);
          cp += lens[i];
        }
      }
      *cp = 0;

      if ( lens )
        xfree(lens);
      if ( parts )
        xfree(parts);
      if (savestr)
        xfree(savestr);

      /* now we have what we want */
      pop_stack();
#ifdef DEBUG
      if (cp - deststr != j) {
        fatal("Length miscalculated in terminal_colour()\n"
              "    Expected: %i Was: %i\n"
              "    String: %s\n    Indent: %i Wrap: %i\n"
             , j, cp - deststr, sp->u.string, indent, wrap);
      }
#endif
      free_svalue(sp);
      put_malloced_string(deststr, sp);
      break;
    }
#endif
    CASE(F_WRITE_FILE);
    {
        int i;

        assign_eval_cost();
        TYPE_TEST1(sp-1, T_STRING)
        TYPE_TEST2(sp,   T_STRING)
        inter_sp = sp;
        inter_pc = pc;
        i = write_file((sp-1)->u.string, sp->u.string);
        pop_stack();
        free_svalue(sp);
        put_number(i);
        break;
    }
    CASE(F_READ_FILE);
    {
        char *str;
        struct svalue *arg;
        int start, len;

        assign_eval_cost();
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        arg = sp- num_arg + 1;
        TYPE_TEST1(arg,   T_STRING)

        start = 0; len = 0;
        if (num_arg > 1) {
            TYPE_TEST2(arg+1, T_NUMBER)
            start = arg[1].u.number;
            if (num_arg == 3) {
                if (arg[2].type != T_NUMBER)
                    goto bad_arg_3;
                len = arg[2].u.number;
                sp--;
            }
            sp--;
        }

        str = read_file(arg[0].u.string, start, len);
        pop_stack();
        if (str == 0)
            push_number(0);
        else {
            push_malloced_string(str);
        }
        break;
    }
    CASE(F_READ_BYTES);
    {
        char *str;
        struct svalue *arg;
        int start, len;

        assign_eval_cost();
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        arg = sp- num_arg + 1;
        TYPE_TEST1(arg,   T_STRING)

        start = 0; len = 0;
        if (num_arg > 1) {
            TYPE_TEST2(arg+1, T_NUMBER)
            start = arg[1].u.number;
            if (num_arg == 3) {
                if (arg[2].type != T_NUMBER)
                    goto bad_arg_2;
                len = arg[2].u.number;
                sp--;
            }
            sp--;
        }

        str = read_bytes(arg[0].u.string, start, len);
        pop_stack();
        if (str == 0)
            push_number(0);
        else {
            push_string_malloced(str);
            xfree(str);
        }
        break;
    }
    CASE(F_WRITE_BYTES);
    {
        int i;

        assign_eval_cost();
        TYPE_TEST1(sp-2, T_STRING)
        TYPE_TEST2(sp-1, T_NUMBER)
        inter_sp = sp;
        inter_pc = pc;
        if (sp->type != T_STRING)
            goto bad_arg_3;
        i = write_bytes((sp-2)->u.string, (sp-1)->u.number, sp->u.string);
        pop_stack();
        sp--;
        free_svalue(sp);
        put_number(i);
        break;
    }
    CASE(F_FILE_SIZE);
    {
        int i;

        assign_eval_cost();
        TYPE_TEST1(sp, T_STRING)
        inter_sp = sp;
        inter_pc = pc;
        i = file_size(sp->u.string);
        free_svalue(sp);
        put_number(i);
        break;
    }
    CASE(F_TELL_OBJECT);
        ASSIGN_EVAL_COST
        TYPE_TEST1(sp-1, T_OBJECT)
        TYPE_TEST2(sp,   T_STRING)
        inter_sp = sp;
        inter_pc = pc;
        tell_object((sp-1)->u.ob, sp->u.string);
        free_string_svalue(sp);
        sp--;
        if (sp->type == T_OBJECT) /* not self-destructed */
            free_object_svalue(sp);
        sp--;
        break;
    CASE(F_RESTORE_OBJECT);
    {
        int i;

        assign_eval_cost();
        TYPE_TEST1(sp, T_STRING)
        inter_sp = sp;
        inter_pc = pc;
        i = restore_object(current_object, sp->u.string);
        free_svalue(sp);
        put_number(i);
        break;
    }
    CASE(F_THIS_PLAYER);
        if (command_giver && !(command_giver->flags & O_DESTRUCTED))
            push_object(command_giver);
        else
            push_number(0);
        break;
    CASE(F_THIS_INTERACTIVE);
        if (current_interactive &&
            !(current_interactive->flags & O_DESTRUCTED))
            push_object(current_interactive);
        else
            push_number(0);
        break;
#ifdef F_FIRST_INVENTORY
    CASE(F_FIRST_INVENTORY);
    {
        struct object *ob;

        if (sp->type == T_OBJECT) {
            ob = sp->u.ob->contains;
            free_object_svalue(sp);
        } else if (sp->type == T_STRING) {
            inter_sp = sp;
            inter_pc = pc;
            ob = first_inventory(sp);
            free_string_svalue(sp);
        } else goto bad_arg_1;
        if (ob)
            put_object(ob);
        else
            put_number(0);
        break;
    }
#endif /* FIRST_INVENTORY */
    CASE(F_LIVING);
    {
        int i;

        if (sp->type != T_OBJECT) {
            if (sp->type == T_NUMBER && !sp->u.number)
                break;
            goto bad_arg_1;
        }
        i = (sp->u.ob->flags & O_ENABLE_COMMANDS) != 0;
        free_object_svalue(sp);
        put_number(i);
        break;
    }
#ifdef F_GETEUID
    CASE(F_GETEUID);
    {
        struct object *ob;

        /*
         * Are there any reasons to support this one in -o mode ?
         */
        TYPE_TEST1(sp, T_OBJECT)
        ob = sp->u.ob;

        if (ob->eff_user) {
            char *tmp;
            tmp = ob->eff_user->name;
            pop_stack();
            push_constant_string(tmp);
        }
        else {
            free_svalue(sp);
            put_number(0);
        }
        break;
    }
#endif /* F_GETEUID */
#ifdef F_EXPORT_UID
    CASE(F_EXPORT_UID);
    {
        struct object *ob;

        TYPE_TEST1(sp, T_OBJECT)
        if (current_object->eff_user == 0)
            ERROR("Illegal to export uid 0\n")
        ob = sp->u.ob;
        if (!ob->eff_user)        /* Only allowed to export when null */
            ob->user = current_object->eff_user;
        free_object(ob, "export_uid");
        sp--;
        break;
    }
#endif /* F_EXPORT_UID */
#ifdef F_SETEUID
    CASE(F_SETEUID);
    {
        struct svalue *ret;
        struct svalue *argp;

        argp = sp;
        if (argp->type == T_NUMBER) {
            if (argp->u.number != 0)
                goto bad_arg_1;
            current_object->eff_user = 0;
            free_svalue(argp);
            argp->type = T_NUMBER;
            argp->u.number = 1;
            break;
        }
        if (argp->type != T_STRING)
            goto bad_arg_1;
        assign_eval_cost();
        inter_sp = _push_volatile_string(argp->u.string,
            _push_valid_ob(current_object, sp) );
        inter_pc = pc;
        ret = apply_master_ob(STR_VALID_SETEUID, 2);
        if (ret == 0 || ret->type != T_NUMBER || ret->u.number != 1) {
            if (out_of_memory) {
                error("Out of memory\n");
            }
            free_svalue(argp);
            argp->type = T_NUMBER;
            argp->u.number = 0;
            break;
        }
        current_object->eff_user = add_name(argp->u.string);
        free_svalue(argp);
        argp->type = T_NUMBER;
        argp->u.number = 1;
        break;
    }
#endif /* F_SETEUID */
#ifdef F_SETUID
    CASE(F_SETUID)
        TYPE_TEST
        setuid();
        push_number(0);
        break;
#endif /* F_SETUID */
#if defined(F_GETUID) || defined(F_CREATOR)
#ifdef F_CREATOR
    CASE(F_CREATOR);
#else
    CASE(F_GETUID);
#endif
    {
        struct object *ob;
        char *name;

        TYPE_TEST1(sp, T_OBJECT)
        ob = sp->u.ob;
        decr_object_ref(ob, "getuid");
        if ( NULL != (name = ob->user->name) ) {
            sp->type = T_STRING;
            sp->x.string_type = STRING_SHARED;
            increment_string_ref(sp->u.string = name);
        } else {
            put_number(0);
        }
        break;
    }
#endif
    CASE(F_EXPLODE);
    {
        struct vector *v;
        TYPE_TEST1(sp-1, T_STRING)
        TYPE_TEST2(sp,   T_STRING)
        inter_sp = sp;
        inter_pc = pc;
#ifdef OLD_EXPLODE_BEHAVIOUR
        v = explode_string((sp-1)->u.string, sp->u.string);
#else
        v = new_explode_string((sp-1)->u.string, sp->u.string);
#endif
        free_string_svalue(sp);
        sp--;
        free_string_svalue(sp);
        put_referenced_vector(v);
        break;
    }
    CASE(F_FILTER_ARRAY);
    {
        GET_NUM_ARG
        inter_pc = pc;
        sp = f_filter_array(sp, num_arg);
        break;
    }
    CASE(F_SET_BIT);
    {
        char *str;
        int len, old_len, ind, bitnum;
        struct svalue *strp;

        TYPE_TEST1(sp-1, T_STRING)
        TYPE_TEST2(sp,   T_NUMBER)
        bitnum = sp->u.number;
        sp = strp = sp-1;
        if (bitnum > MAX_BITS)
            ERRORF(("set_bit: too big bit number: %d\n", bitnum))
        len = svalue_strlen(strp);
        old_len = len;
        ind = bitnum/6;
        if ( (ind < len || (len = ind + 1, MY_FALSE) ) &&
            strp->x.string_type == STRING_MALLOC )
        {
            str = strp->u.string;
        } else {
            str = xalloc(len+1);
            str[len] = '\0';
            if (old_len)
                memcpy(str, strp->u.string, old_len);
            if (len > old_len)
                memset(str + old_len, ' ', len - old_len);
            free_string_svalue(strp);
            strp->x.string_type = STRING_MALLOC;
            strp->u.string = str;
        }
        if (str[ind] > 0x3f + ' ' || str[ind] < ' ')
            ERRORF(("Illegal bit pattern in set_bit character %d\n", ind))
        str[ind] = ((str[ind] - ' ') | 1 << (bitnum % 6) ) + ' ';
        sp = strp;
        break;
    }
    CASE(F_CLEAR_BIT);
    {
        char *str;
        int len, ind, bitnum;
        struct svalue *strp;

        TYPE_TEST1(sp-1, T_STRING)
        TYPE_TEST2(sp,   T_NUMBER)
        bitnum = sp->u.number;
        sp = strp = sp-1;
        if (bitnum > MAX_BITS)
            ERRORF(("clear_bit: too big bit number: %d\n", bitnum))
        len = svalue_strlen(strp);
        ind = bitnum/6;
        if (ind >= len) {
            /* Return first argument unmodified ! */
            break;
        }
        if (strp->x.string_type == STRING_MALLOC) {
            str = strp->u.string;
        } else {
            str = xalloc(len+1);
            memcpy(str, strp->u.string, len+1);        /* Including null byte */
            free_string_svalue(strp);
            strp->x.string_type = STRING_MALLOC;
            strp->u.string = str;
        }
        if (str[ind] > 0x3f + ' ' || str[ind] < ' ')
            ERRORF(("Illegal bit pattern in clear_bit character %d\n", ind))
        str[ind] = ((str[ind] - ' ') & ~(1 << (bitnum % 6))) + ' ';
        break;
    }
    CASE(F_TEST_BIT);
    {
        int len;

        TYPE_TEST1(sp-1, T_STRING)
        TYPE_TEST2(sp,   T_NUMBER)
        len = svalue_strlen(sp-1);
        if (sp->u.number/6 >= len) {
            sp--;
            free_string_svalue(sp);
            put_number(0);
            break;
        }
        if ( ((sp-1)->u.string[sp->u.number/6] - ' ') & 1 << (sp->u.number % 6) ) {
            sp--;
            free_string_svalue(sp);
            put_number(1);
        } else {
            sp--;
            free_string_svalue(sp);
            put_number(0);
        }
        break;
    }
    CASE(F_QUERY_LOAD_AVERAGE);
        push_string_malloced(query_load_av());
        break;
    CASE(F_CATCH);
        /*
         * Catch/Throw - catch errors in system or other peoples routines.
         */
    {
        char *new_pc;

        /*
         * Compute address of next instruction after the CATCH statement.
         */
        new_pc = pc + 1 + EXTRACT_UCHAR(pc);
        pc += 1;

        eval_cost += CATCH_RESERVED_COST;
        push_control_stack(sp, new_pc, fp);
        csp->ob = current_object;
        csp->extern_call = 0;
#ifndef DEBUG
        csp->num_local_variables = 0;        /* No extra variables */
#else
        csp->num_local_variables = (csp-1)->num_local_variables; /* marion */
#endif
        csp->funstart = csp[-1].funstart;
        /*
         * Save some global variables that must be restored separately
         * after a longjmp. The stack will have to be manually popped all
         * the way.
         */

        /* signal catch OK - print no err msg */
        if ( setjmp( push_error_context(sp)->text ) ) {
            /*
             * They did a throw() or error. That means that the control
             * stack must be restored manually here.
             */
#ifdef DEBUG
            /*
             * Restore the value of expected_stack also. It is always 0
             * for catch().
             */
            expected_stack = 0;
#endif
            sp = pull_error_context (inter_sp);

            /* beware of errors after set_this_object() */
            current_object = csp->ob;

            pop_control_stack();
            pc = inter_pc;
            fp = inter_fp;
            *(++sp) = catch_value;
            catch_value.type = T_INVALID;
            eval_cost -= CATCH_RESERVED_COST;
            if (out_of_memory) {
                inter_sp = sp;
                error("Out of memory\n");
            }
        }
        break;
    }
    CASE(F_THROW);
        /* marion
         * the return from catch is now done by a 0 throw
         * amylaar: this is to dangerous, do it with a special END_CATCH code.
         */
        assign_eval_cost();
        transfer_svalue_no_free_spc(&catch_value, sp--, sp, pc);
        inter_sp = sp;
        inter_pc = pc;
        throw_error(); /* do the longjump, with extra checks... */
        break;
    CASE(F_NOTIFY_FAIL);
        if (sp->type != T_STRING && sp->type != T_CLOSURE)
            goto bad_arg_1;
        set_notify_fail_message(sp);
        sp--;
        break;
    CASE(F_INTERACTIVE);
    {
        int i;
        struct object *ob;
        struct interactive *ip;

        TYPE_TEST1(sp, T_OBJECT)
        ob = sp->u.ob;
        ip = O_GET_INTERACTIVE(ob);
        i = ip && ip->sent.type == SENT_INTERACTIVE && !ip->do_close;
        decr_object_ref(ob, "interactive");
        put_number(i);
        break;
    }
    CASE(F_IMPLODE);
    {
        char *str;
        TYPE_TEST1(sp-1, T_POINTER)
        TYPE_TEST2(sp,   T_STRING)
        str = implode_string((sp-1)->u.vec, sp->u.string);
        if (!str)
            ERROR("Out of memory\n")
        free_string_svalue(sp);
        sp--;
        free_vector(sp->u.vec);
        if (str) {
            sp->type = T_STRING;
            sp->x.string_type = STRING_MALLOC;
            sp->u.string = str;
        } else {
            put_number(0);
        }
        break;
    }
#ifdef F_NEXT_INVENTORY
    CASE(F_NEXT_INVENTORY);
    {
        struct object *ob;

        TYPE_TEST1(sp, T_OBJECT)
        ob = sp->u.ob;
        free_object(ob, "next_inventory");
        if (ob->next_inv)
            put_object(ob->next_inv);
        else
            put_number(0);
        break;
    }
#endif /* F_NEXT_INVENTORY */
    CASE(F_ALL_INVENTORY);
    {
        struct vector *vec;

        TYPE_TEST1(sp, T_OBJECT)
        inter_sp = sp;
        inter_pc = pc;
        vec = all_inventory(sp->u.ob);
        free_object_svalue(sp);
        if (vec == 0) {
            put_number(0);
        } else {
            sp->type  = T_POINTER;
            sp->u.vec = vec;                /* ref count is already 1 */
        }
        break;
    }
    CASE(F_DEEP_INVENTORY);
    {
        struct vector *vec;

        TYPE_TEST1(sp, T_OBJECT)
        inter_sp = sp;
        inter_pc = pc;
        vec = deep_inventory(sp->u.ob, 0);
        free_object_svalue(sp);
        sp->type = T_POINTER;
        sp->u.vec = vec;
        break;
    }
    CASE(F_ENVIRONMENT);
    {
        struct object *ob;

        GET_NUM_ARG
        if (num_arg) {
            if (sp->type == T_OBJECT) {
                ob = sp->u.ob->super;
                free_object_svalue(sp);
            } else if (sp->type == T_STRING) {
                inter_sp = sp;
                inter_pc = pc;
                ob = environment(sp);
                free_string_svalue(sp);
            } else
                goto bad_arg_1;
        } else if (!(current_object->flags & O_DESTRUCTED)) {
            ob = current_object->super;
            sp++;
        } else {
            ob = 0; /* != environment(this_object()) *boggle* */
            sp++;
        }
        if (ob)
            put_object(ob);
        else
            put_number(0);
        break;
    }
    CASE(F_THIS_OBJECT);
        if (current_object->flags & O_DESTRUCTED) {
            /* No diagnostic of this case anymore */
            push_number(0);
            break;
        }
        push_object(current_object);
        break;
    CASE(F_PREVIOUS_OBJECT0);
        if (previous_ob == 0 || (previous_ob->flags & O_DESTRUCTED))
            push_number(0);
        else
            push_object(previous_ob);
        break;
#ifdef F_QUERY_ACTIONS
    CASE(F_QUERY_ACTIONS);
    {
        struct vector *v;
        struct svalue *arg;
        struct object *ob;

        arg = sp - 1;
        inter_sp = sp;
        inter_pc = pc;
        if (arg[0].type == T_OBJECT)
            ob = arg[0].u.ob;
        else
        {
            TYPE_TEST1(arg, T_STRING);
            ob = get_object(arg[0].u.string);
            if (ob == 0)
                error("query_actions() failed\n");
        }
        if (arg[1].type == T_STRING)
            v = get_action(ob, arg[1].u.string);
        else if (arg[1].type == T_NUMBER)
            v = get_all_actions(ob, arg[1].u.number);
        else {
            TYPE_TEST2(arg+1, T_OBJECT);
            v = get_object_actions(ob, arg[1].u.ob);
        }
        pop_stack();
        free_svalue(arg);
        if (v) {
            arg->type  = T_POINTER;
            arg->u.vec = v;
        } else
            put_number(0);
        break;
    }
#endif /* F_QUERY_ACTIONS */
    CASE(F_TIME);
        push_number(current_time);
        break;
#ifdef F_TRANSFER
    CASE(F_TRANSFER);
    {
        int i;
        struct object *dest;

        assign_eval_cost();
        TYPE_TEST1(sp-1, T_OBJECT)
        inter_sp = sp;
        inter_pc = pc;
        if (sp->type == T_OBJECT) {
            dest = sp->u.ob;
        } else if (sp->type == T_STRING) {
            dest = get_object(sp->u.string);
            if (dest == 0)
                ERROR("Object not found.\n")
            free_string_svalue(sp);
            put_object(dest);
        } else goto bad_arg_2;
        i = transfer_object(sp-1);
        if (i) {
            /* the objects on the stack might have been changed into 0 if
             * destructed, thus free_object_svalue is not applicable.
             */
            free_svalue(sp);
            free_svalue(sp-1);
        }
        sp--;
        put_number(i);
        break;
    }
#endif
    CASE(F_ADD);
        switch ( sp[-1].type ) {
          case T_STRING:
            switch ( sp->type ) {
              case T_STRING:
              {
                char *res;
                int l = _svalue_strlen(sp-1);
                res = xalloc(l + _svalue_strlen(sp) + 1);
                if (!res)
                    ERROR("Out of memory\n")
                (void)strcpy(res, (sp-1)->u.string);
                (void)strcpy(res+l, sp->u.string);
                free_string_svalue(sp);
                sp--;
                free_string_svalue(sp);
                put_malloced_string(res, sp);
                break;
              }
              case T_NUMBER:
              {
                char buff[20];
                char *res;
                int len1;

                sprintf(buff, "%ld", sp->u.number);
                res = xalloc((len1 = svalue_strlen(sp-1)) + strlen(buff) + 1);
                if (!res)
                    ERROR("Out of memory\n")
                strcpy(res, (sp-1)->u.string);
                strcpy(res+len1, buff);
                pop_n_elems(2);
                push_malloced_string(res);
                break;
              }
#ifdef FLOATS
              case T_FLOAT:
              {
                char buff[42];
                char *res;
                int len1;

                sprintf(buff, "%g", READ_DOUBLE( sp ) );
                res = xalloc((len1 = svalue_strlen(sp-1)) + strlen(buff) + 1);
                if (!res)
                    ERROR("Out of memory\n")
                strcpy(res, (sp-1)->u.string);
                strcpy(res+len1, buff);
                sp--;
                free_string_svalue(sp);
                put_malloced_string(res, sp);
                break;
              }
#endif
              default:
                goto bad_add;
            }
            break;
          case T_NUMBER:
            switch ( sp->type ) {
              case T_STRING:
              {
                char buff[20], *res;
                int len1;

                sprintf(buff, "%ld", (sp-1)->u.number);
                res = xalloc(svalue_strlen(sp) + (len1 = strlen(buff)) + 1);
                if (!res)
                    ERROR("Out of memory\n")
                strcpy(res, buff);
                strcpy(res+len1, sp->u.string);
                free_string_svalue(sp);
                sp--;
                put_malloced_string(res, sp);
                break;
              }
              case T_NUMBER:
              {
                int i;
                i = sp->u.number + (sp-1)->u.number;
                sp--;
                sp->u.number = i;
                break;
              }
#ifdef FLOATS
              case T_FLOAT:
              {
                STORE_DOUBLE_USED
                double sum;

                sum = (double)((sp-1)->u.number) + READ_DOUBLE(sp);
                STORE_DOUBLE(sp-1, sum);
                sp--;
                sp->type = T_FLOAT;
                break;
              }
#endif /* FLOATS */
              default:
                goto bad_add;
            }
            break;
          case T_POINTER:
          {
            if (sp->type != T_POINTER) goto bad_add;
            inter_sp = sp;
            inter_pc = pc;
            (void)inter_add_array(sp->u.vec, &(sp-1)->u.vec);
            sp--;
            break;
          }
#ifdef FLOATS
          case T_FLOAT:
          {
            STORE_DOUBLE_USED
            double sum;

            if (sp->type == T_FLOAT) {
                sum = READ_DOUBLE(sp-1) + READ_DOUBLE(sp);
                STORE_DOUBLE(sp-1, sum);
                sp--;
                break;
            }
            if (sp->type == T_NUMBER) {
                sum = READ_DOUBLE(sp-1) + (double)(sp->u.number);
                STORE_DOUBLE(sp-1, sum);
                sp--;
                break;
            }
            if (sp->type == T_STRING) {
                char buff[42];
                char *res;
                int len1;

                sprintf(buff, "%g", READ_DOUBLE(sp-1) );
                res = xalloc(svalue_strlen(sp) + (len1 = strlen(buff)) + 1);
                if (!res)
                    error("Out of memory\n");
                strcpy(res, buff);
                strcpy(res+len1, (sp)->u.string);
                free_string_svalue(sp);
                sp--;
                put_malloced_string(res, sp);
                break;
            }
            goto bad_add;
          }
#endif /* FLOATS */
#ifdef MAPPINGS
          case T_MAPPING:
          {
            struct mapping *m;

            if (sp->type != T_MAPPING) goto bad_add;
            check_map_for_destr((sp-1)->u.map);
            check_map_for_destr(sp->u.map);
            m = add_mapping((sp-1)->u.map,sp->u.map);
            if (!m) {
                ERROR("Out of memory\n")
            }
            pop_n_elems(2);
            push_mapping(m); /* This will make ref count == 2 */
            m->ref--;
            break;
          }
#endif /* MAPPINGS */
          default:
        bad_add:
            ERROR("Bad type of arg to '+'\n")
            /* not reached */
        }
        break;
    CASE(F_SUBTRACT);
    {
        int i;

        if ((sp-1)->type == T_NUMBER) {
            if (sp->type == T_NUMBER) {
                i = (sp-1)->u.number - sp->u.number;
                sp--;
                sp->u.number = i;
                break;
            }
#ifdef FLOATS
            if (sp->type == T_FLOAT) {
                STORE_DOUBLE_USED
                double diff;

                diff = (double)((sp-1)->u.number) - READ_DOUBLE(sp);
                sp--;
                STORE_DOUBLE(sp, diff);
                sp->type = T_FLOAT;
                break;
            }
#endif /* FLOATS */
        }
#ifdef FLOATS
        else if ((sp-1)->type == T_FLOAT) {
            STORE_DOUBLE_USED
            double diff;

            if (sp->type == T_FLOAT) {
                diff = READ_DOUBLE(sp-1) - READ_DOUBLE(sp);
                sp--;
                STORE_DOUBLE(sp, diff);
                break;
            }
            if (sp->type == T_NUMBER) {
                diff = READ_DOUBLE(sp-1) - (double)(sp->u.number);
                sp--;
                STORE_DOUBLE(sp, diff);
                break;
            }
        }
#endif /* FLOATS */
        else if ((sp-1)->type == T_POINTER && sp->type == T_POINTER) {
            struct vector *v;

            v = sp->u.vec;
            if (v->ref > 1) {
                v->ref--;
                v = slice_array(v, 0, VEC_SIZE(v) - 1 );
            }
            sp--;
            /* subtract_array already takes care of destructed objects */
            v = subtract_array(sp->u.vec, v);
            free_vector(subtract_array_tmp_vec);
            free_vector(sp->u.vec);
            sp->u.vec = v;
            break;
#ifdef MAPPINGS
        } else if ((sp-1)->type == T_MAPPING &&
                   sp->type == T_MAPPING &&
                   !sp->u.map->num_values)
        {
            struct mapping *m;

            m = subtract_mapping(sp[-1].u.map, sp->u.map);
            free_mapping(sp->u.map);
            sp--;
            free_mapping(sp->u.map);
            sp->u.map = m;
            break;
#endif
        } else goto bad_arg_1;
        goto bad_arg_2;
    }
    CASE(F_AND);
    {
        int i;

        if (sp->type == T_POINTER && (sp-1)->type == T_POINTER) {
            inter_sp = sp - 2;
            (sp-1)->u.vec = intersect_array(sp->u.vec, (sp-1)->u.vec);
            sp--;
            break;
        }
        if ((sp-1)->type != T_NUMBER)
            goto bad_arg_1;
        if (sp->type != T_NUMBER)
            goto bad_arg_2;
        i = (sp-1)->u.number & sp->u.number;
        sp--;
        sp->u.number = i;
        break;
    }
    CASE(F_OR);
    {
        int i;

        if ((sp-1)->type != T_NUMBER)
            goto bad_arg_1;
        if (sp->type != T_NUMBER)
            goto bad_arg_2;
        i = (sp-1)->u.number | sp->u.number;
        sp--;
        sp->u.number = i;
        break;
    }
    CASE(F_XOR);
    {
        int i;

        if ((sp-1)->type != T_NUMBER)
            goto bad_arg_1;
        if (sp->type != T_NUMBER)
            goto bad_arg_2;
        i = (sp-1)->u.number ^ sp->u.number;
        sp--;
        sp->u.number = i;
        break;
    }
    CASE(F_LSH);
    {
        int i;

        if ((sp-1)->type != T_NUMBER)
            goto bad_arg_1;
        if (sp->type != T_NUMBER)
            goto bad_arg_2;
        i = sp->u.number;
        sp--;
        sp->u.number = (uint)i > MAX_SHIFT ? 0 : sp->u.number << i;
        break;
    }
    CASE(F_RSH);
    {
        int i;

        if ((sp-1)->type != T_NUMBER)
            goto bad_arg_1;
        if (sp->type != T_NUMBER)
            goto bad_arg_2;
        i = sp->u.number;
        sp--;
        sp->u.number >>= (uint)i > MAX_SHIFT ? MAX_SHIFT : i;
        break;
    }
    CASE(F_MULTIPLY);
    {
        int i;

        switch ( sp[-1].type ) {
        case T_NUMBER:
            if (sp->type == T_NUMBER) {
                i = (sp-1)->u.number * sp->u.number;
                sp--;
                sp->u.number = i;
                break;
            }
#ifdef FLOATS
            if (sp->type == T_FLOAT) {
                STORE_DOUBLE_USED
                double product;

                product = (sp-1)->u.number * READ_DOUBLE(sp);
                sp--;
                STORE_DOUBLE(sp, product);
                sp->type = T_FLOAT;
                break;
            }
#endif /* FLOATS */
            goto bad_arg_2;
#ifdef FLOATS
        case T_FLOAT:
          {
            STORE_DOUBLE_USED
            double product;

            if (sp->type == T_FLOAT) {
                product = READ_DOUBLE(sp-1) * READ_DOUBLE(sp);
                STORE_DOUBLE(sp-1, product);
                sp--;
                break;
            }
            if (sp->type == T_NUMBER) {
                product = READ_DOUBLE(sp-1) * sp->u.number;
                STORE_DOUBLE(sp-1, product);
                sp--;
                break;
            }
            goto bad_arg_2;
          }
#endif /* FLOATS */
        default:
            goto bad_arg_1;
        }
        break;
    }
    CASE(F_DIVIDE);
    {
        int i;

        if ((sp-1)->type == T_NUMBER) {
            if (sp->type == T_NUMBER) {
                if (sp->u.number == 0)
                    ERROR("Division by zero\n")
                i = (sp-1)->u.number / sp->u.number;
                sp--;
                sp->u.number = i;
                break;
            }
#ifdef FLOATS
            if (sp->type == T_FLOAT) {
                double dtmp;
                STORE_DOUBLE_USED

                dtmp = READ_DOUBLE( sp );
                if (dtmp == 0.)
                    ERROR("Division by zero\n")
                sp--;
                dtmp = sp->u.number / dtmp;
                STORE_DOUBLE(sp, dtmp);
                sp->type = T_FLOAT;
                break;
            }
#endif /* FLOATS */
            goto bad_arg_2;
#ifdef FLOATS
        } else if ((sp-1)->type == T_FLOAT) {
            double dtmp;
            STORE_DOUBLE_USED

            if (sp->type == T_FLOAT) {
                dtmp = READ_DOUBLE( sp );
                if (dtmp == 0.) {
                    ERROR("Division by zero\n")
                    return;
                }
                sp--;
                dtmp = READ_DOUBLE(sp) / dtmp;
                STORE_DOUBLE(sp, dtmp);
                break;
            }
            if (sp->type == T_NUMBER) {
                if (sp->u.number == 0) {
                    ERROR("Division by zero\n")
                    return;
                }
                dtmp = (float)sp->u.number;
                sp--;
                dtmp = READ_DOUBLE(sp) / dtmp;
                STORE_DOUBLE(sp, dtmp);
                break;
            }
            goto bad_arg_2;
#endif /* FLOATS */
        }
        goto bad_arg_1;
        break;
    }
    CASE(F_MOD);
    {
        int i;

        if ((sp-1)->type != T_NUMBER)
            goto bad_arg_1;
        if (sp->type != T_NUMBER)
            goto bad_arg_2;
        if (sp->u.number == 0) {
            ERROR("Modulus by zero.\n")
            return;
        }
        i = (sp-1)->u.number % sp->u.number;
        sp--;
        sp->u.number = i;
        break;
    }
    CASE(F_GT);
    {
        int i;

        if ((sp-1)->type == T_STRING && sp->type == T_STRING) {
            i = strcmp((sp-1)->u.string, sp->u.string) > 0;
            free_string_svalue(sp);
            sp--;
            free_string_svalue(sp);
            put_number(i);
            break;
        }
        if ((sp-1)->type == T_NUMBER && sp->type == T_NUMBER) {
            i = (sp-1)->u.number > sp->u.number;
            sp--;
            sp->u.number = i;
            break;
        }
#ifdef FLOATS
        if ((sp-1)->type == T_FLOAT && sp->type == T_FLOAT) {
            i = READ_DOUBLE( sp-1 ) > READ_DOUBLE( sp );
            sp--;
            sp->type = T_NUMBER;
            sp->u.number = i;
            break;
        }
#endif /* FLOATS */
        if (!( (sp-1)->type & (T_NUMBER|T_STRING|T_FLOAT) ))
            goto bad_arg_1;
        if (!(  sp   ->type & (T_NUMBER|T_STRING|T_FLOAT) ))
            goto bad_arg_2;
        ERROR("Arguments to > don't match\n")
    }
    CASE(F_GE);
    {
        int i;

        if ((sp-1)->type == T_STRING && sp->type == T_STRING) {
            i = strcmp((sp-1)->u.string, sp->u.string) >= 0;
            free_string_svalue(sp);
            sp--;
            free_string_svalue(sp);
            put_number(i);
            break;
        }
        if ((sp-1)->type == T_NUMBER && sp->type == T_NUMBER) {
            i = (sp-1)->u.number >= sp->u.number;
            sp--;
            sp->u.number = i;
            break;
        }
#ifdef FLOATS
        if ((sp-1)->type == T_FLOAT && sp->type == T_FLOAT) {
            i = READ_DOUBLE( sp-1 ) >= READ_DOUBLE( sp );
            sp--;
            sp->type = T_NUMBER;
            sp->u.number = i;
            break;
        }
#endif /* FLOATS */
        if (!( (sp-1)->type & (T_NUMBER|T_STRING|T_FLOAT) ))
            goto bad_arg_1;
        if (!(  sp   ->type & (T_NUMBER|T_STRING|T_FLOAT) ))
            goto bad_arg_2;
        ERROR("Arguments to >= don't match\n")
    }
    CASE(F_LT);
    {
        int i;

        if ((sp-1)->type == T_STRING && sp->type == T_STRING) {
            i = strcmp((sp-1)->u.string, sp->u.string) < 0;
            free_string_svalue(sp);
            sp--;
            free_string_svalue(sp);
            put_number(i);
            break;
        }
        if ((sp-1)->type == T_NUMBER && sp->type == T_NUMBER) {
            i = (sp-1)->u.number < sp->u.number;
            sp--;
            sp->u.number = i;
            break;
        }
#ifdef FLOATS
        if ((sp-1)->type == T_FLOAT && sp->type == T_FLOAT) {
            i = READ_DOUBLE( sp-1 ) < READ_DOUBLE( sp );
            sp--;
            sp->type = T_NUMBER;
            sp->u.number = i;
            break;
        }
#endif /* FLOATS */
        if (!( (sp-1)->type & (T_NUMBER|T_STRING|T_FLOAT) ))
            goto bad_arg_1;
        if (!(  sp   ->type & (T_NUMBER|T_STRING|T_FLOAT) ))
            goto bad_arg_2;
        ERROR("Arguments to < don't match\n")
    }
    CASE(F_LE);
    {
        int i;

        if ((sp-1)->type == T_STRING && sp->type == T_STRING) {
            i = strcmp((sp-1)->u.string, sp->u.string) <= 0;
            free_string_svalue(sp);
            sp--;
            free_string_svalue(sp);
            put_number(i);
            break;
        }
        if ((sp-1)->type == T_NUMBER && sp->type == T_NUMBER) {
            i = (sp-1)->u.number <= sp->u.number;
            sp--;
            sp->u.number = i;
            break;
        }
#ifdef FLOATS
        if ((sp-1)->type == T_FLOAT && sp->type == T_FLOAT) {
            i = READ_DOUBLE( sp-1 ) <= READ_DOUBLE( sp );
            sp--;
            sp->type = T_NUMBER;
            sp->u.number = i;
            break;
        }
#endif /* FLOATS */
        if (!( (sp-1)->type & (T_NUMBER|T_STRING|T_FLOAT) ))
            goto bad_arg_1;
        if (!(  sp   ->type & (T_NUMBER|T_STRING|T_FLOAT) ))
            goto bad_arg_2;
        ERROR("Arguments to <= don't match\n")
    }
    CASE(F_EQ);
    {
        int i;

        if ((sp-1)->type != sp->type) {
            pop_stack();
            free_svalue(sp);
            put_number(0);
            break;
        }
        switch(sp->type) {
          case T_NUMBER:
            i = (sp-1)->u.number == sp->u.number;
            break;
          case T_POINTER:
            i = (sp-1)->u.vec == sp->u.vec;
            break;
          case T_STRING:
            i = strcmp((sp-1)->u.string, sp->u.string) == 0;
            break;
          case T_OBJECT:
            i = (sp-1)->u.ob == sp->u.ob;
            break;
#ifdef FLOATS
          case T_FLOAT:
            /* This is of little use... well, at least 0. == 0. ... */
#endif
          case T_CLOSURE:
          case T_SYMBOL:
          case T_QUOTED_ARRAY:
            i = (sp-1)->u.string  == sp->u.string &&
                (sp-1)->x.generic == sp->x.generic;
            break;
#ifdef MAPPINGS
          case T_MAPPING:
            i = (sp-1)->u.map == sp->u.map;
            break;
#endif
          default:
            i = 0;
            break;
        }
        pop_stack();
        free_svalue(sp);
        put_number(i);
        break;
    }
    CASE(F_NE);
    {
        int i;

        if ((sp-1)->type != sp->type) {
            pop_stack();
            assign_svalue(sp, &const1);
            break;
        }
        switch(sp->type) {
          case T_NUMBER:
            i = (sp-1)->u.number != sp->u.number;
            break;
          case T_STRING:
            i = strcmp((sp-1)->u.string, sp->u.string);
            break;
          case T_POINTER:
            i = (sp-1)->u.vec != sp->u.vec;
            break;
          case T_OBJECT:
            i = (sp-1)->u.ob != sp->u.ob;
            break;
#ifdef FLOATS
          case T_FLOAT:
            /* This is of little use... well, at least 0. == 0. ... */
#endif
          case T_CLOSURE:
          case T_SYMBOL:
          case T_QUOTED_ARRAY:
            i = (sp-1)->u.string  != sp->u.string ||
                (sp-1)->x.generic != sp->x.generic;
            break;
#ifdef MAPPINGS
          case T_MAPPING:
            i = (sp-1)->u.map != sp->u.map;
            break;
#endif
          default:
            if (sp->type == T_LVALUE)
                error("Reference passed to !=\n");
            fatal("Illegal type to !=\n");
            return;
        }
        pop_stack();
        free_svalue(sp);
        put_number(i);
        break;
    }
    CASE(F_NOT);
        if (sp->type == T_NUMBER) {
            if (sp->u.number == 0) {
                sp->u.number = 1;
                break;
            }
        } else
            free_svalue(sp);
        put_number(0);
        break;
    CASE(F_COMPL);
        if (sp->type != T_NUMBER)
            ERROR("Bad argument to ~\n")
        sp->u.number = ~ sp->u.number;
        break;
    CASE(F_NEGATE);
#ifdef FLOAT_FORMAT_1
        if (sp->type == T_NUMBER) {
            sp->u.number = - sp->u.number;
            break;
        } else if (sp->type == T_FLOAT) {
            sp->u.mantissa ^= 0x80000000;
            break;
        }
#else
        if (sp->type == T_NUMBER || sp->type == T_FLOAT) {
            sp->u.number = - sp->u.number;
            break;
        }
#endif
        ERROR("Bad argument to unary minus\n")
    CASE(F_PRE_INC);
    {
        struct svalue *svp;

#ifdef DEBUG
        if (sp->type != T_LVALUE)
            ERROR("Bad argument to ++\n")
#endif
        svp = sp->u.lvalue;
        if (svp->type == T_NUMBER) {
            put_number( ++(svp->u.number) );
            break;
        } else if (svp->type == T_CHAR_LVALUE) {
            put_number( ++(*svp->u.string) );
            break;
        } else if (svp->type == T_LVALUE ||
                   svp->type == T_PROTECTED_LVALUE)
        {
            inter_sp = sp;
            put_number(add_number_to_svalue(svp, 1));
            break;
        }
        ERROR("++ of non-numeric argument\n")
        break;
    }
    CASE(F_PRE_DEC);
    {
        struct svalue *svp;

#ifdef DEBUG
        if (sp->type != T_LVALUE)
            ERROR("Bad argument to --\n")
#endif
        svp = sp->u.lvalue;
        if (svp->type == T_NUMBER) {
            put_number( --(svp->u.number) );
            break;
        } else if (svp->type == T_CHAR_LVALUE) {
            put_number( --(*svp->u.string) );
            break;
        } else if (svp->type == T_LVALUE ||
                   svp->type == T_PROTECTED_LVALUE)
        {
            inter_sp = sp;
            put_number(add_number_to_svalue(svp, -1));
            break;
        }
        ERROR("-- of non-numeric argument\n")
        break;
    }
    CASE(F_INC);
    {
        struct svalue *svp;

#ifdef DEBUG
        if (sp->type != T_LVALUE)
            ERROR("Bad argument to ++\n")
#endif
        svp = sp->u.lvalue;
        if (svp->type == T_NUMBER) {
            svp->u.number++;
            sp--;
            break;
        } else if (svp->type == T_CHAR_LVALUE) {
            (*svp->u.string)++;
            sp--;
            break;
        } else if (svp->type == T_LVALUE ||
                   svp->type == T_PROTECTED_LVALUE)
        {
            inter_sp = sp;
            add_number_to_svalue(svp, 1);
            sp--;
            break;
        }
        ERROR("++ of non-numeric argument\n")
        break;
    }
    CASE(F_DEC);
    {
        struct svalue *svp;

#ifdef DEBUG
        if (sp->type != T_LVALUE)
            ERROR("Bad argument to --\n")
#endif
        svp = sp->u.lvalue;
        if (svp->type == T_NUMBER) {
            svp->u.number--;
            sp--;
            break;
        } else if (svp->type == T_CHAR_LVALUE) {
            (*svp->u.string)--;
            sp--;
            break;
        } else if (svp->type == T_LVALUE ||
                   svp->type == T_PROTECTED_LVALUE)
        {
            inter_sp = sp;
            add_number_to_svalue(svp, -1);
            sp--;
            break;
        }
        ERROR("-- of non-numeric argument\n")
        break;
    }
    CASE(F_POST_INC);
    {
        struct svalue *svp;

#ifdef DEBUG
        if (sp->type != T_LVALUE)
            ERROR("Bad argument to ++\n")
#endif
        svp = sp->u.lvalue;
        if (svp->type == T_NUMBER) {
            put_number( svp->u.number++ );
            break;
        } else if (svp->type == T_CHAR_LVALUE) {
            put_number( (*svp->u.string)++ );
            break;
        } else if (svp->type == T_LVALUE ||
                   svp->type == T_PROTECTED_LVALUE)
        {
            inter_sp = sp;
            put_number(add_number_to_svalue(svp, 1) - 1);
            break;
        }
        ERROR("++ of non-numeric argument\n")
        break;
    }
    CASE(F_POST_DEC);
    {
        struct svalue *svp;

#ifdef DEBUG
        if (sp->type != T_LVALUE)
            ERROR("Bad argument to --\n")
#endif
        svp = sp->u.lvalue;
        if (svp->type == T_NUMBER) {
            put_number( svp->u.number-- );
            break;
        } else if (svp->type == T_CHAR_LVALUE) {
            put_number( (*svp->u.string)-- );
            break;
        } else if (svp->type == T_LVALUE ||
                   svp->type == T_PROTECTED_LVALUE)
        {
            inter_sp = sp;
            put_number(add_number_to_svalue(svp, -1) + 1);
            break;
        }
        ERROR("-- of non-numeric argument\n")
        break;
    }
    CASE(F_CALL_OTHER);
    {
        struct svalue *arg;
        struct object *ob;

        ASSIGN_EVAL_COST
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        arg = sp - num_arg + 1;
        if (arg[0].type == T_OBJECT)
            ob = arg[0].u.ob;
        else if (arg[0].type == T_STRING) {
            ob = get_object(arg[0].u.string);
            if (ob == 0)
                ERROR("call_other() failed\n")
        } else goto bad_arg_1;
        TYPE_TEST2(arg+1, T_STRING)
        if (current_object->flags & O_DESTRUCTED) {
            /*
             * No external calls may be done when this object is
             * destructed.
             */
            pop_n_elems(num_arg);
            push_number(0);
            break;
        }
        if (arg[1].u.string[0] == ':')
            ERRORF(("Illegal function name in call_other: %s\n",
                  arg[1].u.string))
        /*
         * Send the remaining arguments to the function.
         */
        if (TRACEP(TRACE_CALL_OTHER)) {
            if (!++traceing_recursion) {
                do_trace("Call other ", arg[1].u.string, "\n");
            }
            traceing_recursion--;
        }
        if (apply_low(arg[1].u.string, ob, num_arg-2, MY_FALSE) == 0) {
            /* Function not found */
            pop_n_elems(num_arg);
            push_number(0);
            break;
        }
        sp -= num_arg - 3;
        /*
         * The result of the function call is on the stack. But, so
         * is the function name and object that was called.
         * These have to be removed.
         */
        arg = sp;           /* Remember where the function call result is */
        free_string_svalue(--sp);
        free_svalue(--sp); /* Remove old arguments to call_other */
        *sp = *arg;           /* Re-insert function result */
        break;
    }
    CASE(F_SIMUL_EFUN);
    {
        int code;
        struct simul_efun_table_s *entry;
        unsigned char *funstart;
        struct object *ob;

        ASSIGN_EVAL_COST
        code = EXTRACT_UCHAR(pc);
        pc++;
        num_arg = simul_efunp[code].num_arg;
        if (num_arg == 0xff) {
            num_arg = EXTRACT_UCHAR(pc); pc++;
        }
        if (current_object->flags & O_DESTRUCTED) {
            /*
             * No external calls may be done when this object is
             * destructed.
             */
            pop_n_elems(num_arg);
            push_number(0);
            break;
        }
        if ( !(ob = simul_efun_object) ) {
            inter_sp = sp;
            inter_pc = pc;
            if ( !(ob = get_simul_efun_object()) ) {
                error("Couldn't load simul_efun object\n");
            }
        }
        entry = &simul_efun_table[code];
        if ( NULL != (funstart = entry->funstart) ) {
            struct program *prog;
            struct svalue *new_sp;

            push_control_stack(sp, pc, fp);
            csp->ob = current_object;
            csp->prev_ob = previous_ob;
            csp->funstart = funstart;
            csp->num_local_variables = num_arg;
            current_prog = prog = entry->program;
            function_index_offset = entry->function_index_offset;
            current_variables = ob->variables + entry->variable_index_offset;
            new_sp = setup_new_frame2(funstart, sp);
            /* The simul_efun object should not use simul_efuns itself... */
            previous_ob = current_object;
            current_object = ob;
            current_strings = prog->strings;
            eval_instruction(funstart + 2, new_sp);
            sp -= num_arg - 1;
            /*
             * The result of the function call is on the stack.
             */
            break;
        }
        inter_sp = sp;
        inter_pc = pc;
        call_simul_efun(code, ob, num_arg);
        /*
         * The result of the function call is on the stack.
         */
        break;
    }
    CASE(F_INTP);
    {
        int i;

        i = sp->type == T_NUMBER;
        free_svalue(sp);
        put_number(i);
        break;
    }
    CASE(F_STRINGP);
    {
        int i;

        i = sp->type == T_STRING;
        free_svalue(sp);
        put_number(i);
        break;
    }
    CASE(F_OBJECTP);
    {
        int i;

        i = sp->type == T_OBJECT;
        free_svalue(sp);
        put_number(i);
        break;
    }
#ifdef F_FLOATP
    CASE(F_FLOATP);
    {
        int i;

        i = sp->type == T_FLOAT;
        free_svalue(sp);
        put_number(i);
        break;
    }
#endif
    CASE(F_POINTERP);
    {
        int i;

        i = sp->type == T_POINTER;
        free_svalue(sp);
        put_number(i);
        break;
    }
    CASE(F_SYMBOLP);
    {
        int i;

        i = sp->type == T_SYMBOL;
        free_svalue(sp);
        put_number(i);
        break;
    }
    CASE(F_CLOSUREP);
    {
        int i;

        i = sp->type == T_CLOSURE;
        free_svalue(sp);
        put_number(i);
        break;
    }
    CASE(F_EXTRACT2);
    {
        int len, from;
        struct svalue *arg;

        arg = sp - 1;
        if (arg->type == T_STRING) {
            char *res;

            len = _svalue_strlen(&arg[0]);
            if ((arg+1)->type != T_NUMBER)
            {
                ERRORF(("Index value must be a number.\n"));
                /* NOTREACHED */
                return; /* Flow control hint */
            }
            from = arg[1].u.number;
            sp--;
            if (from < 0) {
                from = len + from;
                if (from < 0)
                    from = 0;
            }
            if (from >= len) {
                pop_stack();
                push_constant_string("");
                break;
            }
            res = string_copy(arg->u.string + from);
            free_string_svalue(sp);
            put_malloced_string(res, sp);
            break;
        }
        if (arg->type != T_POINTER)
        {
            ERRORF(("Indexed value is neither string nor array.\n"));
            /* NOTREACHED */
            return; /* Flow control hint */
        }
        {
            struct vector *v, *res;

            if ((arg+1)->type != T_NUMBER)
            {
                ERRORF(("Index value must be a number.\n"));
                /* NOTREACHED */
                return; /* Flow control hint */
            }
            v = arg->u.vec;
            len = VEC_SIZE(v);
            from = arg[1].u.number;
            sp--;
            if (from < 0) {
                from = len + from;
            }
            res = slice_array(v, from, len-1);
            free_vector(v);
            put_referenced_vector(res);
            break;
        }
    }
    CASE(F_RANGE);
    {
        if (sp[-1].type != T_NUMBER)
            ERROR("Bad type of start interval to [ .. ] range.\n")
        if (sp[0].type != T_NUMBER)
            ERROR("Bad type of end interval to [ .. ] range.\n")
        if (sp[-2].type == T_POINTER) {
            struct vector *v;
            mp_int end, size;

            v = sp[-2].u.vec;
            v =
              slice_array(
                v,
                sp[-1].u.number,
                (end = sp[0].u.number) >= (size = VEC_SIZE(v)) ?
                  size - 1 :
                  end
              );
            pop_n_elems(3);
            if (v) {
                push_referenced_vector(v);
            } else {
                push_number(0);
            }
        } else if (sp[-2].type == T_STRING) {
            int len, from, to;
            char *res;

            len = _svalue_strlen(&sp[-2]);
            from = sp[-1].u.number;
            if (from < 0) {
                from = 0;
            }
            to = sp[0].u.number;
            if (to >= len)
                to = len-1;
            if (to < from) {
                pop_n_elems(3);
                push_constant_string("");
                break;
            }
            if (to == len-1) {
                res = string_copy(sp[-2].u.string + from);
                pop_n_elems(3);
                push_malloced_string(res);
                break;
            }
            res = xalloc(to - from + 2);
            strncpy(res, sp[-2].u.string + from, to - from + 1);
            res[to - from + 1] = '\0';
            pop_n_elems(3);
            push_malloced_string(res);
        } else {
            ERROR("Bad argument to [ .. ] range operand.\n")
        }
        break;
    }
    CASE(F_RANGE_LVALUE);
        inter_pc = pc;
        sp = range_lvalue(0x000, sp);
        break;
    CASE(F_QUERY_VERB);
        if (last_verb == 0) {
            push_number(0);
            break;
        }
        push_shared_string(last_verb);
        break;
    CASE(F_EXEC);
    {
        int i;

        assign_eval_cost();
        TYPE_TEST1(sp-1, T_OBJECT)
        TYPE_TEST2(sp,   T_OBJECT)
        inter_sp = sp;
        inter_pc = pc;
        i = replace_interactive((sp-1)->u.ob, sp->u.ob, current_prog->name);
        pop_stack();
        free_svalue(sp); /* object might have been destructed */
        put_number(i);
        break;
    }
    CASE(F_FILE_NAME);
    {
        char *name,*res;

        TYPE_TEST1(sp, T_OBJECT)

        /* This function now returns a leading '/', except when -o flag */
        name = sp->u.ob->name;
#ifdef COMPAT_MODE
        res = string_copy(name);
#else
        res = add_slash(name);
#endif
        if (!res)
            ERROR("Out of memory\n")
        free_object_svalue(sp);
        put_malloced_string(res, sp);
        break;
    }
    CASE(F_USERS);
        push_referenced_vector(users());
        break;
    CASE(F_CALL_OUT);
        GET_NUM_ARG
        inter_pc = pc;
        sp = new_call_out(sp, num_arg);
        break;
    CASE(F_CALL_OUT_INFO);
        inter_sp = sp;
        inter_pc = pc;
        if (privilege_violation("call_out_info", sp) > 0 ) {
            push_referenced_vector(get_all_call_outs());
            break;
        } else {
            push_vector(&null_vector);
            break;
        }
    CASE(F_REMOVE_CALL_OUT);
    {
        inter_pc = pc;
        find_call_out(current_object, sp, MY_TRUE);
        break;
    }
    CASE(F_FIND_CALL_OUT);
    {
        inter_pc = pc;
        find_call_out(current_object, sp, MY_FALSE);
        break;
    }
    CASE(F_WRITE);
        assign_eval_cost();
        inter_pc = pc;
        inter_sp = sp;
        do_write(sp);
        pop_stack();
        break;

    CASE (F_MEMBER_ARRAY);
    {
        if (sp->type == T_POINTER) {

#define UP_TO_SVP(up)        ( \
                         (struct svalue *)( \
                          ((PTRTYPE)(up))- \
                          ((PTRTYPE)(&((struct svalue *)0)->u)-(PTRTYPE) 0) \
                         ) \
                        )

            struct vector *vec;
            union u sp_u; char *item_u_p; /* actually union u*,
                                but some compilers can't handle that right */
            int cnt;

            vec = sp->u.vec;
            sp_u = sp[-1].u;
            item_u_p = (char *)&vec->item->u;
            cnt = VEC_SIZE(vec);
            switch(sp[-1].type) {
              case T_STRING:
              {
                char *str;

                str = sp_u.string;
                for(; --cnt >= 0; item_u_p += sizeof(struct svalue) )
                {
                    if (UP_TO_SVP(item_u_p)->type == T_STRING)
                        if (strcmp(
                                sp_u.string,
                                ((union u*)item_u_p)->string
                        ) == 0)
                            break;
                }
                break;
              }
#ifdef FLOATS
              case T_FLOAT:
#endif
              case T_CLOSURE:
              case T_SYMBOL:
              case T_QUOTED_ARRAY:
              {
                short type;
                short x_generic;

                type = sp[-1].type;
                x_generic = sp[-1].x.generic;
                for(; --cnt >= 0; item_u_p += sizeof(struct svalue) )
                {
                    if (sp_u.string == ((union u*)item_u_p)->string)
                        if (x_generic == UP_TO_SVP(item_u_p)->x.generic)
                            if (UP_TO_SVP(item_u_p)->type == type)
                                break;
                    }
                break;
              }
              case T_NUMBER:
                if (!sp_u.number) {
                    struct svalue *svp;
                    short type;

                    for(svp = UP_TO_SVP(item_u_p); --cnt >= 0; svp++) {
                        if ( (type = svp->type) == T_NUMBER) {
                            if ( !svp->u.number )
                                break;
                        } else if (type == T_OBJECT) {
                            if (svp->u.ob->flags & O_DESTRUCTED) {
                                assign_svalue(svp, &const0);
                                break;
                            }
                        }
                    }
                    break;
                }
#ifdef MAPPINGS
              case T_MAPPING:
#endif
              case T_OBJECT: case T_POINTER:
              {
                short type = sp[-1].type;

                for(; --cnt >= 0; item_u_p += sizeof(struct svalue) )
                {
                    if (sp_u.number == ((union u*)item_u_p)->number)
                        if (UP_TO_SVP(item_u_p)->type == type)
                            break;
                }
                break;
              }
              default:
                if (sp[-1].type == T_LVALUE)
                    error("Reference passed to member_array()\n");
                fatal("Bad type to member_array(): %d\n", sp[-1].type);
            }
            if (cnt >= 0) { /* Return -1 for failure */
                cnt = VEC_SIZE(vec) - cnt - 1;
            }
            pop_stack();
            free_svalue(sp);
            put_number(cnt);
            break;
        }
        if (sp->type == T_STRING) {
            char *str, *str2;
            int i;

            if (sp[-1].type != T_NUMBER) goto bad_arg_1;
            str = sp->u.string;
            i = sp[-1].u.number;
            str2 = i & ~0xff ? 0 : strchr(str, i);
            i = str2 ? str2 - str : -1;
            pop_stack();
            free_svalue(sp);
            put_number(i);
            break;
        }
        goto bad_arg_2;
    }
    CASE (F_MEMBER);
    {
        if (sp[-1].type == T_POINTER) {

            struct vector *vec;
            union u sp_u; char *item_u_p;
            int cnt;

            vec = sp[-1].u.vec;
            sp_u = sp->u;
            item_u_p = (char *)&vec->item->u;
            cnt = VEC_SIZE(vec);
            switch(sp->type) {
              case T_STRING:
              {
                char *str;

                str = sp_u.string;
                for(; --cnt >= 0; item_u_p += sizeof(struct svalue) )
                {
                    if (UP_TO_SVP(item_u_p)->type == T_STRING)
                        if (strcmp(
                                sp_u.string,
                                ((union u*)item_u_p)->string
                        ) == 0)
                            break;
                }
                break;
              }
#ifdef FLOATS
              case T_FLOAT:
#endif
              case T_CLOSURE:
              case T_SYMBOL:
              case T_QUOTED_ARRAY:
              {
                short x_generic;
                short type;

                type = sp->type;
                x_generic = sp->x.generic;
                for(; --cnt >= 0; item_u_p += sizeof(struct svalue) )
                {
                    if (sp_u.string == ((union u*)item_u_p)->string)
                        if (x_generic == UP_TO_SVP(item_u_p)->x.generic)
                            if (UP_TO_SVP(item_u_p)->type == type)
                                break;
                    }
                break;
              }
              case T_NUMBER:
                if (!sp_u.number) {
                    struct svalue *svp;
                    short type;

                    for(svp = UP_TO_SVP(item_u_p); --cnt >= 0; svp++) {
                        if ( (type = svp->type) == T_NUMBER) {
                            if ( !svp->u.number )
                                break;
                        } else if (type == T_OBJECT) {
                            if (svp->u.ob->flags & O_DESTRUCTED) {
                                assign_svalue(svp, &const0);
                                break;
                            }
                        }
                    }
                    break;
                }
#ifdef MAPPINGS
              case T_MAPPING:
#endif
              case T_OBJECT: case T_POINTER:
              {
                short type = sp->type;

                for(; --cnt >= 0; item_u_p += sizeof(struct svalue) )
                {
                    if (sp_u.number == ((union u*)item_u_p)->number)
                        if (UP_TO_SVP(item_u_p)->type == type)
                            break;
                }
                break;
              }
              default:
                if (sp->type == T_LVALUE)
                    error("Reference passed to member()\n");
                fatal("Bad type to member(): %d\n", sp->type);
            }
            if (cnt >= 0) { /* Return -1 for failure */
                cnt = VEC_SIZE(vec) - cnt - 1;
            }
            pop_stack();
            free_svalue(sp);
            put_number(cnt);
            break;
        }
        if (sp[-1].type == T_STRING) {
            char *str, *str2;
            int i;

            if (sp->type != T_NUMBER) goto bad_arg_2;
            str = sp[-1].u.string;
            i = sp->u.number;
            str2 = i & ~0xff ? 0 : strchr(str, i);
            i = str2 ? str2 - str : -1;
            pop_stack();
            free_svalue(sp);
            put_number(i);
            break;
        }
#ifdef MAPPINGS
        if (sp[-1].type == T_MAPPING) {
            int i;

            i = get_map_lvalue(sp[-1].u.map, sp, 0) != &const0;
            pop_stack();
            free_svalue(sp);
            put_number(i);
            break;
        }
#endif
        goto bad_arg_1;
    }
    CASE(F_MOVE_OBJECT);
    {
        struct object *o1, *o2;

        ASSIGN_EVAL_COST
        inter_pc = pc;
        inter_sp = sp;
        if ((sp-1)->type == T_OBJECT)
            o1 = (sp-1)->u.ob;
        else if ((sp-1)->type == T_STRING) {
            o1 = get_object((sp-1)->u.string);
            if (o1 == 0)
                error("move_object failed\n");
            free_string_svalue(sp-1);
            sp[-1].type = T_OBJECT;
            sp[-1].u.ob = o1;
            add_ref(o1, "move_object");
        } else goto bad_arg_1;
        if (sp->type == T_OBJECT)
            ;
        else if (sp->type == T_STRING) {
            o2 = get_object(sp->u.string);
            if (o2 == 0)
                error("move_object failed\n");
            free_string_svalue(sp);
            sp->type = T_OBJECT;
            sp->u.ob = o2;
            add_ref(o2, "move_object");
        } else goto bad_arg_2;
        move_object();
        sp -= 2;
        break;
    }
    CASE(F_FUNCTION_EXISTS);
    {
        char *str, *res, *p;

        TYPE_TEST1(sp-1, T_STRING)
        TYPE_TEST2(sp,   T_OBJECT)
        inter_sp = sp; /* error possible when out of memory */
        str = function_exists((sp-1)->u.string, sp->u.ob);
        free_svalue(sp);
        free_svalue(--sp);
        if (str) {
            /* remove .c from end of string. (Alvin) */
            p = strrchr (str, '.');
            *p = 0;
#ifdef COMPAT_MODE
            res = string_copy (str); /* Marion sighs deeply. */
#else
            res = add_slash(str);
#endif
            *p = '.';
            if (!res) {
                sp--;
                ERROR("Out of memory\n")
            }
            put_malloced_string(res, sp);
        } else {
            put_number(0);
        }
        break;
    }
    CASE(F_SNOOP);
    {
        /* This one takes a variable number of arguments. It returns
         * -1, 0 or 1.
         */
        int i;

        assign_eval_cost();
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        if (num_arg == 1) {
            TYPE_TEST1(sp,   T_OBJECT)
            i = set_snoop(sp->u.ob, 0);
        } else {
            TYPE_TEST1(sp-1, T_OBJECT)
            TYPE_TEST2(sp,   T_OBJECT)
            i = set_snoop((sp-1)->u.ob, sp->u.ob);
            pop_stack();
        }
        free_svalue(sp);
        put_number(i);
        break;
    }
#ifdef F_ADD_ACTION
    CASE(F_ADD_ACTION);
    {
        struct svalue *arg;
        struct svalue *verb;

        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        arg = sp - num_arg + 1;
        TYPE_TEST1(arg, T_STRING)
        verb = 0;
        if (num_arg >= 2) {
            TYPE_TEST2(arg+1, T_STRING)
            if (num_arg > 2) {
                if (arg[2].type != T_NUMBER)
                    goto bad_arg_3;
            }
            verb = &arg[1];
        }
        if (add_action(&arg[0], verb,
                   num_arg > 2 ? arg[2].u.number : 0)) {
            /* silent error condition, deallocate strings by hand */
            pop_n_elems(num_arg);
        } else {
            /* add_action has reused the strings or freed it */
            sp -= num_arg;
        }
        break;
    }
#endif /* ADD_ACTION */
    CASE(F_ALLOCATE);
    {
        struct vector *v;

        TYPE_TEST1(sp, T_NUMBER)
        inter_sp = sp;
        inter_pc = pc;
        v = allocate_array(sp->u.number);        /* Will have ref count == 1 */
        sp->type = T_POINTER;
        sp->u.vec = v;
        break;
    }
    CASE(F_ED);
        if (current_object->flags & O_DESTRUCTED) {
            /* could confuse the master... */
            ERROR("Calling ed from destructed object.\n")
        }
        GET_NUM_ARG
        inter_pc = pc;
        assign_eval_cost();
        inter_sp = sp;
        if (num_arg == 0) {
            ed_start(0, 0, 0);
            push_number(1);
            break;
        } else if (num_arg == 1) {
            TYPE_TEST1(sp,   T_STRING)
            ed_start(sp->u.string, 0, 0);
            break;
        } else {
            TYPE_TEST1(sp-1, T_STRING)
            if (sp->type == T_STRING)
                ed_start((sp-1)->u.string, sp->u.string, current_object);
            else if (sp->type == T_NUMBER)
                ed_start((sp-1)->u.string, 0 , 0);
            else goto bad_arg_2;
            pop_stack();
            break;
        }
    CASE(F_CRYPT);
    {
        char salt[2];
        char *res;
        static char choise[] =
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789./";

        TYPE_TEST1(sp-1, T_STRING)
        if (sp->type == T_STRING && svalue_strlen(sp) >= 2) {
            salt[0] = sp->u.string[0];
            salt[1] = sp->u.string[1];
        } else if (sp->type == T_NUMBER) {
            salt[0] = choise[random_number((sizeof choise) - 1)];
            salt[1] = choise[random_number((sizeof choise) - 1)];
        } else goto bad_arg_2;
        res = string_copy(crypt((sp-1)->u.string, salt));
        pop_n_elems(2);
        push_malloced_string(res);
        break;
    }
    CASE(F_DESTRUCT);
        assign_eval_cost();
        TYPE_TEST1(sp, T_OBJECT)
        inter_sp = sp;
        inter_pc = pc;
        destruct_object(sp);
        pop_stack();
        break;
    CASE(F_RANDOM);
        TYPE_TEST1(sp, T_NUMBER)
        if (sp->u.number <= 0) {
            sp->u.number = 0;
            break;
        }
        sp->u.number = random_number(sp->u.number);
        break;
#ifdef F_SAY
    CASE(F_SAY);
    {
        static struct {
            INIT_VEC_TYPE;
            struct svalue second_item[1];
        } vtmp = { VEC_INIT(2, 1, T_NUMBER), { { T_OBJECT } } };

        ASSIGN_EVAL_COST
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        if (num_arg == 1) {
            if (sp->type != T_STRING && sp->type != T_POINTER) goto bad_arg_1;
            vtmp.v.item[0].type = T_NUMBER; /* this marks the place for the
                                               command_giver
                                               */
            vtmp.v.item[1].type = T_NUMBER; /* will not match any object... */
            say(sp, &vtmp.v);
        } else {
            if (sp[-1].type != T_STRING && sp[-1].type != T_POINTER)
                goto bad_arg_1;
            if ( sp->type == T_POINTER ) {
                say(sp-1, sp->u.vec);
            } else if (sp->type == T_OBJECT) {
                vtmp.v.item[0].type = T_NUMBER;
                vtmp.v.item[1].type = T_OBJECT;
                vtmp.v.item[1].u.ob = sp->u.ob;
                add_ref(sp->u.ob, "ass to var");
                say(sp-1, &vtmp.v);
            } else goto bad_arg_2;
            pop_stack();
        }
        pop_stack();
        break;
    }
#endif /* F_SAY */
#ifdef F_TELL_ROOM
    CASE(F_TELL_ROOM);
    {
        struct svalue *arg;
        struct vector *avoid;
        struct object *ob;

        ASSIGN_EVAL_COST
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        arg = sp- num_arg + 1;
        if (arg[0].type == T_OBJECT)
            ob = arg[0].u.ob;
        else if (arg[0].type == T_STRING) {
            ob = get_object(arg[0].u.string);
            if (ob == 0)
                ERROR("Object not found.\n")
        } else goto bad_arg_1;
        if (arg[1].type != T_STRING && arg[1].type != T_POINTER)
            goto bad_arg_2;
        if (num_arg == 2) {
            avoid = &null_vector;
        } else {
            struct vector *vtmpp;
            static struct svalue stmp = { T_POINTER };

            if (arg[2].type != T_POINTER)
                goto bad_arg_3;
            stmp.u.vec = arg[2].u.vec;
            vtmpp = order_alist(&stmp, 1, 1);
            avoid = vtmpp->item[0].u.vec;
            sp->u.vec = avoid; /* in case of an error, this will be freed. */
            sp--;
            vtmpp->item[0].u.vec = stmp.u.vec;
            free_vector(vtmpp);
        }
        tell_room(ob, sp, avoid);
        if (num_arg > 2)
            free_vector(avoid);
        pop_stack();
        pop_stack();
        break;
    }
#endif /* F_TELL_ROOM */
#ifdef F_SHOUT
    CASE(F_SHOUT);
        assign_eval_cost();
        inter_sp = sp;
        inter_pc = pc;
        TYPE_TEST1(sp, T_STRING)
        shout_string(sp->u.string);
        pop_stack();
        break;
#endif /* F_SHOUT */
    CASE(F_SWITCH);
    {
        mp_int offset, o0, o1, def_offs;
        int i, len, tablen, type;
        mp_int d,s,r;
        static int32 off_tab[] = {
                0*sizeof(char*), 0x00001*sizeof(char*), 0x00003*sizeof(char*),
          0x00007*sizeof(char*), 0x0000f*sizeof(char*), 0x0001f*sizeof(char*),
          0x0003f*sizeof(char*), 0x0007f*sizeof(char*), 0x000ff*sizeof(char*),
          0x001ff*sizeof(char*), 0x003ff*sizeof(char*), 0x007ff*sizeof(char*),
          0x00fff*sizeof(char*), 0x01fff*sizeof(char*), 0x03fff*sizeof(char*),
          0x07fff*sizeof(char*), 0x0ffff*sizeof(char*), 0x1ffff*sizeof(char*),
          0x3ffff*sizeof(char*), 0x7ffff*sizeof(char*)
        };
        union { unsigned char b[sizeof(p_int)-1]; short s; } abuf;
        int a, b;
        unsigned char *p0, *p1, *p2, *l, *tabstart, *end_tab, *break_addr;

        p0 = pc;
        tablen = EXTRACT_UCHAR(pc);
        if ( !(len = tablen & 3) ) {
            align_switch(pc);
            tablen = EXTRACT_UCHAR(pc);
            len = tablen & 3;
        }
        tablen &= ~3;
        offset = EXTRACT_UCHAR(pc+1);
        if (len > 1) {
            offset += EXTRACT_UCHAR(pc+2) << 8;
            if (len > 2) {
                offset += EXTRACT_UCHAR(pc+3) << 16;
            }
        }
        p0 = pc + offset;
        if (len > 1) {
            tablen += *(unsigned char *)(p0++) << 8;
            if (len > 2) {
                tablen += *(unsigned char *)(p0++) << 16;
#if SIZEOF_P_INT == 4
                p1 = (unsigned char *)(p0 + (tablen << 1) - (tablen >> 2));
            } else {
                p1 = (unsigned char *)(p0 + tablen + (tablen >> 1));
#else
                p1 = (unsigned char *)(p0 + tablen + tablen*3/sizeof(p_int) );
            } else {
                p1 = (unsigned char *)(p0 + tablen + tablen*2/sizeof(p_int) );
#endif
            }
        } else {
            p1 = (unsigned char *)(p0 + tablen + tablen / sizeof(p_int) );
        }
        b = ((p_int)p0-1) & sizeof abuf.b;
        memcpy((char *)abuf.b, p0, sizeof abuf.b);
        a = sizeof abuf.b - b;
        memcpy((char *)(abuf.b + a), (char *)(p1 + a), b);
        def_offs = abuf.s;
        type = abuf.b[2];
        if (len > 2) {
            def_offs += p1[3] << 16;
            break_addr = p1+sizeof(p_int);
        } else {
            break_addr = p1+sizeof(p_int)-1;
        }
        tabstart = p0 + a;
        end_tab  = tabstart + tablen;
        break_sp -= sizeof(struct svalue)/sizeof(*break_sp);
        *break_sp = break_addr;
        if (type & 0x20) {
            if ( sp->type == T_NUMBER && !sp->u.number ) {
                /* special case: uninitialized string */
                s = (mp_int)ZERO_AS_STR_CASE_LABEL;
            } else if ( sp->type == T_STRING ) {
                switch(sp->x.string_type) {
                case STRING_SHARED:
                    s = (mp_int)sp->u.string;
                    break;
                default:
                    s = (mp_int)findstring(sp->u.string);
                    break;
                }
            } else {
                goto bad_arg_1;
            }
        } else {
            if (sp->type != T_NUMBER) goto bad_arg_1;
            s = sp->u.number;
        }
        i = type & 0x1f;
        pop_stack();
        l = tabstart + off_tab[i];
        d = (off_tab[i]+sizeof(p_int)) >> 1 & ~(sizeof(p_int)-1);
        for(;;) {
            r = *(p_int*)l;
            if (s < r)
                if (d < (mp_int)sizeof(p_int)) {
                    if (!d) {
                        p2 =
                          tabstart + tablen +
                          ((p_int*)l - (p_int*)tabstart)*len;
                        o0 = EXTRACT_UCHAR(p2-1);
                        o1 = EXTRACT_UCHAR(p2);
                        if (len > 1) {
                            o0 += EXTRACT_UCHAR(p2-2) << 8;
                            o1 = EXTRACT_UCHAR(p2+1) + (o1 << 8);
                            if (len > 2) {
                                o0 += EXTRACT_UCHAR(p2-3) << 16;
                                o1 = EXTRACT_UCHAR(p2+2) + (o1 << 8);
                            }
                        }
                        /* test for range */

                        /* Because the pre-table alignment area is in the
                         * indexing underflow memory region, we can't make
                         * useful predictions on the peeked o0 value in case
                         * of underflow.
                         */
                        if (o0 <= 1 && l > tabstart) {
                            r = ((p_int*)l)[-1];
                            if (s >= r) {
                                /* s is in the range */
                                if (!o0) {
                                    /* range with lookup table */
                                    l = pc + o1 + (s-r) * len;
                                    o1 = 0;
                                    i = len;
                                    do {
                                        o1 = (o1 << 8) + *l++;
                                    } while (--i);
                                    /* o1 holds jump destination */
                                    break;
                                }
                                /* o1 holds jump destination */
                                break;
                            }
                        }
                        /* use default address */
                        o1 = def_offs;
                        /* o1 holds jump destination */
                        break;
                    } /* !d */
                    d = 0;
                } else {
                    /* d >= sizeof(p_int) */
                    l -= d;
                    d >>= 1;
                }
            else if (s > r) {
                if (d < (mp_int)sizeof(p_int)) {
                    if (!d) {
                        p2 =
                          tabstart + tablen +
                          (((p_int*)l - (p_int*)tabstart) + 1)*len;
                        o0 = EXTRACT_UCHAR(p2-1);
                        o1 = EXTRACT_UCHAR(p2);
                        if (len > 1) {
                            o0 += EXTRACT_UCHAR(p2-2) << 8;
                            o1 = EXTRACT_UCHAR(p2+1) + (o1 << 8);
                            if (len > 2) {
                                o0 += EXTRACT_UCHAR(p2-3) << 16;
                                o1 = EXTRACT_UCHAR(p2+2) + (o1 << 8);
                            }
                        }
                        /* test for range */
                        if (o0 <= 1) {
                            if (s <= ((p_int*)l)[1]) {
                                /* s is in the range */
                                if (!o0) {
                                    /* range with lookup table */
                                    l = pc + o1 + (s-r) * len;
                                    o1 = 0;
                                    i = len;
                                    do {
                                        o1 = (o1 << 8) + *l++;
                                    } while (--i);
                                    /* o1 holds jump destination */
                                    break;
                                }
                                /* o1 holds jump destination */
                                break;
                            }
                        }
                        /* use default address */
                        o1 = def_offs;
                        /* o1 holds jump destination */
                        break;
                    } /* !d */
                    d = 0;
                } else {
                    /* d >= sizeof(p_int) */
                    l += d;
                    while (l >= end_tab) {
                        d >>= 1;
                        if (d <= (mp_int)sizeof(p_int)/2) {
                            l -= sizeof(p_int);
                            d = 0;
                            break;
                        }
                        l -= d;
                    }
                    d >>= 1;
                }
            } else {
                /* s == r */
                p2 = tabstart + tablen + ((p_int*)l - (p_int*)tabstart)*len;
                o0 = EXTRACT_UCHAR(p2-1);
                o1 = EXTRACT_UCHAR(p2);
                /* We don't care if it is an ordinary match or a match with
                 * the end of an ordinary range
                 */
                if (len > 1) {
                    o0 |= EXTRACT_UCHAR(p2-2);
                    o1 = EXTRACT_UCHAR(p2+1) + (o1 << 8);
                    if (len > 2) {
                        o0 |= EXTRACT_UCHAR(p2-3);
                        o1 = EXTRACT_UCHAR(p2+2) + (o1 << 8);
                    }
                }
                if (!o0 && l > tabstart)
                {
                    /* end of range with lookup table */
                    r = ((p_int*)l)[-1];
                    l = pc + o1 + (s-r) * len;
                    o1 = 0;
                    i = len;
                    do {
                        o1 = (o1 << 8) + *l++;
                    } while (--i);
                    /* o1 holds jump destination */
                    break;
                }
                if (o1 <= 1) {
                    p2 += len;
                    if (o1) {
                        /* start of ordinary range */
                        l = p2;
                        /* the optimizer will probably insert a jump here */
                        o1 = 0;
                        i = len;
                        do {
                            o1 = (o1 << 8) + *l++;
                        } while (--i);
                        /* o1 holds jump destination */
                        break;
                    }
                    /* start of range with lookup table */
                    /* o1 == 0 */
                    i = len;
                    do {
                        o1 = (o1 << 8) + *p2++;
                    } while (--i);
                    l = pc + o1;
                    o1 = 0;
                    i = len;
                    do {
                        o1 = (o1 << 8) + *l++;
                    } while (--i);
                    /* o1 holds jump destination */
                    break;
                }
                break;
            }
        }
        pc += o1;
        break;
    }
    CASE(F_BREAK);
    {
        pc = *break_sp;
        break_sp += sizeof(struct svalue)/sizeof(*break_sp);
        break;
    }
    CASE(F_STRLEN);
    {
        int i;

        if (sp->type == T_STRING) {
            i = _svalue_strlen(sp);
            free_string_svalue(sp);
            put_number(i);
            break;
        }
        if (sp->type == T_NUMBER && sp->u.number == 0)
            break;
        goto bad_arg_1;
    }
    CASE(F_SIZEOF);
    {
        int i;

        if (sp->type == T_POINTER) {
            i = VEC_SIZE(sp->u.vec);
            free_svalue(sp);
            put_number(i);
            break;
        }
#ifdef MAPPINGS
        if (sp->type == T_MAPPING) {
            struct mapping *m = sp->u.map;
            check_map_for_destr(m);
            i = m->condensed->string_size/sizeof(char *) +
                m->condensed->misc_size/sizeof(struct svalue) +
                (m->hash ? m->hash->used - m->hash->condensed_deleted : 0);
            free_svalue(sp);
            put_number(i);
            break;
        }
#endif /* MAPPINGS */
        if (sp->type == T_NUMBER && sp->u.number == 0)
            break;
        goto bad_arg_1;
    }
    CASE(F_LOWER_CASE);
    {
        char *str, *s, *d, c;

        TYPE_TEST1(sp, T_STRING)
        str = xalloc(svalue_strlen(sp)+1);
        for(s = sp->u.string, d = str; '\0' != (c = *s++) ; ) {
            if (isupper((unsigned char)c))
                c += 'a' - 'A';
            *d++ = c;
        }
        *d = c;
        free_string_svalue(sp);
        put_malloced_string(str, sp);
        break;
    }
    CASE(F_SET_HEART_BEAT);
    {
        int i;

        TYPE_TEST1(sp, T_NUMBER)
        i = set_heart_beat(current_object, sp->u.number);
        sp->u.number = i;
        break;
    }
    CASE(F_CAPITALIZE);
        TYPE_TEST1(sp, T_STRING)
        if (islower((unsigned char)(sp->u.string[0]))) {
            char *str;

            str = string_copy(sp->u.string);
            str[0] += 'A' - 'a';
            pop_stack();
            push_malloced_string(str);
        }
        break;
#ifdef F_PROCESS_STRING
    CASE(F_PROCESS_STRING);
    {
        char *str;

        assign_eval_cost();
        TYPE_TEST1(sp, T_STRING)
        inter_sp = sp;
        inter_pc = pc;
        str = process_string(sp->u.string);
        if (str != sp->u.string) {
            free_string_svalue(sp);
            put_malloced_string(str, sp);
        }
        break;
    }
#endif /* F_PROCESS_STRING */
    CASE(F_COMMAND);
    {
        int i;
        struct svalue *arg;

        assign_eval_cost();
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        arg = sp - num_arg + 1;
        if (num_arg == 1) {
            TYPE_TEST1(sp,   T_STRING)
            i = command_for_object(arg[0].u.string, 0);
        } else {
            TYPE_TEST1(sp-1, T_STRING)
            TYPE_TEST2(sp,   T_OBJECT)
#ifndef NATIVE_MODE
            i = command_for_object(arg[0].u.string, arg[1].u.ob);
            pop_stack();
#else
            ERROR("Too many arguments to command()\n")
#endif
        }
        free_svalue(sp);
        put_number(i);
        break;
    }
    CASE(F_GET_DIR);
    {
        struct vector *v;
        TYPE_TEST1(sp-1, T_STRING)
        TYPE_TEST2(sp,   T_NUMBER)
        inter_sp = sp;
        inter_pc = pc;
        v = get_dir(sp[-1].u.string, sp->u.number);
        sp--;
        /* there is now an svalue with type == T_STRING left on the stack. */
        free_string_svalue(sp);
        if (v) {
            sp->type  = T_POINTER;
            sp->u.vec = v;
        } else {
            put_number(0);
        }
        break;
    }
    CASE(F_RM);
    {
        int i;

        assign_eval_cost();
        TYPE_TEST1(sp, T_STRING)
        inter_sp = sp;
        inter_pc = pc;
        i = remove_file(sp->u.string);
        free_svalue(sp);
        put_number(i);
        break;
    }
    CASE(F_CAT);
    {
        int i;
        struct svalue *arg;
        int start, len;

        assign_eval_cost();
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        arg = sp- num_arg + 1;
        TYPE_TEST1(arg, T_STRING)
        start = 0; len = 0;
        if (num_arg > 1) {
            TYPE_TEST2(arg+1, T_NUMBER)
            start = arg[1].u.number;
            if (num_arg == 3) {
                if (arg[2].type != T_NUMBER)
                    goto bad_arg_3;
                len = arg[2].u.number;
            }
        }
        i = print_file(arg[0].u.string, start, len);
        pop_n_elems(num_arg);
        push_number(i);
        break;
    }
    CASE(F_MKDIR);
    {
        int i;
        char *path;

        assign_eval_cost();
        inter_pc = pc;
        inter_sp = sp;
        TYPE_TEST1(sp, T_STRING)
        path = check_valid_path(sp->u.string, current_object, "mkdir", 1);
        /* pop_stack(); see comment above... */
        i = !(path == 0 || mkdir(path, 0775) == -1);
        free_svalue(sp);
        put_number(i);
        break;
    }
    CASE(F_RMDIR);
    {
        int i;
        char *path;

        assign_eval_cost();
        inter_pc = pc;
        inter_sp = sp;
        TYPE_TEST1(sp, T_STRING)
        path = check_valid_path(sp->u.string, current_object, "rmdir", 1);
        /* pop_stack(); rw - what the heck? */
        i = !(path == 0 || rmdir(path) == -1);
        free_svalue(sp);
        put_number(i);
        break;
    }
    CASE(F_INPUT_TO);
    {
        GET_NUM_ARG
        inter_pc = pc;
        sp = input_to(sp, num_arg);
        break;
    }
#ifdef F_PARSE_COMMAND
    CASE(F_PARSE_COMMAND);
    {
        int i;
        struct svalue *arg;

        assign_eval_cost();
        num_arg = EXTRACT_UCHAR(pc);
        pc++;
        inter_pc = pc;
        inter_sp = sp;
        arg = sp - num_arg + 1;
        if (arg[0].type != T_STRING)
            goto bad_arg_1;
        if (arg[1].type != T_OBJECT && arg[1].type != T_POINTER)
            goto bad_arg_2;
        if (arg[2].type != T_STRING)
            goto bad_arg_3;
        if (arg[1].type == T_POINTER)
            check_for_destr(arg[1].u.vec);

        i = parse(arg[0].u.string, &arg[1], arg[2].u.string, &arg[3],
                  num_arg-3);
        pop_n_elems(num_arg);        /* Get rid of all arguments */
        push_number(i);                /* Push the result value */
        break;
    }
#endif /* PARSE_COMMAND */
    CASE(F_SSCANF);
    {
        int i;

        num_arg = EXTRACT_UCHAR(pc);
        pc++;
        inter_pc = pc;
        i = inter_sscanf(num_arg, sp);
        pop_n_elems(num_arg-1);
        free_svalue(sp);
        put_number(i);
        break;
    }
    CASE(F_ENABLE_COMMANDS);
        inter_sp = sp;
        enable_commands(1);
        break;
    CASE(F_DISABLE_COMMANDS);
        enable_commands(0);
        break;
    CASE(F_PRESENT);
        {
            struct svalue *arg;
            struct object *ob;

            assign_eval_cost();
            GET_NUM_ARG
            inter_pc = pc;
            arg = sp - num_arg + 1;
            if (arg->type != T_STRING && arg->type != T_OBJECT) goto bad_arg_1;
            ob = 0;
            if (num_arg > 1) {
                TYPE_TEST2(arg+1, T_OBJECT)
                ob = arg[1].u.ob;
                pop_stack();
            }
            inter_sp = sp;
            ob = object_present(arg, ob);
            free_svalue(arg);
            if (ob)
                put_object(ob);
            else
                put_number(0);
        }
        break;
#ifdef F_SET_LIGHT
    CASE(F_SET_LIGHT);
    {
        struct object *o1;

        TYPE_TEST1(sp, T_NUMBER)
        add_light(current_object, sp->u.number);
        o1 = current_object;
        while(o1->super)
            o1 = o1->super;
        sp->u.number = o1->total_light;
        break;
    }
#endif /* F_SET_LIGHT */
    CASE(F_CONST0);
        push_number(0);
        break;
    CASE(F_CONST1);
        push_number(1);
        break;
    CASE(F_NUMBER);
    {
        sp++;
        sp->type = T_NUMBER;
        memcpy ((char *)&sp->u.number, pc, sizeof sp->u.number);
        pc += sizeof sp->u.number;
        break;
    }
    CASE(F_CLIT);
    {
        int i;

        i = EXTRACT_UCHAR(pc);
        pc++;
        push_number(i);
        break;
    }
    CASE(F_NCLIT);
    {
        int i;

        i = EXTRACT_UCHAR(pc);
        pc++;
        push_number(-i);
        break;
    }
#ifdef FLOATS
    CASE(F_FLOAT);
    {
#if SIZEOF_P_INT == 4
        sp++;
        if (sp == &start_of_stack[EVALUATOR_STACK_SIZE])
            STACK_OVERFLOW(sp, fp, pc);

        sp->type = T_FLOAT;

        memcpy((char *)&sp->u.mantissa, pc, 4);

        memcpy((char *)&sp->x.exponent, pc + 4, 2);
#else
        int32 mantissa;
        short exponent;

        sp++;
        if (sp == &start_of_stack[EVALUATOR_STACK_SIZE])
            STACK_OVERFLOW(sp, fp, pc);

        sp->type = T_FLOAT;

        memcpy((char *)&mantissa, pc, 4);
        sp->u.mantissa = mantissa;

        memcpy((char *)&exponent, pc + 4, 2);
        sp->x.exponent = exponent;
#endif
        pc += 6;
        break;
    }
#endif /* FLOATS */

/* Amylaar: Be careful when assigning a value.
 * The freeing of old array contents before assignment is hazardous.
 * Consider the following assignment:
 *  a = ( ({((a=({0})),(a[0]=a)),(a=0)})[0] = query_verb() );
 * This code line is likely to corrupt the shared string table, namely the
 * entry for the verb in variable a if it's length uses a memory block of the
 * same length as an array of size 2.
 */

    CASE(F_ASSIGN);
    {
        struct svalue *argp;
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            fatal("Bad argument to F_ASSIGN\n");
#endif
        argp = sp->u.lvalue;
        assign_svalue(argp, sp-1);
        sp--;
        break;
    }
    CASE(F_VOID_ASSIGN);
    {
        struct svalue *argp;
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            fatal("Bad argument to F_ASSIGN\n");
#endif
        argp = sp->u.lvalue;
        /* F_VOID_ASSIGN is used pretty often, so here comes the adopted
         * transfer_svalue code.
         */
        switch(argp->type) {
          case T_STRING:
            switch(argp->x.string_type) {
              case STRING_MALLOC:
                xfree(argp->u.string);
                break;
              case STRING_SHARED:
                free_string(argp->u.string);
                break;
            }
            break;
          case T_OBJECT:
          {
            struct object *ob = argp->u.ob;
            free_object(ob, "void_assign");
            break;
          }
          case T_QUOTED_ARRAY:
          case T_POINTER:
          {
            struct vector *v = argp->u.vec;

            transfer_svalue_no_free_spc(argp, sp-1, sp, pc);
            sp -= 2;
            free_vector(v);
            goto again;
          }
          case T_SYMBOL:
            free_string(argp->u.string);
            break;
          case T_CLOSURE:
            free_closure(argp);
            break;
          case T_CHAR_LVALUE:
          {
            if (sp[-1].type == T_NUMBER) {
                *argp->u.string = sp[-1].u.number;
            } else {
                free_svalue(sp-1);
            }
            sp -= 2;
            goto again;
          }
/* the assignment class of operators always gets 'fresh' lvalues. Thus, if we
 * encounter a protected lvalue of any flavour, this is due to a dereference
 * of a reference stored in the original lvalue, and the protected lvalue
 * must not be freed.
 */
          case T_PROTECTED_CHAR_LVALUE:
          {
            struct protected_char_lvalue *p;

            p = (struct protected_char_lvalue *)argp;
            if (p->lvalue->type == T_STRING &&
                p->lvalue->u.string == p->start)
            {
                if (sp[-1].type == T_NUMBER) {
                    *p->v.u.string = sp[-1].u.number;
                    sp -= 2;
                    goto again;
                }
            }
            sp--;
            pop_stack();
            goto again;
          }
          case T_POINTER_RANGE_LVALUE:
            transfer_pointer_range(sp-1);
            sp -= 2;
            goto again;
          case T_PROTECTED_POINTER_RANGE_LVALUE:
            transfer_protected_pointer_range(
              (struct protected_range_lvalue *)argp, sp-1
            );
            sp -= 2;
            goto again;
          case T_STRING_RANGE_LVALUE:
            inter_sp = sp;
            assign_string_range(sp-1, 1);
            sp -= 2;
            goto again;
          case T_PROTECTED_STRING_RANGE_LVALUE:
            inter_sp = sp;
            assign_protected_string_range(
              (struct protected_range_lvalue *)argp, sp-1, 1
            );
            sp -= 2;
            goto again;
          case T_LVALUE:
          case T_PROTECTED_LVALUE:
          {
            transfer_svalue(argp->u.lvalue, sp-1);
            sp -= 2;
            goto again;
          }
#ifdef MAPPINGS
          case T_MAPPING:
          {
            struct mapping *m = argp->u.map;

            transfer_svalue_no_free_spc(argp, sp-1, sp, pc);
            sp -= 2;
            free_mapping(m);
            goto again;
          }
#endif
        }
        transfer_svalue_no_free_spc(argp, sp-1, sp, pc);
        sp -= 2;
        break;
    }
    CASE(F_CTIME);
    {
        char *ts, *cp;

        TYPE_TEST1(sp, T_NUMBER)
        ts = time_string(sp->u.number);
        cp = strchr(ts, '\n');
        if (cp) {
            int len = cp - ts;
            cp = xalloc(len + 1);
            if (!cp)
                ERROR("Out of memory\n")
            strncpy(cp, ts, len);
            cp[len] = 0;
        } else {
            cp = string_copy(ts);
            if (!cp)
                ERROR("Out of memory\n")
        }
        put_malloced_string(cp, sp);
        break;
    }
    CASE(F_ADD_EQ);
    CASE(F_VOID_ADD_EQ);
    {
        short type2;
        union u u2;
        struct svalue *argp;

        type2 = sp[-1].type;
        u2 = sp[-1].u;

#ifdef DEBUG
        if (sp->type != T_LVALUE)
            goto bad_arg_1;
#endif
        argp = sp->u.lvalue;
        for (;;) {
         switch(argp->type) {
          case T_STRING:
          {
            char *new_string;
            if (type2 == T_STRING) {
                int l = _svalue_strlen(argp);
                if ( !(new_string = xalloc(l + strlen(u2.string) + 1)) )
                    ERROR("Out of memory\n");
                strcpy(new_string, argp->u.string);
                strcpy(new_string+l, u2.string);
                free_string_svalue(sp-1);
                sp -= 2;
            } else if (type2 == T_NUMBER) {
                char buff[20];
                sprintf(buff, "%ld", (long)u2.number);
                if ( !(new_string =
                       xalloc(svalue_strlen(argp) + strlen(buff) + 1)) )
                    ERROR("Out of memory\n");
                strcpy(new_string, argp->u.string);
                strcat(new_string, buff);
                sp -= 2;
#ifdef FLOATS
            } else if (type2 == T_FLOAT) {
                char buff[42];
                sprintf(buff, "%g", READ_DOUBLE(sp-1) );
                if ( !(new_string =
                       xalloc(svalue_strlen(argp) + strlen(buff) + 1)) )
                    ERROR("Out of memory\n");
                strcpy(new_string, argp->u.string);
                strcat(new_string, buff);
                sp -= 2;
#endif
            } else {
                goto bad_arg_2;
            }
            free_string_svalue(argp);
            argp->x.string_type = STRING_MALLOC;
            argp->u.string = new_string;
            break;
          }
          case T_NUMBER:
            if (type2 == T_NUMBER) {
                if (instruction == F_VOID_ADD_EQ - F_OFFSET) {
                    argp->u.number += u2.number;
                    sp -= 2;
                    goto again;
                }
                (--sp)->u.number = argp->u.number += u2.number;
                goto again;
            } else {
                ERROR("Bad type number to rhs +=.\n")
            }
            break;
          case T_CHAR_LVALUE:
            if (type2 == T_NUMBER) {
                if (instruction == F_VOID_ADD_EQ - F_OFFSET) {
                    *argp->u.string += u2.number;
                    sp -= 2;
                    goto again;
                }
                (--sp)->u.number = *argp->u.string += u2.number;
                goto again;
            } else {
                ERROR("Bad type number to rhs +=.\n")
            }
            break;
#ifdef MAPPINGS
          case T_MAPPING:
            if (type2 != T_MAPPING) {
                ERROR("Bad type to rhs +=.\n")
            } else {
#if 0
                struct mapping *m, *m_old;
                m_old = argp->u.map;
                check_map_for_destr(m_old);
                check_map_for_destr(u2.map);
                m = add_mapping(argp->u.map, u2.map);
                sp -= 2;
                argp->u.map = m;
                free_mapping(u2.map);
                free_mapping(m_old);
#else
                check_map_for_destr(u2.map);
                add_to_mapping(argp->u.map, u2.map);
                sp -= 2;
                free_mapping(u2.map);
#endif
            }
            break;
#endif /* MAPPINGS */
          case T_POINTER:
            if (type2 != T_POINTER) {
                ERROR("Bad type to rhs +=.\n")
            } else {
                struct vector *v;
                inter_sp = sp;
                inter_pc = pc;
                v = inter_add_array(u2.vec, &argp->u.vec);
                if (instruction == F_VOID_ADD_EQ - F_OFFSET) {
                    sp -= 2;
                    goto again;
                }
                sp--;
                sp->u.vec = v;
                v->ref++;
                goto again;
            }
            break;
#ifdef FLOATS
          case T_FLOAT:
            if (type2 == T_FLOAT) {
                STORE_DOUBLE_USED
                double d;

/* don't use the address of u2, this would prevent putting it in a register */
                d = READ_DOUBLE(argp) + READ_DOUBLE(sp-1);
                STORE_DOUBLE(argp, d);
                sp -= 2;
            } else {
                goto bad_right;
            }
            break;
#endif /* FLOATS */
          case T_LVALUE:
          case T_PROTECTED_LVALUE:
            argp = argp->u.lvalue;
            continue;
          default:
            ERROR("Bad type to lhs +=\n")
         } /* end of switch */
         break;
        }
        if (instruction == F_VOID_ADD_EQ - F_OFFSET) {
            break;
        }
        sp++;
        assign_svalue_no_free(sp, argp);
        break;
    }
    CASE(F_SUB_EQ);
    {
        short type2;
        union u u2;
        struct svalue *argp;

        type2 = sp[-1].type;
        u2 = sp[-1].u;

#ifdef DEBUG
        if (sp->type != T_LVALUE)
            goto bad_arg_1;
#endif
        argp = sp->u.lvalue;
        for (;;) {
         switch (argp->type) {
          case T_NUMBER:
            if (type2 != T_NUMBER)
                goto bad_right;
            sp--;
            sp->u.number = argp->u.number -= u2.number;
            break;
          case T_CHAR_LVALUE:
            if (type2 != T_NUMBER)
                goto bad_right;
            sp--;
            sp->u.number = *argp->u.string -= u2.number;
            break;
          case T_POINTER:
          {
            struct vector *v, *v_old;

            if (type2 != T_POINTER)
                goto bad_right;
            v = u2.vec;
            if (v->ref > 1) {
                v->ref--;
                v = slice_array(v, 0, VEC_SIZE(v)-1 );
            }
            sp--;
            v_old = argp->u.vec;
            v = subtract_array(v_old, v);
            argp->u.vec = v;
            free_vector(subtract_array_tmp_vec);
            free_vector(v_old);
            put_vector(v);
            break;
          }
#ifdef FLOATS
          case T_FLOAT:
            if (type2 == T_FLOAT) {
                STORE_DOUBLE_USED
                double d;

/* don't use the address of u2, this would prevent putting it in a register */
                sp--;
                d = READ_DOUBLE(argp) - READ_DOUBLE(sp);
                STORE_DOUBLE(argp, d);
                *sp = *argp;
            } else {
                goto bad_right;
            }
            break;
#endif /* FLOATS */
#ifdef MAPPINGS
          case T_MAPPING:
            if (type2 == T_MAPPING && !sp[-1].u.map->num_values) {
                struct mapping *m;

                sp--;
                m = sp->u.map;
                if (m == argp->u.map) {
                    /* m->ref > 1, because the content of the lvalue is
                     * associated with a ref
                     */
                    m->ref--;
                    m = copy_mapping(m);
                }
                walk_mapping(
                  m,
                  sub_from_mapping_filter,
                  (char *)argp->u.map
                );
                free_mapping(m);
                (sp->u.map = argp->u.map)->ref++;
            } else {
                goto bad_right;
            }
            break;
#endif /* MAPPINGS */
          case T_LVALUE:
          case T_PROTECTED_LVALUE:
            argp = argp->u.lvalue;
            continue;
          default:
            goto bad_left;
         }
         break;
        }
        break;
    }
    CASE(F_MULT_EQ);
    {
        struct svalue *argp;
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            goto bad_arg_1;
#endif
        for(argp = sp->u.lvalue; ; argp = argp->u.lvalue) {
            if (argp->type == T_NUMBER) {
                sp--;
                if (sp->type != T_NUMBER)
                    goto bad_right;
                sp->u.number = argp->u.number *= sp->u.number;
                break;
            }
            if (argp->type == T_LVALUE || argp->type == T_PROTECTED_LVALUE)
                continue;
#ifdef FLOATS
            if (argp->type == T_FLOAT) {
                STORE_DOUBLE_USED
                double d;

                sp--;
                if (sp->type != T_FLOAT)
                    goto bad_right;
                d = READ_DOUBLE(argp) * READ_DOUBLE(sp);
                STORE_DOUBLE(argp, d);
                *sp = *argp;
                break;
            }
#endif
            goto bad_left;
        }
        break;
    }
    CASE(F_AND_EQ);
    {
        struct svalue *argp;
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            goto bad_arg_1;
#endif
        for (argp = sp->u.lvalue; ; argp = argp->u.lvalue) {
            if (argp->type == T_NUMBER) {
                if (sp[-1].type != T_NUMBER)
                    goto bad_right;
                sp--;
                sp->u.number = argp->u.number &= sp->u.number;
                break;
            }
            if (argp->type == T_LVALUE || argp->type == T_PROTECTED_LVALUE)
                continue;
            if (argp->type == T_POINTER && sp[-1].type == T_POINTER) {
                struct vector *vec1, *vec2;

                inter_sp = sp - 2;
                vec1 = argp->u.vec;
                vec2 = sp[-1].u.vec;
                argp->type = T_NUMBER;
                vec1 = intersect_array(vec1, vec2);
                argp->type = T_POINTER;
                sp--;
                vec1->ref++;
                sp->u.vec = argp->u.vec = vec1;
                free_svalue(sp+1);
                break;
            }
            if (argp->type == T_LVALUE || argp->type == T_PROTECTED_LVALUE)
                continue;
            goto bad_left;
        }
        break;
    }
    CASE(F_OR_EQ);
    {
        struct svalue *argp;
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            goto bad_arg_1;
#endif
        for (argp = sp->u.lvalue; ; argp = argp->u.lvalue) {
            if (argp->type == T_NUMBER) {
                sp--;
                if (sp->type != T_NUMBER)
                    goto bad_right;
                sp->u.number = argp->u.number |= sp->u.number;
                break;
            }
            if (argp->type == T_LVALUE || argp->type == T_PROTECTED_LVALUE)
                continue;
            goto bad_left;
        }
        break;
    }
    CASE(F_XOR_EQ);
    {
        struct svalue *argp;
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            goto bad_arg_1;
#endif
        for (argp = sp->u.lvalue; ; argp = argp->u.lvalue) {
            if (argp->type == T_NUMBER) {
                sp--;
                if (sp->type != T_NUMBER)
                    goto bad_right;
                sp->u.number = argp->u.number ^= sp->u.number;
                break;
            }
            if (argp->type == T_LVALUE || argp->type == T_PROTECTED_LVALUE)
                continue;
            goto bad_left;
        }
        break;
    }
    CASE(F_LSH_EQ);
    {
        int i;
        struct svalue *argp;
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            goto bad_arg_1;
#endif
        for (argp = sp->u.lvalue; ; argp = argp->u.lvalue) {
            if (argp->type == T_NUMBER) {
                sp--;
                if (sp->type != T_NUMBER)
                    goto bad_right;
                i = sp->u.number;
                sp->u.number =
                  (uint)i > MAX_SHIFT ? (argp->u.number = 0) : (argp->u.number <<= i);
                break;
            }
            if (argp->type == T_LVALUE || argp->type == T_PROTECTED_LVALUE)
                continue;
            goto bad_left;
        }
        break;
    }
    CASE(F_RSH_EQ);
    {
        int i;
        struct svalue *argp;
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            goto bad_arg_1;
#endif
        for (argp = sp->u.lvalue; ; argp = argp->u.lvalue) {
            if (argp->type == T_NUMBER) {
                sp--;
                if (sp->type != T_NUMBER)
                    goto bad_right;
                i = sp->u.number;
                sp->u.number = argp->u.number >>= (uint)i > MAX_SHIFT ? MAX_SHIFT : i;
                break;
            }
            if (argp->type == T_LVALUE || argp->type == T_PROTECTED_LVALUE)
                continue;
            goto bad_left;
        }
        break;
    }
#ifdef F_COMBINE_FREE_LIST
    CASE(F_COMBINE_FREE_LIST);
#ifdef MALLOC_malloc
        push_number(resort_free_list());
#else
        push_number(0);
#endif
        break;
#endif
    CASE(F_DIV_EQ);
    {
        struct svalue *argp;
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            goto bad_arg_1;
#endif
        for (argp = sp->u.lvalue; ; argp = argp->u.lvalue) {
            if (argp->type == T_NUMBER) {
                sp--;
                if (sp->type != T_NUMBER)
                    goto bad_right;
                if (sp->u.number == 0)
                    ERROR("Division by 0\n")
                sp->u.number = argp->u.number /= sp->u.number;
                break;
            }
#ifdef FLOATS
            if (argp->type == T_FLOAT) {
                STORE_DOUBLE_USED
                double d;

                sp--;
                if (sp->type != T_FLOAT)
                    goto bad_right;
                d = READ_DOUBLE(sp);
                if (d == 0.0)
                    ERROR("Division by 0\n")
                d = READ_DOUBLE(argp) / d;
                STORE_DOUBLE(argp, d);
                *sp = *argp;
                break;
            }
#endif
            if (argp->type == T_LVALUE || argp->type == T_PROTECTED_LVALUE)
                continue;
            goto bad_left;
        }
        break;
    }
    CASE(F_MOD_EQ);
    {
        struct svalue *argp;
#ifdef DEBUG
        if (sp->type != T_LVALUE)
            goto bad_arg_1;
#endif
        for (argp = sp->u.lvalue; ; argp = argp->u.lvalue) {
            if (argp->type == T_NUMBER) {
                sp--;
                if (sp->type != T_NUMBER)
                    goto bad_right;
                if (sp->u.number == 0)
                    ERROR("Division by 0\n")
                sp->u.number = argp->u.number %= sp->u.number;
                break;
            }
            if (argp->type == T_LVALUE || argp->type == T_PROTECTED_LVALUE)
                continue;
            goto bad_left;
        }
        break;
    }
    CASE(F_STRING);
    {
        unsigned short string_number[2];
        ((char *)string_number)[0] = pc[0];
        ((char *)string_number)[1] = pc[1];
        pc += 2;
        /* amylaar: STRING_CONSTANT is no good here, because strings are
                    freed when the program is freed */
        push_shared_string(current_strings[string_number[0]]);
        break;
    }
    CASE(F_CSTRING3);
    {
        push_shared_string(current_strings[EXTRACT_UCHAR(pc)+0x300]);
        pc++;
        break;
    }
    CASE(F_CSTRING2);
    {
        push_shared_string(current_strings[EXTRACT_UCHAR(pc)+0x200]);
        pc++;
        break;
    }
    CASE(F_CSTRING1);
    {
        push_shared_string(current_strings[EXTRACT_UCHAR(pc)+0x100]);
        pc++;
        break;
    }
    CASE(F_CSTRING0);
    {
        /* amylaar: STRING_CONSTANT is no good here, because strings are
                    freed when the program is freed */
        push_shared_string(current_strings[EXTRACT_UCHAR(pc)]);
        pc++;
        break;
    }
#ifdef F_RUSAGE
    CASE(F_RUSAGE);
    {
        struct rusage rus;
        struct vector *res;
        struct svalue *v;
#ifndef GETRUSAGE_RESTRICTED
        int maxrss;
#endif

        if (getrusage(RUSAGE_SELF, &rus) < 0) {
            push_number(0);
            break;
        }
        res = allocate_array(16);
        v = res->item;
        v[ 0].u.number = RUSAGE_TIME(rus.ru_utime);
        v[ 1].u.number = RUSAGE_TIME(rus.ru_stime);
#ifndef GETRUSAGE_RESTRICTED
        maxrss = rus.ru_maxrss;
#ifdef sun
        maxrss *= getpagesize() / 1024;
#endif
        v[ 2].u.number = maxrss;
        v[ 3].u.number = rus.ru_ixrss;
        v[ 4].u.number = rus.ru_idrss;
        v[ 5].u.number = rus.ru_isrss;
        v[ 6].u.number = rus.ru_minflt;
        v[ 7].u.number = rus.ru_majflt;
        v[ 8].u.number = rus.ru_nswap;
        v[ 9].u.number = rus.ru_inblock;
        v[10].u.number = rus.ru_oublock;
        v[11].u.number = rus.ru_msgsnd;
        v[12].u.number = rus.ru_msgrcv;
        v[13].u.number = rus.ru_nsignals;
        v[14].u.number = rus.ru_nvcsw;
        v[15].u.number = rus.ru_nivcsw;
#endif /* GETRUSAGE_RESTRICTED */
        push_referenced_vector(res);
        break;
    }
#endif /* F_RUSAGE */
#ifdef F_DESCRIBE
    CASE(F_DESCRIBE);
    {
        char *str;
        int live;

        assign_eval_cost();
        GET_NUM_ARG
        if (num_arg < 3) live = 0;
        else {
            if (sp->type != T_NUMBER) goto bad_arg_3;
            live = sp->u.number;
            pop_stack ();
        }
        TYPE_TEST2(sp, T_STRING)
        str = describe_items(sp-1, sp->u.string, live);
        pop_n_elems(2);
        if (str) push_malloced_string (string_copy (str));
        else     push_number(0);
        break;
    }
#endif /* F_DESCRIBE */
    CASE(F_UNIQUE_ARRAY); {
        struct vector *res;

        assign_eval_cost();
        inter_pc = pc;
        inter_sp = sp;
        TYPE_TEST1(sp-2, T_POINTER)
        TYPE_TEST2(sp-1, T_STRING)
        check_for_destr((sp-2)->u.vec);
        res = make_unique((sp-2)->u.vec, (sp-1)->u.string, sp);
        pop_stack ();
        pop_stack ();
        free_svalue(sp);
        if (res) {
            sp->type  = T_POINTER;
            sp->u.vec = res;                /* ref count is already 1 */
        } else
            put_number (0);
        break;
    }
#ifdef F_RENAME
    CASE(F_RENAME);
    {
        int i;

        assign_eval_cost();
        inter_pc = pc;
        inter_sp = sp;
        TYPE_TEST1(sp-1, T_STRING)
        TYPE_TEST2(sp,   T_STRING)
        i = do_rename((sp-1)->u.string, sp->u.string);
        pop_n_elems(2);
        push_number(i);
        break;
    }
#endif
    CASE(F_MAP_ARRAY); {
        struct vector *res;
        struct svalue *arg;
        char *func;
        struct object *ob;
        int num_extra;

        assign_eval_cost();
        GET_NUM_ARG
        inter_pc = pc;
        inter_sp = sp;
        arg = sp - num_arg + 1; ob = 0;
        TYPE_TEST1(arg,   T_POINTER)
        ob = 0;
        if (arg[1].type == T_CLOSURE) {
            func = (char *)(arg + 1);
            num_extra = num_arg - 2;
        } else {
            TYPE_TEST2(arg+1, T_STRING)
            func = arg[1].u.string;

            if (num_arg > 2) {
                if (arg[2].type == T_OBJECT)
                    ob = arg[2].u.ob;
                else if (arg[2].type == T_STRING &&
                    NULL != ( ob = get_object(arg[2].u.string) )) NOOP;
                else goto bad_arg_3;
                num_extra = num_arg - 3;
            } else {
                ob = current_object;
                num_extra = 0;
            }
        }

        if (arg[0].type == T_POINTER) {
            check_for_destr(arg[0].u.vec);
            map_array (arg[0].u.vec, func, ob,
                        num_extra, sp - num_extra + 1);
            res = sp[1].u.vec;
        } else {
            res = 0;
        }
        pop_n_elems (num_arg - 1);
        free_svalue(sp);
        if (res) {
            put_referenced_vector (res);
        } else
            put_number (0);
        break;
    }
    CASE(F_SORT_ARRAY);
    {
        struct vector *res;
        struct svalue *arg;
        struct object *ob;
        char *func;

        assign_eval_cost();
        inter_pc = pc;
        TYPE_TEST1(sp-2, T_POINTER)
        inter_sp = sp;
        arg = sp - 2; ob = 0;

        if (arg[1].type == T_CLOSURE) {
            func = (char *)(arg + 1);
        } else {
            TYPE_TEST2(arg+1, T_STRING)
            func = arg[1].u.string;
            if (arg[2].type == T_OBJECT)
                ob = arg[2].u.ob;
            else if (arg[2].type == T_STRING)
                ob = get_object(arg[2].u.string);

            if (!ob)
                goto bad_arg_3;
        }

        if (arg[0].type == T_POINTER) {
            /* sort_array already takes care of destructed objects */
            res = sort_array (
              slice_array(arg[0].u.vec, 0, VEC_SIZE(arg[0].u.vec)-1),
              func, ob);
        } else
            res = 0;
        pop_n_elems (3);
        sp++;
        if (res) {
            sp->type = T_POINTER;
            sp->u.vec = res;
        }
        else
            put_number(0);
        break;
    }
#ifdef F_ORDER_ALIST
    CASE(F_ORDER_ALIST);
    {
        int i;
        struct svalue *args;
        struct vector *list;
        int listsize;
        int reuse;
        size_t keynum;

        GET_NUM_ARG
        args = sp-num_arg+1;
        TYPE_TEST1(args, T_POINTER)
        if (num_arg == 1 && ((list = args->u.vec), (listsize = VEC_SIZE(list)))
          && list->item[0].type == T_POINTER) {
            args     = list->item;
            reuse = list->ref == 1;
        } else {
            listsize = num_arg;
            reuse = 1;
        }
        keynum = VEC_SIZE(args[0].u.vec);
        for (i=0; i<listsize; i++) {
            if (args[i].type != T_POINTER
             || VEC_SIZE(args[i].u.vec) != keynum) {
                ERRORF(("bad data array %d in call to order_alist\n",i))
            }
        }
        list = order_alist(args, listsize, reuse);
        pop_n_elems(num_arg);
        sp++;
        sp->type = T_POINTER;
        sp->u.vec = list;
        break;
    }
#endif /* F_ORDER_ALIST */
#ifdef F_INSERT_ALIST
    CASE(F_INSERT_ALIST)
    {
        /* When the key list of an alist contains destructed objects
           it is better not to free them till the next reordering by
           order_alist to retain the alist property.
         */
        int i;
        struct vector *list;
        int listsize;
        size_t keynum;
        struct svalue *key,*key_data,*ret;
        static struct { INIT_VEC_TYPE; } tempvec =
          { VEC_INIT(1, 1, T_NUMBER) };

        GET_NUM_ARG
        if (sp->type != T_POINTER)
            bad_arg_pc(num_arg,F_INSERT_ALIST-F_OFFSET, sp, pc);
        if ( !(listsize = VEC_SIZE(sp->u.vec)) ||
          sp->u.vec->item[0].type != T_POINTER ) {
            list = &tempvec.v;
            *list->item = *sp;
            listsize = 1;
        } else
            list = sp->u.vec;
        keynum = VEC_SIZE(list->item[0].u.vec);
        for (i=1; i<listsize; i++) {
            if (list->item[i].type != T_POINTER ||
                VEC_SIZE(list->item[i].u.vec) != keynum)
            {
                bad_arg_pc(num_arg,F_INSERT_ALIST-F_OFFSET, sp, pc);
            }
        }
        if (num_arg == 2) {
            if (sp[-1].type != T_POINTER) {
                key_data = (struct svalue*)NULL;
                key = sp-1;
            } else {
                if (VEC_SIZE(sp[-1].u.vec) != (size_t)listsize)
                    goto bad_arg_1;
                key_data = key = sp[-1].u.vec->item;
            }
        } else {
            if (num_arg - 1 != listsize)
                goto bad_arg_1;
            key_data = key = sp-num_arg+1;
        }
        inter_sp = sp; /* array might get too big */
        ret = insert_alist(key,key_data,list);
        pop_n_elems(num_arg);
        sp++;
        *sp = *ret;
        break;
    }
#endif /* F_INSERT_ALIST */
#ifdef F_ASSOC
    CASE(F_ASSOC);
    {
        /* When the key list of an alist contains destructed objects
           it is better not to free them till the next reordering by
           order_alist to retain the alist property.
         */
        struct svalue *args;
        struct vector *keys,*data;
        struct svalue *fail_val;
        int ix;

        GET_NUM_ARG
        args = sp -num_arg +1;
        TYPE_TEST2(args+1, T_POINTER)
        if ( !VEC_SIZE(args[1].u.vec) ||
          args[1].u.vec->item[0].type != T_POINTER ) {
            keys = args[1].u.vec;
            if (num_arg == 2) {
                data = (struct vector *)NULL;
            } else {
                if (args[2].type != T_POINTER ||
                  VEC_SIZE(args[2].u.vec) != VEC_SIZE(keys)) {
                    goto bad_arg_3;
                }
                data = args[2].u.vec;
            }
            if (num_arg == 4) {
                fail_val = &args[3];
            } else {
                fail_val = &const0;
            }
        } else {
            keys = args[1].u.vec->item[0].u.vec;
            if (VEC_SIZE(args[1].u.vec) > 1) {
                if (args[1].u.vec->item[1].type != T_POINTER ||
                    VEC_SIZE(args[1].u.vec->item[1].u.vec) != VEC_SIZE(keys))
                {
                        goto bad_arg_2;
                }
                data = args[1].u.vec->item[1].u.vec;
            } else {
                data = (struct vector *)NULL;
            }
            if (num_arg == 3) fail_val = &args[2];
            else if (num_arg == 2) fail_val = &const0;
            else {
                ERROR ("too many args to efun assoc\n")
                return;
            }
        }
        ix = assoc(&args[0],keys);
        if (data == (struct vector *)NULL) {
            pop_n_elems(num_arg);
            push_number(ix);
        } else {
            assign_svalue(args, ix==-1 ? fail_val :
                (data->item[ix].type == T_OBJECT &&
                 data->item[ix].u.ob->flags & O_DESTRUCTED ?
                        &const0 : &data->item[ix]));
            pop_n_elems(num_arg-1);
        }
        break;
    }
#endif /* F_ASSOC */
#ifdef F_INTERSECT_ALIST
    CASE(F_INTERSECT_ALIST);
    {
        struct vector *tmp;

        TYPE_TEST1(sp-1, T_POINTER)
        TYPE_TEST2(sp,   T_POINTER)
        tmp = intersect_alist( (sp-1)->u.vec, sp->u.vec );
        pop_stack();
        free_vector(sp->u.vec);
        sp->u.vec = tmp;
        break;
    }
#endif /* F_INTERSECT_ALIST */
    CASE(F_REPLACE_PROGRAM);
    {
        struct replace_ob *tmp;
        int name_len;
        char *name;
        struct program *new_prog;
        int offsets[2];

        TYPE_TEST1(sp, T_STRING)
#ifdef DEBUG
        if (d_flag)
            debug_message("replace_program called\n");
#endif
        if (!current_object)
            ERROR("replace_program called with no current object\n")
        if (current_object == simul_efun_object)
            ERROR("replace_program on simul_efun object\n")
        if (current_object->flags & O_LAMBDA_REFERENCED)
            ERROR(
              "Cannot schedule replace_program after binding lambda closures\n")
        name_len = svalue_strlen(sp);
        name = alloca(name_len+3);
        strcpy(name,sp->u.string);
        if (name[name_len-2] != '.' || name[name_len-1] != 'c')
            strcat(name,".c");
        if (*name == '/')
            name++;
        new_prog = search_inherited(name, current_object->prog, offsets);
        if (!new_prog) {
            if ( strcmp(name, current_object->prog->name ) == 0 ) {
                new_prog = current_object->prog;
                offsets[0] = offsets[1] = 0;
            } else {
                ERROR("\
program to replace the current one with has to be inherited\n")
            }
        }
        if ( !(current_object->prog->flags & P_REPLACE_ACTIVE) ||
            !(tmp = retrieve_replace_program_entry()) )
        {
            tmp = (struct replace_ob *)xalloc(sizeof *tmp);
            tmp->lambda_rpp = 0;
            tmp->ob = current_object;
            tmp->next = obj_list_replace;
            obj_list_replace = tmp;
            current_object->prog->flags |= P_REPLACE_ACTIVE;
        }
        tmp->new_prog = new_prog;
        tmp->var_offset = offsets[0];
        tmp->fun_offset = offsets[1];
#ifdef DEBUG
        if (d_flag)
            debug_message("replace_program finished\n");
#endif
        pop_stack();
        break;
    }
    CASE(F_SET_THIS_OBJECT);
    {
        TYPE_TEST1(sp, T_OBJECT)
        if (current_variables == master_ob->variables ||
            current_variables == simul_efun_object->variables ||
            privilege_violation("set_this_object", sp) > 0 )
        {
            struct control_stack *p;

            for (p = csp; !p->extern_call; p--) NOOP;
            p->extern_call |= 0x80;
            p->pretend_to_be = current_object = sp->u.ob;
        }
        pop_stack();
        break;
    }
#ifdef MAPPINGS
    CASE(F_MAP_INDEX);
    {
        struct mapping *m;
        mp_uint n;
        struct svalue *data;

        TYPE_TEST1(sp-2, T_MAPPING)
        TYPE_TEST3(sp, T_NUMBER)
        m = sp[-2].u.map;
        n = (mp_uint)sp->u.number;
        if (n >= (mp_uint)m->num_values) { /* again also checks for n < 0 */
            ERROR("Illegal index\n")
        }
        sp--;
        data = get_map_lvalue(m, sp, 0);
        pop_stack();
        if (data == &const0) {
            put_number(0);
        } else {
            assign_checked_svalue_no_free(sp, data + n, sp, pc);
        }
        free_mapping(m);
        break;
    }
    CASE(F_PUSH_INDEXED_MAP_LVALUE);
    {
        struct svalue *data;
        struct mapping *m;
        mp_uint n;

        TYPE_TEST1(sp-2, T_MAPPING)
        TYPE_TEST3(sp, T_NUMBER)
        m = sp[-2].u.map;
        n = (mp_uint)sp->u.number;
        if (n >= (mp_uint)m->num_values) { /* again also checks for n < 0 */
            ERROR("Illegal index\n")
        }
        sp--;
        data = get_map_lvalue(m, sp, 1);
        pop_stack();
        if (!--m->ref) {
            assign_svalue (&indexing_quickfix, data + n);
            m->ref++;
            free_mapping(m);
            sp->type = T_LVALUE;
            sp->u.lvalue = &indexing_quickfix;
            break;
        }
        sp->type = T_LVALUE;
        sp->u.lvalue = data + n;
        break;
    }
#endif /* MAPPINGS */
    CASE(F_SYMBOL);
    {
        char *str;

        unsigned short string_number[2];
        ((char *)string_number)[0] = pc[0];
        ((char *)string_number)[1] = pc[1];
        sp++;
        sp->type = T_SYMBOL;
        sp->x.quotes = EXTRACT_UCHAR(&pc[2]);
        sp->u.string = str = current_strings[string_number[0]];
        pc += 3;
        increment_string_ref(str);
        break;
    }
    CASE(F_QUOTE);
    {
        switch (sp->type) {
          case T_QUOTED_ARRAY:
          case T_SYMBOL:
            sp->x.quotes++;
            break;
          case T_POINTER:
            sp->type = T_QUOTED_ARRAY;
            sp->x.quotes = 1;
            break;
          case T_STRING:
            if (sp->x.string_type != STRING_SHARED) {
                char *str = sp->u.string;
                sp->u.string = make_shared_string(str);
                if (sp->x.string_type == STRING_MALLOC)
                    xfree(str);
            }
            sp->type = T_SYMBOL;
            sp->x.quotes = 1;
            break;
          default:
            goto bad_arg_1;
        }
        break;
    }
    CASE(F_CLOSURE);
    {
        unsigned short tmp_ushort[2];
        int ix;

        ((char *)tmp_ushort)[0] = pc[0];
        ((char *)tmp_ushort)[1] = pc[1];
        pc += 2;
        ix = tmp_ushort[0];
        if (ix < 0xf000) {
            sp++;
            closure_literal(sp, ix);
        } else {
            sp++;
            sp->type = T_CLOSURE;
            sp->x.closure_type = (short)(ix >= CLOSURE_SIMUL_EFUN_OFFS ? ix :
              (instrs[ix - CLOSURE_EFUN_OFFS].Default == -1 ?
                ix + CLOSURE_OPERATOR-CLOSURE_EFUN :
                    ix));
            add_ref(sp->u.ob = current_object, "closure");
        }
        break;
    }
    CASE(F_SYMBOL_FUNCTION);
    {
        struct object *ob;
        struct program *prog;
        int i;

        if (sp[-1].type != T_SYMBOL)
            if (sp[-1].type == T_STRING) {
                if (sp[-1].x.string_type != STRING_SHARED) {
                    char *str;

                    str = sp[-1].u.string;
                    sp[-1].u.string = make_shared_string(str);
                    if (sp[-1].x.string_type == STRING_MALLOC)
                        xfree(str);
                    sp[-1].x.string_type = STRING_SHARED;
                }
            } else goto bad_arg_1;
        if (sp->type != T_OBJECT) {
            if (sp->type == T_NUMBER && sp->u.number == 0) {
                sp--;
                inter_pc = pc;
                symbol_efun(sp);
                break;
            }
            TYPE_TEST2(sp, T_STRING)
            inter_sp = sp;
            inter_pc = pc;
            ob = get_object(sp->u.string);
            if (ob == 0)
                error("Object not found.\n");
            add_ref(ob, "symbol_function");
            free_svalue(sp);
            sp->u.ob = ob;
            sp->type = T_OBJECT;
        } else {
            ob = sp->u.ob;
        }
        if (O_PROG_SWAPPED(ob)) {
            if (load_ob_from_swap(ob) < 0) {
                inter_sp = sp;
                error("Out of memory\n");
            }
        }
        prog = ob->prog;
        i = find_function(sp[-1].u.string, prog);
        if ( i >= 0 &&
             ( !(prog->functions[i] & (TYPE_MOD_STATIC|TYPE_MOD_PRIVATE) ) ||
               ( !(prog->functions[i] & TYPE_MOD_PRIVATE) &&
                 current_object == ob) ) )
        {
            struct lambda *l;
            ph_int closure_type;

            if (ob == current_object) {
                l = (struct lambda *)
                    xalloc(sizeof *l - sizeof l->function +
                           sizeof l->function.index);
                l->function.index = i;
                closure_type = CLOSURE_LFUN;
            } else {
                l = (struct lambda *)
                    xalloc(sizeof *l - sizeof l->function +
                           sizeof l->function.alien);
                l->function.alien.ob = ob;
                l->function.alien.index = i;
                closure_type = CLOSURE_ALIEN_LFUN;
                add_ref(current_object, "symbol_function");
            }
            l->ref = 1;
            l->ob = current_object;
            sp--;
            decrement_string_ref(sp->u.string);
            sp->type = T_CLOSURE;
            sp->x.closure_type = closure_type;
            sp->u.lambda = l;
            if ( !(prog->flags & P_REPLACE_ACTIVE) ||
                 !lambda_ref_replace_program(l, closure_type, 0, 0, 0) )
            {
                ob->flags |= O_LAMBDA_REFERENCED;
            }
            break;
        }
        free_object(ob, "symbol_function");
        sp--;
        free_string(sp->u.string);
        put_number(0);
        break;
    }
    CASE(F_LAMBDA);
    {
        struct lambda *l;
        struct vector *args;

        inter_pc = pc;
        inter_sp = sp;
        if (sp[-1].type != T_POINTER) {
            if (sp[-1].type != T_NUMBER || sp[-1].u.number)
                goto bad_arg_1;
            (args = &null_vector)->ref++;
        } else {
            args = sp[-1].u.vec;
        }
        l = lambda(args, sp, current_object);
        add_ref(l->ob = current_object, "lambda");
        pop_stack();
        free_vector(args);
        sp->type = T_CLOSURE;
        sp->x.closure_type = CLOSURE_LAMBDA;
        sp->u.lambda = l;
        break;
    }
    CASE(F_APPLY);
    {
        struct svalue *args;

        GET_NUM_ARG
        if (sp->type == T_POINTER) {
            struct vector *vec;
            struct svalue *svp;
            int i;

            vec = sp->u.vec;
            if ( (num_arg += (i = VEC_SIZE(vec)) - 1) > 0x100) {
                switch( (sp - num_arg + i)->x.closure_type ) {
                  case CLOSURE_LFUN:
                  case CLOSURE_ALIEN_LFUN:
                  case CLOSURE_LAMBDA:
                  case CLOSURE_BOUND_LAMBDA:
                    if (num_arg < EVALUATOR_STACK_SIZE)
                        break;
                  default:
                    bad_arg_pc(num_arg - i + 1, instruction, sp, pc);
                }
            }
            if (--vec->ref) {
                for (svp = vec->item; --i >= 0; ) {
                    assign_svalue_no_free(sp++, svp++);
                }
            } else {
                for (svp = vec->item; --i >= 0; ) {
                    transfer_svalue_no_free_spc(sp++, svp++, sp, pc);
                }
                free_empty_vector(vec);
            }
            sp--;
        }

        args = sp -num_arg +1;
        TYPE_TEST1(args, T_CLOSURE)
        if (current_object->flags & O_DESTRUCTED) {
            /*
             * No external calls may be done when this object is
             * destructed.
             */
            pop_n_elems(num_arg);
            push_number(0);
            break;
        }
        inter_pc = pc;
        inter_sp = sp;
        call_lambda(args, num_arg - 1);
        sp = args;
        free_closure(sp);
        *sp = sp[1];
        break;
    }
    CASE(F_FUNCALL);
    {
        struct svalue *args;

        GET_NUM_ARG
        args = sp -num_arg +1;
        if (args->type == T_CLOSURE) {
            if (current_object->flags & O_DESTRUCTED) {
                /*
                 * No external calls may be done when this object is
                 * destructed.
                 */
                pop_n_elems(num_arg);
                push_number(0);
                break;
            }
            inter_pc = pc;
            inter_sp = sp;
            call_lambda(args, num_arg - 1);
            sp = args;
            free_closure(sp);
            *sp = sp[1];
            break;
        } else {
            while (--num_arg)
                free_svalue(sp--);
            break;
        }
    }
    CASE(F_BIND_LAMBDA);
    {
        struct object *ob;

        TYPE_TEST1(sp-1, T_CLOSURE)
        if (sp->type == T_OBJECT) {
            ob = sp->u.ob;
            if (ob != current_object &&
                privilege_violation("bind_lambda", sp) <= 0 )
            {
                free_object(ob, "bind_lambda");
                sp--;
                break;
            }
        } else if (sp->type == T_NUMBER && sp->u.number == 1) {
            ob = current_object;
            add_ref(ob, "bind_lambda");
        } else goto bad_arg_2;
        sp--;
        switch(sp->x.closure_type) {
          case CLOSURE_LFUN:
          case CLOSURE_LAMBDA:
          case CLOSURE_IDENTIFIER:
          case CLOSURE_PRELIMINARY:
            free_object(ob, "bind_lambda");
            if (sp[1].type == T_NUMBER)
                break;
            goto bad_arg_1;
          case CLOSURE_ALIEN_LFUN:
            free_object(sp->u.lambda->ob, "bind_lambda");
            sp->u.lambda->ob = ob;
            break;
          default:
            /* efun, simul_efun, operator closures */
            free_object(sp->u.ob, "bind_lambda");
            sp->u.ob = ob;
            break;
          case CLOSURE_BOUND_LAMBDA:
          {
            struct lambda *l;

            if ( (l = sp->u.lambda)->ref == 1) {
                struct object **obp;

                obp = &l->ob;
                free_object(*obp, "bind_lambda");
                *obp = ob;
                break;
            } else {
                struct lambda *l2;

                l->ref--;
                l2 = xalloc(
                    sizeof *l - sizeof l->function + sizeof l->function.lambda
                );
                l2->ref = 1;
                l2->ob = ob;
                l2->function.lambda = l->function.lambda;
                sp->u.lambda = l2;
                break;
            }
          }
          case CLOSURE_UNBOUND_LAMBDA:
          {
            struct lambda *l;

            l = (struct lambda *)xalloc(
                sizeof *l - sizeof l->function + sizeof l->function.lambda
            );
            l->ref = 1;
            l->ob = ob;
            l->function.lambda = sp->u.lambda;
            sp->x.closure_type = CLOSURE_BOUND_LAMBDA;
            sp->u.lambda = l;
            break;
          }
        }
        break;
    }
    CASE(F_LAMBDA_CCONSTANT);
    {
#define LAMBDA_VALUE_OFFSET (sizeof(struct svalue) + \
            ((PTRTYPE)(&((struct lambda *)0)->function.code[1])-(PTRTYPE) 0) )
        int ix = EXTRACT_UCHAR(pc);
        pc++;
        sp++;
        assign_checked_svalue_no_free(
          sp,
          ((struct svalue *)(csp->funstart - LAMBDA_VALUE_OFFSET)) - ix
        );
        break;
    }
    CASE(F_LAMBDA_CONSTANT);
    {
        int ix = (EXTRACT_UCHAR(pc) << 8) + EXTRACT_UCHAR(pc + 1);
        pc += 2;
        sp++;
        assign_checked_svalue_no_free(
          sp,
          ((struct svalue *)(csp->funstart - LAMBDA_VALUE_OFFSET)) - ix
        );
        break;
    }
    CASE(F_TEFUN)
    {
        int code = EXTRACT_UCHAR(pc);
        pc++;
#ifdef TRACE_CODE
        previous_instruction[last] = code + 0x100;
#endif
#ifdef OPCPROF
    opcount[code+0x100]++;
#endif
        inter_pc = pc;
        sp = (*efun_table[code-128])(sp);
        break;
    }
    CASE(F_VEFUN)
    {
        int code = EXTRACT_UCHAR(pc);
        int numarg = EXTRACT_UCHAR(pc+1);
        pc += 2;
#ifdef TRACE_CODE
        previous_instruction[last] = code + 0x200;
#endif
#ifdef OPCPROF
    opcount[code+0x200]++;
#endif
        inter_pc = pc;
        sp = (*vefun_table[code])(sp, numarg);
        break;
    }
    CASE(F_ESCAPE);
    {

#define XCASE(x) CASE((x)-0x100)
#undef GET_NUM_ARG
#define GET_NUM_ARG num_arg = EXTRACT_UCHAR(pc); pc++;

#undef TYPE_TEST1
#define TYPE_TEST1(arg, t) if ( (arg)->type != t ) goto xbad_arg_1;

        int code = EXTRACT_UCHAR(pc);

#ifdef TRACE_CODE
        previous_instruction[last] = code + 0x100;
#endif

#ifdef OPCPROF
        opcount[code+0x100]++;
#endif

        pc++;

        switch(code) {
          XCASE(F_END_CATCH);
            pop_stack();
            /* We come here when no longjmp() was executed. */
            pop_control_stack();
            pc = inter_pc;
            fp = inter_fp;
            pop_error_context();
            push_number(0);
            eval_cost -= CATCH_RESERVED_COST;
            break;
          XCASE(F_BREAKN_CONTINUE);
            break_sp +=
              EXTRACT_UCHAR(pc) * (sizeof(struct svalue)/sizeof(*break_sp));
            pc++;
            /* fall through */
          XCASE(F_BREAK_CONTINUE);
          {
            unsigned short offset[2];

            break_sp += sizeof(struct svalue)/sizeof(*break_sp);
            ((char *)offset)[0] = pc[0];
            ((char *)offset)[1] = pc[1];
            offset[0] += pc - current_prog->program;
            pc = current_prog->program + offset[0];
            break;
          }
          XCASE(F_QUERY_IP_NUMBER);
          {
            inter_pc = pc;
            sp = query_ip_name(sp, MY_FALSE);
            break;
          }
          XCASE(F_QUERY_IP_NAME);
          {
            inter_pc = pc;
            sp = query_ip_name(sp, MY_TRUE);
            break;
          }
          XCASE(F_EXTRACT1);
          {
            if (sp->type != T_STRING) goto xbad_arg_1;
            break;
          }
          XCASE(F_EXTRACT);
          {
            if (sp[-1].type != T_NUMBER) goto xbad_arg_2;
            if (sp[0].type != T_NUMBER) goto xbad_arg_3;
            if (sp[-2].type == T_POINTER) {
                struct vector *v;
                mp_int end, size;

                v = sp[-2].u.vec;
                v =
                  slice_array(
                    v,
                    sp[-1].u.number,
                    (end = sp[0].u.number) >= (size = VEC_SIZE(v)) ?
                      size - 1 :
                      end
                  );
                pop_n_elems(3);
                if (v) {
                    push_referenced_vector(v);
                } else {
                    push_number(0);
                }
            } else if (sp[-2].type == T_STRING) {
                int len, from, to;
                char *res;

                len = _svalue_strlen(&sp[-2]);
                from = sp[-1].u.number;
                if (from < 0) {
                    from = len + from;
                    if (from < 0)
                        from = 0;
                }
                to = sp[0].u.number;
                if (to < 0)
                    to = len + to;
                if (to >= len)
                    to = len-1;
                if (to < from) {
                    pop_n_elems(3);
                    push_constant_string("");
                    break;
                }
                if (to == len-1) {
                    res = string_copy(sp[-2].u.string + from);
                    pop_n_elems(3);
                    push_malloced_string(res);
                    break;
                }
                res = xalloc(to - from + 2);
                strncpy(res, sp[-2].u.string + from, to - from + 1);
                res[to - from + 1] = '\0';
                pop_n_elems(3);
                push_malloced_string(res);
            } else {
                goto xbad_arg_1;
            }
            break;
          }
          XCASE(F_EXTERN_CALL);
          {
            push_number(csp->extern_call&1);
            break;
          }
          XCASE(F_PREVIOUS_OBJECT);
          {
            int i;
            struct control_stack *p;
            struct object *prev_ob;

            if (sp->type != T_NUMBER) goto xbad_arg_1;
            i = sp->u.number;
            if (i > MAX_TRACE) {
                sp->u.number = 0;
                break;
            }
            p = csp;
            do {
                do {
                    if (p == control_stack) {
                        sp->u.number = 0;
                        goto again;
                    }
                } while ( !(--p)[1].extern_call );
            } while (--i >= 0);
            if (p[1].extern_call & 0x80)
                prev_ob = p[1].pretend_to_be;
            else
                prev_ob = p[1].ob;
            if (prev_ob == 0 || prev_ob->flags & O_DESTRUCTED)
                sp->u.number = 0;
            else
                put_object(prev_ob);
            break;
          }
          XCASE(F_CALLER_STACK_DEPTH);
          {
            int i;
            struct control_stack *p;

            p = csp;
            i = 0;
            for(;;) {
                do {
                    if (p == control_stack) {
                        push_number(i);
                        goto again;
                    }
                } while ( !(--p)[1].extern_call );
                i++;
            }
          }
          XCASE(F_NR_RANGE);
          XCASE(F_RN_RANGE);
          XCASE(F_RR_RANGE);
          {
            if (sp[-1].type != T_NUMBER)
                ERROR("Bad type of start interval to [ .. ] range.\n")
            if (sp[0].type != T_NUMBER)
                ERROR("Bad type of end interval to [ .. ] range.\n")
            if (sp[-2].type == T_POINTER) {
                struct vector *v;
                int size, i1, i2;

                size = VEC_SIZE(sp[-2].u.vec);
                if (code == F_NR_RANGE-F_OFFSET-0x100)
                    i1 = sp[-1].u.number;
                else
                    i1 = size - sp[-1].u.number;
                if (code == F_RN_RANGE-F_OFFSET-0x100)
                    i2 = sp[0].u.number;
                else
                    i2 = size - sp[0].u.number;
                pop_stack();
                pop_stack();
                if (i2 >= size)
                    i2 = size - 1;
                v = slice_array(sp->u.vec, i1, i2);
                free_vector(sp->u.vec);
                if (v) {
                    sp->u.vec = v;
                } else {
                    put_number(0);
                }
            } else if (sp[-2].type == T_STRING) {
                int len, from, to;
                char *res;

                len = svalue_strlen(&sp[-2]);
                if (code == F_NR_RANGE-F_OFFSET-0x100)
                    from = sp[-1].u.number;
                else
                    from = len - sp[-1].u.number;
                if (from < 0) {
                    from = 0;
                }
                if (code == F_RN_RANGE-F_OFFSET-0x100)
                    to = sp[0].u.number;
                else
                    to = len - sp[0].u.number;
                if (to >= len)
                    to = len-1;
                if (to < from) {
                    pop_n_elems(3);
                    push_constant_string("");
                    break;
                }
                if (to == len-1) {
                    res = string_copy(sp[-2].u.string + from);
                    pop_n_elems(3);
                    push_malloced_string(res);
                    break;
                }
                res = xalloc(to - from + 2);
                strncpy(res, sp[-2].u.string + from, to - from + 1);
                res[to - from + 1] = '\0';
                pop_n_elems(3);
                push_malloced_string(res);
            } else {
                ERROR("Bad argument to [ .. ] range operand.\n")
            }
            break;
          }
          XCASE(F_PUSH_PROTECTED_INDEXED_LVALUE);
            sp = push_protected_indexed_lvalue(sp, pc);
            break;
          XCASE(F_PUSH_PROTECTED_RINDEXED_LVALUE);
            sp = push_protected_rindexed_lvalue(sp, pc);
            break;
          XCASE(F_STRSTR);
          {
            char *p1, *p2;
            int offs;

            if (sp[-2].type != T_STRING) goto xbad_arg_1;
            if (sp[-1].type != T_STRING) goto xbad_arg_2;
            if (sp[ 0].type != T_NUMBER) goto xbad_arg_3;
            p1 = sp[-2].u.string;
            if ( 0 != (offs = sp->u.number) ) {
                if (offs < 0) {
                    offs += svalue_strlen(sp-2);
                }
                do {
                    if (!*p1++) {
                        p1--;
                        break;
                    }
                } while (--offs);
            }
            p2 = strstr(p1, sp[-1].u.string);
            sp--;
            pop_stack();
            free_string_svalue(sp);
            put_number(p2 ? (p2 - p1) + sp[2].u.number : -1);
            break;
          }
          XCASE(F_PROGRAM_TIME);
          {
            if (sp->type != T_OBJECT) goto xbad_arg_1;
            free_object_svalue(sp);
            if (O_PROG_SWAPPED(sp->u.ob))
                if (load_ob_from_swap(sp->u.ob) < 0) {
                    sp--;
                    ERROR("Out of memory\n")
                }
            put_number(sp->u.ob->prog->load_time);
            break;
          }
#ifdef F_SWAP
          XCASE(F_SWAP);
          {
            struct object *ob;

            if (sp->type != T_OBJECT) goto xbad_arg_1;
            ob = sp->u.ob;
            if (ob != current_object) /* should also check csp */
                if (!O_PROG_SWAPPED(ob))
                    (void)swap_program(ob);
                if (!O_VAR_SWAPPED(ob))
                    (void)swap_variables(ob);
            pop_stack();
            break;
          }
#endif
          XCASE(F_PROTECTED_INDEX_LVALUE);
            sp = protected_index_lvalue(sp, pc);
            break;
          XCASE(F_PROTECTED_RINDEX_LVALUE);
            sp = protected_rindex_lvalue(sp, pc);
            break;
          XCASE(F_TO_INT);
          {
            int n;

            switch(sp->type) {
              default:
                goto xbad_arg_1;
              case T_FLOAT:
                n = (long)READ_DOUBLE(sp);
                break;
              case T_STRING:
                n = atol(sp->u.string);
                free_string_svalue(sp);
                break;
              case T_CLOSURE:
                if (sp->x.closure_type != CLOSURE_IDENTIFIER)
                    goto xbad_arg_1;
                n = sp->u.lambda->function.index;
                free_closure(sp);
                break;
              case T_NUMBER:
                n = sp->u.number;
                break;
            }
            put_number(n);
            break;
          }
          XCASE(F_TO_FLOAT);
          {
            STORE_DOUBLE_USED
            double d;

            switch(sp->type) {
              default:
                goto xbad_arg_1;
              case T_NUMBER:
                d = (double)sp->u.number;
                break;
              case T_FLOAT:
                NOOP;
                break;
              case T_STRING:
                d = atof(sp->u.string);
                free_string_svalue(sp);
                break;
            }
            if (sp->type != T_FLOAT)
            {
                sp->type = T_FLOAT;
                STORE_DOUBLE(sp, d);
            }
            break;
          }
          XCASE(F_TO_STRING);
          {
            char buf[32], *s;

            switch(sp->type) {
              default:
                goto xbad_arg_1;
              case T_NUMBER:
                sprintf(buf,"%ld", sp->u.number);
                s = string_copy(buf);
                break;
              case T_FLOAT:
                sprintf(buf,"%g", READ_DOUBLE(sp));
                s = string_copy(buf);
                break;
              case T_OBJECT:
                s = add_slash(sp->u.ob->name);
                if (!s)
                    ERROR("Out of memory\n")
                free_object_svalue(sp);
                break;
              case T_POINTER:
              {
                long size;
                struct svalue *svp;
                char c, *d;

                size = VEC_SIZE(sp->u.vec) + 1;
                svp = sp->u.vec->item;
                d = s = xalloc(size);
                for (;;) {
                    if (!--size) {
                        *d++ = '\0';
                        break;
                    }
                    if (svp->type != T_NUMBER || !(c = svp->u.number) ) {
                        *d++ = '\0';
                        d = string_copy(s);
                        xfree(s);
                        s = d;
                        break;
                    }
                    *d++ = c;
                    svp++;
                }
                free_vector(sp->u.vec);
                break;
              }
              case T_CLOSURE:
              {
                struct lambda *l = sp->u.lambda;
                struct object *ob;
                int ix;

                switch(sp->x.closure_type) {
                  case CLOSURE_IDENTIFIER:
                  {
                    if (O_PROG_SWAPPED(l->ob))
                        load_ob_from_swap(l->ob);
                    sp->type = T_STRING;
                    sp->x.string_type = STRING_SHARED;
                    increment_string_ref(
                      sp->u.string =
                        l->ob->prog->variable_names[l->function.index].name
                    );
                    break;
                  }
                  case CLOSURE_LFUN:
                    ob = l->ob;
                    ix = l->function.index;
                    goto to_string_lfun;
                  case CLOSURE_ALIEN_LFUN:
                    ob = l->function.alien.ob;
                    ix = l->function.alien.index;
                  to_string_lfun:
                  {
                    struct program *prog;
                    uint32 flags;
                    char *function_name;
                    struct inherit *inheritp;

                    prog = ob->prog;
                    flags = prog->functions[ix];
                    while (flags & NAME_INHERITED) {
                        inheritp = &prog->inherit[flags & INHERIT_MASK];
                        ix -= inheritp->function_index_offset;
                        prog = inheritp->prog;
                        flags = prog->functions[ix];
                    }
                    memcpy(
                      (char *)&function_name,
                      prog->program + (flags & FUNSTART_MASK) - 1 -
                        sizeof function_name,
                      sizeof function_name
                    );
                    sp->type = T_STRING;
                    sp->x.string_type = STRING_SHARED;
                    increment_string_ref(sp->u.string = function_name);
                    break;
                  }
                  default:
                    goto xbad_arg_1;
                }
                break;
              }
              case T_SYMBOL:
              {
                sp->type = T_STRING;
                sp->x.string_type = STRING_SHARED;
                break;
              }
              case T_STRING:
                break;
            }
            if (sp->type != T_STRING)
                put_malloced_string(s, sp);
            break;
          }
          XCASE(F_TO_ARRAY);
          {
            struct vector *v;
            char *s;
            struct svalue *svp;

            if (sp->type == T_STRING || sp->type == T_SYMBOL) {
                inter_sp = sp;
                inter_pc = pc;
                v = allocate_uninit_array(svalue_strlen(sp) + 1);
                s = sp->u.string;
                svp = v->item;
                while (svp->type = T_NUMBER, svp->u.number =  *s++) svp++;
                free_string_svalue(sp);
                sp->type = T_POINTER;
                sp->u.vec = v;
                break;
            } else if (sp->type == T_QUOTED_ARRAY) {
                sp->type = T_POINTER;
                break;
            } else if (sp->type == T_POINTER) {
                break;
            } else goto xbad_arg_1;
            break;
          }
          XCASE(F_NR_RANGE_LVALUE);
            inter_pc = pc;
            sp = range_lvalue(0x001, sp);
            break;
          XCASE(F_RN_RANGE_LVALUE);
            inter_pc = pc;
            sp = range_lvalue(0x100, sp);
            break;
          XCASE(F_RR_RANGE_LVALUE);
            inter_pc = pc;
            sp = range_lvalue(0x101, sp);
            break;
          XCASE(F_QUERY_ONCE_INTERACTIVE);
          {
            struct object *obj;

            if (sp->type != T_OBJECT) goto xbad_arg_1;
            obj = sp->u.ob;
            put_number(obj->flags & O_ONCE_INTERACTIVE ? 1 : 0);
            decr_object_ref(obj, "query_once_interactive");
            break;
          }
          XCASE(F_SET_EXTRA_WIZINFO_SIZE);
          {
            if (sp->type != T_NUMBER) goto xbad_arg_1;
            if (privilege_violation("set_extra_wizinfo_size", sp) > 0)
                wiz_info_extra_size = sp->u.number;
            sp--;
            break;
          }
          XCASE(F_QUERY_MUD_PORT);  /* mud_port,        21  */
          {
#ifndef MAXNUMPORTS
            push_number(port_number);
#else
            inter_pc = pc;
            sp = query_ip_port(sp);
#endif
            break;
          }
#ifdef CATCH_UDP_PORT
          XCASE(F_QUERY_IMP_PORT); /* udp_port,        22  */
          {
            push_number(udp_port);
            break;
          }
#endif
          XCASE(F_QUERY_INPUT_PENDING);
          {
            struct object *ob;
            struct interactive *ip;

            TYPE_TEST1(sp, T_OBJECT)
            ob = sp->u.ob;
            decr_object_ref(ob, "query_input_pending");
            if (NULL != (ip = O_GET_INTERACTIVE(ob)) &&
                ip->sent.type == SENT_INTERACTIVE && ip->input_to)
            {
                add_ref(sp->u.ob = ip->input_to->ob, "query_input_pending");
            } else {
                put_number(0);
            }
            break;
          }
#ifdef F_SIN
          XCASE(F_SIN);
          {
            STORE_DOUBLE_USED
            double d;

            if (sp->type != T_FLOAT) goto xbad_arg_1;
            d = sin(READ_DOUBLE(sp));
            STORE_DOUBLE(sp, d);
            break;
          }
#endif
#ifdef F_ASIN
          XCASE(F_ASIN);
          {
            STORE_DOUBLE_USED
            double d;

            if (sp->type != T_FLOAT || (d = READ_DOUBLE(sp)) < -1. || d > 1. )
                goto xbad_arg_1;
            d = asin(d);
            STORE_DOUBLE(sp, d);
            break;
          }
#endif
#ifdef F_COS
          XCASE(F_COS);
          {
            STORE_DOUBLE_USED
            double d;

            if (sp->type != T_FLOAT) goto xbad_arg_1;
            d = cos(READ_DOUBLE(sp));
            STORE_DOUBLE(sp, d);
            break;
          }
#endif
#ifdef F_ACOS
          XCASE(F_ACOS);
          {
            STORE_DOUBLE_USED
            double d;

            if (sp->type != T_FLOAT || (d = READ_DOUBLE(sp)) < -1. || d > 1. )
                goto xbad_arg_1;
            d = acos(d);
            STORE_DOUBLE(sp, d);
            break;
          }
#endif
#ifdef F_TAN
          XCASE(F_TAN);
          {
            STORE_DOUBLE_USED
            double d;

            if (sp->type != T_FLOAT) goto xbad_arg_1;
            d = tan(READ_DOUBLE(sp));
            STORE_DOUBLE(sp, d);
            break;
          }
#endif
#ifdef F_ATAN
          XCASE(F_ATAN);
          {
            STORE_DOUBLE_USED
            double d;

            if (sp->type != T_FLOAT) goto xbad_arg_1;
            d = atan(READ_DOUBLE(sp));
            STORE_DOUBLE(sp, d);
            break;
          }
#endif
#ifdef F_LOG
          XCASE(F_LOG);
          {
            STORE_DOUBLE_USED
            double d;

            if (sp->type != T_FLOAT || (d = READ_DOUBLE(sp)) <= 0.)
                goto xbad_arg_1;
            d = log(d);
            STORE_DOUBLE(sp, d);
            break;
          }
#endif
#ifdef F_EXP
          XCASE(F_EXP);
          {
            STORE_DOUBLE_USED
            double d;

            if (sp->type != T_FLOAT) goto xbad_arg_1;
            d = exp(READ_DOUBLE(sp));
            STORE_DOUBLE(sp, d);
            break;
          }
#endif
#ifdef F_SQRT
          XCASE(F_SQRT);
          {
            STORE_DOUBLE_USED
            double d;

            if (sp->type != T_FLOAT || (d = READ_DOUBLE(sp)) < 0.)
                goto xbad_arg_1;
            d = sqrt(d);
            STORE_DOUBLE(sp, d);
            break;
          }
#endif
          XCASE(F_CALL_RESOLVED);
          {
            struct svalue *arg;
            struct object *ob;

            ASSIGN_EVAL_COST
            GET_NUM_ARG
            inter_pc = pc;
            inter_sp = sp;
            arg = sp - num_arg + 1;
            if (arg[0].type != T_LVALUE) goto xbad_arg_1;
            if (arg[1].type == T_OBJECT)
                ob = arg[1].u.ob;
            else if (arg[1].type == T_STRING) {
                ob = get_object(arg[1].u.string);
                if (ob == 0)
                    ERROR("call_resolved() failed\n")
            } else goto xbad_arg_2;
            if (arg[2].type != T_STRING) goto xbad_arg_3;
            if (current_object->flags & O_DESTRUCTED) {
                /*
                 * No external calls may be done when this object is
                 * destructed.
                 */
                pop_n_elems(num_arg);
                push_number(0);
                break;
            }
            /*
             * Send the remaining arguments to the function.
             */
            if (TRACEP(TRACE_CALL_OTHER)) {
                if (!++traceing_recursion) {
                    inter_sp = sp;
                    do_trace("Call other ", arg[1].u.string, "\n");
                }
                traceing_recursion--;
            }
            if (apply_low(arg[2].u.string, ob, num_arg-3, MY_FALSE) == 0) {
                /* Function not found */
                pop_n_elems(num_arg-1);
                free_svalue(sp);
                put_number(0);
                break;
            }
            /*
             * The result of the function call is on the stack. But, so
             * is the function name and object that was called.
             * These have to be removed.
             */
            sp = inter_sp;
            transfer_svalue(arg, sp--);        /* Copy the function call result */
            pop_n_elems(2);        /* Remove old arguments to call_solved */
            free_svalue(sp);        /* Free the lvalue */
            put_number(1);
            break;
          }
#ifdef F_MAPPING_CONTAINS
          XCASE(F_MAPPING_CONTAINS);
          {
            struct svalue *item;
            int i;

            GET_NUM_ARG
            for (i = -num_arg; ++i < -1; )
                if (sp[i].type != T_LVALUE)
                    bad_arg_pc(num_arg + i, code + 0x100, sp, pc);
            if (sp[-1].type != T_MAPPING ||
                sp[-1].u.map->num_values != num_arg -2)
                    bad_arg_pc(num_arg + i, code + 0x100, sp, pc);
            item = get_map_lvalue(sp[-1].u.map, sp, 0);
            if (item == &const0) {
                pop_n_elems(num_arg-1);
                free_svalue(sp);
                put_number(0);
                break;
            }
            free_svalue(sp--); /* free key */
            for (i = -num_arg + 1; ++i < 0; ) {
                /* get_map_lvalue() may return destructed objects. */
                /* TODO: May this cause problems elsewhere, too? */
                if (T_OBJECT == item->type
                 && (O_DESTRUCTED & item->u.ob->flags))
                {
                    assign_svalue(sp[i].u.lvalue, &const0);
                    item++;
                }
                else
                    /* mapping must not have been freed yet */
                    assign_svalue(sp[i].u.lvalue, item++);
                free_svalue(&sp[i]);
            }
            free_svalue(sp--); /* free mapping */
            sp += 3 - num_arg;
            put_number(1);
            break;
          }
#endif
#ifdef F_ALLOCATE_MAPPING
          XCASE(F_ALLOCATE_MAPPING);
          {
            if ( sp[-1].type != T_NUMBER || sp[-1].u.number < 0)
                goto xbad_arg_1;
            if ( sp->type != T_NUMBER || sp->u.number < 0)
                goto xbad_arg_2;
            sp--;
            sp->type = T_MAPPING;
            if (!(sp->u.map = allocate_mapping(sp->u.number, sp[1].u.number)))
            {
                sp++;
                ERROR("Out of memory\n")
            }
            break;
          }
#endif
#ifdef F_COPY_MAPPING
          XCASE(F_COPY_MAPPING);
          {
            struct mapping *m, *m2;

            TYPE_TEST1(sp, T_MAPPING)
            m = sp->u.map;
            check_map_for_destr(m);
            m2 = copy_mapping(m);
            free_mapping(m);
            sp->u.map = m2;
            break;
          }
#endif
          XCASE(F_REFERENCEP);
          {
            int i;

            if (sp->type != T_LVALUE) goto xbad_arg_1;
            i = sp->u.lvalue->type == T_LVALUE;
            free_svalue(sp);
            put_number(i);
            break;
          }
          XCASE(F_RAISE_ERROR);
          {
            if (sp->type != T_STRING) goto xbad_arg_1;
            ERRORF(("%s", sp->u.string));
          }
          XCASE(F_GET_EVAL_COST);
          {
            push_number(-eval_cost);
            break;
          }
          XCASE(F_GARBAGE_COLLECTION);
          {
            extra_jobs_to_do = garbage_collect_to_do = MY_TRUE;
            time_last_gc = 0;
            break;
          }
          XCASE(F_TYPEOF);
          {
            mp_int i = sp->type;
            free_svalue(sp);
            put_number(i);
            break;
          }
          XCASE(F_GET_TYPE_INFO);
          {
            mp_int i, j;

            i = sp[-1].type;
            switch(i) {
              default:
                j = -1;
                break;
              case T_MAPPING:
                j = sp[-1].u.map->num_values;
                break;
              case T_CLOSURE:
                if (sp->u.number == 2 && sp->type == T_NUMBER) {
                    struct object *ob;

                    sp--;
                    switch(sp->x.closure_type) {
                      default:
                        ob = 0;
                        break;
                      case CLOSURE_LFUN:
                      case CLOSURE_IDENTIFIER:
                        ob = sp->u.lambda->ob;
                        break;
                      case CLOSURE_ALIEN_LFUN:
                        ob = sp->u.lambda->function.alien.ob;
                        break;
                    }
                    free_closure(sp);
                    if (!ob || ob->flags & O_DESTRUCTED)
                        put_number(0);
                    else
                        put_object(ob);
                    goto again;
                }
              case T_SYMBOL:
              case T_QUOTED_ARRAY:
                j = sp[-1].x.generic;
                break;
            }
            if (sp->type == T_NUMBER) {
                free_svalue(--sp);
                if (sp[1].u.number != 1)
                    if (sp[1].u.number)
                        j = -1;
                    else
                        j = i;
                put_number(j);
            } else {
                struct vector *v;

                inter_sp = sp;
                inter_pc = pc;
                v = allocate_array(2);
                v->item[0].u.number = i;
                v->item[1].u.number = j;
                free_svalue(sp);
                free_svalue(--sp);
                put_referenced_vector(v);
            }
            break;
          }
          XCASE(F_PROTECTED_RANGE_LVALUE);
            inter_pc = pc;
            sp = protected_range_lvalue(0x000, sp);
            break;
          XCASE(F_PROTECTED_NR_RANGE_LVALUE);
            inter_pc = pc;
            sp = protected_range_lvalue(0x001, sp);
            break;
          XCASE(F_PROTECTED_RN_RANGE_LVALUE);
            inter_pc = pc;
            sp = protected_range_lvalue(0x100, sp);
            break;
          XCASE(F_PROTECTED_RR_RANGE_LVALUE);
            inter_pc = pc;
            sp = protected_range_lvalue(0x101, sp);
            break;
          XCASE(F_PROTECTED_EXTRACT_LVALUE);
          {
            sp[1] = sp[0];
            put_number(1);
            sp++;
            if (sp[-2].u.number < 0 && sp[-2].type == T_NUMBER) {
                sp[-2].u.number = -sp[-2].u.number;
                inter_pc = pc;
                sp = protected_range_lvalue(0x101, sp);
            } else  {
                inter_pc = pc;
                sp = protected_range_lvalue(0x001, sp);
            }
            break;
          }
          XCASE(F_PUSH_PROTECTED_INDEXED_MAP_LVALUE);
            push_protected_indexed_map_lvalue(sp, pc);
            break;
          XCASE(F_UNDEF);
          {
            char *name;

            /* pc has already been incremented */
            if (pc > current_prog->program && pc <= PROGRAM_END(*current_prog))
            {
                memcpy((char *)&name, pc - 5 - sizeof name, sizeof name);
            } else {
                name = "Object the closure was bound to has been destructed";
            }
            ERRORF(("Undefined function: %s\n", name))
          }
          default:
            fatal("Unknown stackmachine escape code\n");
        xbad_arg_1: instruction = code + 0x100; goto bad_arg_1;
        xbad_arg_2: instruction = code + 0x100; goto bad_arg_2;
        xbad_arg_3: instruction = code + 0x100; goto bad_arg_3;
        }
        break;
    } /* end of F_ESCAPE */

    } /* end of the monumental switch */
#ifdef DEBUG
    if (expected_stack && expected_stack != sp) {
        fatal("Bad stack after evaluation.\n\
sp: %lx expected: %lx\n\
Instruction %d, num arg %d\n",
              (long)sp, (long)expected_stack,
              instruction + F_OFFSET, num_arg);
    }
    if (sp < fp + csp->num_local_variables - 1) {
        fatal("Bad stack after evaluation.\n\
sp: %lx minimum expected: %lx\n\
Instruction %d, num arg %d\n",
              (long)sp, (long)(fp + csp->num_local_variables - 1),
              instruction + F_OFFSET, num_arg);
    }
#endif /* DEBUG */
    goto again;
}
#undef push_malloced_string
#undef push_number

/*
 * Apply a fun 'fun' to the program in object 'ob', with
 * 'num_arg' arguments (already pushed on the stack).
 * If the function is not found, search in the object pointed to by the
 * inherit pointer.
 * If the function name starts with '::', search in the object pointed out
 * through the inherit pointer by the current object. The 'current_object'
 * stores the base object, not the object that has the current function being
 * evaluated. Thus, the variable current_prog will normally be the same as
 * current_object->prog, but not when executing inherited code. Then,
 * it will point to the code of the inherited object. As more than one
 * object can be inherited, the call of function by index number has to
 * be adjusted. The function number 0 in a superclass object must not remain
 * number 0 when it is inherited from a subclass object. The same problem
 * exists for variables. The global variables function_index_offset and
 * variable_index_offset keep track of how much to adjust the index when
 * executing code in the superclass objects.
 *
 * There is a special case when called from the heart beat, as
 * current_prog will be 0. When it is 0, set current_prog
 * to the 'ob->prog' sent as argument.
 *
 * Arguments are always removed from the stack.
 * If the function is not found, return 0 and nothing on the stack.
 * Otherwise, return 1, and a pushed return value on the stack.
 *
 * Note that the object 'ob' can be destructed. This must be handled by
 * the caller of apply().
 *
 * If the function failed to be called, then arguments must be deallocated
 * manually !
 */

#ifdef DEBUG
static char debug_apply_fun[30]; /* For debugging */
#endif

#ifdef APPLY_CACHE_STAT
int apply_cache_hit = 0, apply_cache_miss = 0;
#endif

#if APPLY_CACHE_BITS < 1
#error APPLY_CACHE_BITS must be at least 1.
#else
#define CACHE_SIZE (1 << APPLY_CACHE_BITS)
#endif

    static char *cache_name[CACHE_SIZE];
      /* The name of the cached function, shared for existing functions,
       * allocated if the object does not have the function.
       */
    static struct program *cache_progp[CACHE_SIZE];
      /* The pointer to the program code of the function, or NULL if the
       * object does not implement the function.
       */
    static int cache_id[CACHE_SIZE];
      /* The id-number of the object. */

static int
apply_low (char *fun, struct object *ob, int num_arg
          , short /* TODO: BOOL */ b_ign_prot)
{
    static uint32 cache_flags[CACHE_SIZE];
    static unsigned char *cache_funstart[CACHE_SIZE];
    static int cache_function_index_offset[CACHE_SIZE];
    static int cache_variable_index_offset[CACHE_SIZE];

    struct program *progp;
    struct control_stack *save_csp;
    p_int ix;

    ob->time_of_ref = current_time;        /* Used by the swapper */
    /*
     * This object will now be used, and is thus a target for
     * reset later on (when time due).
     */
    ob->flags &= ~O_RESET_STATE;
#ifdef DEBUG
    strncpy(debug_apply_fun, fun, sizeof debug_apply_fun);
    debug_apply_fun[sizeof debug_apply_fun - 1] = '\0';
#endif
#ifdef DEBUG
    {
        if (num_error > 2) {
            fatal("apply_low with too many errors.\n");
            goto failure;
        }
    }
#endif
    /*
     * If there is a chain of objects shadowing, start with the first
     * of these.
     */
    if (ob->flags & O_SHADOW) {
        struct object *shadow;

        while (NULL != (shadow = O_GET_SHADOW(ob)->shadowed_by) &&
               shadow != current_object)
        {
            ob = shadow;
        }
    }
retry_for_shadow:
    if (ob->flags & O_SWAPPED)
        if (load_ob_from_swap(ob) < 0)
            error("Out of memory\n");
    progp = ob->prog;
#ifdef DEBUG
    if (ob->flags & O_DESTRUCTED)
        fatal("apply() on destructed object '%s' function '%s'\n"
             , ob->name != NULL ? ob->name : "<null>"
             , fun != NULL ? fun : "<null>"
             );
#endif
    if ( !((p_int)fun & 2) ) {
        /* Heuristic to find out if fun is an unshared string,
         * building on the six-byte-overhead of shared strings.
         * This means that every shared string has a set bit 1,
         * whereas other strings have no overhead and thus bit 1
         * cleared. Function names are always shared, so if there
         * is no shared string twin for fun, the called function does
         * not exist anywhere. As a side effect, we get the shared
         * string for fun which makes the cache-lookup much faster.
         */
        fun = findstring(fun);
        if (!fun)
            goto failure2;
    }
    /* *fun is now (more or less) guaranteed to be a shared string */
    ix =
      ( progp->id_number ^ (p_int)fun ^ ( (p_int)fun >> APPLY_CACHE_BITS ) ) &
        (CACHE_SIZE-1);
    if (cache_id[ix] == progp->id_number
     && (cache_name[ix] == fun || !strcmp(cache_name[ix], fun))
       ) {
        /* We have found a matching entry in the cache. The contents have
         * to match, not only the pointers, because cache entries for
         * functions not existant in _this_ object <ob> are stored as
         * separately allocated copy, not as another ref to the shared
         * string. Yet they shall be found here.
         */
#ifdef APPLY_CACHE_STAT
        apply_cache_hit++;
#endif
        if (cache_progp[ix]
          /* Static functions may not be called from outside. */
          && (   !cache_flags[ix] || b_ign_prot
              || (   !(cache_flags[ix] & TYPE_MOD_PROTECTED)
                  && current_object == ob)) )
        {
            /* the cache will tell us in wich program the function is, and
             * where
             */
            unsigned char *funstart;

            push_control_stack(inter_sp, inter_pc, inter_fp);
            csp->ob = current_object;
            csp->prev_ob = previous_ob;
            csp->num_local_variables = num_arg;
            csp->funstart = funstart = cache_funstart[ix];
            current_prog = cache_progp[ix];
            current_strings = current_prog->strings;
            function_index_offset = cache_function_index_offset[ix];
            current_variables = ob->variables +
              cache_variable_index_offset[ix];
            inter_sp = setup_new_frame2(funstart, inter_sp);
#ifdef        OLD_PREVIOUS_OBJECT_BEHAVIOUR
/* some mudlib security relies on this test */
            if (current_object != ob)
#endif
                previous_ob = current_object;
            current_object = ob;
            save_csp = csp;
            eval_instruction(funstart + 2, inter_sp);
#ifdef DEBUG
            if (save_csp-1 != csp)
                fatal("Bad csp after execution in apply_low\n");
#endif
            /*
             * Arguments and local variables are now removed. One
             * resulting value is always returned on the stack.
             */
            return 1;
        } /* when we come here, the cache has told us that the function isn't
           * defined in the object
           */
    } else {
        /* we have to search the function */
        char *shared_name;

#ifdef APPLY_CACHE_STAT
        apply_cache_miss++;
#endif
        /* This call to findstring() is not really necessary, but serves
         * as safeguard should a non-shared string escape the attention
         * of the 'heuristic' filter before.
         */
        if ( NULL != (shared_name = ((cache_name[ix] == fun) ? fun : findstring(fun))) ) {
            int fx;

            eval_cost++;
            fx = find_function(shared_name, progp);
            if (fx >= 0) {
                /* The searched function is found */
                uint32 flags;
                unsigned char *funstart;

                push_control_stack(inter_sp, inter_pc, inter_fp);
                                        /* if an error occurs here,
                                         * it won't leave the cache in an
                                         * inconsistent state.
                                         */
                csp->ob = current_object;
                csp->prev_ob = previous_ob;
                if (!cache_progp[ix]) {
                    /* The old cache entry was for an undefined function,
                       so the name had to be malloced */
                    xfree(cache_name[ix]);
                }
                cache_id[ix] = progp->id_number;
                cache_name[ix] = shared_name;
                csp->num_local_variables = num_arg;
                current_prog = progp;
                flags = setup_new_frame1(fx, 0, 0);
                current_strings = current_prog->strings;
                cache_progp[ix] = current_prog;
                cache_function_index_offset[ix] = function_index_offset;
                cache_variable_index_offset[ix] = variable_index_offset;
                current_variables = ob->variables +
                  variable_index_offset;
                funstart = current_prog->program + (flags & FUNSTART_MASK);
                cache_funstart[ix] = funstart;
                /* Static functions may not be called from outside. */
                if (0 != (cache_flags[ix] =
                  progp->functions[fx] & (TYPE_MOD_STATIC|TYPE_MOD_PROTECTED))
                  && ((cache_flags[ix] & TYPE_MOD_PROTECTED) || current_object != ob)
                  && !b_ign_prot
                    )
                {
                    previous_ob = csp->prev_ob;
                    current_object = csp->ob;
                    pop_control_stack();
                    if (ob->flags & O_SHADOW && O_GET_SHADOW(ob)->shadowing) {
                        /* This is an object shadowing another. The function
                           was not found, but can maybe be found in the object
                           we are shadowing.
                         */
                        ob = O_GET_SHADOW(ob)->shadowing;
                        goto retry_for_shadow;
                    } else goto failure;
                }
                csp->funstart = funstart;
                inter_sp = setup_new_frame2(funstart, inter_sp);
#ifdef OLD_PREVIOUS_OBJECT_BEHAVIOUR
                if (current_object != ob)
#endif
                    previous_ob = current_object;
                current_object = ob;
                save_csp = csp;
                eval_instruction(funstart + 2, inter_sp);
#ifdef DEBUG
                if (save_csp-1 != csp)
                    fatal("Bad csp after execution in apply_low\n");
#endif
                /*
                 * Arguments and local variables are now removed. One
                 * resulting value is always returned on the stack.
                 */
                return 1;
            } /* end for */
        } /* end if(shared_name) */
        /* We have to mark a function not to be in the object. */
        if (!cache_progp[ix]) {
            /* The old cache entry was for an undefined function, so the
               name had to be malloced */
            xfree(cache_name[ix]);
        }
        cache_id[ix] = progp->id_number;
        cache_name[ix] = string_copy(fun);
        cache_progp[ix] = (struct program *)0;
    }
    if (ob->flags & O_SHADOW && O_GET_SHADOW(ob)->shadowing) {
        /*
         * This is an object shadowing another. The function was not found,
         * but can maybe be found in the object we are shadowing.
         */
        ob = O_GET_SHADOW(ob)->shadowing;
        goto retry_for_shadow;
    }
failure:
    if (fun[0] == ':')
        error("Illegal function call\n");
failure2:
    /* Failure. Deallocate stack. */
#if 0 /* F_SIMUL_EFUN still needs the arguments */
    pop_n_elems(num_arg);
#endif
    return 0;
}

/*
 * Arguments are supposed to be
 * pushed (using push_string() etc) before the call. A pointer to a
 * 'struct svalue' will be returned. It will be a null pointer if the called
 * function was not found. Otherwise, it will be a pointer to a static
 * area in apply(), which will be overwritten by the next call to apply.
 * Reference counts will be updated for this value, to ensure that no pointers
 * are deallocated.
 */

struct svalue apply_return_value = { T_NUMBER };

void push_apply_value() {
    *++inter_sp = apply_return_value;
    apply_return_value.type = T_NUMBER;
}

void pop_apply_value () {
    free_svalue(&apply_return_value);
    apply_return_value = *inter_sp--;
}

struct svalue *
sapply_int (char *fun, struct object *ob, int num_arg
           , short /* TODO: BOOL */ b_find_static)
{
#ifdef DEBUG
    struct svalue *expected_sp;
#endif

    if (TRACEP(TRACE_APPLY)) {
        if (!++traceing_recursion) {
            do_trace("Apply", "", "\n");
        }
        traceing_recursion--;
    }
#ifdef DEBUG
    expected_sp = inter_sp - num_arg;
#endif
    if (apply_low(fun, ob, num_arg, b_find_static) == 0) {
        inter_sp = _pop_n_elems(num_arg, inter_sp);
        return 0;
    }
    transfer_svalue(&apply_return_value, inter_sp);
    inter_sp--;
#ifdef DEBUG
    if (expected_sp != inter_sp)
        fatal("Corrupt stack pointer.\n");
#endif
    return &apply_return_value;
}

struct svalue *apply(fun, ob, num_arg)
    char *fun;
    struct object *ob;
    int num_arg;
{
    tracedepth = 0;
    return sapply_int(fun, ob, num_arg, MY_FALSE);
}

/*
 * This function is similar to apply(), except that it will not
 * call the function, only return object name if the function exists,
 * or 0 otherwise.
 */
char *function_exists(fun, ob)
    char *fun;
    struct object *ob;
{
    char *shared_name;
    unsigned char *funstart;
    struct program *progp;
    int ix;
    uint32 flags;

#ifdef DEBUG
    if (ob->flags & O_DESTRUCTED)
        fatal("function_exists() on destructed object\n");
#endif
    if (O_PROG_SWAPPED(ob))
        if (load_ob_from_swap(ob) < 0)
            error("Out of memory\n");
    shared_name = findstring(fun);
    progp = ob->prog;
    if ( (ix = find_function(shared_name, progp)) < 0)
        return 0;
    flags = progp->functions[ix];
    if (flags & TYPE_MOD_PRIVATE ||
        (flags & TYPE_MOD_STATIC && current_object != ob))
        return 0;
    while (flags & NAME_INHERITED) {
        struct inherit *inheritp;

        inheritp = &progp->inherit[flags & INHERIT_MASK];
        ix -= inheritp->function_index_offset;
        progp = inheritp->prog;
        flags = progp->functions[ix];
    }
    funstart = progp->program  + (flags & FUNSTART_MASK);
    if (funstart[2] == F_ESCAPE - F_OFFSET &&
        funstart[3] == F_UNDEF  - F_OFFSET - 0x100)
    {
        return 0;
    }
    return progp->name;
}

/*
 * Call a specific function address in an object. This is done with no
 * frame set up. It is expected that there are no arguments. Returned
 * values are removed.
 */

void call_function(progp, fx)
    struct program *progp;
    int fx;
{
    uint32 flags;
    unsigned char *funstart;

    push_control_stack(inter_sp, inter_pc, inter_fp);
    csp->ob = current_object;
    csp->prev_ob = previous_ob;
#ifdef DEBUG
    if (csp != control_stack)
        fatal("call_function with bad csp\n");
#endif
    csp->num_local_variables = 0;
    current_prog = progp;
    flags = setup_new_frame(fx);
    funstart = current_prog->program + (flags & FUNSTART_MASK);
    csp->funstart = funstart;
    previous_ob = current_object;
    tracedepth = 0;
    eval_instruction(funstart + 2, inter_sp);
    free_svalue(inter_sp--);        /* Throw away the returned result */
}

/*
 * This can be done much more efficiently, but the fix has
 * low priority.
 */
int get_line_number(p, progp, namep)
    char *p;
    struct program *progp;
    char **namep;
{
    struct incinfo {
        char *name;
        struct incinfo *super;
        int super_line;
    };

    int offset;
    int i;
    char **include_names;
    struct incinfo *inctop = 0;
    int relocated_from = 0, relocated_to = -1;
    p_int old_total;

    if (progp == 0)
        return 0;
    if (p == 0)
        return 0;
    old_total = 0;
    if (!progp->line_numbers)
        if (!load_line_numbers_from_swap(progp)) {
            int save_privilege;

            old_total = progp->total_size;
            save_privilege = malloc_privilege;
            malloc_privilege = MALLOC_SYSTEM;
            load_line_numbers_from_swap(progp);
            malloc_privilege = save_privilege;
        }
    offset = p - progp->program;
    if (p < progp->program || p > PROGRAM_END(*progp))
    {
        (void)printf("get_line_number(): Illegal offset %d in object %s\n", offset, progp->name);
        debug_message("get_line_number(): Illegal offset %d in object %s\n", offset, progp->name);
        return 0;
    }
    include_names = progp->strings + progp->num_strings;
    for (i=0,p=progp->line_numbers; ; ) {
        int o;

        o = EXTRACT_UCHAR(p);
        if (o <= 63) {
            if (o >= LI_MAXOFFSET) {
                if (o != LI_MAXOFFSET) {
                    switch (o) {
                      case LI_INCLUDE:
                      {
                        struct incinfo *inc_new;

                        i++;
                        inc_new = xalloc(sizeof *inc_new);
                        inc_new->name = *--include_names;
                        inc_new->super = inctop;
                        inc_new->super_line = i;
                        inctop = inc_new;
                        i = 0;
                        break;
                      }
                      case LI_INCLUDE_END:
                      {
                        struct incinfo *inc_old;

                        inc_old = inctop;
                        i = inc_old->super_line;
                        inctop = inc_old->super;
                        xfree( (char*)inc_old );
                        break;
                      }
                      case LI_L_RELOCATED:
                      {
                        int h, l;

                        p++;
                        h = EXTRACT_UCHAR(p);
                        p++;
                        l = EXTRACT_UCHAR(p);
                        i -= 2;
                        relocated_to = i;
                        relocated_from = relocated_to - ((h << 8) + l);
                        p++; /* skip trailing LI_L_RELOCATED */
                        break;
                      }
                    }
                } else {
                    offset -= o;
                }
            } else {
                offset -= o;
                i++;
                if (offset <= 0)
                    break;
            }
        } else if (o <= 127) {
            offset -= (o&7) + 1;
            i += (o>>3) - 6;
            if (offset <= 0)
                break;
        } else if (o >= 256-LI_MAXEMPTY) {
            i += 256-o;
        } else {
            i -= 2;
            relocated_from = (relocated_to = i) - (o - LI_RELOCATED);
        }
        p++;
    }
    if (i == relocated_to + 1)
        i = relocated_from + 1;
    if (inctop) {
        static char namebuf[80];

        *namep = inctop->name;
        if (strlen(*namep) + strlen(progp->name) < sizeof(namebuf) - 3) {
            sprintf(namebuf, "%s (%s)", progp->name, *namep);
            *namep = namebuf;
        }
        do {
            struct incinfo *inc_old;

            inc_old = inctop;
            inctop = inc_old->super;
            xfree( (char *)inc_old );
        } while (inctop);
    } else {
        *namep = progp->name;
    }
    if (old_total) {
        xfree(progp->line_numbers);
        total_prog_block_size -= progp->total_size - old_total;
        progp->total_size = old_total;
        reallocate_reserved_areas();
    }
    return i;
}

#define SIMUL_EFUN_FUNSTART ((char *)-1)

/*
 * Write out a trace. If there is an heart_beat(), then return the
 * object that had that heart beat.
 */
char *dump_trace(how)
    int how;
{
    struct control_stack *p;
    char *ret = 0;
    char *pc = inter_pc;
    int line = 0;
    char *name;
    char *file;
    struct object *ob = NULL;
    unsigned char *last_catch = NULL;

    if (current_prog == 0)
        return 0;
    if (csp < &control_stack[0]) {
        (void)printf("No trace.\n");
        debug_message("No trace.\n");
        return 0;
    }
#ifdef TRACE_CODE
    if (how) {
#ifdef DEBUG
        (void)last_instructions(60, 1, 0);
        printf("%6lx: %3d %3d %3d %3d %3d %3d %3d %3d\n", (long)pc,
          pc[0], pc[1], pc[2], pc[3], pc[4], pc[5], pc[6], pc[7] );
#else  /* DEBUG */
        (void)last_instructions(20, 1, 0);
#endif /* DEBUG */
    }
#endif /* TRACE_CODE */
    p = &control_stack[0];
    do {
        char *dump_pc;
        struct program *prog;
        if (p->extern_call) {
            struct control_stack *q = p;
            for (;;) {
                if (++q > csp) {
                    ob = current_object;
                    break;
                }
                if (q->extern_call) {
                    ob = q->ob;
                    break;
                }
            }
            last_catch = NULL;
        }
        if (p == csp) {
            dump_pc = pc;
            prog = current_prog;
        } else {
            dump_pc = p[1].pc;
            prog = p[1].prog;
        }
        /* Use some heuristics first to see if it could possibly be a CATCH */
        if (p > &control_stack[0] && p->funstart == p[-1].funstart)
        {
            unsigned char *pc2 = p->pc;

            if (!pc2)
                goto not_catch;  /* shouldn't happen... */

            if (*pc2 == F_LBRANCH - F_OFFSET) {
                union {
                    char bytes[2];
                    short offset;
                } ou;
                pc2++;
                ou.bytes[0] = pc2[0];
                ou.bytes[1] = pc2[0]; /* TODO: not pc2[1] ??? */
                if (ou.offset <= 0)
                    goto not_catch;
                pc2 += ou.offset;
            }
#ifdef F_XLBRANCH
code needs fixing
#endif
            if ( (pc2 - (unsigned char *)p->funstart) < 4)
                goto not_catch;
            if (pc2[-2] != F_ESCAPE-F_OFFSET ||
                pc2[-1] != F_END_CATCH-F_OFFSET-0x100)
            {
                goto not_catch;
            }
            if (last_catch == pc2)
                goto not_catch;
            last_catch = pc2;
            name = "CATCH";
            goto name_computed;
        }
not_catch:
            if (p[0].funstart == SIMUL_EFUN_FUNSTART) {
                /* prog is undefined */
                (void)printf("<simul_efun closure> bound to '%20s' ('%20s')\n",
                    ob->prog->name, ob->name);
                debug_message("<simul_efun closure> bound to '%20s' ('%20s')\n",
                    ob->prog->name, ob->name);
                continue;
            } else if (prog && (p[0].funstart < prog->program ||
                                p[0].funstart > PROGRAM_END(*prog)))
            {
                (void)printf("<lambda 0x%6lx> in '%20s' ('%20s')offset %ld\n",
                    (long)p[0].funstart, ob->prog->name, ob->name,
                    dump_pc - p[0].funstart - 2L );
                debug_message("<lambda 0x%6lx> in '%20s' ('%20s')offset %ld\n",
                    (long)p[0].funstart, ob->prog->name, ob->name,
                    dump_pc - p[0].funstart - 2L );
                continue;
            }
            else if (!prog || !dump_pc)
            {
                printf("<function symbol> in '%20s' ('%20s')\n"
                      , ob->prog->name, ob->name);
                debug_message("<function symbol> in '%20s' ('%20s')\n"
                             , ob->prog->name, ob->name);
                continue;
            }
            line = get_line_number(dump_pc, prog, &file);
            memcpy((char*)&name, p[0].funstart - 1 - sizeof name, sizeof name);
name_computed:
        if (strcmp(name, "heart_beat") == 0 && p != csp)
            ret = p->extern_call ? (p->ob?p->ob->name:0) : ob->name;
        (void)printf("'%15s' in '%20s' ('%20s')line %d\n",
                     name, file, ob->name, line);
        debug_message("'%15s' in '%20s' ('%20s')line %d\n",
                     name, file, ob->name, line);
    } while (++p <= csp);
    return ret;
}

int get_line_number_if_any(name)
    char **name;
{
    if (csp >= &control_stack[0] && csp->funstart == SIMUL_EFUN_FUNSTART) {
        *name = "<simul_efun closure>";
        return 0;
    }
    if (current_prog) {
        if (csp->funstart < current_prog->program ||
            csp->funstart > PROGRAM_END(*current_prog))
        {
            static char name_buffer[24];

            sprintf(name_buffer, "<lambda 0x%6lx>", (long)csp->funstart);
            *name = name_buffer;
            return inter_pc - csp->funstart - 2;
        }
        return get_line_number(inter_pc, current_prog, name);
    }
    *name = "";
    return 0;
}

struct sscanf_info {
    struct svalue *arg_start, *arg_current, *arg_end;
    char *fmt_end;
    char *match_end;
    mp_uint field, min;
    mp_uint string_max, string_min;
    struct sscanf_flags { int do_assign: 16; int count_match: 16; } flags;
    mp_uint sign;
    mp_int number_of_matches;
};

static void sscanf_decimal(str, info)
    char *str;
    struct sscanf_info *info;
{
    static struct svalue tmp_svalue = { T_NUMBER };

    mp_int i, num;
    char c;

    num = 0;
    i = info->min;
    if (i > 0) {
        info->field -= i;
        do {
            if (!lexdigit(c = *str)) {
                if (info->fmt_end[-1] != 'd') {
                    info->match_end = 0;
                } else {
                    info->match_end = str;
                    info->fmt_end = "d"+1;
                }
                return;
            }
            str++;
            num = num * 10 + c - '0';
        } while (--i);
    }
    i = info->field;
    while  (--i >= 0) {
        if (!lexdigit(c = *str))
            break;
        str++;
        num = num * 10 + c - '0';
    }
    info->match_end = str;
    if (info->flags.do_assign) {
        if (info->arg_current >= info->arg_end)
            return;
        tmp_svalue.u.number = (num ^ info->sign) - info->sign;
        transfer_svalue((info->arg_current++)->u.lvalue, &tmp_svalue);
    }
    info->number_of_matches += info->flags.count_match;
    return;
}

/* fmt points to the first character after the '%'.
 * str points to the first character to match.
 * return new value for str if matching is to be continued, else return 0,
 * and write in info->match_end the match end if a match was found,
 * 0 otherwise.
 * If a match was found, also write info->fmt_end with a pointer to the
 * conversion character, and info->flags, info->field, info->min.
 */
INLINE static char *sscanf_match_percent(str, fmt, info)
    char *str, *fmt;
    struct sscanf_info *info;
{
    char c;
    mp_uint *nump;

    /* Initialize field with a large value that will become
     * zero when doubled. Because 10 is divisible by 2, the multiply
     * will zero it. Note that it is negative before we decrement it
     * the first time.
     */
    *(nump = &info->field) = (((mp_uint)-1 / 2)) + 1;
    info->min = 1;
    info->flags.do_assign = 1;
    info->flags.count_match = 1;
    for (;;) {
        switch(c = *fmt++) {
          case '!':
            info->flags.count_match ^= 1;
          case '~':
            info->flags.do_assign ^= 1;
            continue;
          case '0': case '1': case '2': case '3': case '4':
          case '5': case '6': case '7': case '8': case '9':
            *nump = *nump * 10 + c - '0';
            continue;
          case '*':
            if (info->arg_current >= info->arg_end ||
                info->arg_current->u.lvalue->type != T_NUMBER)
            {
                info->match_end = 0;
                return 0;
            }
            *nump = (info->arg_current++)->u.lvalue->u.number;
            continue;
          case '.':
            *(nump = &info->min) = 0;
            continue;
          case 'd':
            while(isspace((unsigned char)*str))
                str++;
          case 'D':
            if (*str == '-') {
                info->sign = -1;
                str++;
            } else {
                if (*str == '+')
                    str++;
#ifndef _DCC /* DICE understands only a subset of C . */
          case 'U':
#endif
                info->sign = 0;
            }
            info->fmt_end = fmt;
            sscanf_decimal(str, info);
            return 0;
#ifdef _DCC
          case 'U':
            info->sign = 0;
            info->fmt_end = fmt;
            sscanf_decimal(str, info);
            return 0;
#endif
          case 's':
            /* min = (min was explicitly given) ? min : 0; */
            info->string_max = info->field;
            info->field = 0;
            info->string_min = *nump;
            info->fmt_end = fmt;
            info->match_end = str;
            return 0;
          default:
            error("Bad type : '%%%c' in sscanf fmt string.\n", fmt[-1]);
            return 0;
          case 't':
          {
            mp_int i;

            info->field -= (i = info->min);
            while (--i >= 0) {
                if (!isspace((unsigned char)*str)) {
                    info->match_end = 0;
                    return 0;
                }
                str++;
            }
            i = info->field;
            while (--i >= 0) {
                if (!isspace((unsigned char)*str))
                    break;
                str++;
            }
            info->fmt_end = fmt;
            return str;
          }
        }
    }
}

/* Find position in str after matching text from fmt, and place it in
 * info->match_end. Write 0 for no match.
 */
static void sscanf_match(str, fmt, info)
    register char *str, *fmt;
    struct sscanf_info *info;
{
    register char c;

    info->arg_current = info->arg_start;
    for (;;) {
        if ( !(c = *fmt) ) {
            info->match_end = str;
            info->fmt_end = "d"+1;
            return;
        }
        fmt++;
        if (c == '%') {
            c = *fmt;
            if (c != '%') {
                char *new_str;

                new_str = sscanf_match_percent(str, fmt, info);
                if (!new_str)
                    return;
                str = new_str;
                fmt = info->fmt_end;
                continue;
            }
            fmt++;
        }
        if (c == *str++) {
            continue;
        } else {
            info->match_end = 0;
            return;
        }
    }
}

/* find start of match in str. return 0 for no match. */
static char *sscanf_search(str, fmt, info)
    char *str, *fmt;
    struct sscanf_info *info;
{
    char a, b, c;
    mp_int n;

    a = *fmt;
    if (!a) {
        info->fmt_end = "d"+1;
        info->arg_current = info->arg_start;
        return info->match_end = str + strlen(str);
    }
    fmt++;
    b = *fmt++;
    if (a == '%') {
        if (b != '%') {
            for (fmt -= 2; *str; str++) {
                sscanf_match(str, fmt, info);
                if (info->match_end)
                    return str;
            }
            return 0;
        } else {
            b = *fmt++;
        }
    }
    if (b == a) {
        n = 0;
        do {
            n++;
            b = *fmt++;
        } while (b == a);
        if (a == '%') {
            if (n & 1) {
                n >>= 1;
                fmt--;
                goto a_na_search;
            }
            n >>= 1;
        }
        if (b == 0) {
            fmt--;
            goto a_na_search;
        }
        if (b == '%') {
            b = *fmt++;
            if (b != '%') {
                fmt -= 2;
                goto a_na_search;
            }
        }
        {
            char ch;
            mp_int i;

a_na_b_search:
            if ( !(ch = *str++) )
                return 0;
            if (ch != a)
                goto a_na_b_search;
            i = n;
            do {
                if ( !(ch = *str++) )
                    return 0;
                if (ch != a)
                    goto a_na_b_search;
            } while (--i);
            do {
                if ( !(ch = *str++) )
                    return 0;
            } while (ch == a);
            if (ch == b) {
                sscanf_match(str, fmt, info);
                if (info->match_end)
                    return str - n - 2;
            }
            goto a_na_b_search;
        }
        /* not reached */
    }
    if (!b) {
        n = 0;
        fmt--;
        {
            char ch;
            mp_int i;

a_na_search:
            if ( !(ch = *str++) )
                return 0;
            if (ch != a)
                goto a_na_search;
            if ( 0 != (i = n)) do {
                if ( !(ch = *str++) )
                    return 0;
                if (ch != a)
                    goto a_na_search;
            } while (--i);
            do {
                sscanf_match(str, fmt, info);
                if (info->match_end)
                    return str - n - 1;
                if ( !(ch = *str++) )
                    return 0;
            } while (ch == a);
            goto a_na_search;
        }
        /* not reached */
    }
    if (b == '%') {
        b = *fmt++;
        if (b != '%') {
            fmt -= 2;
            n = 0;
            goto a_na_search;
        }
    }
    c = *fmt;
    if (!c) {
        n = 0;
        goto ab_nab_search;
    }
    if (c == '%') {
        c = *++fmt;
        if (c != '%') {
            fmt--;
            n = 0;
            goto ab_nab_search;
        }
    }
    fmt++;
    if (c == a) {
        c = *fmt++;
        if (c == '%') {
            c = *fmt;
            if (c != '%') {
                fmt -= 2 + (a == '%');
                n = 0;
                goto ab_nab_search;
            }
            fmt++;
        }
        if (c != b) {
            if (!c) {
                fmt -= 2 + (a == '%');
                n = 0;
                goto ab_nab_search;
            }
            for (;;) {
                char ch;

                ch = *str++;
a_b_a_c_check_a:
                if (!ch)
                    return 0;
                if (ch != a)
                    continue;
                ch = *str++;
a_b_a_c_check_b:
                if (ch != b)
                    goto a_b_a_c_check_a;
                ch = *str++;
                if (ch != a)
                    continue;
                ch = *str++;
                if (ch != c)
                    goto a_b_a_c_check_b;
                sscanf_match(str, fmt, info);
                if (info->match_end)
                    return str - 4;
                goto a_b_a_c_check_a;
            }
            /* not reached */
        }
        /* c == b */
        n = 2;
        {
            char ch;
            int i;

            goto ab_nab_search;
ab_nab_check_0:
            if (!ch)
                return 0;
ab_nab_search:
            ch = *str++;
ab_nab_check_a:
            if (ch != a)
                goto ab_nab_check_0;
            ch = *str++;
            if (ch != b)
                goto ab_nab_check_a;
            if (0 != (i = n)) do {
                ch = *str++;
                if (ch != a)
                    goto ab_nab_check_0;
                ch = *str++;
                if (ch != b)
                    goto ab_nab_check_a;
            } while (i -= 2);
            do {
                sscanf_match(str, fmt, info);
                if (info->match_end)
                    return str - n - 2;
                ch = *str++;
                if (ch != a)
                    goto ab_nab_check_0;
                ch = *str++;
            } while (ch == b);
            goto ab_nab_check_0;
        }
        /* not reached */
    }
    /* c != a */
    for (;;) {
        char ch;

        ch = *str++;
a_b_c_check_a:
        if (!ch)
            return 0;
        if (ch != a)
            continue;
        ch = *str++;
        if (ch != b)
            goto a_b_c_check_a;
        ch = *str++;
        if (ch != c)
            goto a_b_c_check_a;
        sscanf_match(str, fmt, info);
        if (info->match_end)
            return str - 3;
    }
}

/* You can think of '!' as negating on a wholesale basis, while '~'
 * negates only individual bits. Thus, '%!' negates both do_assign
 * and count_match, while '%~' only negates do_assign.
 */
static int inter_sscanf(num_arg, sp)
    int num_arg;
    struct svalue *sp;
{
    char *fmt;                /* Format description */
    char *in_string;        /* The string to be parsed. */
    struct sscanf_flags flags;
    struct svalue sv_tmp;
    struct svalue *arg0;
    struct sscanf_info info;

    inter_sp = sp; /* we can have an error() deep inside */
    arg0 = sp - num_arg + 1;
    /*
     * First get the string to be parsed.
     */
    if (arg0[0].type != T_STRING)
        bad_efun_vararg(1, sp);
    in_string = arg0[0].u.string;
    /*
     * Now get the format description.
     */
    if (arg0[1].type != T_STRING)
        bad_efun_vararg(2, sp);
    fmt = arg0[1].u.string;
    info.arg_end = arg0 + num_arg;
    info.arg_current = arg0 + 2;
    /*
     * Loop for every % or substring in the format. Update the
     * arg pointer continuosly. Assigning is done manually, for speed.
     */

    for (info.number_of_matches = 0; info.arg_current <= info.arg_end;) {
        info.arg_start = info.arg_current;
        sscanf_match(in_string, fmt, &info);
        in_string = info.match_end;
        if (!in_string)
            break;
match_skipped:
        fmt = info.fmt_end;
        if (fmt[-1] == 's') {
            mp_uint max;
            mp_int num;
            char *match;
            struct svalue *arg;

            flags = info.flags;
            num = info.string_min;
            if (num > 0) {
                if (num > (mp_int)strlen(in_string))
                    break;
                match = in_string + num;
            } else {
                /* num = 0 */
                match = in_string;
            }
            max = info.string_max;
            if ((info.arg_start = (arg=info.arg_current) + flags.do_assign) >
                info.arg_end)
            {
                break;
            }
            if (NULL != (match = sscanf_search(match, fmt, &info)) &&
                (mp_uint)(num = match - in_string) <= max)
            {
                if (flags.do_assign) {
                    match = xalloc(num+1);
                    if (!match)
                        error("Out of memory\n");
                    (void)strncpy(match, in_string, num);
                    match[num] = '\0';
                    sv_tmp.type = T_STRING;
                    sv_tmp.x.string_type = STRING_MALLOC;
                    sv_tmp.u.string = match;
                    transfer_svalue(arg->u.lvalue, &sv_tmp);
                }
                in_string = info.match_end;
                info.number_of_matches += flags.count_match;
                info.arg_start = info.arg_current;
                goto match_skipped;
            }
            /* no match found */
            break;
        }
        if (!fmt[0])
            break;
    }
    return info.number_of_matches;
}

/* test stuff ... -- LA */
#ifdef OPCPROF
void opcdump()
{
    int i;

    for(i = 0; i < MAXOPC; i++)
        if (opcount[i])
#ifdef VERBOSE_OPCPROF
            printf("%d: \"%-16s\" %6d\n",i+F_OFFSET,get_f_name(i), opcount[i]);
#else
            printf("%d: %d\n", i+F_OFFSET, opcount[i]);
#endif
    fflush(stdout); /* amylaar */
}
#endif

/*
 * Reset the virtual stack machine.
 */
void reset_machine(first)
    int first;
{
    csp = control_stack - 1;
    traceing_recursion = -1;
    if (first) {
        inter_sp = start_of_stack - 1;
        tracedepth = 0;
    } else
        inter_sp = _pop_n_elems(inter_sp - start_of_stack + 1, inter_sp);
}

/* this is used in the main loop of backend.c to find out why reset_state
 * in the main loop has an effect
 */
#ifdef DEBUG
int check_state() {
    if (error_recovery_pointer->type != ERROR_RECOVERY_BACKEND) {
        debug_message("error_recovery_context_exists inconsistent!");
        return 1;
    }
    if (csp != control_stack - 1) {
        debug_message("csp inconsistent!");
        return 2;
    }
    if (inter_sp != start_of_stack - 1) {
        debug_message("sp inconsistent!");
        return 3;
    }
    return 0;
}
#endif

#ifdef TRACE_CODE

static char *get_arg(a, b)
    int a, b;
{
    static char buff[10];
    char *from, *to;

    from = previous_pc[a]; to = previous_pc[b];
    if (to - from < 2)
        return "";
    if (to - from == 2) {
        sprintf(buff, "%d", from[1]);
        return buff;
    }
    if (to - from == 3) {
        short arg[2];
        ((char *)arg)[0] = from[1];
        ((char *)arg)[1] = from[2];
        sprintf(buff, "%d", arg[0]);
        return buff;
    }
    if (to - from == 5) {
        int arg;
        ((char *)&arg)[0] = from[1];
        ((char *)&arg)[1] = from[2];
        ((char *)&arg)[2] = from[3];
        ((char *)&arg)[3] = from[4];
        sprintf(buff, "%d", arg);
        return buff;
    }
    return "";
}

static void last_instr_output(str, svpp)
    char *str;
    struct svalue **svpp;
{
    if (svpp) {
        if ( !(str = string_copy(str)) )
            error("Out of memory\n");
        (*svpp)->type = T_STRING;
        (*svpp)->x.string_type = STRING_MALLOC;
        (*svpp)->u.string = str;
        (*svpp)++;
    } else {
        printf("%s\n", str);
    }
}

static int program_referenced(prog, prog2)
    struct program *prog, *prog2;
{
    struct inherit *inh;
    int i;

    if (prog == prog2) return 1;
    if ((p_int)prog2 & 1) return 0;
    for (i = prog2->num_inherited, inh = prog2->inherit; --i >= 0; inh++) {
        if (program_referenced(prog, inh->prog))
            return 1;
    }
    return 0;
}

static int program_exists(prog, guess)
    struct program *prog;
    struct object *guess;
{
    if (program_referenced(prog, guess->prog))
        return 1;
    for (guess = obj_list; guess; guess = guess->next_all) {
        if (guess->flags & O_DESTRUCTED)
            continue;
        if (program_referenced(prog, guess->prog))
            return 1;
    }
    return 0;
}

int last_instructions(length, verbose, svpp)
    int length, verbose;
    struct svalue **svpp;
{
    int i;
    struct object *old_obj;
    char old_file[160], buf[400];
    int old_line, line = 0;

    old_obj = 0;
    *old_file = old_file[sizeof old_file - 1] = 0;
    old_line = 0;
    i = (last - length + TOTAL_TRACE_LENGTH) % TOTAL_TRACE_LENGTH;
    do {
        i = (i + 1) % TOTAL_TRACE_LENGTH;
        if (previous_instruction[i] != 0) {
            if (verbose) {
                char *file;
                struct program *ppr;
                char *ppc;

                ppr = previous_programs[i];
                ppc = previous_pc[i]+1;
                if (!program_exists(ppr, previous_objects[i])) {
                    file = "program deallocated";
                    line = 0;
                } else if (ppc < ppr->program ||
                    ppc > PROGRAM_END(*ppr))
                {
                    file = "<lambda ???>";
                    line = 0;
                } else {
                    line = get_line_number(ppc, ppr, &file);
                }
                if (previous_objects[i] != old_obj || strcmp(file, old_file)) {
                    sprintf(buf, "%.170s %.160s line %d",
                      previous_objects[i]->name, file, line
                    );
                    last_instr_output(buf, svpp);
                    old_obj = previous_objects[i];
                    strncpy(old_file, file, sizeof old_file - 1);
                }
            }
            sprintf(buf, "%6lx: %3d %8s %-25s (%d)", (long)previous_pc[i],
                   previous_instruction[i], /* instrs.h has these numbers */
                   get_arg(i, (i+1) % TOTAL_TRACE_LENGTH),
                   get_f_name(previous_instruction[i]),
                   stack_size[i] + 1
            );
            if (verbose && line != old_line)
                sprintf(buf + strlen(buf), "\tline %d", old_line = line);
            last_instr_output(buf, svpp);
        }
    } while (i != last);
    return last;
}

struct svalue *f_last_instructions(sp)
    struct svalue *sp;
{
    struct vector *v, *v2;
    mp_int num_instr, size;
    struct svalue *svp;

    if (sp[-1].type != T_NUMBER || (num_instr = sp[-1].u.number) <= 0)
        bad_xefun_arg(1, sp);
    if (sp->type != T_NUMBER)
        bad_xefun_arg(2, sp);
    sp--;
    inter_sp = sp; /* Out of memory possible */
    if (num_instr > TOTAL_TRACE_LENGTH)
        num_instr = TOTAL_TRACE_LENGTH;
    size = sp[1].u.number ? num_instr << 1 : num_instr;
    v = allocate_array(size);
    /* enter into stack now, so that it will be freed when an
     * out of memory error occurs.
     */
    sp->type = T_POINTER;
    sp->u.vec = v;
    svp = v->item;
    last_instructions(num_instr, sp[1].u.number, &svp);
    if (svp - v->item < size) {
        size = svp - v->item;
        v2 = allocate_array(size);
        memcpy((char*)v2->item, (char*)v->item, size * sizeof *svp);
        sp->u.vec = v2;
        free_empty_vector(v);
    }
    return sp;
}

#endif /* TRACE_CODE */


#ifdef DEBUG

static struct program *check_a_lot_ref_counts_search_prog;
void count_inherits(progp)
    struct program *progp;
{
    int i;
    struct program *progp2;

    /* Clones will not add to the ref count of inherited progs */
    for (i=0; i< progp->num_inherited; i++) {
        progp2 = progp->inherit[i].prog;
        progp2->extra_ref++;
        if (progp2 == check_a_lot_ref_counts_search_prog)
            printf("Found prog, inherited by %s, new total ref %ld\n",
              progp->name, progp2->ref);
        if (register_pointer((char *)progp2))
            continue;
        progp2->extra_ref = 1;
        count_inherits(progp2);
    }
}

static void check_extra_ref_in_vector PROT((struct svalue *svp, mp_int num));

static void count_extra_ref_in_mapping_filter(key, data, extra)
    struct svalue *key, *data;
    char *extra;
{
    count_extra_ref_in_vector(key, 1);
    count_extra_ref_in_vector(data, (mp_int)extra);
}

static void check_extra_ref_in_mapping_filter(key, data, extra)
    struct svalue *key, *data;
    char *extra;
{
    check_extra_ref_in_vector(key, 1);
    check_extra_ref_in_vector(data, (mp_int)extra);
}

void count_extra_ref_in_object(ob)
    struct object *ob;
{
    ob->extra_ref++;
    if ( register_pointer( (char *)(ob) ) )
        return;
    ob->extra_ref = 1;
    if ( !O_PROG_SWAPPED(ob) ) {
        ob->prog->extra_ref++;
        if (ob->prog == check_a_lot_ref_counts_search_prog)
            printf("Found program for object %s\n", ob->name);
    }
    /* Clones will not add to the ref count of inherited progs */
    if (O_PROG_SWAPPED(ob)) {
        /* hmmm, what are we going to do here?
           we could swap in the object, but this would make debugging
           of swapping rather unrealistic.
           At any rate, doing nothing is probably better then referencing
           a pointer to a freed memory block... unless you can guarantee
           that freed blocks are never reused again...
           ... and the pointer is lost, anyway.
        */
    } else {
        if (register_pointer((char *)ob->prog))
            return;
        ob->prog->extra_ref = 1;
        count_inherits(ob->prog);
    }
    if (ob->flags & O_SHADOW) {
        struct ed_buffer *buf;

        if ( NULL != (buf = O_GET_SHADOW(ob)->ed_buffer) )
            count_ed_buffer_extra_refs(buf);
    }
}

static void count_extra_ref_in_closure(l, type)
    struct lambda *l;
    ph_int type;
{
    if (CLOSURE_HAS_CODE(type)) {
        mp_int num_values;
        struct svalue *svp;

        svp = (struct svalue *)l;
        if ( (num_values = EXTRACT_UCHAR(l->function.code)) == 0xff)
            num_values = svp[-0xff].u.number;
        svp -= num_values;
        count_extra_ref_in_vector(svp, num_values);
    } else {
        if (type == CLOSURE_BOUND_LAMBDA)
        {
            struct lambda *l2 = l->function.lambda;

            if ( !register_pointer( (char *)(l2) ) )
                count_extra_ref_in_closure(l2, CLOSURE_UNBOUND_LAMBDA);
        } else if (type == CLOSURE_ALIEN_LFUN) {
            count_extra_ref_in_object(l->function.alien.ob);
        }
    }
    if (type != CLOSURE_UNBOUND_LAMBDA)
        count_extra_ref_in_object(l->ob);
}

void count_extra_ref_in_vector(svp, num)
    struct svalue *svp;
    mp_int num;
{
    struct svalue *p;

    for (p = svp; p < svp+num; p++) {
        switch(p->type) {
          case T_CLOSURE:
            if (CLOSURE_MALLOCED(p->x.closure_type)) {
                struct lambda *l;

                l = p->u.lambda;
                if ( register_pointer( (char *)(l) ) ) continue;
                count_extra_ref_in_closure(l, p->x.closure_type);
                continue;
            }
            /* else fall through */
          case T_OBJECT:
          {
            count_extra_ref_in_object(p->u.ob);
            continue;
          }
          case T_QUOTED_ARRAY:
          case T_POINTER:
            p->u.vec->extra_ref++;
            if ( register_pointer( (char *)(p->u.vec) ) )
                continue;
            p->u.vec->extra_ref = 1;
            count_extra_ref_in_vector(&p->u.vec->item[0], VEC_SIZE(p->u.vec));
            continue;
#ifdef MAPPINGS
          case T_MAPPING:
            if ( register_pointer( (char *)(p->u.map) ) ) continue;
            walk_mapping(
              p->u.map,
              count_extra_ref_in_mapping_filter,
              (char *)p->u.map->num_values
            );
            continue; /* no extra ref count implemented */
#endif
        }
    }
}

static void check_extra_ref_in_vector(svp, num)
    struct svalue *svp;
    mp_int num;
{
    struct svalue *p;

    for (p = svp; p < svp+num; p++) {
        switch(p->type) {
          case T_QUOTED_ARRAY:
          case T_POINTER:
            if ( register_pointer( (char *)(p->u.vec) ) ) continue;
            check_extra_ref_in_vector(&p->u.vec->item[0], VEC_SIZE(p->u.vec));
            p->u.vec->extra_ref = 0;
            continue;
#ifdef MAPPINGS
          case T_MAPPING:
            if ( register_pointer( (char *)(p->u.map) ) ) continue;
            walk_mapping(
              p->u.map,
              check_extra_ref_in_mapping_filter,
              (char *)p->u.map->num_values
            );
            continue; /* no extra ref count implemented */
#endif
        }
    }
}

/*
 * Loop through every object and variable in the game and check
 * all reference counts. This will surely take some time, and should
 * only be used for debugging.
 */
void check_a_lot_ref_counts(search_prog)
    struct program *search_prog;
{
    struct object *ob;
    struct pointer_record *pointer_table_space[256];
    int i;

    check_a_lot_ref_counts_search_prog = search_prog;

    /*
     * Pass 1: clear the ref counts. Not needed.
     */

    /*
     * Pass 2: Compute the ref counts.
     */

    init_pointer_table(pointer_table_space);
    /*
     * List of all objects.
     */
    for (ob=obj_list; ob; ob = ob->next_all) {
        if (ob->flags & O_DESTRUCTED)
            continue;
        if (O_VAR_SWAPPED(ob))
            load_ob_from_swap(ob);
        count_extra_ref_in_vector(ob->variables, ob->extra_num_variables);
        count_extra_ref_in_object(ob);
    }
    if (d_flag > 3) {
        debug_message("obj_list evaluated\n");
    }

    /*
     * The current stack.
     */
    count_extra_ref_in_vector(start_of_stack, inter_sp - start_of_stack + 1);
    if (d_flag > 3) {
        debug_message("stack evaluated\n");
    }
    count_extra_ref_from_call_outs();
    count_extra_ref_from_wiz_list();
    count_simul_efun_extra_refs();
    count_comm_extra_refs();
    if (master_ob) master_ob->extra_ref++; /* marion */
#ifdef TRACE_CODE
{
    int j;

    for (j = TOTAL_TRACE_LENGTH; --j >= 0; ) {
        if ( NULL != (ob = previous_objects[j]) )
            count_extra_ref_in_object(ob);
    }
}
#endif

    count_extra_ref_in_vector(&indexing_quickfix, 1);
    count_extra_ref_in_vector(&last_indexing_protector, 1);
    null_vector.extra_ref++;
    for (i = NUM_CLOSURE_HOOKS; --i >= 0; ) {
        if (closure_hook[i].type == T_CLOSURE &&
            closure_hook[i].x.closure_type == CLOSURE_LAMBDA)
        {
            closure_hook[i].x.closure_type = CLOSURE_UNBOUND_LAMBDA;
        }
    }
    count_extra_ref_in_vector(closure_hook, NUM_CLOSURE_HOOKS);
    for (i = NUM_CLOSURE_HOOKS; --i >= 0; ) {
        if (closure_hook[i].type == T_CLOSURE &&
            closure_hook[i].x.closure_type == CLOSURE_UNBOUND_LAMBDA)
        {
            closure_hook[i].x.closure_type = CLOSURE_LAMBDA;
        }
    }
    free_pointer_table();
    if (search_prog)
        return;

    /*
     * Pass 3: Check the ref counts.
     */
    init_pointer_table(pointer_table_space);

    for (ob=obj_list; ob; ob = ob->next_all) {
        if (ob->flags & O_DESTRUCTED)
            continue;
        if (ob->ref != ob->extra_ref)
             fatal("Bad ref count in object %s, %ld - %ld\n", ob->name,
                  ob->ref, ob->extra_ref);
        if ( !(ob->flags & O_SWAPPED) ) {
            if (ob->prog->ref != ob->prog->extra_ref) {
                /* an inheriting file might be swapped */
                if (TIME_TO_SWAP > 0 && time_to_swap + 1 > 0 &&
                    ob->prog->ref > ob->prog->extra_ref)
                {
                    debug_message("high ref count in prog %s, %ld - %ld\n",
                        ob->prog->name, ob->prog->ref, ob->prog->extra_ref);
                } else {
                    check_a_lot_ref_counts(ob->prog);
                    fatal("Bad ref count in prog %s, %ld - %ld\n",
                        ob->prog->name, ob->prog->ref, ob->prog->extra_ref);
                }
            }
        } /* !SWAPPED */
        check_extra_ref_in_vector(ob->variables, ob->extra_num_variables);
    } /* for */

    check_extra_ref_in_vector(start_of_stack, inter_sp - start_of_stack + 1);

    free_pointer_table();
}

#endif /* DEBUG */

/* Generate a debug message to the player */
static void
do_trace(msg, fname, post)
char *msg, *fname, *post;
{
    char buf[10000];
    char *objname;

    if (!TRACEHB)
        return;
    objname = TRACETST(TRACE_OBJNAME) ? (current_object && current_object->name ? current_object->name : "??")  : "";
    sprintf(buf, "*** %d %*s %s %s %s%s", tracedepth, tracedepth, "", msg, objname, fname, post);
    add_message(buf);
#ifdef DEBUG
    add_message(message_flush);
#endif
}

static void secure_apply_error(save_sp, save_csp)
    struct svalue *save_sp;
    struct control_stack *save_csp;
{
    if (csp != save_csp) { /* could be error before push */
        csp = save_csp+1;
        previous_ob = csp->prev_ob;
        current_object = csp->ob;
        pop_control_stack();
    }
    inter_sp = _pop_n_elems (inter_sp - save_sp, inter_sp);
    if (num_error == 3) {
        if (!out_of_memory) {
            debug_message("Master failure: %s", current_error);
            xfree(current_error);
            xfree(current_error_file);
            xfree(current_error_object_name);
        }
    } else if (!out_of_memory) {
        int a;
        struct object *save_cmd;

        push_malloced_string(current_error);
        a = 1;
        if (current_error_file) {
            push_malloced_string(current_error_file);
            push_malloced_string(current_error_object_name);
            push_number(current_error_line_number);
            a += 3;
        }
        save_cmd = command_giver;
        apply_master_ob(STR_RUNTIME, a);
        command_giver = save_cmd;
    }
    num_error--;
}

struct svalue *secure_apply(fun, ob, num_arg)
    char *fun;
    struct object *ob;
    int num_arg;
{
    struct error_recovery_info error_recovery_info;
    struct svalue *save_sp;
    struct control_stack *save_csp;
    struct svalue *result;

    if (ob->flags & O_DESTRUCTED)
        return (struct svalue *)0;
    error_recovery_info.last = error_recovery_pointer;
    error_recovery_info.type = ERROR_RECOVERY_APPLY;
    error_recovery_pointer = &error_recovery_info;
    save_sp = inter_sp;
    save_csp = csp;
    if (setjmp(error_recovery_info.con.text)) {
        secure_apply_error(save_sp - num_arg, save_csp);
        result = (struct svalue *)0;
    } else {
        result = sapply(fun, ob, num_arg);
    }
    error_recovery_pointer = error_recovery_info.last;
    return result;
}

struct svalue *apply_master_ob(fun, num_arg)
    char *fun;
    int num_arg;
{
    static int eval_cost_reserve = MASTER_RESERVED_COST;

    int reserve_used = 0;
    struct error_recovery_info error_recovery_info;
    struct svalue *save_sp;
    struct control_stack *save_csp;
    struct svalue *result;

    assert_master_ob_loaded();
    if ( (eval_cost >= MIN_TRACE_COST ?
            eval_cost > MAX_TRACE_COST - MASTER_RESERVED_COST :
            eval_cost > -MASTER_RESERVED_COST) &&
        eval_cost_reserve > 1)
    {
        eval_cost -= eval_cost_reserve;
        assigned_eval_cost -= eval_cost_reserve;
        eval_cost_reserve >>= 1;
        reserve_used = 1;
    }
    error_recovery_info.last = error_recovery_pointer;
    error_recovery_info.type = ERROR_RECOVERY_APPLY;
    error_recovery_pointer = &error_recovery_info;
    save_sp = inter_sp;
    save_csp = csp;
    if (setjmp(error_recovery_info.con.text)) {                /* amylaar */
        secure_apply_error(save_sp - num_arg, save_csp);
        printf("Error in master_ob->%s()\n", fun);
        debug_message("Error in master_ob->%s()\n", fun);
        result = (struct svalue *)0;
    } else {
        result = sapply_int(fun, master_ob, num_arg, MY_TRUE);
    }
    if (reserve_used) {
        eval_cost_reserve <<= 1;
        assigned_eval_cost = eval_cost += eval_cost_reserve;
    }
    error_recovery_pointer = error_recovery_info.last;
    return result;
}

void assert_master_ob_loaded()
{
    static int inside = 0;
    static struct object *destructed_master_ob = 0;

    if (master_ob == 0 || master_ob->flags & O_DESTRUCTED) {
        /*
         * The master object has been destructed. Free our reference,
         * and load a new one.
         *
         * amylaar:
         * We could be 'inside' if, while calling a master function from
         * yyparse() (e.g. log_error() ), the master self-destructs and
         * then causes an error.
         * Another possibility is that some driver hook invokes some
         * function that uses apply_master_ob().
         *
         * The master object might have been reloaded without noticing that
         * it is the master.
         * This could happen when there already was a call to
         * assert_master_ob_loaded(), clearing master_ob, and the master
         * inherits itself. Partial working self-inheritance is possible if
         * the H_INCLUDE_DIRS hook does something strange.
         */
        if (inside || !master_ob) {
            if (destructed_master_ob) {
                struct object *ob, **pp;
                int i;
                int removed = 0;

                if ( NULL != (ob = find_object(master_name)) ) {
                    emergency_destruct(ob);
                }
                ob = destructed_master_ob;
                destructed_master_ob = 0;
                if (new_destructed)
                  for (pp = &obj_list; *pp; pp = &(*pp)->next_all) {
                    if (*pp != ob)
                        continue;
                    *pp = ob->next_all;
                    removed = 1;
                    new_destructed--;
                    break;
                }
                ob->flags &= ~O_DESTRUCTED;
                enter_object_hash(ob);
                if (!removed && ob->prog->num_variables) {
                    int save_privilege = malloc_privilege;
                    struct svalue *v;

                    malloc_privilege = MALLOC_SYSTEM;
                    ob->variables = v = (struct svalue *)
                        xalloc(sizeof *v * ob->prog->num_variables);
                    malloc_privilege = save_privilege;
                    for (i = ob->prog->num_variables; --i >= 0; )
                        *v++ = const0;
                }
                ob->next_all = obj_list;
                obj_list = ob;
                ob->super = 0;
                ob->contains = 0;
                ob->next_inv = 0;
                master_ob = ob;
                add_ref(ob, "assert_master_ob_loaded");
                if (current_object == &dummy_current_object_for_loads)
                    current_object = master_ob;
                push_number(removed);
                sapply(STR_REACTIVATE, ob, 1);
                push_number(2 - removed);
                sapply(STR_INAUGURATE, ob, 1);
                fprintf(stderr, "Old master reactivated.\n");
                inside = 0;
                return;
            }
            fprintf(stderr, "Failed to load master object.\n");
            add_message("Failed to load master file !\n");
            exit(1);
        }
        fprintf(stderr, "assert_master_ob_loaded: Reloading master.c\n");
        destructed_master_ob = master_ob;
        /*
         * Clear the pointer, in case the load failed.
         */
        master_ob = 0;
        inside = 1;
        clear_auto_include_string();
        if (!current_object) {
            current_object = &dummy_current_object_for_loads;
        }
        {
            int i;

            i = NUM_CLOSURE_HOOKS - 1;
            do {
                if (closure_hook[i].type == T_CLOSURE &&
                    closure_hook[i].x.closure_type == CLOSURE_LAMBDA)
                {
                    closure_hook[i].x.closure_type = CLOSURE_UNBOUND_LAMBDA;
                }
                free_svalue(&closure_hook[i]);
                closure_hook[i] = const0;
            } while (--i >= 0);
        }
        init_telopts();
        master_ob = get_object(master_name);
        if (current_object == &dummy_current_object_for_loads) {
            /* This might be due to the above assignment, or to setting
             * it in the backend.
             */
            current_object = master_ob;
        }
        initialize_master_uid();
        push_number(3);
        apply_master_ob(STR_INAUGURATE, 1);
        assert_master_ob_loaded();
        inside = 0;
        add_ref(master_ob, "assert_master_ob_loaded");
        if (destructed_master_ob)
            free_object(destructed_master_ob, "assert_master_ob_loaded");
        fprintf(stderr, "Reloading done.\n");
    }
}

struct svalue *secure_call_lambda(closure, num_arg)
    struct svalue *closure;
    int num_arg;
{
    struct error_recovery_info error_recovery_info;
    struct svalue *save_sp;
    struct control_stack *save_csp;
    struct svalue *result;

    error_recovery_info.last = error_recovery_pointer;
    error_recovery_info.type = ERROR_RECOVERY_APPLY;
    error_recovery_pointer = &error_recovery_info;
    save_sp = inter_sp;
    save_csp = csp;
    if (setjmp(error_recovery_info.con.text)) {
        secure_apply_error(save_sp - num_arg, save_csp);
        result = (struct svalue *)0;
    } else {
        call_lambda(closure, num_arg);
        transfer_svalue((result = &apply_return_value), inter_sp);
        inter_sp--;
    }
    error_recovery_pointer = error_recovery_info.last;
    return result;
}

/*
 * When an object is destructed, all references to it must be removed
 * from the stack.
 */
void remove_object_from_stack(ob)
    struct object *ob;
{
    struct svalue *svp;

    for (svp = start_of_stack; svp <= inter_sp; svp++) {
        if (svp->type != T_OBJECT)
            continue;
        if (svp->u.ob != ob)
            continue;
        free_object(ob, "remove_object_from_stack");
        svp->type = T_NUMBER;
        svp->u.number = 0;
    }
}

static int trace_test(b)
    int b;
{
    struct interactive *ip;

    return command_giver && NULL != (ip = O_GET_INTERACTIVE(command_giver)) &&
      ip->sent.type == SENT_INTERACTIVE && ip->trace_level & b &&
      (ip->trace_prefix == 0 ||
       (current_object && strpref(ip->trace_prefix, current_object->name)));
}

static int
strpref(p, s)
char *p, *s;
{
    while (*p)
        if (*p++ != *s++)
            return 0;
    return 1;
}

/* Concatenation of two arrays into one, freeing the summands.
 */
static struct vector *inter_add_array(q, vpp)
    struct vector *q, **vpp;
{
    struct vector *p = *vpp;
    mp_int cnt;
    struct vector *r;
    struct svalue *s, *d;
    mp_int p_size, q_size;

    inter_sp -= 2;
    /* Out of memory might result in some memory leaks. Better that freeing
     * arrays with 0 ref count, or indigestion in garbage_collection() .
     * It will simply give some more debugging output...
     */

    /* *vpp could be in the summands, thus don't free p / q before
     * assigning.
     * On the other hand, with an uninitialized array, we musn't assign
     * before the copying is done.
     */
    p_size = VEC_SIZE(p);
    q_size = VEC_SIZE(q);
    s = p->item;
    if (!(p->ref-1)) {
#ifdef MALLOC_smalloc
        /* we must not free any old array before we did the assignment,
         * thus vanilla realloc is not acepptable.
         */
        /* this is provisoric, i need some stats from several days uptime
         * to judge if it actually pays to do the test.
         */
        if ( NULL != (d = (struct svalue *)malloc_increment_size((char *)p, q_size<<1)) ) {
            (r = p)->ref = 1;
            r->user->size_array -= p_size;
            (r->user = current_object->user)->size_array += p_size + q_size;
            if (p_size + q_size > MAX_ARRAY_SIZE) {
                *vpp = allocate_array(0);
                d = r->item + p_size;
                for (cnt = q_size; --cnt >=0; ) {
                    d[cnt].type = T_INVALID;
                }
                free_vector(r);
                free_vector(q);
                error("Illegal array size: %ld.\n", p_size + q_size);
            }
        } else
#endif
        {
            r = allocate_uninit_array(p_size + q_size);
            p->ref--;
            d = r->item;
            for (cnt = p_size; --cnt >= 0; ) {
                *d++ = *s++;
            }
        }
    } else {
        r = allocate_uninit_array(p_size + q_size);
        p->ref--;
        d = r->item;
        for (cnt = p_size; --cnt >= 0; ) {
            assign_checked_svalue_no_free (d++, s++, inter_sp, inter_pc);
        }
    }
    s = q->item;
    if (!--q->ref) {
        for (cnt = q_size; --cnt >= 0; ) {
            if (s->type == T_OBJECT && s->u.ob->flags & O_DESTRUCTED)
                zero_object_svalue(s);
            *d++ = *s++;
        }
        *vpp = r;
        free_empty_vector(q);
    } else {
        for (cnt = q_size; --cnt >= 0; ) {
            assign_checked_svalue_no_free (d++, s++, inter_sp, inter_pc);
        }
        *vpp = r;
    }
    if (!p->ref && p != q)
        free_empty_vector(p);
    return r;
}

#undef ERROR
#define ERROR(s) {inter_sp = sp; error(s);}

#undef TYPE_TEST1
#define TYPE_TEST1(arg1, type1, instruction) {\
    if ((arg1)->type != (type1)) {\
        bad_efun_arg(1, (instruction)-F_OFFSET, sp);\
    }\
}

#undef TYPE_TEST2
#define TYPE_TEST2(arg1, type2, instruction) {\
    if ((arg1)->type != (type2)) {\
        bad_efun_arg(2, (instruction)-F_OFFSET, sp);\
    }\
}

struct svalue *f_set_is_wizard(sp)
    struct svalue *sp;
{
    int i;
    unsigned short *flagp;

    TYPE_TEST1(sp-1, T_OBJECT, F_SET_IS_WIZARD)
    TYPE_TEST2(sp,   T_NUMBER, F_SET_IS_WIZARD)
    flagp = &sp[-1].u.ob->flags;
    i = (*flagp & O_IS_WIZARD) != 0;
    switch (sp->u.number) {
        default: bad_xefun_arg(2, sp);
        case  0: *flagp &= ~O_IS_WIZARD; is_wizard_used = 1; break;
        case  1: *flagp |=  O_IS_WIZARD; is_wizard_used = 1; break;
        case -1: break; /* only report status */
    }
    sp--;
    free_object_svalue(sp);
    put_number(i);
    return sp;
}

struct svalue *f_set_modify_command(sp)
    struct svalue *sp;
{
    struct object *old, *new;
    struct interactive *ip;

    inter_sp = sp;
    if ( !(ip = O_GET_INTERACTIVE(current_object)) ||
         ip->sent.type != SENT_INTERACTIVE ||
         ip->closing)
    {
        error("set_modify_command in non-interactive object\n");
    }
    old = ip->modify_command;
    if (old && old->flags & O_DESTRUCTED) {
        free_object(old, "set_modify_command");
        old = 0;
        ip->modify_command = 0;
    }
    new = sp->u.ob;
    switch(sp->type) {
      default:
bad_arg_1:
        bad_xefun_arg(1, sp);
      case T_STRING:
        new = get_object(sp->u.string);
        if (!new) goto bad_arg_1;
      case T_OBJECT:
        add_ref(new, "set_modify_command");
        ip->modify_command = new;
        break;
      case T_NUMBER:
        if (sp->u.number == 0 ) {
            /* ref count of old is reused below, so don't free now */
            ip->modify_command = 0;
        } else {
            if (sp->u.number != -1) goto bad_arg_1;
            if (old) add_ref(old, "set_modify_command");
        }
    }
    free_svalue(sp);
    if (old) {
        sp->type = T_OBJECT;
        sp->u.ob = old; /* reuse ref count */
    } else
        put_number(0);
    return sp;
}

struct svalue *f_set_prompt(sp)
    struct svalue *sp;
{
    struct svalue *prompt;
    struct interactive *ip;

    if (sp->type != T_OBJECT ||
        !(ip = O_GET_INTERACTIVE(sp->u.ob)) ||
        ip->sent.type != SENT_INTERACTIVE ||
        ip->closing)
    {
        bad_xefun_arg(2, sp);
    }
    prompt = query_prompt(sp->u.ob);
    free_object_svalue(sp);
    sp--;
    if (sp->type == T_STRING || sp->type == T_CLOSURE) {
        if (sp->type == T_STRING &&
            sp->x.string_type == STRING_VOLATILE)
        {
            char *str = make_shared_string(sp->u.string);

            if (!str) {
                inter_sp = sp;
                error("Out of memory\n");
            } else {
                sp->u.string = str;
                sp->x.string_type = STRING_SHARED;
            }
        }
        sp[1] = *prompt;
        *prompt = *sp;
        *sp = sp[1];
    } else if (sp->type == T_NUMBER &&
                (sp->u.number == 0 || sp->u.number == -1) )
    {
        assign_svalue(sp, prompt);
    } else {
        bad_xefun_arg(1, sp);
        return sp; /* flow control hint */
    }
    return sp;
}


/* This is here to be able to use the inline functions. */

struct svalue *f_transpose_array(sp)
    struct svalue *sp;
{
    struct vector *v;
    struct vector *w;
    int a, b, i, j;
    short no_copy;
    struct svalue *x, *y, *z;
    int o;

    TYPE_TEST1(sp, T_POINTER, F_TRANSPOSE_ARRAY)
    v = sp->u.vec;
    if ( !(a = VEC_SIZE(v)) )
        return sp;
    TYPE_TEST1(v->item, T_POINTER, F_TRANSPOSE_ARRAY)

    if ( !(b = VEC_SIZE(v->item->u.vec)) ) {
        (sp->u.vec = v->item->u.vec)->ref++;
        free_vector(v);
        return sp;
    }
    no_copy = v->ref == 1 ? 1 : 0;
    w = allocate_uninit_array(b);
    for (j = b, x = w->item; --j >= 0; x++) {
        x->type = T_POINTER;
        x->u.vec = allocate_array(a);
    }
    o = (char *)(((struct vector *)0)->item) - (char *)((struct vector *)0);
    for (i = a, y = v->item; --i >= 0; o += sizeof(struct svalue), y++) {
        x = w->item;
        if (y->type != T_POINTER)
            break;
        z = y->u.vec->item;
        if (VEC_SIZE(y->u.vec) < (size_t)b)
            if ( !(b = VEC_SIZE(y->u.vec)) )
                break;
        if (y->u.vec->ref == no_copy) {
            j = b;
            do {
                transfer_svalue_no_free_spc(
                  (struct svalue *)((char*)x->u.vec+o),
                  z,
                  sp, inter_pc
                );
                x++;
                z++;
            } while (--j > 0);
            free_empty_vector(y->u.vec);
            y->type = T_INVALID;
        } else {
            j = b;
            do {
                assign_svalue_no_free(
                  (struct svalue *)((char*)x->u.vec+o),
                  z
                );
                x++;
                z++;
            } while (--j > 0);
        }
    }
    free_vector(sp->u.vec);
    sp->u.vec = w;
    return sp;
}

struct svalue *f_trace(sp)
    struct svalue *sp;
{
    int ot;
    struct interactive *ip;

    TYPE_TEST1(sp, T_NUMBER, F_TRACE)
    ot = -1;
    if (command_giver &&
        NULL != (ip = O_GET_INTERACTIVE(command_giver)) &&
        ip->sent.type == SENT_INTERACTIVE)
    {
        struct svalue *arg;
        assign_eval_cost();
        inter_sp = _push_constant_string("trace", sp);
        arg = apply_master_ob(STR_PLAYER_LEVEL, 1);
        if (!arg) {
            if (out_of_memory)
                error("Out of memory\n");
        } else {
            if (arg->type != T_NUMBER || arg->u.number != 0) {
                ot = ip->trace_level;
                trace_level |=
                  ip->trace_level = sp->u.number;
            }
        }
    }
    sp->u.number = ot;
    (void)SET_TRACE_EXEC;
    return sp;
}

struct svalue *f_traceprefix(sp)
    struct svalue *sp;
{
    char *old;
    struct interactive *ip;

    if (sp->type != T_STRING && sp->type != T_NUMBER)
        bad_xefun_arg(1, sp);
    old = 0;
    if (command_giver &&
        NULL != (ip = O_GET_INTERACTIVE(command_giver)) &&
        ip->sent.type == SENT_INTERACTIVE)
    {
        struct svalue *arg;
        inter_sp = _push_constant_string("trace", sp);
        assign_eval_cost();
        arg = apply_master_ob(STR_PLAYER_LEVEL,1);
        if (!arg) {
            if (out_of_memory)
                error("Out of memory\n");
        } else {
            if (arg && (arg->type != T_NUMBER || arg->u.number)) {
                old = ip->trace_prefix;
                if (sp->type == T_STRING) {
                    ip->trace_prefix = make_shared_string(sp->u.string);
                } else
                    ip->trace_prefix = 0;
            }
        }
    }
    free_svalue(sp);
    if (old) {
        sp->type = T_STRING;
        sp->x.string_type = STRING_SHARED;
        sp->u.string = old;
    } else {
        put_number(0);
    }
    return sp;
}

#ifdef F_DEBUG_INFO
/* provide debugging information that can't be obtained by regular efuns */
struct svalue *f_debug_info(sp, num_arg)
    struct svalue *sp;
    int num_arg;
{
    int i;
    struct svalue *arg;
    struct svalue res;
    struct object *ob;

    arg = sp-num_arg+1;
    inter_sp = sp;
    TYPE_TEST1(arg, T_NUMBER, F_DEBUG_INFO)
    assign_svalue_no_free(&res,&const0);
    ASSIGN_EVAL_COST
    switch ( arg[0].u.number ) {
/* Give information about an object, deciphering it's flags, nameing it's
   position in the list of all objects, total light and all the stuff
   that is of interest with respect to look_for_objects_to_swap .
   This one was used to hunt down the last reset lag bug.                */
      case 0:
      {
        int flags;
        struct object *prev, *obj2;

        if (num_arg != 2)
            ERROR("bad number of arguments to debug_info\n")
        TYPE_TEST2(arg+1, T_OBJECT, F_DEBUG_INFO)
        ob = arg[1].u.ob;
        flags = ob->flags;
        add_message("O_HEART_BEAT      : %s\n",
          flags&O_HEART_BEAT      ?"TRUE":"FALSE");
        add_message("O_IS_WIZARD       : %s\n",
          flags&O_IS_WIZARD       ?"TRUE":"FALSE");
        add_message("O_ENABLE_COMMANDS : %s\n",
          flags&O_ENABLE_COMMANDS ?"TRUE":"FALSE");
        add_message("O_CLONE           : %s\n",
          flags&O_CLONE           ?"TRUE":"FALSE");
        add_message("O_DESTRUCTED      : %s\n",
          flags&O_DESTRUCTED      ?"TRUE":"FALSE");
        add_message("O_SWAPPED         : %s\n",
          flags&O_SWAPPED          ?"TRUE":"FALSE");
        add_message("O_ONCE_INTERACTIVE: %s\n",
          flags&O_ONCE_INTERACTIVE?"TRUE":"FALSE");
        add_message("O_APPROVED        : %s\n",
          flags&O_APPROVED        ?"TRUE":"FALSE");
        add_message("O_RESET_STATE     : %s\n",
          flags&O_RESET_STATE     ?"TRUE":"FALSE");
        add_message("O_WILL_CLEAN_UP   : %s\n",
          flags&O_WILL_CLEAN_UP   ?"TRUE":"FALSE");
        add_message("total light : %d\n", ob->total_light);
        add_message("next_reset  : %d\n", ob->next_reset);
        add_message("time_of_ref : %d\n", ob->time_of_ref);
        add_message("ref         : %ld\n", ob->ref);
#ifdef DEBUG
        add_message("extra_ref   : %ld\n", ob->extra_ref);
#endif
        add_message("swap_num    : %ld\n", O_SWAP_NUM(ob));
        add_message("name        : '%s'\n", ob->name);
        for (obj2 = ob->next_all; obj2 && obj2->flags & O_DESTRUCTED; )
            obj2 = obj2->next_all;
        if (obj2)
            add_message("next_all    : OBJ(%s)\n",
              obj2->next_all?obj2->name:"NULL");
        for (prev=0,obj2=obj_list,i=0; obj2; obj2=obj2->next_all) {
            if ( !(obj2->flags & O_DESTRUCTED) )
                prev = obj2, i++;
            if (obj2->next_all == ob) {
                if (prev) {
                    add_message(
                        "Previous object in object list: OBJ(%s)\n",
                        prev->name);
                    add_message("position in object list:%d\n",i);
                } else
                    add_message(
                        "This object is the head of the object list.\n");
                break;
            }
        }
        break;
      }
      case 1:
      {
/* Give information about an object's program with regard to memory usage.
   This is meant to point out where memory can be saved in program structs. */
        struct program *pg;

        if (num_arg != 2)
                ERROR("bad number of arguments to debug_info\n")
        TYPE_TEST2(arg+1, T_OBJECT, F_DEBUG_INFO)
        if (O_PROG_SWAPPED(sp->u.ob) && load_ob_from_swap(sp->u.ob) < 0)
            ERROR("Out of memory\n");
        pg = sp->u.ob->prog;
        add_message("program ref's %3ld\n",        pg->ref);
        add_message("Name: '%s'\n",                pg->name);
        add_message("program size    %6ld\n"
          ,(long)(PROGRAM_END(*pg) - pg->program));
        add_message("num func's:  %3d (%4d)\n", pg->num_functions
          ,pg->num_functions * sizeof(uint32) +
           pg->num_function_names * sizeof(short));
        add_message("num vars:    %3d (%4d)\n", pg->num_variables
          ,pg->num_variables * sizeof(struct variable));
        add_message("num strings: %3d (%4d)\n", pg->num_strings
          ,pg->num_strings   * sizeof(char *));
        add_message("num inherits %3d (%4d)\n", pg->num_inherited
          ,pg->num_inherited * sizeof(struct inherit));
        add_message("total size      %6ld\n"
          ,pg->total_size);
        break;
      }
      case 2:
      {
        ob = obj_list;
        i = 0;
        if (num_arg > 1) {
            if (num_arg > 2)
                ERROR("bad number of arguments to debug_info\n")
            if (sp->type == T_NUMBER) {
                i = sp->u.number;
            } else {
                TYPE_TEST2(sp, T_OBJECT, F_DEBUG_INFO)
                ob = sp->u.ob;
                i = 1;
            }
        }
        while (ob && (ob->flags & O_DESTRUCTED || --i >= 0)) ob = ob->next_all;
        if (ob) {
            res.type = T_OBJECT;
            res.u.ob = ob;
            add_ref(ob, "debug_info");
        }
        break;
      }
      case 3:
      {
#if defined(MALLOC_smalloc) || defined(MALLOC_malloc)
        if (num_arg != 1)
                ERROR("bad number of arguments to debug_info\n")
        dump_malloc_data();
#endif
        break;
      }
      case 4:
      {
        if (num_arg != 2)
                ERROR("bad number of arguments to debug_info\n")
        if (sp->type == T_NUMBER && sp->u.number == 0) {
            sp->u.string = "";
        } else {
            TYPE_TEST2(arg+1, T_STRING, F_DEBUG_INFO)
        }
        res.u.number = status_parse(sp->u.string);
        break;
      }
      default: bad_xefun_vararg(1, sp);
    }
    pop_n_elems(num_arg);
    sp++;
    *sp=res;
    return sp;
}
#endif

void call_lambda(lsvp, num_arg)
    struct svalue *lsvp;
    int num_arg;
{
    struct svalue *sp;
    struct svalue lambda_protect;
      /* To prevent that the lambda is deleted while it is executed, an
       * assignment of lsvp to this structure for the duration of the
       * execution will give it the lifegiving extra refcount.
       * This is especially necessary for the driver hooks who have
       * only 1 refcount.
       * TODO: On runtime errors, this protective refcount is leaked.
       */
    struct lambda *l = lsvp->u.lambda;

    sp = inter_sp;
    push_control_stack(sp, inter_pc, inter_fp);

    csp->ob = current_object;
    csp->prev_ob = previous_ob;
    csp->num_local_variables = num_arg;
    previous_ob = current_object;

    switch(lsvp->x.closure_type) {
      case CLOSURE_LFUN:
      {
        uint32 flags;
        unsigned char *funstart;

        if ( (current_object = l->ob)->flags & (O_DESTRUCTED|O_SWAPPED)) {
            int d;

            if (0 != (d = current_object->flags & O_DESTRUCTED) ||
                load_ob_from_swap(current_object) < 0)
            {
                /* inter_sp == sp */
                previous_ob = csp->prev_ob;
                current_object = csp->ob;
                pop_control_stack();
                error(d ?
                  "Object the closure was bound to has been destructed\n" :
                  "Out of memory\n"
                );
                return; /* flow control hint */
            }
        }
        current_prog = current_object->prog;
        /* inter_sp == sp */
        flags = setup_new_frame(l->function.index);
        funstart = current_prog->program + (flags & FUNSTART_MASK);
        csp->funstart = funstart;
        assign_svalue_no_free(&lambda_protect, lsvp);
        eval_instruction(funstart + 2, inter_sp);
        free_svalue(&lambda_protect);
        /* The result is on the stack (inter_sp) */
        return;
      }
      case CLOSURE_ALIEN_LFUN:
      {
        uint32 flags;
        unsigned char *funstart;

        if ( ((current_object = l->ob)->flags|l->function.alien.ob->flags) &
                (O_DESTRUCTED|O_SWAPPED))
        {
            int d;

            if (0 != (d =
                 (current_object->flags|l->function.alien.ob->flags) &
                  O_DESTRUCTED) ||
                (current_object->flags & O_SWAPPED &&
                load_ob_from_swap(current_object) < 0) ||
                (l->function.alien.ob->flags & O_SWAPPED &&
                load_ob_from_swap(l->function.alien.ob) < 0) )
            {
                /* inter_sp == sp */
                previous_ob = csp->prev_ob;
                current_object = csp->ob;
                pop_control_stack();
                error(d ?
                  "Object the closure was bound to has been destructed\n" :
                  "Out of memory\n"
                );
                return; /* flow control hint */
            }
        }
        push_control_stack(sp, 0, inter_fp);
        csp->ob = current_object;
        csp->prev_ob = previous_ob;
        csp->num_local_variables = num_arg;
        previous_ob = current_object;
        current_object = l->function.alien.ob;
        current_prog = current_object->prog;
        /* inter_sp == sp */
        flags = setup_new_frame(l->function.alien.index);
        funstart = current_prog->program + (flags & FUNSTART_MASK);
        csp->funstart = funstart;
        assign_svalue_no_free(&lambda_protect, lsvp);
        eval_instruction(funstart + 2, inter_sp);
        free_svalue(&lambda_protect);
        /* The result is on the stack (inter_sp) */
        current_object = csp->ob;
        previous_ob = csp->prev_ob;
        pop_control_stack();
        return;
      }
      case CLOSURE_IDENTIFIER:
      {
        int i;

        previous_ob = csp->prev_ob;
        current_object = csp->ob;
        pop_control_stack();
        if (num_arg) error("Argument to variable\n");
        if ((l->ob->flags & O_DESTRUCTED)
         || (   (l->ob->flags & O_SWAPPED)
             && load_ob_from_swap(l->ob) < 0)
           )
            error("Object the closure was bound to has been destructed\n");
        if ( (i = l->function.index) < 0)
            error("Variable not inherited\n");
        assign_svalue_no_free(++sp, &l->ob->variables[i]);
        inter_sp = sp;
        return;
      }
      case CLOSURE_BOUND_LAMBDA:
      {
        struct lambda *l2;

        l2 = l->function.lambda;
        l2->ob = l->ob;
        l = l2;
      }
      /* fall through */
      case CLOSURE_LAMBDA:
      {
        unsigned char *funstart;

        if ( (current_object = l->ob)->flags & (O_DESTRUCTED|O_SWAPPED)) {
            int d;

            if (0 != (d = current_object->flags & O_DESTRUCTED) ||
                load_ob_from_swap(current_object) < 0)
            {
                /* inter_sp == sp */
                previous_ob = csp->prev_ob;
                current_object = csp->ob;
                pop_control_stack();
                error(d ?
                  "Object the closure was bound to has been destructed\n" :
                  "Out of memory\n"
                );
                return; /* flow control hint */
            }
        }
        current_prog = current_object->prog;
        variable_index_offset = 0;
        function_index_offset = 0;
        funstart = l->function.code + 1;
        csp->funstart = funstart;
        sp = setup_new_frame2(funstart, sp);
        current_variables = current_object->variables;
        current_strings = current_prog->strings;
        assign_svalue_no_free(&lambda_protect, lsvp);
        eval_instruction(funstart + 2, sp);
        free_svalue(&lambda_protect);
        /* The result is on the stack (inter_sp) */
        return;
      }
      default:
      {
        int i;

        if ( (current_object = lsvp->u.ob)->flags & O_DESTRUCTED) {
            /* inter_sp == sp */
            previous_ob = csp->prev_ob;
            current_object = csp->ob;
            pop_control_stack();
            error("Object the closure was bound to has been destructed\n");
            return; /* flow control hint */
        }
        i = lsvp->x.closure_type;
        if (i < CLOSURE_SIMUL_EFUN) {
            if ((i -= CLOSURE_EFUN) >= 0 ||
                instrs[i -= CLOSURE_OPERATOR-CLOSURE_EFUN].min_arg)
            {
                char *p, code[5];
                int min, max, def;

                min = instrs[i].min_arg;
                max = instrs[i].max_arg;
                p = code;
                if (num_arg < min) {
                    int f;

                    if (num_arg == min-1 &&
                        0 != (def = instrs[i].Default) && def != -1)
                    {
                        *p++ = def - F_OFFSET;
                        max--;
                        min--;
                    } else if ( (f = proxy_efun(i, num_arg)) < 0 ||
                                (i = f,MY_FALSE) )
                    {
                        csp->extern_call = 1;
                        inter_pc = csp->funstart = (char *)(p_int)i;
                        error("Too few arguments to %s\n", instrs[i].name);
                    }
                } else if (num_arg > max && max != -1) {
                    csp->extern_call = 1;
                    inter_pc = csp->funstart = (char *)(p_int)i;
                    error("Too many arguments to %s\n", instrs[i].name);
                }
                if (i > 0xff)
                    *p++ = i >> F_ESCAPE_BITS;
                *p++ = i;
                if (min != max)
                    *p++ = num_arg;
                if ( instrs[i].ret_type == TYPE_VOID )
                    *p++ = F_RETURN0-F_OFFSET;
                else
                    *p++ = F_RETURN-F_OFFSET;
                /* Note: TubMud suggested marking efun closures by
                 * clearing csp->prog here, and adding a corresponding
                 * test in dump_trace(). However, csp->prog must remain
                 * valid else the driver will think on F_RETURN that
                 * it encountered the bottom of the cs stack and
                 * forget to update current_strings. At least.
                 * TODO: If more places crop up where a ob->prog of NULL
                 * TODO:: is a problem (like in dump_trace()), we should
                 * TODO:: invent a fake struct prog to keep things easy.
                 * TODO:: See also p-990203-1.
                 */
                csp->funstart = code - 2;
                csp->num_local_variables = 0;
                inter_fp = sp - num_arg + 1;
                assign_svalue_no_free(&lambda_protect, lsvp);
                eval_instruction(code, sp);
                free_svalue(&lambda_protect);
                /* The result is on the stack (inter_sp) */
                return;
            } else {
                /* It is an operator: fall through to uncallable closure type */
                break;
            }
        } else {
            /* simul_efun */
            struct object *ob;

            inter_pc = csp->funstart = SIMUL_EFUN_FUNSTART;
            if ( !(ob = simul_efun_object) ) {
                /* inter_sp == sp */
                if ( !(ob = get_simul_efun_object()) ) {
                    csp->extern_call = 1;
                    error("Couldn't load simul_efun object\n");
                }
            }
            call_simul_efun(i - CLOSURE_SIMUL_EFUN, ob, num_arg);
            previous_ob = csp->prev_ob;
            current_object = csp->ob;
            pop_control_stack();
        }
        /* The result is on the stack (inter_sp) */
        return;
      }
      case CLOSURE_UNBOUND_LAMBDA:
      case CLOSURE_PRELIMINARY:
        /* inter_sp == sp */
        /* no valid current_object ==> pop the control stack */
        previous_ob = csp->prev_ob;
        current_object = csp->ob;
        pop_control_stack();
    }
    error("Uncallable closure\n");
    return; /* flow control hint */
}

static void call_simul_efun(code, ob, num_arg)
    int code;
    struct object *ob;
    int num_arg;
{
    char *function_name;

    /*
     * Send the arguments to the function.
     */
    function_name = simul_efunp[code].name;
    if (apply_low(function_name, ob, num_arg, MY_FALSE) == 0) {
        /* Function not found */
        if (simul_efun_vector) {
            int i;
            struct svalue *v;

            i = VEC_SIZE(simul_efun_vector);
            for(v = simul_efun_vector->item+1; ; v++) {
                if (--i <= 0 || v->type != T_STRING) {
                    error("Calling a vanished simul_efun\n");
                    return;
                }
                if ( !(ob = get_object(v->u.string)) )
                    continue;
                if (apply_low(function_name, ob, num_arg, MY_FALSE))
                    return;
            }
            return;
        }
        error("Calling a vanished simul_efun\n");
        return;
    }
    /*
     * The result of the function call is on the stack.
     */
}

void free_interpreter_temporaries() {
    free_protector_svalue(&last_indexing_protector);
    last_indexing_protector.type = T_NUMBER;
    free_svalue(&indexing_quickfix);
    indexing_quickfix.type = T_NUMBER;
    free_svalue(&apply_return_value);
    apply_return_value.type = T_NUMBER;
}

#ifdef MALLOC_smalloc
void clear_interpreter_refs() {
#ifdef TRACE_CODE
{
    int i;

    for (i = TOTAL_TRACE_LENGTH; --i >= 0; ) {
        struct object *ob;

        if (NULL != (ob = previous_objects[i]) && ob->flags & O_DESTRUCTED && ob->ref)
        {
            ob->ref = 0;
            ob->prog->ref = 0;
            clear_inherit_ref(ob->prog);
        }
    }
}
#endif
}

void count_interpreter_refs() {
    int i;

    for (i = CACHE_SIZE; --i>= 0; ) {
        if (!cache_progp[i])
            note_malloced_block_ref(cache_name[i]);
    }
#ifdef TRACE_CODE
    for (i = TOTAL_TRACE_LENGTH; --i >= 0; ) {
        struct object *ob;

        if ( NULL != (ob = previous_objects[i]) ) {
            if (ob->flags & O_DESTRUCTED) {
                previous_objects[i] = 0;
                previous_instruction[i] = 0;
                reference_destructed_object(ob);
            } else {
                ob->ref++;
            }
        }
    }
#endif
}
#endif /* MALLOC_smalloc */

void invalidate_apply_low_cache() {
    int i;

    i = sizeof cache_id / sizeof cache_id[0];
    do {
        cache_id[--i] = 0;
    } while (i);
}

/* add a large amount of eval_cost */
void add_eval_cost(num)
    int num;
{
    if (eval_cost < 0) {
        eval_cost += num;
        if (eval_cost > 0)
            eval_cost = 0;
        return;
    }
    if (eval_cost >= MIN_TRACE_COST && eval_cost < MAX_TRACE_COST)
    {
        eval_cost += num;
        if (eval_cost - MAX_TRACE_COST >= 0)
            eval_cost = MAX_TRACE_COST;
        return;
    }
}

struct svalue *f_to_object(sp)
    struct svalue *sp;
{
    int n;
    struct object *o;

    switch(sp->type) {
      case T_NUMBER:
        if (!sp->u.number)
            return sp;
      default:
        bad_xefun_arg(1, sp);
      case T_CLOSURE:
        n = sp->x.closure_type;
        o = sp->u.ob;
        if (CLOSURE_MALLOCED(n)) {
            if (n == CLOSURE_UNBOUND_LAMBDA)
                bad_xefun_arg(1, sp);
            o = ((struct lambda *)(o))->ob;
        }
        if (o->flags & O_DESTRUCTED)
            o = 0;
        free_closure(sp);
        break;
      case T_OBJECT:
        return sp;
      case T_STRING:
        o = find_object(sp->u.string);
        free_svalue(sp);
        break;
    }
    if (o)
        put_object(o);
    else
        put_number(0);
    return sp;
}

/*---------------------------------------------------------------------------*/
void
init_interpret (void)

/* Initialize the interpreter data structures, especially the apply cache.
 */

{
  /* The cache is inited to hold entries for 'functions' in a non-existing
   * program (id 0). The first real apply calls will thus see a (virtual)
   * collision with 'older' cache entries.
   */
  memset(cache_id, 0, sizeof cache_id);
  memset(cache_progp, 1, sizeof cache_progp);
}
