#ifndef INTERPRET_H
#define INTERPRET_H

#include "port.h"
#include "config.h"

extern void init_interpret(void);

union u {
    char *string;
    p_int number;
    struct object *ob;
    struct vector *vec;
    struct mapping *map;
    struct lambda *lambda;
    p_int mantissa;
    struct svalue *lvalue;
    struct protected_lvalue *protected_lvalue;
    struct protected_char_lvalue *protected_char_lvalue;
    struct protected_range_lvalue *protected_range_lvalue;
    void (*error_handler) PROT((struct svalue *));
    struct const_list_svalue *const_list;
};

/*
 * The value stack element.
 * If it is a string, then the way that the string has been allocated differ,
 * wich will affect how it should be freed.
 */
struct svalue {
    ph_int type;
    union {
        ph_int string_type;
        ph_int exponent;
	ph_int closure_type;
	ph_int quotes;
	ph_int num_arg; /* for call_out.c */
	ph_int generic;
    } x;
    union u u;
};

#define SVALUE_FULLTYPE(svp) ((p_int *)(svp))

#define T_INVALID	0x0
#define T_LVALUE	0x1
#define T_NUMBER	0x2
#define T_STRING	0x3
#define T_POINTER	0x4
#define T_OBJECT	0x5
#define T_MAPPING	0x6
#define T_FLOAT 	0x7
#define T_CLOSURE	0x8
#define T_SYMBOL	0x9
#define T_QUOTED_ARRAY	0xa

#define T_CHAR_LVALUE   	0xb
#define T_STRING_RANGE_LVALUE	0xc
#define T_POINTER_RANGE_LVALUE	0xd
#define T_PROTECTED_CHAR_LVALUE 	 0x0e
#define T_PROTECTED_STRING_RANGE_LVALUE	 0x0f
#define T_PROTECTED_POINTER_RANGE_LVALUE 0x10

#define T_PROTECTED_LVALUE	0x11
#define T_PROTECTOR_MAPPING	0x12

#define T_ERROR_HANDLER 	0x13

#define T_MOD_SWAPPED		0x80

#define STRING_MALLOC	0	/* Allocated by malloc() */
#define STRING_VOLATILE 1	/* Do not has to be freed at all */
#define STRING_CONSTANT	1	/* Old, but misleading name */
#define STRING_SHARED	2	/* Allocated by the shared string library */

#define CLOSURE_OPERATOR	(-0x1800)
#define CLOSURE_EFUN		(-0x1000)
#define CLOSURE_SIMUL_EFUN	(-0x0800)
#define CLOSURE_MALLOCED(c) ((c) >= 0)
#define CLOSURE_LFUN		0
#define CLOSURE_ALIEN_LFUN	1
#define CLOSURE_IS_LFUN(c)	(((c)&~1) == 0)
#define CLOSURE_IDENTIFIER	2
#define CLOSURE_PRELIMINARY	3
#define CLOSURE_REFERENCES_CODE(c) ((c) > 3)
#define CLOSURE_BOUND_LAMBDA	4
#define CLOSURE_HAS_CODE(c) ((c) > 4)
#define CLOSURE_LAMBDA		5
#define CLOSURE_UNBOUND_LAMBDA	6
#define CLOSURE_CALLABLE(c) ((c) >= CLOSURE_EFUN && (c) <= CLOSURE_LAMBDA)

#define CLOSURE_IDENTIFIER_OFFS 0xe800
#define CLOSURE_OPERATOR_OFFS	(CLOSURE_OPERATOR & 0xffff)
#define CLOSURE_EFUN_OFFS	(CLOSURE_EFUN & 0xffff)
#define CLOSURE_SIMUL_EFUN_OFFS	(CLOSURE_SIMUL_EFUN & 0xffff)

#ifdef MALLOC_smalloc
#include "smalloc.h"
#endif

struct vector {
#ifndef MALLOC_smalloc
    p_int size;
#endif
    p_int ref;
#ifdef DEBUG
    p_int extra_ref;
#endif
    struct wiz_list *user;	/* Save who made the vector */
    struct svalue item[1];
};

#define null_vector null_vector_aggregate.v

extern struct null_vector_aggregate_struct  {
#ifdef MALLOC_smalloc
    p_int s[SMALLOC_OVERHEAD];
#endif
    struct vector v;
} null_vector_aggregate;

#ifdef DEBUG
#define VEC_DEBUGREF(ref) ref,
#else
#define VEC_DEBUGREF(ref)
#endif

#ifdef MALLOC_smalloc
#define VEC_SIZE(v) (\
  ( malloced_size(v) - \
    ( SMALLOC_OVERHEAD + \
      ( sizeof(struct vector) - sizeof(struct svalue) ) / SIZEOF_P_INT \
    ) \
  ) / (sizeof(struct svalue)/SIZEOF_P_INT) \
)
#define INIT_VEC_TYPE p_uint s[SMALLOC_OVERHEAD]; struct vector v
#define VEC_INIT(size, ref, type) \
	{ ( size * sizeof(struct svalue) + \
	    sizeof(struct vector) - \
	    sizeof(struct svalue) \
	  ) / SIZEOF_P_INT + SMALLOC_OVERHEAD }, \
	{ ref, VEC_DEBUGREF(ref) (struct wiz_list *)NULL, { { type } } }
#else /* !MALLOC_smalloc */
#define VEC_SIZE(v) ((v)->size)
#define INIT_VEC_TYPE struct vector v
#define VEC_INIT(size, ref, type) \
	{ size, ref, VEC_DEBUGREF(ref) (struct wiz_list *)NULL, { { type } } }
#endif

struct mapping {
    p_int ref;
    struct hash_mapping *hash;
    struct condensed_mapping *condensed;
    struct wiz_list *user;
    int num_values; /* number of values per key */
};

struct hash_mapping {
    p_int mask;
    p_int used;
    p_int condensed_deleted;
    p_int ref;
    struct map_chain *deleted;
    struct mapping *next_dirty;
    struct map_chain *chains[1];
};

struct condensed_mapping {
    /* values for misc keys go above */
#define CM_MISC(m) ((struct svalue *)(m))
    /* struct svalue misc[0]; */
    p_int misc_size;
    p_int string_size;
#define CM_STRING(m) ((char **)((m)+1))
    /* char *string[0]; */
    /* values for string keys go below */
};

struct lambda {
    p_int ref;
    struct object *ob;
    union {
	unsigned short index;	/* lfun closures */
	char code[1];	/* lambda closures */
	struct lambda *lambda;
	struct {
	    struct object *ob;
	    unsigned short index;
	} alien;	/* alien_lfun closures */
    } function;
};

struct map_chain {
    struct map_chain *next;
    struct svalue key;
    struct svalue data[1]; /* data == &key+1 is used in some places */
};

#define MAP_CHAIN_SIZE(n) (\
	(sizeof(struct map_chain) - sizeof(struct svalue) ) +\
		sizeof(struct svalue)*(n) )

#if !defined(MALLOC_smalloc) || !defined(SMALLOC_TRACE) || !defined(__STDC__)
#define ALLOC_VECTOR(nelem, file, line) \
    (struct vector *)xalloc(sizeof (struct vector) + \
			    sizeof(struct svalue) * (nelem - 1))
#else
#define ALLOC_VECTOR(nelem, file, line) \
    (struct vector *)smalloc(sizeof (struct vector) + \
			    sizeof(struct svalue) * (nelem - 1), file, line)
#endif /* SMALLOC_TRACE */

void free_vector PROT((struct vector *)), free_all_values();

/*
 * Control stack element.
 * 'prog' is usually same as 'ob->prog' (current_object), except when
 * when the current function is defined by inheritance.
 * The pointer, csp, will point to the values that will be used at return.
 */
struct control_stack {
    struct object *ob;		/* Current object */
    struct object *prev_ob;	/* Save previous object */
    struct program *prog;	/* Current program */
    char *pc;
    struct svalue *fp;
    char *funstart;
    int num_local_variables;	/* Local + arguments */
    int function_index_offset;	/* Used when executing functions in inherited
				   programs */
    struct svalue *current_variables;	/* Same */
    short extern_call;		/* Flag if evaluator should return */
    short dummy;
    char **break_sp;
    struct object *pretend_to_be;
};

#include <setjmp.h>

struct con_struct { jmp_buf text; };

extern struct error_recovery_info {
    struct error_recovery_info *last;
    int type;
    struct con_struct con;
} *error_recovery_pointer;

#define ERROR_RECOVERY_NONE	0
#define ERROR_RECOVERY_BACKEND	1
#define ERROR_RECOVERY_APPLY	2
#define ERROR_RECOVERY_CATCH	3

#define LI_MAXOFFSET	0x3c
#define LI_INCLUDE	0x3d
#define LI_INCLUDE_END	0x3e
#define LI_L_RELOCATED	0x3f

#define LI_RELOCATED	0xc0
#define LI_SMALL_REL	0x20

#define LI_MAXEMPTY	0x20

#define MAX_SHIFT ((sizeof(p_int) << 3) - 1)

#if defined(atarist) || (defined(AMIGA) && defined(_DCC))
/* Faster routines, using inline and knowlegde about double format.
 * the exponent isn't actually in 'exponent', but that doesn't really matter
 * as long as the accesses are consistent.
 * the DICE compiler for the amiga lacks the ldexp() and frexp() functions,
 * therefore these functions here are more than a speedup there...
 */
#define FLOAT_FORMAT_1
/* STORE_DOUBLE doesn't do any rounding, but truncates off the least
 * significant bits of the mantissa that won't fit together with the exponent
 * into 48 bits. To compensate for this, we initialise the unknown bits of
 * the mantissa with 0x7fff in READ_DOUBLE . This keeps the maximum precision
 * loss of a store/read pair to the same value as rounding, while being faster
 * and being more stable.
 */
static
#ifdef atarist
inline
#endif
double READ_DOUBLE(struct svalue *svalue_pnt)
{	double tmp;
	(*(long*)&tmp) = svalue_pnt->u.mantissa;
	((short*)&tmp)[2] = svalue_pnt->x.exponent;
	((short*)&tmp)[3] = 0x7fff;
	return tmp;
}

#define SPLIT_DOUBLE(double_value, int_pnt) (\
(*(int_pnt) = ((short*)&double_value)[2]),\
*((long*)&double_value)\
)

#define STORE_DOUBLE_USED
#define STORE_DOUBLE(dest, double_value) (\
(dest)->u.mantissa = *((long*)&double_value),\
(dest)->x.exponent = ((short*)&double_value)[2]\
)
#endif

#ifndef STORE_DOUBLE
#define FLOAT_FORMAT_0
#define READ_DOUBLE(svalue_pnt) ( ldexp( (double)((svalue_pnt)->u.mantissa) , \
		(svalue_pnt)->x.exponent-31 ) )

/* if your machine doesn't use the exponent to designate powers of two,
   the use of ldexp in SPLIT_DOUBLE won't work; you'll have to mulitply
   with 32768. in this case */
#define SPLIT_DOUBLE(double_value, int_pnt) \
( (long)ldexp( frexp( (double_value), (int_pnt) ), 31) )

#define STORE_DOUBLE_USED int __store_double_int_;
#define STORE_DOUBLE(dest, double_value) (\
((dest)->u.mantissa = SPLIT_DOUBLE((double_value), &__store_double_int_)),\
 (dest)->x.exponent = __store_double_int_\
)
#endif

#endif /* INTERPRET_H */
