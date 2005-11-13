#ifndef __DATATYPES_H__
#define __DATATYPES_H__ 1

/*---------------------------------------------------------------------------
 * Datatypes used by the virtual machine.
 *
 *---------------------------------------------------------------------------
 * The types are used directly in a lot of places so it makes sense to
 * put them into just one include file.
 *
 * However, if you intend to do more (like calling functions, instantiating
 * or else), you have to include to appropriate module include, too.
 *---------------------------------------------------------------------------
 */

#include "driver.h"

/* --- Types --- */

union u {
    char *string;
    p_int number;
      /* Lots of code assumes that number is big enough to hold a complete
       * pointer, that's why it is of type 'p_int'.
       */
    struct object *ob;
    struct vector *vec;
    struct mapping *map;
    struct lambda *lambda;
    p_int mantissa;
    struct svalue *lvalue;
    struct protected_lvalue *protected_lvalue;
    struct protected_char_lvalue *protected_char_lvalue;
    struct protected_range_lvalue *protected_range_lvalue;
    void (*error_handler) (struct svalue *);
      /* lvalues only: function to be executed on free_svalue(),
       * argument is this very svalue. */
    struct const_list_svalue *const_list;
};

/*
 * The value stack element.
 * If it is a string, then the way that the string has been allocated differ,
 * wich will affect how it should be freed.
 */
struct svalue {
    ph_int type;  /* Primary type information */
    union {       /* Secondary type information */
        ph_int string_type;
        ph_int exponent;
        ph_int closure_type;
        ph_int quotes;
        ph_int num_arg; /* used by call_out.c to for vararg callouts */
        ph_int generic;
          /* used for types without secondary information,
           * usually u.number << 1
           */
    } x;
    union u u;
};

#define SVALUE_FULLTYPE(svp) ((p_int *)(svp))
/* TODO: Sanity test: sizeof struct { ph_int, ph_int } <= sizeof p_int */

#define T_INVALID        0x0
#define T_LVALUE        0x1
#define T_NUMBER        0x2
#define T_STRING        0x3
#define T_POINTER        0x4
#define T_OBJECT        0x5
#define T_MAPPING        0x6
#define T_FLOAT         0x7
#define T_CLOSURE        0x8
#define T_SYMBOL        0x9
#define T_QUOTED_ARRAY        0xa

#define T_CHAR_LVALUE           0xb
#define T_STRING_RANGE_LVALUE        0xc
#define T_POINTER_RANGE_LVALUE        0xd
#define T_PROTECTED_CHAR_LVALUE          0x0e
#define T_PROTECTED_STRING_RANGE_LVALUE         0x0f
#define T_PROTECTED_POINTER_RANGE_LVALUE 0x10

#define T_PROTECTED_LVALUE        0x11
#define T_PROTECTOR_MAPPING        0x12

#define T_ERROR_HANDLER         0x13

#define T_MOD_SWAPPED                0x80

#define STRING_MALLOC        0        /* Allocated by malloc() */
#define STRING_VOLATILE 1        /* Do not has to be freed at all */
#define STRING_CONSTANT        1        /* Old, but misleading name */
#define STRING_SHARED        2        /* Allocated by the shared string library */

#define CLOSURE_OPERATOR        (-0x1800)
#define CLOSURE_EFUN                (-0x1000)
#define CLOSURE_SIMUL_EFUN        (-0x0800)
#define CLOSURE_MALLOCED(c) ((c) >= 0)
#define CLOSURE_LFUN                0
#define CLOSURE_ALIEN_LFUN        1
#define CLOSURE_IS_LFUN(c)        (((c)&~1) == 0)
#define CLOSURE_IDENTIFIER        2
#define CLOSURE_PRELIMINARY        3
#define CLOSURE_REFERENCES_CODE(c) ((c) > 3)
#define CLOSURE_BOUND_LAMBDA        4
#define CLOSURE_HAS_CODE(c) ((c) > 4)
#define CLOSURE_LAMBDA                5
#define CLOSURE_UNBOUND_LAMBDA        6
#define CLOSURE_CALLABLE(c) ((c) >= CLOSURE_EFUN && (c) <= CLOSURE_LAMBDA)

#define CLOSURE_IDENTIFIER_OFFS 0xe800
#define CLOSURE_OPERATOR_OFFS        (CLOSURE_OPERATOR & 0xffff)
#define CLOSURE_EFUN_OFFS        (CLOSURE_EFUN & 0xffff)
#define CLOSURE_SIMUL_EFUN_OFFS        (CLOSURE_SIMUL_EFUN & 0xffff)


/* --- struct vector: the array datatype ---
 *
 * When smalloc is used, the number of elements can be deduced from
 * the memory block size, so the .size entry is not needed.
 */

struct vector {
#ifndef MALLOC_smalloc
    p_int size;       /* Number of contained elements */
#endif
    p_int ref;        /* Number of references */
#ifdef DEBUG
    p_int extra_ref;  /* Second refcount, used to check .ref. */
#endif
    struct wiz_list *user;    /* Save who made the vector */
    struct svalue item[1];
};


/* --- struct mapping: the mapping datatypes --- */

/* The main structure of a mapping
 */

struct mapping {
    p_int                     ref;         /* Number of references */
    struct hash_mapping      *hash;        /* 'dirty' part of the mapping */
    struct condensed_mapping *condensed;
      /* 'clean' part of the mapping. If this pointer is NULL, the mapping
       * is pending for final deallocation.
       */
    struct wiz_list          *user;        /* Save who made the mapping */
    int                       num_values;  /* Number of values per key */
};


/* The 'dirty' part of a mapping.
 *
 * It is allocated big enough for chains[] to hold all (.mask+1) chain
 * heads. The number of chains is chosen, and if necessary increased,
 * so that the average length of a chain is less or equal two entries.
 */

struct hash_mapping {
    p_int mask;
      /* Index mask for chains[], converting the raw hash value into
       * the valid index number using a bit-and operation.
       * Incremented by one, it's the number of chains.
       */
    p_int used;                    /* Number of entries in the hash */
    p_int condensed_deleted;
      /* Number of entries deleted from the condensed part
       */
    p_int ref;
      /* Refcount if this mapping is part of a T_PROTECTOR_MAPPING svalue.
       * The value is <= the mappings main refcount.
       */
    struct map_chain *deleted;
      /* Protector mappings only: list of deleted entries, which are kept
       * pending because the they may still be used as destination for
       * a lvalue.
       */
    struct mapping   *next_dirty;  /* Next dirty mapping in the list */
    struct map_chain *chains[ 1 /* +.mask */ ]; 
      /* The hash chain heads ('hash buckets')
       */
};


/* One 'dirty' entry in a mapping.
 * It is allocated big enough for data[] to hold all values.
 */

struct map_chain {
    struct map_chain *next;  /* next dirty entry in hash_mapping chain */
    struct svalue key;       /* the key value */
    struct svalue data[1];   /* &data == &key+1 is used in some places */
};


/* The 'clean' part of a mapping.
 *
 * The memory layout is a bit complicated...
 */

struct condensed_mapping {

    /* struct svalue m_values[ ... ];
     *
     *   The values associated with the following misc (non-string) keys.
     *   The association is (with num values per key):
     *
     *     misc[x] -> m_values[num * x .. num * x + num - 1]
     *
     * struct svalue misc[ ... ]
     *
     *   The misc keys and their associated values.
     *   Invalid keys have the svalue-type T_INVALID, the associated
     *   values are (usually) all svalue-0; and both are always located at
     *   the end of the array.
     */

    p_int misc_size;    /* Total size of the misc key values in byte(!) */
    p_int string_size;  /* Total size of the string key pointers in byte(!) */
      /* Using the size in Byte for these two values allows a faster setup
       * for the search when indexing
       */

    /* char *string[ ... ];
     *
     *   Pointer to the key strings. Uneven pointer values denote
     *   deleted/invalidated keys and no longer point to valid
     *   memory, the associated values are (usually) all svalue-0.
     *
     * struct svalue s_values[ ... ];
     *
     *   The values associated with the preceeding string keys.
     *   The association is (with num values per key):
     *
     *     string[x] -> s_values[num * x .. num * x + num - 1]
     */
};

#define CM_MISC(m) ((struct svalue *)(m))
  /* For condensed_mapping <m>, return a pointer to the svalue
   * after the last misc key in that mapping.
   */

#define CM_STRING(m) ((char **)((m)+1))
  /* For condensed_mapping <m>, return a pointer to the first
   * string key in that mapping.
   */

#define MAP_CHAIN_SIZE(n) (\
        (sizeof(struct map_chain) - sizeof(struct svalue) ) +\
                sizeof(struct svalue)*(n) )
  /* Size of a map_chain structure for <n> values
   */

/* --- struct lambda:  --- */

struct lambda {
    p_int ref;
    struct object *ob;
    union {
        unsigned short index;        /* lfun closures */
        char code[1];        /* lambda closures */
        struct lambda *lambda;
        struct {
            struct object *ob;
            unsigned short index;
        } alien;        /* alien_lfun closures */
    } function;
};


/* -- Float Support --- */

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
{        double tmp;
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

#endif /* __DATATYPES_H__ */
