#ifndef SVALUE_H__
#define SVALUE_H__ 1

/*---------------------------------------------------------------------------
 * Stack-value datatypes used by the virtual machine.
 *
 *---------------------------------------------------------------------------
 * This file refers to, but does not define 'basic' types like mappings,
 * arrays or lambdas.
 */

#include "driver.h"
#include "typedefs.h"

/* --- Types --- */

/* --- union u: the hold-all type ---
 *
 * This union is used to hold the data referenced by a svalue.
 * It contains no type information, which must be provided by the
 * containing svalue.
 *
 * Lots of code assumes that '.number' is big enough to contain the whole
 * union regardless of its actual content, therefore it is of type 'p_int'.
 */
union u {
    string_t *str;
      /* T_STRING: pointer to the string.
       * T_BYTES:  pointer to the byte string.
       * T_SYMBOL: pointer to the (shared) symbol string.
       * T_(PROTECTED_)STRING_RANGE_LVALUE: the target string holding
       *   the range.
       */
    p_int number;
      /* T_NUMBER: the number.
       */
    object_t *ob;
      /* T_OBJECT: pointer to the object structure.
       * T_CLOSURE: efun-, simul_efun-, operator closures: the object
       *            the closure is bound to.
       */
    lwobject_t *lwob;
      /* T_LWOBJECT: pointer to the lightweight object structure.
       */
    vector_t *vec;
      /* T_POINTER, T_QUOTED_ARRAY: pointer to the vector structure.
       */
     struct_t *strct;
      /* T_STRUCT: pointer to the structure instance.
       */
    mapping_t *map;
      /* T_MAPPING: pointer to the mapping structure.
       */
    lambda_t *lambda;
      /* T_CLOSURE: allocated closures: the closure structure.
       */
#ifdef FLOAT_FORMAT_2
    double  float_number;
    /* T_FLOAT: the double value for this float in FLOAT_FORMAT_2.
     */
#else
    int32_t mantissa;
      /* T_FLOAT: The mantissa (or at least one half of the float bitpattern).
       */
#endif // FLOAT_FORMAT_2
    callback_t *cb;
      /* T_CALLBACK: A callback structure referenced from the stack
       *   to allow proper cleanup during error recoveries. The interpreter
       *   knows how to free it, but that's all.
       */

    void *generic;
      /* Use read-only, this member is used to read the svalue generically
       * as "a pointer". It is mostly used in comparisons.
       */

    svalue_t *lvalue;
      /* T_LVALUE/LVALUE_UNPROTECTED: pointer to a normal svalue
       *    which this lvalue references. This entry may point to
       *    another (but then protected) T_LVALUE. This is used for
       *    reseating of references (so there is usually no need to
       *    unravel the chain).
       */

    struct protected_lvalue *protected_lvalue;
      /* T_LVALUE/LVALUE_PROTECTED: A svalue with a reference counter.
       */

    struct protected_char_lvalue *protected_char_lvalue;
      /* T_LVALUE/LVALUE_PROTECTED_CHAR: A string, an index and a
       *    reference counter.
       */

    struct protected_range_lvalue *protected_range_lvalue;
      /* T_LVALUE/LVALUE_PROTECTED_RANGE: Contains the sequence
       * (vector or string), the indices and a reference counter.
       */

    struct protected_mapentry_lvalue *protected_mapentry_lvalue;
      /* T_LVALUE/LVALUE_MAPENTRY: Contains a mapping, key, index
       * and a reference counter.
       */

    error_handler_t *error_handler;
      /* T_ERROR_HANDLER: the function error_handler->fun is
       * executed on a free_svalue(), receiving the error_handler_t*
       * as argument. This allows the transparent implemention of cleanup
       * functions which are called even after runtime errors. In order
       * to pass additional information to the error_handler(), embed
       * the T_ERROR_HANDLER svalue into a larger structure (possible since
       * it has to be referenced by pointer) and let the error_handler->fun()
       * execute the appropriate casts.
       */

    bytecode_p break_addr;
      /* Points to address to branch to at next F_BREAK from within a switch().
       */
};

/* --- struct svalue_s: the LPC data structure ---
 *
 * svalues ('stack values)' are used throughout the driver to hold LPC
 * values. One svalue consists of an instance of union u to hold the
 * actual value, and a type field describing the type of the value.
 *
 * Some values need specific, additional information (e.g. string values
 * distinguish shared from allocated strings), which is stored in the
 * union 'x'.
 *
 * The T_LVALUE type family is special in that the svalue instance does
 * not contain the actual value, but instead a reference to another
 * svalue. lvalues are necessary whenever existing svalue instance
 * are to be assigned. A special case are protected lvalues where the
 * svalue referenced by the T_LVALUE.u.lvalue is not the target svalue,
 * but instead a T_PROTECTED_xxx_LVALUE which then points to the target
 * and its protector. Similar, indexed and range-indexed lvalues don't
 * point to the data itself, but instead to one of the dedicated T_xxx_LVALUE
 * types which contain the necessary information to perform the indexing
 * operation; effectively limiting the access to the target value to
 * the view specified by the index/range bounds.
 *
 * T_LVALUEs are also used to reference meta data, like T_ERROR_HANDLER
 * svalues. TODO: Maybe they should get the T_META type.
 */
struct svalue_s
{
    ph_int type;  /* Primary type information */
    union {       /* Secondary type information */
#ifndef FLOAT_FORMAT_2
        int16_t exponent;    /* Exponent of a T_FLOAT */
#endif
        ph_int closure_type; /* Type of a T_CLOSURE */
        ph_int lvalue_type;  /* Type of a T_LVALUE */
        ph_int quotes;       /* Number of quotes of a quoted array or symbol */
        ph_int num_arg;      /* used by call_out.c to for vararg callouts */
        ph_int generic;
          /* For types without secondary type information, this is set to
           * a fixed value, usually (u.number << 1).
           * Also, this field is also used as generic 'secondary type field'
           * handle, e.g. when comparing svalues.
           */
    } x;
    union u u;  /* The value */
};

#define SVALUE_FULLTYPE(svp) ((p_int *)(svp))
  /* Return a pointer to an integer with the primary and secondary type
   * information.
   */
/* TODO: Sanity test: sizeof struct { ph_int, ph_int } <= sizeof p_int */


/* struct svalue_s.type: Primary types.
 *   There must be no more than 16 different primary types, as some
 *   type tables (prolang.y::hook_type_map[] for example) store
 *   them as bitflags in shorts.
 * TODO: Create these constants, plus the names in
 * TODO:: interpret.c::svalue_typename[] from a descriptive _spec file.
 */

#define T_INVALID       0x0  /* empty svalue */
#define T_LVALUE        0x1  /* a lvalue */
#define T_NUMBER        0x2  /* an integer number */
#define T_STRING        0x3  /* a string */
#define T_POINTER       0x4  /* a vector */
#define T_OBJECT        0x5  /* an object */
#define T_MAPPING       0x6  /* a mapping */
#define T_FLOAT         0x7  /* a float number */
#define T_CLOSURE       0x8  /* a closure */
#define T_SYMBOL        0x9  /* a symbol */
#define T_QUOTED_ARRAY  0xa  /* a quoted array */
#define T_STRUCT        0xb  /* a struct */
#define T_BYTES         0xc  /* a byte string */
#define T_LWOBJECT      0xd  /* a lightweight object */

#define T_CALLBACK      0xe
  /* A callback structure referenced from the stack to allow
   * proper cleanup during error recoveries. The interpreter
   * knows how to free it, but that's all.
   */

#define T_ERROR_HANDLER 0xf
  /* Not an actual value, this is used internally for cleanup
   * operations. See the description of the error_handler() member
   * for details.
   */

#define T_BREAK_ADDR    0x10
  /* Not an actual type, it's used internally for saving
   * the address where break statements within switch statements
   * should branch to.
   */

#undef T_NULL /* There is some T_NULL definition in system headers. */
#define T_NULL          0x11
  /* Not an actual type, this is used in the efun_lpc_types[] table
   * to encode the acceptance of '0' instead of the real datatype.
   */

#define T_MOD_SWAPPED   0x80
  /* This flag is |-ed to the swapped-out type value if the value
   * data has been swapped out.
   */

/* T_CLOSURE secondary information. */

  /* The secondary information (.x.closure_type) defines the type
   * of closure and the data structure used in the .u union.
   *
   * Positive numbers have an allocated data structure in .u.lambda:
   *
   *    CLOSURE_LFUN           uses .u.lambda->function.lfun
   *    CLOSURE_IDENTIFIER     uses .u.lambda->function.var_index
   *    CLOSURE_BOUND_LAMBDA   uses .u.lambda->function.lambda
   *    CLOSURE_LAMBDA         uses .u.lambda->function.code
   *    CLOSURE_UNBOUND_LAMBDA uses .u.lambda->function.code
   *
   * Negative numbers consist of an index and the corresponding
   * object in .u.ob or .u.lwob. The index (operator, efun or
   * simul-efun index) must be less than 0x0800 (2048). The
   * secondary information is then as follows:
   *
   *    CLOSURE_OPERATOR + operator index   for operator closures
   *    CLOSURE_PYTHON_EFUN + efun index    for python efuns
   *    CLOSURE_EFUN + efun index           for native efuns
   *    CLOSURE_SIMUL_EFUN + function index for simul-efuns
   *
   * The object the closure is bound to is stord in .u.ob.
   * If it is a lightweight object, then CLOSURE_LWO is added
   * to the secondary information and .u.lwob is used instead.
   */

#define CLOSURE_LWO             (-0x4000)  /* == 0xc000 */

#ifdef USE_PYTHON
#  define CLOSURE_OPERATOR      (-0x2000)  /* == 0xe000 */
#  define CLOSURE_PYTHON_EFUN   (-0x1800)  /* == 0xe800 */
#else
#  define CLOSURE_OPERATOR      (-0x1800)  /* == 0xe800 */
#endif
#define CLOSURE_EFUN            (-0x1000)  /* == 0xf000 */
#define CLOSURE_SIMUL_EFUN      (-0x0800)  /* == 0xf800 */

#define CLOSURE_LWO_OFFS        (CLOSURE_LWO & 0xffff)
#define CLOSURE_OPERATOR_OFFS   (CLOSURE_OPERATOR & 0xffff)
#ifdef USE_PYTHON
#  define CLOSURE_PYTHON_EFUN_OFFS (CLOSURE_PYTHON_EFUN & 0xffff)
#endif
#define CLOSURE_EFUN_OFFS       (CLOSURE_EFUN & 0xffff)
#define CLOSURE_SIMUL_EFUN_OFFS (CLOSURE_SIMUL_EFUN & 0xffff)

  /* The other closure types are created from actual objects and lambdas,
   * the detailed information is stored in the u.lambda field.
   * The first types are 'just' references to existing lfuns and variables,
   * the others actually point to code.
   */

#define CLOSURE_LFUN            0  /* lfun in an object */

    /* Code 1 was used for ALIEN_LFUNs - some mudlibs however rely on the
     * old code values.
     */

#define CLOSURE_IDENTIFIER      2  /* variable in this object */

    /* Code 3 was used for preliminary closures, that were efun closures
     * used in static initialization of variables in LDMud 3.2 and before.
     */

#define CLOSURE_BOUND_LAMBDA    4  /* Bound unbound-lambda closure */
#define CLOSURE_LAMBDA          5  /* normal lambda closure */
#define CLOSURE_UNBOUND_LAMBDA  6  /* unbound lambda closure. */


#ifdef USE_PYTHON
#  define CLOSURE_IDENTIFIER_OFFS 0xe000
#else
#  define CLOSURE_IDENTIFIER_OFFS 0xe800
#endif
  /* When creating lfun/variable closures, the lfun/variable to bind
   * is given as unsigned number in the bytecode:
   *   number < C_I_OFFS:  number is index into function table
   *   number >= C_I_OFFS: (number-C_I_OFFS) is index into variable table.
   *   nummer >= C_OPERATOR_OFFS: operator or efun closure.
   */

/* Predicates operating on T_CLOSURE secondary information */

#define CLOSURE_MALLOCED(c) ((c) >= 0)
  /* TRUE if the closure was created at LPC runtime, ie is not an operator,
   * efun or simul_efun.
   */

#define CLOSURE_IS_LFUN(c)        ((c) == CLOSURE_LFUN)
  /* TRUE if the closure is of the #'<lfun> type.
   */

#define CLOSURE_REFERENCES_CODE(c) ((c) >= CLOSURE_BOUND_LAMBDA)
  /* TRUE if the closure may have or reference code.
   */

#define CLOSURE_HAS_CODE(c) ((c) > CLOSURE_BOUND_LAMBDA)
  /* TRUE if the closure points to actual code.
   */

#define CLOSURE_CALLABLE(c) ((c) >= CLOSURE_EFUN && (c) <= CLOSURE_LAMBDA)
  /* TRUE if the closure is callable.
   */


/* T_LVALUE secondary information. */

#define LVALUE_UNPROTECTED                  0x00
  /* .u.lvalue points to the referenced svalue. */

#define LVALUE_UNPROTECTED_CHAR             0x01
  /* Doesn't have a value. The referenced character is
   * stored in <current_unprotected_char>.
   */

#define LVALUE_UNPROTECTED_RANGE            0x02
  /* Doesn't have a value. The vector and indices are stored in
   * <current_unprotected_range>.
   */

#define LVALUE_UNPROTECTED_MAPENTRY         0x03
  /* Doesn't have a value. The mapping, key and index are
   * stored in <current_unprotected_mapentry>
   */

#define LVALUE_PROTECTED                    0x10
  /* .u.protected_lvalue points to the reference counted svalue. */

#define LVALUE_PROTECTED_CHAR               0x11
  /* .u.protected_char_lvalue contains the referenced string and index. */

#define LVALUE_PROTECTED_RANGE              0x12
  /* .u.protected_range contains the referenced vector and indices. */

#define LVALUE_PROTECTED_MAPENTRY           0x13
 /* .u.protected_mapentry contains the referenced mapping and key. */


#define LVALUE_IS_UNPROTECTED(l)        ((l) < LVALUE_PROTECTED)
  /* TRUE if the lvalue is an unprotected one.
  */

#define LVALUE_IS_PROTECTED(l)          ((l) >= LVALUE_PROTECTED)
  /* TRUE if the lvalue is a protected one.
  */

/* --- The primary types in bit-flag encoding ---
 *
 * This flag encoding is used for the runtime typetests.
 * Not all svalue types have a corresponding bitflag.
 */

#define TF_INVALID       (1 << T_INVALID)
#define TF_LVALUE        (1 << T_LVALUE)
#define TF_NUMBER        (1 << T_NUMBER)
#define TF_STRING        (1 << T_STRING)
#define TF_POINTER       (1 << T_POINTER)
#define TF_OBJECT        (1 << T_OBJECT)
#define TF_MAPPING       (1 << T_MAPPING)
#define TF_FLOAT         (1 << T_FLOAT)
#define TF_CLOSURE       (1 << T_CLOSURE)
#define TF_SYMBOL        (1 << T_SYMBOL)
#define TF_QUOTED_ARRAY  (1 << T_QUOTED_ARRAY)
#define TF_NULL          (1 << T_NULL)
#define TF_STRUCT        (1 << T_STRUCT)
#define TF_BYTES         (1 << T_BYTES)
#define TF_LWOBJECT      (1 << T_LWOBJECT)

#define TF_ANYTYPE       (~0)
  /* This is used in the efun_lpc_types[]
   * table to encode the acceptance of any type.
   */

/* --- Float Support --- */

/* The driver uses a 48-Bit floating point format with a 32 bit mantissa
 * and a 16 bit exponent, stored in u.mantissa and x.exponent. This should
 * be well below all existing host floating point formats so that we get
 * the same accuracy on all platforms.
 *
 * To keep the implementation transparent, the following macros/functions are
 * defined:
 *
 *   int FLOAT_FORMAT:
 *     0 for the old, tradition format with 48 bit floats
 *     1 for an old, now removed format
 *     2 for using native doubles
 *     Additional numbers may be defined for more formats.
 *
 *   double READ_DOUBLE(struct svalue * sp)
 *     Return the floating point number stored in *sp.
 *
 *   int32_t SPLIT_DOUBLE (double d, int * p)
 *     Store the bytes 4..5 of <d> to the address given in <p>, and
 *     return the bytes 0..3 of <d> as a long.
 *     Used by the compiler to generate F_FLOAT instructions.
 *     TODO: This code makes heave assumptions about data sizes and layout
 *     TODO:: of integral types.
 *
 *   void STORE_DOUBLE (struct svalue * dest, double d)
 *     Store the float <d> into the svalue *dest.
 *
 *   STORE_DOUBLE_USED
 *     UNUSED and defined empty.
 *     Did declare a local variable which STORE_DOUBLE needed.
 */

// STORE_DOUBLE_USED is not needed anymore and defined empty.
#define STORE_DOUBLE_USED

// used for joining/splitting the native, internal format used in FLOAT_FORMAT_0
// into a native double. Needed in all FLOAT_FORMAT_* because this is used
// during save_/restore_svalue() for saving/restoring older savefile versions.
static INLINE double JOIN_DOUBLE(int32_t mantissa, int16_t exponent) {
    return ldexp( (double)(mantissa), exponent - 31 );
}
static INLINE int32_t SPLIT_DOUBLE(double doublevalue, int *int_p) {
    return (int32_t)ldexp( frexp( doublevalue, int_p ), 31);
}
/* if your machine doesn't use the exponent to designate powers of two,
 the use of ldexp in SPLIT_DOUBLE won't work; you'll have to multiply
 with 32768. in this case */

#ifdef FLOAT_FORMAT_2
    static INLINE double READ_DOUBLE(svalue_t *sv) {
        return sv->u.float_number;
    }

    static INLINE void STORE_DOUBLE(svalue_t *dest, double doublevalue) {
        dest->u.float_number = doublevalue;
    }
#else
/* --- The portable format, used if no other format is defined */
#   define FLOAT_FORMAT_0
    static INLINE double READ_DOUBLE(svalue_t *sv) {
        return JOIN_DOUBLE(sv->u.mantissa, sv->x.exponent);
    }

    static INLINE void STORE_DOUBLE(svalue_t *dest, double doublevalue) {
        int exponent;
        dest->u.mantissa = SPLIT_DOUBLE(doublevalue, &exponent);
        dest->x.exponent = exponent;
    }
#endif // FLOAT_FORMAT_2

/* --- svalue helper functions and macros --- */

#define addref_closure(sp, from) \
  MACRO( svalue_t * p = sp; \
         if (CLOSURE_MALLOCED(p->x.closure_type)) \
             p->u.lambda->ref++; \
         else if (p->x.closure_type < CLOSURE_LWO) \
            ref_lwobject(p->u.lwob); \
         else \
             (void)ref_object(p->u.ob, from); \
  )
  /* void addref_closure(sp, from): Add one ref to the closure svalue <sp>
   *   in the function <from>.
   */


/* svalue_t svalue_<type>(value): Creates a svalue_t value of <type>.
 *
 * void put_<type>(sp, value): Initialise svalue *sp with value of <type>.
 *   'sp' is evaluated several times and must point to an otherwise
 *   empty svalue. If <value> is a refcounted value, its refcount is
 *   NOT incremented.
 *
 * void put_ref_<type>(sp, value): Initialise svalue *sp with value
 *   of <type>. 'sp' is evaluated several times and must point to an
 *   otherwise empty svalue. <value> must be a refcounted value, and
 *   its refcount is incremented.
 *   These functions are not defined here, but in the header files
 *   of the corresponding types.
 *
 * void push_<type>(), void push_ref_<type>()
 *   As the put_() macros, but <sp> is incremented once first.
 *
 * As a rule, all arguments must not have sideeffects!
 *
 * The psh_ macros are not in use (yet) - they have been renamed from push_
 * so that the first use stands out.
 *
 * TODO: Add push_xxx() macros, see MudOS:interpret.h. In general, get
 * TODO:: rid of the local sp/pc copies since they make error handling
 * TODO:: very difficult.
 * TODO: Also add 'adopt_<type>()' and 'pop_<type>()'macros, e.g.
 * TODO:: 'adopt_object(sp)', which
 * TODO:: retrieve the type from the *sp and set sp->type to T_INVALID.
 */

static INLINE svalue_t svalue_number(const p_int num)
                                                    __attribute__((const));
static INLINE svalue_t svalue_number(const p_int num)
/* Return an svalue for the number <num>.
 */
{
    return (svalue_t){ T_NUMBER, {}, {.number = num } };
}

static INLINE svalue_t svalue_float(const double val)
                                                    __attribute__((const));
static INLINE svalue_t svalue_float(const double val)
/* Return an svalue for the floating point value <val.>
 */
{
#ifdef FLOAT_FORMAT_2
    return (svalue_t){ T_FLOAT, {}, {.float_number = val } };
#else
    int exponent;
    int32_t mantissa = SPLIT_DOUBLE(val, &exponent)

    return (svalue_t){ T_FLOAT, {.exponent = exponent}, {.mantissa = mantissa } };
#endif
}

static INLINE svalue_t svalue_string(string_t * const str)
                        __attribute__((nonnull(1))) __attribute__((const));
static INLINE svalue_t svalue_string(string_t * const str)
/* Return an svalue for the string <str>.
 */
{
    return (svalue_t){ T_STRING, {}, {.str = str } };
}

static INLINE svalue_t svalue_bytes(string_t * const str)
                        __attribute__((nonnull(1))) __attribute__((const));
static INLINE svalue_t svalue_bytes(string_t * const str)
/* Return an svalue for the byte sequence <str>.
 */
{
    return (svalue_t){ T_BYTES, {}, {.str = str } };
}

static INLINE svalue_t svalue_symbol(string_t * const str, const ph_int numquotes)
                        __attribute__((nonnull(1))) __attribute__((const));
static INLINE svalue_t svalue_symbol(string_t * const str, const ph_int numquotes)
/* Return an svalue for the symbol <str> with <numquotes> number of quotes.
 */
{
    return (svalue_t){ T_SYMBOL, {.quotes = numquotes}, {.str = str } };
}

static INLINE svalue_t svalue_array(vector_t * const vec)
                        __attribute__((nonnull(1))) __attribute__((const));
static INLINE svalue_t svalue_array(vector_t * const vec)
/* Return an svalue for the array <vec>.
 */
{
    return (svalue_t){ T_POINTER, {}, {.vec = vec } };
}

static INLINE svalue_t svalue_mapping(mapping_t * const map)
                        __attribute__((nonnull(1))) __attribute__((const));
static INLINE svalue_t svalue_mapping(mapping_t * const map)
/* Return an svalue for the mapping <map>.
 */
{
    return (svalue_t){ T_MAPPING, {}, {.map = map } };
}

static INLINE svalue_t svalue_struct(struct_t * const s)
                        __attribute__((nonnull(1))) __attribute__((const));
static INLINE svalue_t svalue_struct(struct_t * const s)
/* Return an svalue for the struct <s>.
 */
{
    return (svalue_t){ T_STRUCT, {}, {.strct = s } };
}

static INLINE svalue_t svalue_object(object_t * const obj)
                        __attribute__((nonnull(1))) __attribute__((const));
static INLINE svalue_t svalue_object(object_t * const obj)
/* Return an svalue for the object <obj>.
 */
{
    return (svalue_t){ T_OBJECT, {}, {.ob = obj } };
}

static INLINE svalue_t svalue_lwobject(lwobject_t * const lwobj)
                        __attribute__((nonnull(1))) __attribute__((const));
static INLINE svalue_t svalue_lwobject(lwobject_t * const lwobj)
/* Return an svalue for the lightweight object <lwobj>.
 */
{
    return (svalue_t){ T_LWOBJECT, {}, {.lwob = lwobj } };
}

static INLINE svalue_t svalue_callback(callback_t * const cb)
                        __attribute__((nonnull(1))) __attribute__((const));
static INLINE svalue_t svalue_callback(callback_t * const cb)
/* Return an svalue for the callback <cb>.
 */
{
    return (svalue_t){ T_CALLBACK, {}, {.cb = cb } };
}

static INLINE void put_number(svalue_t * const dest, const p_int num)
                                                __attribute__((nonnull(1)));
static INLINE void put_number(svalue_t * const dest, const p_int num)
/* Put the number <num> into <dest>, which is considered empty.
 */
{
    *dest = svalue_number(num);
}

static INLINE void put_float(svalue_t * const dest, const double val)
                                                __attribute__((nonnull(1)));
static INLINE void put_float(svalue_t * const dest, const double val)
/* Put the floating point number <num> into <dest>, which is considered empty.
 */
{
    *dest = svalue_float(val);
}

static INLINE void put_string(svalue_t * const dest, string_t * const str)
                                                __attribute__((nonnull(1,2)));
static INLINE void put_string(svalue_t * const dest, string_t * const str)
/* Put the string <str> into <dest>, which is considered empty.
 */
{
    *dest = svalue_string(str);
}

static INLINE void put_bytes(svalue_t * const dest, string_t * const str)
                                                __attribute__((nonnull(1,2)));
static INLINE void put_bytes(svalue_t * const dest, string_t * const str)
/* Put the byte sequence <str> into <dest>, which is considered empty.
 */
{
    *dest = svalue_bytes(str);
}

static INLINE void put_symbol(svalue_t * const dest, string_t * const str, const ph_int numquotes)
                                                __attribute__((nonnull(1,2)));
static INLINE void put_symbol(svalue_t * const dest, string_t * const str, const ph_int numquotes)
/* Put the symbol <str> with <numquotes> number of quotes into <dest>,
 * which is considered empty.
 */
{
    *dest = svalue_symbol(str, numquotes);
}

static INLINE void put_array(svalue_t * const dest, vector_t * const vec)
                                                __attribute__((nonnull(1,2)));
static INLINE void put_array(svalue_t * const dest, vector_t * const vec)
/* Put the array <vec> into <dest>, which is considered empty.
 */
{
    *dest = svalue_array(vec);
}

static INLINE void put_mapping(svalue_t * const dest, mapping_t * const map)
                                                __attribute__((nonnull(1,2)));
static INLINE void put_mapping(svalue_t * const dest, mapping_t * const map)
/* Put the mapping <map> into <dest>, which is considered empty.
 */
{
    *dest = svalue_mapping(map);
}

static INLINE void put_struct(svalue_t * const dest, struct_t * const s)
                                                __attribute__((nonnull(1,2)));
static INLINE void put_struct(svalue_t * const dest, struct_t * const s)
/* Put the struct <s> into <dest>, which is considered empty.
 */
{
    *dest = svalue_struct(s);
}

static INLINE void put_object(svalue_t * const dest, object_t * const obj)
                                                __attribute__((nonnull(1,2)));
static INLINE void put_object(svalue_t * const dest, object_t * const obj)
/* Put the object <obj> into <dest>, which is considered empty.
 */
{
    *dest = svalue_object(obj);
}

static INLINE void put_lwobject(svalue_t * const dest, lwobject_t * const lwobj)
                                                __attribute__((nonnull(1,2)));
static INLINE void put_lwobject(svalue_t * const dest, lwobject_t * const lwobj)
/* Put the lightweight object <lwobj> into <dest>, which is considered empty.
 */
{
    *dest = svalue_lwobject(lwobj);
}

static INLINE void put_callback(svalue_t * const dest, callback_t * const cb)
                                                __attribute__((nonnull(1,2)));
static INLINE void put_callback(svalue_t * const dest, callback_t * const cb)
/* Put the callback <cb> into <dest>, which is considered empty.
 */
{
    *dest = svalue_callback(cb);
}

#define push_number(sp,num) \
    ( (sp)++, put_number(sp, num) )

#define push_float(sp,fnum) \
    ( (sp)++, put_float(sp, fnum) )

#define push_ref_array(sp,arr) \
    ( (sp)++, put_ref_array(sp, arr) )
#define push_array(sp,arr) \
    ( (sp)++, put_array(sp, arr) )

#define push_ref_struct(sp,st) \
    ( (sp)++, put_ref_struct(sp, st) )
#define push_struct(sp,st) \
    ( (sp)++, put_struct(sp, st) )

#define push_ref_mapping(sp,val) \
    ( (sp)++, put_ref_mapping(sp,val) )
#define push_mapping(sp,val) \
    ( (sp)++, put_mapping(sp,val) )

#define push_ref_object(sp,val,from) \
    ( (sp)++, put_ref_object(sp,val,from) )
#define push_object(sp,val) \
    ( (sp)++, put_object(sp,val) )

#define push_ref_valid_object(sp,val,from) \
    ( (sp)++, put_ref_valid_object(sp,val,from) )
#define push_valid_object(sp,val) \
    ( (sp)++, put_valid_object(sp,val) )

#define push_lwobject(sp,val) \
    ( (sp)++, put_lwobject(sp,val) )

#define push_ref_string(sp,val) \
    ( (sp)++, put_ref_string(sp,val) )
#define push_string(sp,val) \
    ( (sp)++, put_string(sp,val) )
#define push_c_string(sp,txt) \
    ( (sp)++, put_c_string(sp, txt) )
#define push_c_n_string(sp,txt,len) \
    ( (sp)++, put_c_n_string(sp, txt, len) )

#define push_ref_bytes(sp,val) \
    ( (sp)++, put_ref_bytes(sp,val) )
#define push_bytes(sp,val) \
    ( (sp)++, put_bytes(sp,val) )

#define push_callback(sp,val) \
    ( (sp)++, put_callback(sp,val) )

/* The following macros implement the dynamic cost evaluations:
 *
 *   DYN_STRING_COST(l):  increase eval_cost depending on string length <l>.
 *   DYN_ARRAY_COST(l):   increase eval_cost depending on array length <l>.
 *   DYN_MAPPING_COST(l): increase eval_cost depending on mapping length <l>.
 *
 * Covered so far are:
 *   F_ALLOCATE, F_ADD, F_ADD_EQ, F_VOID_ADD_EQ, F_MULTIPLY,
 *   F_MULT_EQ of strings
 *
 * TODO: Expand this to all datatypes and sizes.
 */

#if defined(DYNAMIC_COSTS)

#define DYN_STRING_COST(l)  eval_cost += (l) / 1000;
#define DYN_ARRAY_COST(l)  eval_cost += (l) / 1000;
#define DYN_MAPPING_COST(l)  eval_cost += (l) / 1000;

#else

#define DYN_STRING_COST(l)
#define DYN_ARRAY_COST(l)
#define DYN_MAPPING_COST(l)

#endif

/* --- Prototypes (in interpret.c) --- */

extern void free_string_svalue(svalue_t *v);
extern void free_object_svalue(svalue_t *v);
extern void zero_object_svalue(svalue_t *v);
extern void free_svalue(svalue_t *v);
extern void assign_svalue_no_free(svalue_t *to, svalue_t *from);
extern void assign_svalue(svalue_t *dest, svalue_t *v);
extern void transfer_svalue_no_free(svalue_t *dest, svalue_t *v);
extern void transfer_svalue(svalue_t *dest, svalue_t *v);
extern svalue_t *pop_n_elems (int n, svalue_t *sp);

#endif /* SVALUE_H__ */
