#ifndef SMALLOC_H
#define SMALLOC_H 1

#include "driver.h"

#include "datatypes.h"  /* struct svalue */
#include "strfuns.h"    /* strbuf_t */

#ifndef MALLOC_smalloc /* sigh */

#define svalue_strlen(v) (strlen((v)->u.string))
#define _svalue_strlen(v) (strlen((v)->u.string))
#define malloced_strlen(s) (strlen(s)+1)

#else

#define MASK           0x0fffffff
#define M_REF           0x20000000

#ifdef SMALLOC_LPC_TRACE
#ifdef SMALLOC_TRACE
#define SMALLOC_OVERHEAD (7)
#else
#define SMALLOC_OVERHEAD (4)
#endif
#else /* !SMALLOC_LPC_TRACE */
#ifdef SMALLOC_TRACE
#define SMALLOC_OVERHEAD (4)
#else
#define SMALLOC_OVERHEAD (1)
#endif
#endif /* SMALLOC_LPC_TRACE */

#define malloced_size(ptr) ( ((p_uint *)(ptr))[-SMALLOC_OVERHEAD] & MASK )

/* TODO: svalue_strlen() should go into a strings/datatypes module.
 * This would also make the include of datatypes.h unnecessary.
 */
#ifdef USES_SVALUE_STRLEN
static INLINE size_t _svalue_strlen(v)
register struct svalue *v;
{
    register short type = v->x.string_type;
    register char *p = v->u.string;
    register long i;

    if (type == STRING_MALLOC) {
        i = (signed)(
                (((p_uint*)(p))[-SMALLOC_OVERHEAD] & MASK) -
                 (SMALLOC_OVERHEAD + 1)
            ) * SIZEOF_CHAR_P;
        if (*(p+=i))
            if (*++p)
                if (!*++p) return (size_t)(i+2);
#if SIZEOF_CHAR_P == 4
                else return (size_t)(i+3);
#else
                else return (size_t)(i+3)+strlen(p+1);
#endif
            else return (size_t)(i+1);
        return (size_t)i;
    } else if (type == STRING_SHARED) {
            if ( (i = (signed)
               ((*(p_uint*)
                 (p - sizeof(short) - (SMALLOC_OVERHEAD+1) * SIZEOF_CHAR_P) &
                MASK) * SIZEOF_CHAR_P -
               sizeof(short) -
               (SMALLOC_OVERHEAD+2) * SIZEOF_CHAR_P)
             ) >= 0)
#if SIZEOF_CHAR_P == 4
        {
                if (*(p+=i))
                    if (*++p)
                        if (!*++p) return (size_t)(i+2);
                    else return (size_t)(i+3);
                    else return (size_t)(i+1);
            return (size_t)i;
            }
        if (!*p) return 0;
        return 1;
#else
            return (size_t)i + strlen(p+i);
        return strlen(p);
#endif
    } else {
        return strlen(p);
    }
}

#define svalue_strlen(v) (_svalue_strlen((v)))
#endif /* USES_SVALUE_STRLEN */

#define malloced_strlen(s) ( ( \
        (*(p_uint *)((s)-sizeof(p_int)*SMALLOC_OVERHEAD) & MASK) \
        - SMALLOC_OVERHEAD) * SIZEOF_CHAR_P)

/* --- Variables --- */
extern int debugmalloc;

/* --- Prototypes --- */

/* TODO: It would be nice to have a 'safe xalloc' which would either return
 * TODO:: a pointer or throw an Out-of-memory error. Alas, with all the
 * TODO:: local copies of inter_sp/inter_pc this is not so easy.
 */
#ifdef SMALLOC_TRACE
extern POINTER smalloc PROT((size_t size, const char *file, int line));
#else
extern POINTER smalloc PROT((size_t size));
#endif

extern void xfree PROT((POINTER ptr));
/* smalloc.c source says sfree(), but redef's it as xfree() */

extern POINTER amalloc PROT((size_t size));
extern POINTER permanent_xalloc PROT((size_t size));
#ifdef PFREE_RETURN_TYPE
extern PFREE_RETURN_TYPE pfree PROT((POINTER p));
#endif

#if MALLOC_ALIGN > SINT || defined(FREE_NULL_POINTER)
extern FREE_RETURN_TYPE afree PROT((POINTER p));
#endif

extern POINTER rexalloc PROT((POINTER p, size_t size));

extern char *dprintf_first PROT((int fd, char *s, p_int a));
extern void dprintf1 PROT((int fd, char *s, p_int a));
extern void dprintf2 PROT((int fd, char *s, p_int a, p_int b));
extern void dprintf3 PROT((int fd, char *s, p_int a, p_int b, p_int c));

extern int resort_free_list PROT((void)); /* TODO: Delete me? */
extern int malloc_size_mask PROT((void));
extern void dump_malloc_data PROT((strbuf_t *sbuf));
extern POINTER calloc PROT((size_t nelem, size_t sizel));
extern void clear_M_REF_flags PROT((void));
extern void free_unreferenced_memory PROT((void));
extern void *malloc_increment_size PROT((void *p, size_t size));
extern void walk_new_small_malloced PROT(( void (*func)(POINTER, long) ));

#ifdef SMALLOC_TRACE
extern void store_print_block_dispatch_info PROT((char *block, void (*func)(int, char *, int) ));
extern int is_freed PROT((char *p, p_uint minsize));
#endif /* SMALLOC_TRACE */

#endif /* MALLOC_smalloc */

#endif /* SMALLOC_H */
