#ifndef SMALLOC_H__
#define SMALLOC_H__ 1

#include "driver.h"
#include "typedefs.h"

#include "svalue.h" 

#ifndef MALLOC_smalloc /* sigh */

#    define svalue_strlen(v) (strlen((v)->u.string))
#    define _svalue_strlen(v) (strlen((v)->u.string))
#    define malloced_strlen(s) (strlen(s)+1)

#else

/* TODO: This assumes a 32-Bit machine */
#define M_MASK 0x0fffffff  /* Mask for the size field */
#define M_REF  0x20000000  /* REF'd flag */

/* The SMALLOC_OVERHEAD */

#ifdef MALLOC_LPC_TRACE
#    ifdef MALLOC_TRACE
#        define SMALLOC_OVERHEAD (7)
#    else
#        define SMALLOC_OVERHEAD (4)
#    endif
#else /* !MALLOC_LPC_TRACE */
#    ifdef MALLOC_TRACE
#        define SMALLOC_OVERHEAD (4)
#    else
#        define SMALLOC_OVERHEAD (1)
#    endif
#endif /* MALLOC_LPC_TRACE */

#define malloced_size(ptr) ( ((p_uint *)(ptr))[-SMALLOC_OVERHEAD] & M_MASK )

/* TODO: svalue_strlen() should go into a strings/datatypes module.
 * This would also make the include of svalue.h unnecessary.
 */
#    ifdef USES_SVALUE_STRLEN

static INLINE size_t _svalue_strlen (svalue_t *v)

/* A quick function to determine a string length by inspecting
 * the memory structures.
 */

{
    short type = v->x.string_type;
    char *p = v->u.string;
    long i;

    if (type == STRING_MALLOC)
    {
        i = (long)(
                     ( ((p_uint*)p)[-SMALLOC_OVERHEAD] & M_MASK)
                     - SMALLOC_OVERHEAD - 1
                    ) * SIZEOF_CHAR_P;
        if (*(p += i))
        {
            /* The string ends somewhere in this word */
            if (*++p)
                if (!*++p) return (size_t)(i+2);
#if SIZEOF_CHAR_P == 4
                else return (size_t)(i+3);
#else
                else return (size_t)(i+3)+strlen(p+1);
#endif
            else return (size_t)(i+1);
        }
        /* Got the end in one */
        return (size_t)i;
    }
    else if (type == STRING_SHARED)
    {
        i = (long)
            ((*(p_uint*)
                 (p - sizeof(StrRefCount) - (SMALLOC_OVERHEAD+1) * SIZEOF_CHAR_P) &
                M_MASK) * SIZEOF_CHAR_P -
               sizeof(StrRefCount) -
               (SMALLOC_OVERHEAD+2) * SIZEOF_CHAR_P
            );
        if (i >= 0)
#if SIZEOF_CHAR_P == 4
        {
            if (*(p+=i))
            {
                if (*++p)
                {
                    if (!*++p) return (size_t)(i+2);
                    else return (size_t)(i+3);
                }
                else return (size_t)(i+1);
            }
            return (size_t)i;
        }
        if (!*p) return 0;
        return 1;
#else
        return (size_t)i + strlen(p+i);
        return strlen(p);
#endif
    }
    else /* volatile/constant string */
    {
        return strlen(p);
    }
} /* _svalue_strlen() */

#    define svalue_strlen(v) (_svalue_strlen((v)))

#endif /* USES_SVALUE_STRLEN */

#define malloced_strlen(s) ( ( \
        (*(p_uint *)((s)-sizeof(p_int)*SMALLOC_OVERHEAD) & M_MASK) \
        - SMALLOC_OVERHEAD) * SIZEOF_CHAR_P)

/* --- Variables --- */
extern int debugmalloc;

/* --- Prototypes --- */

/* TODO: It would be nice to have a 'safe xalloc' which would either return
 * TODO:: a pointer or throw an Out-of-memory error. Alas, with all the
 * TODO:: local copies of inter_sp/inter_pc this is not so easy.
 * TODO: Check what  C99 says about free() and friends; if possible, exchange
 * TODO:: all POINTER with void*.
 */
#ifdef MALLOC_TRACE
extern POINTER smalloc(size_t size, const char *file, int line);
#else
extern POINTER smalloc(size_t size);
#endif

extern POINTER rexalloc(POINTER ptr, size_t size);
extern POINTER amalloc(size_t size);
extern POINTER pxalloc(size_t size);
extern void xfree(POINTER ptr);
extern void pfree(POINTER ptr);
extern void afree(POINTER ptr);

extern char * smalloc_string_copy (const char *str, const char *file, int line);
extern int malloc_size_mask(void);
extern void dump_malloc_data(strbuf_t *sbuf);
extern void smalloc_dinfo_data(svalue_t *svp, int value);
extern void clear_M_REF_flags(void);
extern void free_unreferenced_memory(void);
extern void consolidate_freelists (void);
extern void *malloc_increment_size(void *p, size_t size);
extern void walk_new_small_malloced( void (*func)(POINTER, long) );
#ifdef CHECK_MAPPING_TOTAL
extern mp_int available_memory(void);
#endif /* CHECK_MAPPING_TOTAL */

#ifdef MALLOC_TRACE
extern void store_print_block_dispatch_info(void *block, void (*func)(int, char *, int) );
extern int is_freed(void *p, p_uint minsize);
#endif /* MALLOC_TRACE */

#ifdef CHECK_OBJECT_GC_REF
extern void note_object_allocation_info ( void *block );
extern void note_program_allocation_info ( void *block );
extern Bool is_object_allocation ( void *block );
extern Bool is_program_allocation ( void *block );
#endif /* CHECK_OBJECT_RC_REF */

#endif /* MALLOC_smalloc */

#endif /* SMALLOC_H__ */
