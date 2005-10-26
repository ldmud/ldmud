#ifndef XALLOC_H__
#define XALLOC_H__ 1

#include "driver.h"

#include "strfuns.h"
#include "svalue.h"

/* --- Macros --- */

/* void xallocate(void * dest, size_t size, const char * txt)
 *   Allocate <size> bytes using xalloc() and assign the pointer to <dest>.
 *   If the memory can't be allocated, call error() using an error message
 *   containing the description <txt>
 */

#define xallocate(dest,size,txt) \
    if (NULL == ((dest) = xalloc(size))) {\
        error("(%s:%d) Out of memory (%lu bytes) for %s\n"\
             , __FILE__, __LINE__, (unsigned long)(size), txt); \
    } else {}


/* void outofmem(const char * txt)
 *   Throw an 'out of memory' error for an allocation of unknown size with the
 *   description <txt>.
 *
 * void outofmem(size_t size, const char * txt)
 *   Throw an 'out of memory' error for an allocation of <size> with the
 *   description <txt>.
 *
 * void outofmem1(size_t size, const char * txt, mixed arg)
 *   Like outofmem(), but allows for printf-style args. In return, txt must
 *   be a string literal.
 * TODO: Add more macros like: outofmem_swap(ob), outofmem_fun(funname,len,txt),
 * TODO:: outofmem_gen(txt but no length)..., then convert all out-of-mem
 * TODO:: descriptions.
 */

#define outofmemory(txt) \
    error("(%s:%d) Out of memory for %s\n", __FILE__, __LINE__, txt)

#define outofmem(size,txt) \
    error("(%s:%d) Out of memory (%lu bytes) for %s\n"\
         , __FILE__, __LINE__, (unsigned long)(size), txt)

#define outofmem1(size,txt,arg1) \
    error("(%s:%d) Out of memory (%lu bytes) for " txt "\n"\
         , __FILE__, __LINE__, (unsigned long)(size), arg1)


/* void memsafe(void * expr, size_t size, const char * txt)
 *   <expr> is a memory allocating function (allocating roughly <size> bytes),
 *   returning a pointer. If this pointer is NULL, an 'out of memory' error
 *   is thrown using <txt> as description.
 */

#define memsafe(expr,size,txt) \
    do { \
        size_t memsafe_size = size; \
        if (NULL == (expr)) {\
            error("(%s:%d) Out of memory (%lu bytes) for %s\n"\
                 , __FILE__, __LINE__, (unsigned long)(memsafe_size), txt); \
        } else {} \
    } while(0)

/* --- Constants --- */

/* Allocation privilege levels */

#define MALLOC_USER    (0)
#define MALLOC_MASTER  (1)
#define MALLOC_SYSTEM  (2)

/* --- Variables --- */

extern Bool out_of_memory;
extern int malloc_privilege;
extern char *reserved_user_area;
extern char *reserved_master_area;
extern char *reserved_system_area;
extern mp_int reserved_user_size;
extern mp_int reserved_master_size;
extern mp_int reserved_system_size;
extern mp_int min_malloced;
extern mp_int min_small_malloced;
extern mp_int max_malloced;
extern int stack_direction;


/* --- Prototypes --- */

/* TODO: It would be nice to have a 'safe xalloc' which would either return
 * TODO:: a pointer or throw an Out-of-memory error. Alas, with all the
 * TODO:: local copies of inter_sp/inter_pc this is not so easy.
 * TODO: Check what  C99 says about free() and friends; if possible, exchange
 * TODO:: all POINTER with void*.
 */
/* xalloc(): normal allocation
 * xalloc_traced(): allocation with given file/line
 * xalloc_pass(): allocation using MTRACE_PASS as file/line args
 */

#if defined(MALLOC_TRACE)

#define xalloc(size)        xalloc_traced((size), __FILE__, __LINE__)
#define pxalloc(size)       pxalloc_traced((size), __FILE__, __LINE__)
#define rexalloc(old, size) rexalloc_traced((old), (size), __FILE__, __LINE__)

#else

#define xalloc(size)        xalloc_traced(size)
#define pxalloc(size)       pxalloc_traced(size)
#define rexalloc(old, size) rexalloc_traced((old), (size))

#endif

#define xalloc_pass(size) xalloc_traced((size) MTRACE_PASS)

extern size_t  xalloced_size (POINTER p);
extern size_t  xalloc_overhead (void);
extern POINTER xalloc_traced(size_t size MTRACE_DECL) MALLOC;
extern void    xfree(POINTER);
extern POINTER rexalloc_traced(POINTER, size_t MTRACE_DECL) MALLOC;
extern POINTER pxalloc_traced(size_t MTRACE_DECL) MALLOC;
extern void    pfree(POINTER);
extern void  * malloc_increment_size (void *vp, size_t size);

#ifdef GC_SUPPORT
extern void x_clear_ref (POINTER p);
extern int x_mark_ref (POINTER p);
extern Bool x_test_ref (POINTER p);
#endif /* GC_SUPPORT */

#ifdef MALLOC_TRACE
extern void store_print_block_dispatch_info(void *block, void (*func)(int, void *, int) );
extern int is_freed(void *p, p_uint minsize);
#endif /* MALLOC_TRACE */

#ifdef CHECK_OBJECT_GC_REF
extern void note_object_allocation_info ( void *block );
extern void note_program_allocation_info ( void *block );
extern Bool is_object_allocation ( void *block );
extern Bool is_program_allocation ( void *block );
#endif /* CHECK_OBJECT_RC_REF */

/* Functions directly exported from the allocator: */

extern void mem_dump_data(strbuf_t *sbuf);
extern void mem_dinfo_data(svalue_t *svp, int value);
extern void mem_consolidate (Bool force);

#ifdef GC_SUPPORT
extern void mem_clear_ref_flags(void);
extern void mem_free_unrefed_memory(void);
#endif /* GC_SUPPORT */

/* --- Associated functions --- */

#if defined(MALLOC_TRACE)
#define string_copy(s) string_copy_traced(s, __FILE__ "::string_copy", __LINE__)
#else
#define string_copy(s) string_copy_traced(s)
#endif

extern char * string_copy_traced(const char *str MTRACE_DECL) MALLOC;
extern void dump_lpc_trace (int d, void *p);
extern void dump_malloc_trace (int d, void *adr);

extern void get_stack_direction (void);
extern void assert_stack_gap(void);
extern void reserve_memory (void);

#endif /* XALLOC_H__ */
