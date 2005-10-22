#ifndef XALLOC_H__
#define XALLOC_H__ 1

#include "driver.h"

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


/* --- SMalloc --- */

#ifdef MALLOC_smalloc

/* xalloc(): normal allocation
 * xalloc_traced(): allocation with given file/line
 * xalloc_pass(): allocation using MTRACE_PASS as file/line args
 */

#if defined(MALLOC_TRACE)

#define xalloc_traced(size, file, line)  smalloc((size), (file), (line))
#define xalloc(size) smalloc((size), __FILE__, __LINE__)

extern POINTER smalloc(size_t, const char *, int);

#define string_copy_traced(s, file, line) smalloc_string_copy(s, file, line)
#define string_copy(s) smalloc_string_copy(s, __FILE__ "::string_copy", __LINE__)
extern char * smalloc_string_copy(const char *, const char *, int);

#else

#define xalloc_traced(size, file, line) smalloc((size))
#define xalloc(size)                    smalloc((size))

extern POINTER smalloc(size_t);

#endif

#define xalloc_pass(size)  smalloc((size) MTRACE_PASS)

extern POINTER rexalloc(POINTER, size_t);
extern POINTER amalloc(size_t);
extern POINTER pxalloc(size_t);
extern void xfree(POINTER);
extern void pfree(POINTER);
extern void afree(POINTER);

#endif /* MALLOC_smalloc */


/* --- System malloc() --- */

#ifdef MALLOC_sysmalloc

#include <stdlib.h>

extern POINTER xalloc(size_t size);

#if defined(MALLOC_TRACE)
#  define xalloc_traced(size,  file, line) xalloc((size))
#  define xalloc_pass(size)  xalloc((size))
#else
#  define xalloc_traced(size,  file, line) xalloc((size))
#  define xalloc_pass(size)  xalloc((size))
#endif

#define xfree    free
#define rexalloc realloc
#define amalloc  xalloc
#define pxalloc  xalloc
#define afree    free
#define pfree    free

#endif /* MALLOC_sysmalloc */


/* --- Associated functions --- */

extern void dump_lpc_trace (int d, void *p);
extern void dump_malloc_trace (int d, void *adr);

extern void get_stack_direction (void);
extern void assert_stack_gap(void);
extern void reserve_memory (void);

#ifndef string_copy

#define string_copy_traced(s, file, line) string_copy(s)
extern char * string_copy(const char *str);

#endif

#endif /* XALLOC_H__ */
