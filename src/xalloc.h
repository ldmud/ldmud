#ifndef __XALLOC_H__
#define __XALLOC_H__ 1

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


/* void outofmem(size_t size, const char * txt)
 *   Throw an 'out of memory' error for an allocation of <size> with the
 *   description <txt>.
 */

#define outofmem(size,txt) \
    error("(%s:%d) Out of memory (%lu bytes) for %s\n"\
         , __FILE__, __LINE__, (unsigned long)(size), txt)

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
#ifdef MAX_MALLOCED
extern mp_int max_malloced;
#endif


/* --- SMalloc --- */

#ifdef MALLOC_smalloc

#if defined(MALLOC_TRACE)

#define xalloc_traced(size,  file, line) smalloc((size), (file), (line))
#define xalloc(size) (smalloc((size), __FILE__, __LINE__))

extern POINTER smalloc(size_t, const char *, int);

#define string_copy_traced(s, file, line) (smalloc_string_copy(s, file, line))
#define string_copy(s) (smalloc_string_copy(s, __FILE__ "::string_copy", __LINE__))
extern char * smalloc_string_copy(const char *, const char *, int);

#else

#define xalloc_traced(size,  file, line) smalloc((size))
#define xalloc(size) (smalloc((size)))
extern POINTER smalloc(size_t);

#endif

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
#else
#  define xalloc_traced(size,  file, line) xalloc((size))
#endif

#define xfree    free
#define rexalloc realloc
#define amalloc  xalloc
#define pxalloc  xalloc
#define afree    free
#define pfree    free

#endif /* MALLOC_sysmalloc */


/* --- Associated functions --- */

#ifndef string_copy

#define string_copy_traced(s, file, line) string_copy(s)
extern char * string_copy(const char *str);

#endif

#endif /* XALLOC_H__ */
