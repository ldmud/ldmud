#ifndef __DRIVER_H__
#define __DRIVER_H__

/*------------------------------------------------------------------
 * Global mandatory include file.
 *
 * It contains various global macros and declarations, and takes
 * care of the proper inclusion of the configuration/portability
 * include files.
 *------------------------------------------------------------------
 */

#include "config.h"

/* Verify some of the definitions in config.h */

#ifndef MASTER_NAME
#ifdef COMPAT_MODE
#define MASTER_NAME "obj/master"
#else
#define MASTER_NAME "secure/master"
#endif
#endif

#if defined(NATIVE_MODE) && !defined(EUIDS)
#define EUIDS
#endif

#if defined(ENFORCE_ONE_PORT)
#undef MAXNUMPORTS
#endif

#if !defined(CATCH_UDP_PORT)
#undef UDP_SEND
#endif

#include "machine.h"
#include "port.h"

/* TODO: this ctype-stuff might go into lex.h (impl in efun_defs.c) */
#define _MCTe 0x01 /* escaped character in save/restore object. */
#define _MCTd 0x02 /* numeric digit                */


#define _MCTs 0x10 /* whitespace EXCLUDING '\n'        */

#define _MCTx 0x40 /* hexadecimal                */
#define _MCTa 0x80 /* alphanumeric or '_'         */
extern unsigned char _my_ctype[];
#define isescaped(c) (_my_ctype[(unsigned char)(c)]&_MCTe)
#define isalunum( c) (_my_ctype[(unsigned char)(c)]&_MCTa)
#define lexdigit( c) (_my_ctype[(unsigned char)(c)]&_MCTd)

#ifndef MAXINT
#    define MAXINT (0x7fffffff)
#endif

/* Boolean values */
#define MY_TRUE  (1)
#define MY_FALSE (0)

/* A define to point out empty loop bodies. */
#define NOOP

/* TODO: -> mallocator */
#if defined(MALLOC_smalloc) && !defined(MAKE_FUNC)
#    if !defined( SMALLOC ) || defined( SBRK_OK )
#        undef malloc
#    endif
#    undef calloc
#    ifdef SBRK_OK
#        define amalloc  malloc
#        define afree    free
#    else  /* SBRK_OK */
         POINTER amalloc(size_t);
         POINTER smalloc_calloc(size_t, size_t);
         FREE_RETURN_TYPE afree(POINTER);
#        ifndef SMALLOC
#            define malloc  amalloc
#        endif
#        define calloc  smalloc_calloc
#        define free    afree
#    endif /* SBRK_OK */
     void xfree(POINTER);
     POINTER rexalloc(POINTER, size_t);
#    if MALLOC_ALIGN > SIZEOF_P_INT || FREE_NULL_POINTER
#        define PFREE_RETURN_TYPE void
#        define PFREE_RETURN return;
         PFREE_RETURN_TYPE pfree(POINTER);
#    else  /* MALLOC_ALIGN */
#        define PFREE_RETURN_TYPE FREE_RETURN_TYPE
#        define PFREE_RETURN FREE_RETURN
#        define pfree  afree
#    endif /* MALLOC_ALIGN */
     POINTER permanent_xalloc(size_t);
     PFREE_RETURN_TYPE pfree(POINTER);
#else  /* MALLOC_smalloc && !MAKE_FUNC */
#    define xfree            free
#    define rexalloc         realloc
#    define amalloc          xalloc
#    define permanent_xalloc xalloc
#    define afree            free
#    define pfree            free
#endif /* MALLOC_smalloc && !MAKE_FUNC */

#if defined(MALLOC_smalloc) && defined(SMALLOC_TRACE)
#    define xalloc(size) (smalloc((size), __FILE__, __LINE__))
     POINTER smalloc(size_t, const char *, int);
#endif /* SMALLOC_TRACE */
#ifndef xalloc
     POINTER xalloc(size_t);
#endif

/* TODO: MALLOC_* -> malloc.h */
#define MALLOC_USER   (0)
#define MALLOC_MASTER (1)
#define MALLOC_SYSTEM (2)

#endif /* __DRIVER_H__ */
