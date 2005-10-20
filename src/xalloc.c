/*---------------------------------------------------------------------------
 * The Memory Allocator wrapper.
 *
 *---------------------------------------------------------------------------
 * This file #includes the source for the memory allocator selected in
 * in config.h. If sysmalloc is selected, this file provides the basic
 * xalloc()... implementation using malloc()... .
 *
 * It is the task of the memory allocator source included to provide
 * simulations of malloc()... where required.
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include "xalloc.h"

#include "interpret.h"
#include "simulate.h"

/*-------------------------------------------------------------------------*/

/* -- Global Variables/Arguments dealing with memory -- */

Bool out_of_memory = MY_FALSE;              /* True: we are out of memory */
int malloc_privilege = MALLOC_USER; /* Privilege for next allocation */

char *reserved_user_area   = NULL;  /* Reserved memory areas */
char *reserved_master_area = NULL;
char *reserved_system_area = NULL;

mp_int reserved_user_size   = RESERVED_USER_SIZE;   /* The reserved sizes */
mp_int reserved_master_size = RESERVED_MASTER_SIZE;
mp_int reserved_system_size = RESERVED_SYSTEM_SIZE;

#ifdef MAX_MALLOCED
mp_int max_malloced       = MAX_MALLOCED;           /* Allocation limits */
#endif

/*-------------------------------------------------------------------------*/

/* Include the allocator source */

#ifdef MALLOC_smalloc

#include "smalloc.c"

#endif /* MALLOC_smalloc */

/* ======================================================================= */

#ifdef MALLOC_sysmalloc

/*-------------------------------------------------------------------------*/
POINTER
xalloc (size_t size)

/* Allocate <size> bytes of memory like malloc() does.
 * This function catches out of memory situations and tries to recover
 * from them by using the reserved memory areas. If it totally runs
 * out of memory, the program exit()s with code 3.
 */

{
    char *p;
    static int going_to_exit;

    if (going_to_exit)
        exit(3);
    if (size == 0)
        fatal("Tried to allocate 0 bytes.\n");
#ifdef MAX_MALLOCED
    {
        static mp_int total_malloced = 0;

        if ((total_malloced += size + sizeof(p_int)) > max_malloced)
        {
            total_malloced -= size + sizeof(p_int);
            p = NULL;
        }
        else
        {
            p = malloc(size);
        }
    }
#else
    p = malloc(size);
#endif

    if (p == NULL)
    {
        if (reserved_user_area) {
            free(reserved_user_area);
            reserved_user_area = NULL;
            p = "Temporary out of MEMORY. Freeing reserve.\n";
            write(1, p, strlen(p));
            return xalloc(size);        /* Try again */
        }
        if (malloc_privilege >= MALLOC_MASTER && reserved_master_area) {
            free(reserved_master_area);
            reserved_master_area = NULL;
            p = "Temporary out of MEMORY. Freeing master reserve.\n";
            write(1, p, strlen(p));
            return xalloc(size);        /* Try again */
        }
        if (malloc_privilege >= MALLOC_SYSTEM && reserved_system_area) {
            free(reserved_system_area);
            reserved_system_area = NULL;
            p = "Temporary out of MEMORY. Freeing system reserve.\n";
            write(1, p, strlen(p));
            return xalloc(size);        /* Try again */
        }
        /* We can hardly survive out of memory without the garbage collector */
        going_to_exit = MY_TRUE;
        p = "Totally out of MEMORY.\n";
        write(1, p, strlen(p));
        (void)dump_trace(MY_FALSE);
        exit(2);
    }
    return p;
} /* xalloc() */

#endif /* MALLOC_sysmalloc */

/* ======================================================================= */

#ifndef string_copy

/*-------------------------------------------------------------------------*/
char *
string_copy (const char *str)

/* string_copy() acts like strdup(), but uses xalloc() instead of malloc().
 * It also has the additional bonus that it can trace file/line of the
 * calling place if MALLOC_TRACE is defined (and the allocator supports
 * tracing).
 */

{
    char *p;

    p = xalloc(strlen(str)+1);
    if (p) {
        (void)strcpy(p, str);
    }
    return p;
}

#endif /* string_copy */

/***************************************************************************/

