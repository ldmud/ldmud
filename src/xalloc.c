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

#include <stdio.h>

#include "xalloc.h"

#include "interpret.h"
#include "simulate.h"
#include "main.h"

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

mp_int min_malloced       = MIN_MALLOCED;           /* Allocation limits */
mp_int min_small_malloced = MIN_SMALL_MALLOCED;     /* Allocation limits */
mp_int max_malloced       = MAX_MALLOCED; 

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
    {
        static mp_int total_malloced = 0;

        if (max_malloced > 0
         && (total_malloced += size + sizeof(p_int)) > max_malloced)
        {
            total_malloced -= size + sizeof(p_int);
            p = NULL;
        }
        else
        {
            p = malloc(size);
        }
    }

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
        (void)dump_trace(MY_FALSE, NULL);
        exit(2);
    }
    return p;
} /* xalloc() */

/*-------------------------------------------------------------------------*/
void
dump_malloc_trace (int d, void *adr)

/* Stand in for the malloc trace dump routine which the stdlib allocator
 * lacks.
 */

{
#define WRITES(d, s) write((d), (s), strlen(s))
    WRITES(d, "No malloc trace.\n");
#undef WRITES
} /* dump_malloc_trace() */

#endif /* MALLOC_sysmalloc */

/* ======================================================================= */

/*-------------------------------------------------------------------------*/
void
reserve_memory (void)

/* Reserve the memory blocks according to reserved_xxx_area, and the
 * min_{small}_malloced limits.
 */

{
    void * ptr;
      
    /* First, check if max_malloced is a sensible value.
     * We overestimate the requirement a bit...
     */
    if (max_malloced > 0)
    {
        mp_int required_mem = 0;
        mp_int required_reserve = 0;

        if (reserved_user_size > 0) required_reserve += reserved_user_size;
        if (reserved_master_size > 0) required_reserve += reserved_master_size;
        if (reserved_system_size > 0) required_reserve += reserved_system_size;

        if (min_malloced > 0)
        {
            if (min_malloced > required_reserve)
                required_mem += min_malloced;
            else
                required_mem += required_reserve + min_malloced;
        }
        else
            required_mem += required_reserve;

        if (min_small_malloced > 0) required_mem += min_small_malloced;

        if (max_malloced < required_mem)
        {
            printf("%s max_malloced is %ld bytes, "
                   "but driver requires %ld bytes.\n"
                  , time_stamp(), (long)max_malloced, (long)required_mem
                  );
            debug_message("%s max_malloced is %ld bytes, "
                          "but driver requires %ld bytes.\n"
                         , time_stamp(), (long)max_malloced, (long)required_mem
                         );
            max_malloced = required_mem;
        }
    }

    if (min_malloced > 0)
    {
        ptr = xalloc(min_malloced);

        if (ptr)
            xfree(ptr);
        else
        {
            printf("%s Failed to allocate MIN_MALLOCED block of %ld bytes.\n"
                  , time_stamp(), (long)min_malloced);
            debug_message("%s Failed to allocate MIN_MALLOCED block of %ld bytes.\n"
                  , time_stamp(), (long)min_malloced);
        }
    }

#ifdef MALLOC_smalloc

    if (min_small_malloced > 0)
        small_chunk_size = min_small_malloced;

#endif /* MALLOC_smalloc */

    if (reserved_system_size > 0)
    {
        reserved_system_area = xalloc((size_t)reserved_system_size);
        if (reserved_system_area == NULL)
        {
            printf("%s Failed to allocate system reserve of %ld bytes.\n"
                  , time_stamp(), (long)reserved_system_area);
            debug_message("%s Failed to allocate system reserve of %ld bytes.\n"
                  , time_stamp(), (long)reserved_system_area);
        }
    }
    if (reserved_master_size > 0)
    {
        reserved_master_area = xalloc((size_t)reserved_master_size);
        if (reserved_master_area == NULL)
        {
            printf("%s Failed to allocate master reserve of %ld bytes.\n"
                  , time_stamp(), (long)reserved_master_area);
            debug_message("%s Failed to allocate master reserve of %ld bytes.\n"
                  , time_stamp(), (long)reserved_master_area);
        }
    }
    if (reserved_user_size > 0)
    {
        reserved_user_area = xalloc((size_t)reserved_user_size);
        if (reserved_user_area == NULL)
        {
            printf("%s Failed to allocate user reserve of %ld bytes.\n"
                  , time_stamp(), (long)reserved_user_area);
            debug_message("%s Failed to allocate user reserve of %ld bytes.\n"
                  , time_stamp(), (long)reserved_user_area);
        }
    }
} /* reserve_memory() */

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

