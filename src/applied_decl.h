/*---------------------------------------------------------------------------
 * Applied lfun declarations
 *
 *---------------------------------------------------------------------------
 */

#ifndef APPLIED_DECL_H__
#define APPLIED_DECL_H__

#include "driver.h"
#include "exec.h"
#include "typedefs.h"
#include "svalue.h"

typedef struct applied_decl_s applied_decl_t;

/* --- Applied lfun ---
 *
 * Specification of an applied lfun.
 *
 * Initially master and regular object lfuns are initialized with the function
 * name as a C string. At program startup they are replaced by a tabled
 * string. (We can't reference tabled strings in static initialization, because
 * they are created after program startup.)
 */
struct applied_decl_s
{
    lpctype_t *returntype;      /* Return type of the lfun.                   */

    union
    {
        const char * cname;     /* Name of the lfun as a C string.            */
        int hook;               /* Number of the driver hook.                 */
    };

    const char * vis_name;      /* Name of the visibility.                    */
    funflag_t visibility;       /* These are the disallowed(!) flags.         */
    int num_arg;                /* Number of arguments.                       */
    int arg_index;              /* Index into applied_decl_args.              */

    bool varargs        :1;     /* No restriction on the number of arguments. */
    bool xvarargs       :1;     /* There can be additional arguments.         */
    bool void_allowed   :1;     /* Also no return type is allowed.            */

    /* These are filled & used by the compiler. */
    string_t * name;            /* Name of the lfun.                          */
    applied_decl_t *next;       /* Next entry in hash table.                  */
};

extern applied_decl_t applied_decl_master[];    /* Lfuns for master object.   */
extern applied_decl_t applied_decl_regular[];   /* Lfuns for regular objects. */
extern applied_decl_t applied_decl_hook[];      /* Lfuns for driver hooks.    */

extern lpctype_t* applied_decl_args[];          /* Argument types of lfuns.   */

/* These are for use by the compiler, but declared here, because
 * we determine the size of the table here.
 */
extern applied_decl_t* applied_decl_hash_table[];/* Uninitialized hash table. */
extern int applied_decl_hash_table_mask;         /* The size of the table -1. */

#endif /* APPLIED_DECL_H_ */
