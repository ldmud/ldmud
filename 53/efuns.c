/*---------------------------------------------------------------------------
 * Various Efuns.
 *
 *---------------------------------------------------------------------------
 * This file acts as a repository for various (new) efuns. Over the time
 * it will probably grow large enough to justify a split into several files.
 *
 * The implemented efuns, sorted by topic, are:
 *
 * Strings:
 *    make_shared_string()
 *    upper_case()
 *
 * Objects:
 *    all_environment()
 *
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include <ctype.h>
#include <stdio.h>

#define USES_SVALUE_STRLEN
#include "efuns.h"

#include "array.h"
#include "datatypes.h"
#include "interpret.h"
#include "mapping.h"
#include "simulate.h"
#include "smalloc.h"
#include "stralloc.h"

/*=========================================================================*/
/*                              STRINGS                                    */

/*-------------------------------------------------------------------------*/
struct svalue *
f_make_shared_string (struct svalue *sp)

/* EFUN make_shared_string()
 *
 *    string make_shared_string(string s)
 *
 * If the passed string <s> is not shared, the efun enters it into
 * the shared string table and returns the shared version. Else the
 * passed string is returned.
 *
 * TODO: Improve the string handling of the driver so that this efun
 * TODO:: becomes unnecessary.
 */

{
    if (sp->type != T_STRING)
    {
        bad_xefun_arg(1, sp);
        /* NOTREACHED */
        return sp;
    }

    if (sp->x.string_type != STRING_SHARED)
    {
        char *s = make_shared_string(sp->u.string);

        if (sp->x.string_type == STRING_MALLOC)
            xfree(sp->u.string);
        sp->u.string = s;
        sp->x.string_type = STRING_SHARED;
    }

    return sp;
} /* f_make_shared_string() */

/*-------------------------------------------------------------------------*/
struct svalue *
f_upper_case (struct svalue *sp)

/* EFUN upper_case()
 *
 *    string upper_case (string s)
 *
 * Convert all characters in <s> to upper case and return the new string.
 */
 
{
    char *str, *s, *d, c;
    
    if (sp->type != T_STRING)
    {
        bad_xefun_arg(1, sp);
        /* NOTREACHED */
        return sp;
    }
    
    str = xalloc(svalue_strlen(sp)+1);
    if (!str)
    {
        error("Out of memory.\n");
        /* NOTREACHED */
        return sp;
    }
    
    for (s = sp->u.string, d = str; '\0' != (c = *s++) ; )
    {
        if (islower((unsigned char)c))
            c = toupper(c);
        *d++ = c;
    }

    *d = c;
    free_string_svalue(sp);
    sp->type = T_STRING;
    sp->x.string_type = STRING_MALLOC;
    sp->u.string = str;

    return sp;
}

/*=========================================================================*/
/*                              OBJECTS                                    */

/*-------------------------------------------------------------------------*/
struct svalue *
x_all_environment (struct svalue *sp, int numarg)

/* EFUN all_environment()
 *
 *    object * all_environment(object o)
 *
 * Return an array with all environments of object <o> in 'outgoing'
 * order. If <o> has no environment, 0 is returned.
 *
 * The caller checked the correctness of the arguments.
 */

{
    struct object *o;


    /* Get the arg from the stack, if any */
    if (numarg)
    {
        o = sp->u.ob;
        free_object_svalue(sp);
    }
    else
    {
        o = current_object;
        sp++;
    }


    /* Default return value: 0 */
    sp->type = T_NUMBER;
    sp->u.number = 0;

    if (!(o->flags & O_DESTRUCTED))
    {
        mp_int num;
        struct object *env;
        struct vector *v;
        struct svalue *svp;
        
        /* Count the number of environments */
        for ( num = 0, env = o->super
            ; NULL != env
            ; num++, env = env->super)
            NOOP;

        if (num)
        {
            /* Get the array and fill it */
            v = allocate_uninit_array(num);
            for ( svp = v->item, env = o->super
                ; NULL != env
                ; svp++, env = env->super)
            {
                svp->type = T_OBJECT;
                svp->u.ob = env;
            }

            /* Put the result on the stack and return */
            sp->type = T_POINTER;
            sp->u.vec = v;
        }
    }

    return sp;
}

/*=========================================================================*/
/***************************************************************************/

