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
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#define USES_SVALUE_STRLEN
#include "efuns.h"

#include "array.h"
#include "datatypes.h"
#include "interpret.h"
#include "main.h"
#include "mapping.h"
#include "simulate.h"
#include "smalloc.h"
#include "stralloc.h"
#include "swap.h"

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
    ptrdiff_t initial_len;
    
    if (sp->type != T_STRING)
    {
        bad_xefun_arg(1, sp);
        /* NOTREACHED */
        return sp;
    }
    
    /* Find the first non-uppercase character in the string */
    for (s = sp->u.string; '\0' != (c = *s) && !islower((unsigned char)c); s++)
        NOOP;
    
    if ('\0' != *s)  /* there are lowercase characters */
    {
        if (STRING_MALLOC == sp->x.string_type)
        {
            /* MALLOCed strings can be changed in-place */
            for ( ; '\0' != (c = *s); s++)
            {
                if (islower((unsigned char)c))
                    *s = toupper(c);
            }       
        }
        else
        {
            /* Other strings must be duplicated and then changed */
            str = xalloc(svalue_strlen(sp)+1);
            if (!str)
            {
                error("Out of memory.\n");
                /* NOTREACHED */
                return sp;
            }
            
            initial_len = s - sp->u.string;
            /* Copy the initial part */
            if (initial_len)
                memcpy(str, sp->u.string, initial_len);

            /* Copy and change the rest */
            for (d = str + initial_len; '\0' != (c = *s++) ; )
            {
                if (islower((unsigned char)c))
                    c = toupper(c);
                *d++ = c;
            }

            *d = '\0';
            free_string_svalue(sp);
            sp->type = T_STRING;
            sp->x.string_type = STRING_MALLOC;
            sp->u.string = str;
        }
    }
    
    /* That's it */
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
        add_ref(o, "all_environment");
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
                add_ref(env, "all_environment");
            }

            /* Put the result on the stack and return */
            sp->type = T_POINTER;
            sp->u.vec = v;
        }
    }

    if (numarg)
        free_object(o, "all_environment");
    return sp;
}

/*-------------------------------------------------------------------------*/
struct svalue *
f_object_info (struct svalue *sp)

/* EFUN object_info()
 *
 *    mixed * object_info(object o, int type)
 *
 * Return an array with information about the object <o>. The
 * type of information returned is determined by <type>.
 */

{
    struct vector *v;
    struct object *o, *o2, *prev;
    struct program *prog;
    struct svalue *svp;
    int flags, pos;

    /* Test and get the arguments from the stack */
    if (sp[-1].type != T_OBJECT)
    {
        bad_xefun_arg(1, sp);
        /* NOTREACHED */
        return sp;
    }
    o = sp[-1].u.ob;
    
    if (sp->type != T_NUMBER)
    {
        bad_xefun_arg(2, sp);
        /* NOTREACHED */
        return sp;
    }

    /* Depending on the <type> argument, determine the
     * data to return.
     */
    switch(sp->u.number)
    {
    default:
        error("Illegal value %d for object_info().\n", sp->u.number);
        /* NOTREACHED */
        return sp;

    /* --- The basic information from the object structure */
    case 0:
        v = allocate_array(24);
        svp = v->item;

        flags = o->flags;

        svp[ 0].u.number = (flags & O_HEART_BEAT) ? 1 : 0;
        svp[ 1].u.number = (flags & O_IS_WIZARD) ? 1 : 0;
        svp[ 2].u.number = (flags & O_ENABLE_COMMANDS) ? 1 : 0;
        svp[ 3].u.number = (flags & O_CLONE) ? 1 : 0;
        svp[ 4].u.number = (flags & O_DESTRUCTED) ? 1 : 0;
        svp[ 5].u.number = (flags & O_SWAPPED) ? 1 : 0;
        svp[ 6].u.number = (flags & O_ONCE_INTERACTIVE) ? 1 : 0;
        svp[ 7].u.number = (flags & O_APPROVED) ? 1 : 0;
        svp[ 8].u.number = (flags & O_RESET_STATE) ? 1 : 0;
        svp[ 9].u.number = (flags & O_WILL_CLEAN_UP) ? 1 : 0;
        svp[10].u.number = (flags & O_LAMBDA_REFERENCED) ? 1 : 0;
        svp[11].u.number = (flags & O_SHADOW) ? 1 : 0;
        svp[12].u.number = o->total_light;
        svp[13].u.number = o->next_reset;
        svp[14].u.number = o->time_of_ref;
        svp[15].u.number = o->ref;
        svp[16].u.number = o->gigaticks;
        svp[17].u.number = o->ticks;
        svp[18].u.number = O_SWAP_NUM(o);
        svp[19].u.number = O_PROG_SWAPPED(o) ? 1 : 0;
        svp[20].u.number = O_VAR_SWAPPED(o) ? 1 : 0;

        svp[21].type = T_STRING;
        svp[21].x.string_type = STRING_MALLOC;
        svp[21].u.string = string_copy(o->name);

        svp[22].type = T_STRING;
        svp[22].x.string_type = STRING_SHARED;
        svp[22].u.string = o->load_name;
        increment_string_ref(o->load_name);

        for (o2 = o->next_all; o2 && o2->flags & O_DESTRUCTED; )
            o2 = o2->next_all;
        if (o2)
        {
            svp[23].type = T_OBJECT;
            svp[23].u.ob = o2;
            add_ref(o2, "object_info(0)");
        } /* else the element was already allocated as 0 */

        break;
    
    /* --- Position in the object list */
    case 1:
        v = allocate_array(3);
        svp = v->item;

        for (o2 = o->next_all; o2 && o2->flags & O_DESTRUCTED; )
            o2 = o2->next_all;
        if (o2)
        {
            svp[0].type = T_OBJECT;
            svp[0].u.ob = o2;
            add_ref(o2, "object_info(1) next");
        } /* else the element was already allocated as 0 */

        /* Find the non-destructed predecessor of the object */
        if (obj_list == o)
        {
            pos = 0;
            prev = NULL;
        }
        else
        for (prev = NULL, o2 = obj_list, pos = 0; o2; o2 = o2->next_all)
        {
            if (!(o2->flags & O_DESTRUCTED))
            {
                prev = o2;
                pos++;
            }
            if (o2->next_all == o)
                break;
        }
        
        if (o2) /* Found it in the list */
        {
            if (prev)
            {
                svp[1].type = T_OBJECT;
                svp[1].u.ob = prev;
                add_ref(prev, "object_info(1) prev");
            } /* else the element was already allocated as 0 */
        }
        else /* Not found (this shouldn't happen) */
            pos = -1;

        svp[2].u.number = pos;

        break;

    /* --- Memory information */
    case 2:
        v = allocate_array(12);
        svp = v->item;

        if (O_PROG_SWAPPED(o) && load_ob_from_swap(o) < 0)
            error("Out of memory.\n");

        prog = o->prog;
        
        svp[ 0].u.number = prog->ref;

        svp[ 1].type = T_STRING;
        svp[ 1].x.string_type = STRING_MALLOC;
        svp[ 1].u.string = string_copy(prog->name);

        svp[ 2].u.number = (long)(PROGRAM_END(*prog) - prog->program);
          /* Program size */
        svp[ 3].u.number = prog->num_functions;
        svp[ 4].u.number = prog->num_functions * sizeof(uint32)
                        + prog->num_function_names * sizeof(short);
          /* Number of function names and the memory usage */
        svp[ 5].u.number = prog->num_variables;
        svp[ 6].u.number = prog->num_variables * sizeof(struct variable);
          /* Number of variables and the memory usage */
        svp[ 7].u.number = prog->num_strings;
        svp[ 8].u.number = prog->num_strings * sizeof(char*);
          /* Number of strings and the memory usage */
        svp[ 9].u.number = prog->num_inherited;
        svp[10].u.number = prog->num_inherited * sizeof(struct inherit);
          /* Number of inherites and the memory usage */
        svp[11].u.number = prog->total_size;
        break;
    }
    
    free_svalue(sp);
    sp--;
    free_svalue(sp);

    /* Assign the result */
    sp->type = T_POINTER;
    sp->u.vec = v;

    return sp;
}

/*=========================================================================*/
/***************************************************************************/

