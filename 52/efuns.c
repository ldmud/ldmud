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
 *
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include <stdio.h>

#include "efuns.h"

#include "array.h"
#include "datatypes.h"
#include "interpret.h"
#include "mapping.h"
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
        bad_xefun_arg(1, sp);

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
/*=========================================================================*/
/***************************************************************************/

