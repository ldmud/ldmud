#include "/inc/base.inc"
#include "/sys/driver_info.h"

/* Regression test: with MALLOC_EXT_STATISTICS the slab allocator's
 * mem_dump_extdata() divided by the number of used slabs of each size
 * class without checking for zero. Freshly booted, some size classes
 * have no slabs at all, so this crashed with SIGFPE.
 *
 * This test must run in a fresh driver (not from t-efuns.c), because a
 * busy driver may have used every size class already.
 */

string *epilog(int eflag)
{
    mixed s;

    msg("\nRunning test for driver_info(DI_STATUS_TEXT_MALLOC_EXTENDED):\n"
          "--------------------------------------------------------------\n");

    s = driver_info(DI_STATUS_TEXT_MALLOC_EXTENDED);
    if (stringp(s) || s == 0)
    {
        msg("Success.\n");
        shutdown(0);
    }
    else
    {
        msg("FAILURE: unexpected result %O.\n", s);
        shutdown(1);
    }
    return 0;
}
