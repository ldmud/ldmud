#include "/inc/base.inc"
#include "/sys/configuration.h"
#include "/sys/driver_info.h"

/* Regression test: hitting the hard malloc limit must raise a normal LPC
 * error and leave the driver usable.
 *
 * xalloc()/rexalloc() used to check the limit only after the allocation
 * had been made, and then returned NULL: for xalloc() the block was
 * allocated, counted and never freed (a leak), for rexalloc() the old
 * block had already been freed by mem_realloc(), so the caller was left
 * holding a dangling pointer.
 */

string grow_string()
{
    string s = "x";
    for (int i = 0; i < 30; i++)
        s = s + s;
    return s;
}

string *epilog(int eflag)
{
    int used, before, after;
    mixed err;

    msg("\nRunning test for the hard malloc limit:\n"
          "----------------------------------------\n");

    used = driver_info(DI_SIZE_MEMORY_USED);
    configure_driver(DC_MEMORY_LIMIT, ({ 0, used + 100000 }));

    before = driver_info(DI_SIZE_MEMORY_USED);
    for (int i = 0; i < 10; i++)
        err = catch(grow_string(); nolog);
    after = driver_info(DI_SIZE_MEMORY_USED);

    configure_driver(DC_MEMORY_LIMIT, ({ 0, 0 }));

    msg("error: %O\n", err);
    msg("used before: %d, after 10 failed allocations: %d (%+d)\n"
       , before, after, after - before);

    if (!stringp(err))
    {
        msg("FAILURE: expected an error.\n");
        shutdown(1);
        return 0;
    }

    /* The driver must still be usable afterwards. */
    if (sizeof(allocate(1000)) != 1000)
    {
        msg("FAILURE: driver not usable after the limit was hit.\n");
        shutdown(1);
        return 0;
    }

    msg("Success.\n");
    shutdown(0);
    return 0;
}
