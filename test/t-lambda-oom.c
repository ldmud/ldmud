#include "/inc/base.inc"
#include "/sys/configuration.h"
#include "/sys/driver_info.h"

/* Regression test: when realloc_values() cannot grow the value block,
 * lambda_error() used to free one svalue more than had been assigned,
 * because insert_value_push() had already taken the new entry out of
 * .values_left. The result was a fatal "Illegal svalue".
 *
 * The hard memory limit is swept over a range so that the growth of the
 * value block fails somewhere in it regardless of the exact allocation
 * sizes of the build. The test insists on having seen that failure, so
 * it cannot pass by never reaching the code path at all.
 */

string *epilog(int eflag)
{
    mixed prog = ({ #', });
    int seen = 0;

    msg("\nRunning test for lambda() cleanup after a failed allocation:\n"
          "-------------------------------------------------------------\n");

    for (int i = 0; i < 200; i++)
        prog += ({ "v" + i });

    for (int margin = 2400; margin <= 4200; margin += 100)
    {
        int used = driver_info(DI_SIZE_MEMORY_USED);
        mixed err;

        configure_driver(DC_MEMORY_LIMIT, ({ 0, used + margin }));
        err = catch(lambda(0, prog); nolog);
        configure_driver(DC_MEMORY_LIMIT, ({ 0, 0 }));

        if (stringp(err) && strstr(err, "for new values") >= 0)
            seen++;
    }

    msg("value block growth failed cleanly %d times.\n", seen);

    if (!seen)
    {
        msg("FAILURE: never reached the failing growth - test is not testing anything.\n");
        shutdown(1);
        return 0;
    }

    msg("Success.\n");
    shutdown(0);
    return 0;
}
