#pragma save_types, rtt_checks
#include "/inc/base.inc"
#include "/inc/msg.inc"

#include "/sys/functionlist.h"
#include "/sys/configuration.h"

/* get_line_number() allocates one incinfo frame per include level
 * while decoding the position of code that lives in an included file.
 * When that allocation fails, it must report the position as unknown
 * instead of dereferencing the NULL pointer.
 *
 * We force the failure with the runtime hard memory limit: probe for
 * the smallest settable limit (the driver rejects limits at or below
 * the current allocation), activate it, and then ask for the line
 * number of a function nested DEPTH includes deep. The decoder has to
 * allocate DEPTH frames, several kilobytes in sum, which is well above
 * whatever headroom the probe left - so one of the allocations fails.
 *
 * A failed configure_driver() leaves the limit unchanged, so every
 * catch() below runs with an unlimited allocator and can report its
 * error safely.
 */

#define DEPTH 12

void cleanup_files()
{
    rm("/tmp-lnoom-ob.c");
    for (int i = 0; i < DEPTH; i++)
        rm(sprintf("/tmp-lnoom-%d.inc", i));
}

void run_test()
{
    int line_normal, line_oom, i, lo, hi;
    object ob;
    string fname, incfile;
    mixed *restore = ({ 0, 0 });   /* preallocated: back to unlimited */
    mixed *setarr  = ({ 0, 0 });   /* preallocated probe/set array */

    msg("\nRunning test for get_line_number() under memory pressure:\n"
          "---------------------------------------------------------\n");

    /* Build the nested includes at runtime. */
    for (i = 0; i < DEPTH; i++)
    {
        string body = sprintf("int fun_%d() { return %d; }\n", i, i);
        if (i < DEPTH - 1)
            body = sprintf("%s#include \"tmp-lnoom-%d.inc\"\n", body, i + 1);
        write_file(sprintf("/tmp-lnoom-%d.inc", i), body, 1);
    }
    write_file("/tmp-lnoom-ob.c", "#include \"tmp-lnoom-0.inc\"\n", 1);
    ob = load_object("/tmp-lnoom-ob");

    fname = sprintf("fun_%d", DEPTH - 1);
    line_normal = function_exists(fname, ob, FEXISTS_LINENO);
    incfile = function_exists(fname, ob, FEXISTS_FILENAME);
    msg("normal: %s at line %d in %s\n", fname, line_normal, incfile);

    if (line_normal != 1
     || incfile != sprintf("/tmp-lnoom-ob.c (/tmp-lnoom-%d.inc)", DEPTH - 1))
    {
        msg("FAILURE: unexpected line info for the nested function.\n");
        cleanup_files();
        shutdown(1);
        return;
    }

    /* Find the smallest settable hard limit. */
    lo = 1; hi = 0x40000000;
    while (hi - lo > 64)
    {
        int mid = lo + (hi - lo) / 2;

        setarr[1] = mid;
        if (catch(configure_driver(DC_MEMORY_LIMIT, setarr); nolog))
            lo = mid;
        else
        {
            configure_driver(DC_MEMORY_LIMIT, restore);
            hi = mid;
        }
    }

    /* Activate it; widen if the usage drifted upwards meanwhile. */
    for (i = 0; ; i++)
    {
        setarr[1] = hi + i * 64;
        if (!catch(configure_driver(DC_MEMORY_LIMIT, setarr); nolog))
            break;
    }

    /* From here to the restore: no allocations except the ones
     * inside get_line_number().
     */
    line_oom = function_exists(fname, ob, FEXISTS_LINENO);

    configure_driver(DC_MEMORY_LIMIT, restore);

    msg("under memory pressure: line %d\n", line_oom);

    cleanup_files();

    if (line_oom != 0)
    {
        msg("FAILURE: expected the position to be unknown (0), got %d.\n"
           , line_oom);
        shutdown(1);
        return;
    }

    msg("Success.\n");
    shutdown(0);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
