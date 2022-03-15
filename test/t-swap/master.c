#include "/sys/object_info.h"

#include "/inc/base.inc"
#include "/inc/gc.inc"

/* We create objects, swap them out and back in and check. */
void run_test()
{
    int errors;

    msg("\nRunning test for swapping:\n"
          "--------------------------\n");

#if __EFUN_DEFINED__(swap)
    foreach(string file: get_dir("/swap-*.c"))
    {
        object ob;
        int size;
        bytes dummy;

        msg("Running Test %s...", file[0..<3]);

        ob = load_object(file[0..<3]);
        ob->prepare();

        size = object_info(ob, OI_PROG_SIZE_TOTAL);
        swap(ob);
        dummy = b" "*(size-40);

        if (!object_info(ob, OI_PROG_SWAPPED))
        {
            errors++;
            msg(" FAILURE (program not swapped)!\n");
        }
        else if (!object_info(ob, OI_VAR_SWAPPED))
        {
            errors++;
            msg(" FAILURE (variables not swapped)!\n");
        }
        else if (!ob.check())
        {
            errors++;
            msg(" FAILURE (check failed)!\n");
        }
        else
            msg(" Success.\n");
    }
    if (errors)
        shutdown(1);
    else
        start_gc(#'shutdown);
#else
    msg("swap() efun not available, skipping test.");
    shutdown(0);
#endif

    return 0;
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
