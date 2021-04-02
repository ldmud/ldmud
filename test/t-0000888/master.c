#include "/inc/base.inc"
#include "/sys/functionlist.h"
#include "/sys/lpctypes.h"

/* We create an object with a variable.
 * Then we'll swap it out and read the value with variable_list and check
 * for correctness (and for crash obviously).
 */
void run_test()
{
    object ob;

    msg("\nRunning test for #0000888:\n"
          "--------------------------\n");

#if __EFUN_DEFINED__(swap)
    ob = load_object("swap_ob");
    swap(ob, 2);
    mixed *arr = variable_list(ob, RETURN_VARIABLE_VALUE);
    shutdown(arr[0] != 42);
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
