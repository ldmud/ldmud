/* This test tries to crash the driver by referencing
 * destructed objects multiple times.
 */
#include "/inc/base.inc"
#include "/inc/deep_eq.inc"

int dest_prev()
{
    destruct(previous_object());
    return 1;
}

int kill_me()
{
    object ob = clone_object(this_object());
    int* result = (({ob})*4096)->dest_prev();

    return deep_eq(result, ({1}) + ({0}) * 4095);
}

void run_test()
{
    int result;

    msg("\nRunning test for references to destructed objects:\n"
          "--------------------------------------------------\n");

    result = clone_object(this_object()).kill_me();

    /* Clear the trace log. */
    foreach(int i: 4096)
        funcall(1,2,3);

    shutdown(!result);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
