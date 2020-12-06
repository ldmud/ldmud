/* This test tries to crash the driver by referencing
 * destructed objects multiple times.
 */
#include "/inc/base.inc"

int dest_prev()
{
    destruct(previous_object());
    return 1;
}

void kill_me()
{
    object ob = clone_object(this_object());
    (({ob})*4096)->dest_prev();
}

void run_test()
{
    msg("\nRunning test for references to destructed objects:\n"
          "--------------------------------------------------\n");

    clone_object(this_object()).kill_me();

    /* Clear the trace log. */
    foreach(int i: 4096)
        funcall(1,2,3);

    shutdown(0);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
