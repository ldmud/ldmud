/* Check interactive_info(ob, II_WRITE_BUFFER_FILL). */

#include "/inc/base.inc"
#include "/inc/client.inc"

#include "/sys/interactive_info.h"
#include "/sys/configuration.h"

void run_server()
{
    int max, before, after, drained;

    /* A small socket buffer makes the kernel stop accepting data early,
     * so that the driver has to queue the rest.
     */
    configure_interactive(this_object(), IC_SOCKET_BUFFER_SIZE, 4096);

    max = interactive_info(this_object(), IC_MAX_WRITE_BUFFER_SIZE);
    before = interactive_info(this_object(), II_WRITE_BUFFER_FILL);

    if (before != 0)
    {
        msg("FAILURE: fill is %d on a fresh connection, expected 0.\n", before);
        shutdown(1);
        return;
    }

    /* Write more than any socket send buffer will swallow at once. The
     * remainder has to be queued, because nothing drains the buffer
     * until this execution returns to the backend.
     */
    for (int i = 0; i < 200; i++)
        write("x" * 10000);

    after = interactive_info(this_object(), II_WRITE_BUFFER_FILL);

    if (after <= 0)
    {
        msg("FAILURE: fill is %d after writing 2000000 bytes.\n", after);
        shutdown(1);
        return;
    }

    msg("fill: %d before, %d after writing 2000000 bytes (maximum %d).\n"
       , before, after, max);

    /* Non-interactive objects and the default query have no fill. */
    if (!catch(interactive_info(blueprint(), II_WRITE_BUFFER_FILL); nolog))
    {
        msg("FAILURE: no error for a non-interactive object.\n");
        shutdown(1);
        return;
    }

    if (!catch(interactive_info(0, II_WRITE_BUFFER_FILL); nolog))
    {
        msg("FAILURE: no error for the default value query.\n");
        shutdown(1);
        return;
    }

    /* And it drops again once the data has been sent. */
    call_out("check_drained", 2);
}

void check_drained()
{
    int fill = interactive_info(this_object(), II_WRITE_BUFFER_FILL);

    msg("fill after draining: %d.\n", fill);

    if (fill != 0)
    {
        msg("FAILURE: buffer did not drain.\n");
        shutdown(1);
        return;
    }

    msg("Success.\n");
    shutdown(0);
}

void run_client()
{
    /* Nothing to do - the driver reads our side for us. */
}

void run_test()
{
    msg("\nRunning test for II_WRITE_BUFFER_FILL:\n"
          "---------------------------------------\n");

    connect_self("run_server", "run_client");
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
