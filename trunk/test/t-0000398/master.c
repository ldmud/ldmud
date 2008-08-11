#include "/inc/base.inc"

void run_test()
{
    msg("\nRunning test for #0000398:\n"
          "--------------------------\n");

    /* Waiting until LDMud is ready for users. */
    call_out("run_test2", 0);
}

void run_test2()
{
    "/player"->start_client();
}

object connect()
{
    return load_object("ed");
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
