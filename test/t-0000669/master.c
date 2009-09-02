#include "/inc/base.inc"
#include "/inc/sefun.inc"

void run_test()
{
    msg("\nRunning test for #0000669:\n"
          "--------------------------\n");

    call_out(#'shutdown, 1, 1);

    if("/unpriv"->run_test())
        shutdown(1);
    else
    {
        remove_call_out(#'shutdown);
        shutdown(0);
    }
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}

int valid_seteuid(object ob, string str)
{
    return 1;
}
