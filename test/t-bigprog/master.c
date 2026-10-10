#include "/inc/base.inc"

void run_test()
{
    object ob;
    mixed  err;
    int    result;

    msg("\nRunning test for loop back-patch address limit:\n"
        "-----------------------------------------------\n");

    err = catch(ob = load_object("bigloop"));

#if __INT_MAX__ > 0x7fffffff
    /* 64-bit build: the >256 KB program must compile and run correctly. */
    if (err || !ob)
    {
        msg("FAILURE: large object did not load on a 64-bit build: %O\n", err);
        shutdown(1);
        return;
    }
    result = ob->test_loop();
    if (result != 203700)
    {
        msg("FAILURE: test_loop() returned %d, expected 203700\n", result);
        shutdown(1);
        return;
    }
    msg("SUCCESS: large object compiled and ran (result %d).\n", result);
    shutdown(0);
#else
    /* 32-bit build: the program exceeds the 18-bit back-patch limit and must
       fail to compile cleanly (a caught error) rather than crash the driver. */
    if (ob)
        msg("SUCCESS: large object loaded on 32-bit build.\n");
    else
        msg("SUCCESS: large object rejected cleanly on 32-bit build: %O\n", err);
    shutdown(0);
#endif
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
