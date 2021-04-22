#pragma warn_applied_functions

/* H_COMMAND hook, wrong argument types. */
protected int command(varargs int* args)
{
    return 0;
}

int run_test()
{
    return __MASTER_OBJECT__.warning_occured();
}
