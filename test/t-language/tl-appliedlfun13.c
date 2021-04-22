#pragma warn_applied_functions

/* H_COMMAND hook, missing arguments */
protected int command()
{
    return 0;
}

int run_test()
{
    return __MASTER_OBJECT__.warning_occured();
}
