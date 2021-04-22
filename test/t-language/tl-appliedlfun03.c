#pragma warn_applied_functions

/* Wrong visibility. */
protected int id(string str)
{
    return 0;
}

int run_test()
{
    return __MASTER_OBJECT__.warning_occured();
}
