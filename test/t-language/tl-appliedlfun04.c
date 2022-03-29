#pragma warn_applied_functions

/* Wrong number of arguments */
int id()
{
    return 0;
}

int run_test()
{
    return __MASTER_OBJECT__.warning_occured();
}
