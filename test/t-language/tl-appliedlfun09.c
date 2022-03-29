#pragma warn_applied_functions

/* Wrong argument type. */
int id(object name)
{
    return 0;
}

int run_test()
{
    return __MASTER_OBJECT__.warning_occured();
}
