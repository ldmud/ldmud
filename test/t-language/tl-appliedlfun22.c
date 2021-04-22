#pragma warn_applied_functions

/* Return type needed. */
clean_up(int arg)
{
    return 0;
}

int run_test()
{
    return __MASTER_OBJECT__.warning_occured();
}
