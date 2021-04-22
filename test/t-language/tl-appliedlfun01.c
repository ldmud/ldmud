#pragma warn_applied_functions

/* Everything okay, there should be no warning. */
int id(string str)
{
    return 0;
}

int run_test()
{
    return !__MASTER_OBJECT__.warning_occured();
}
