#pragma warn_applied_functions

/* Optional arguments are okay. */
int id(string name, int count = 0)
{
    return 0;
}

int run_test()
{
    return !__MASTER_OBJECT__.warning_occured();
}
