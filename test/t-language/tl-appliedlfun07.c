#pragma warn_applied_functions

/* Optional arguments are okay. */
varargs int id(string name, int count)
{
    return 0;
}

int run_test()
{
    return !__MASTER_OBJECT__.warning_occured();
}
