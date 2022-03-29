#pragma warn_applied_functions

/* Optional arguments are okay. */
int id(varargs string* name)
{
    return 0;
}

int run_test()
{
    return !__MASTER_OBJECT__.warning_occured();
}
