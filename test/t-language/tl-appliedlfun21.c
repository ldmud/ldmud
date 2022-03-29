#pragma warn_applied_functions

/* H_CLEAN_UP, valid with int and void. */
string clean_up(int arg)
{
    return 0;
}

int run_test()
{
    return __MASTER_OBJECT__.warning_occured();
}
