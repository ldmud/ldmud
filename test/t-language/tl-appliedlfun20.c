#pragma warn_applied_functions

/* H_CLEAN_UP, valid with int and void. */
void clean_up(int arg)
{
    return;
}

int run_test()
{
    return !__MASTER_OBJECT__.warning_occured();
}
